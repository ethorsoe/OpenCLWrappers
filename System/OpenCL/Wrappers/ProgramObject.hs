module System.OpenCL.Wrappers.ProgramObject 
    (clCreateProgramWithSource
    ,clCreateProgramWithBinary
    ,clRetainProgram
    ,clReleaseProgram
    ,clBuildProgram
    ,clUnloadCompiler
    ,clGetProgramInfo
    ,clGetProgramBuildInfo)
where

import Prelude hiding(catch)
import Control.Monad.Cont
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign(alloca,peek,withArray,Ptr,nullFunPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.C
import Control.Applicative
import Control.Exception
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Internal as SBS

clCreateProgramWithSource :: Context -> String -> IO Program
clCreateProgramWithSource ctx source_code = do
    let count = length strings
        strings = lines source_code
        lengths = (fromIntegral . length) <$> strings
    withArray lengths $ \lengthsP -> 
        withCStringArray0 strings $ \stringsP -> 
            wrapErrorResult $ raw_clCreateProgramWithSource ctx (fromIntegral count) stringsP lengthsP

clCreateProgramWithBinary :: Context -> [(DeviceID,SBS.ByteString)] -> IO Program
clCreateProgramWithBinary context devbin_pair = 
    withArray (map (fromIntegral . SBS.length) bins) $ \lengths -> 
    withArray (map (unsafeForeignPtrToPtr . bsPtr) bins) $ \binaries ->
    withArray device_list $ \devices -> 
    alloca $ \binary_status ->
    alloca $ \errcode_ret -> do
        program <- raw_clCreateProgramWithBinary context (fromIntegral num_devices) devices lengths binaries binary_status errcode_ret
        errcode <- ErrorCode <$> peek errcode_ret
        binstatus <- ErrorCode <$> peek binary_status
        when (errcode /= clSuccess) $ throwIO (CLError errcode)
        when (binstatus /= clSuccess) $ throwIO (CLError binstatus)
        return program
    where bsPtr (SBS.PS p _ _) = p
          num_devices = length device_list
          (device_list,bins) = unzip devbin_pair
        
clRetainProgram :: Program -> IO ()
clRetainProgram prog = wrapError $ raw_clRetainProgram prog

clReleaseProgram :: Program -> IO ()
clReleaseProgram prog = wrapError $ raw_clReleaseProgram prog

clBuildProgram :: Program -> [DeviceID] -> String -> Maybe BuildProgramCallback -> Ptr () -> IO ()
clBuildProgram program devices ops pfn_notifyF user_data = 
    withArray devices $ \device_list -> 
    withCString ops $ \options -> do 
        pfn_notify <- maybe (return nullFunPtr) wrapBuildProgramCallback pfn_notifyF
        wrapError $ raw_clBuildProgram program (fromIntegral num_devices) device_list options pfn_notify user_data
    where num_devices = length devices   

clUnloadCompiler :: IO ()
clUnloadCompiler = wrapError raw_clUnloadCompiler

clGetProgramInfo :: Program -> ProgramInfo -> IO CLProgramInfoRetval
clGetProgramInfo program c@(ProgramInfo param_name) = do
    (x,size) <- wrapGetInfo $ raw_clGetProgramInfo program param_name
    case () of
        ()
            | c == clProgramReferenceCount -> peekOneInfo ProgramInfoRetvalCLUint x
            | c == clProgramContext        -> peekOneInfo ProgramInfoRetvalContext x
            | c == clProgramNumDevices     -> peekOneInfo ProgramInfoRetvalCLUint x
            | c == clProgramDevices        -> peekManyInfo ProgramInfoRetvalDeviceIDList x size
            | c == clProgramSource         -> peekStringInfo ProgramInfoRetvalString x
            | c == clProgramBinarySizes    -> peekManyInfo ProgramInfoRetvalCLsizeiList x size
            | c == clProgramBinaries       -> peekManyInfo ProgramInfoRetvalPtrList x size
            | otherwise                    -> badArgument "clGetProgramInfo" c

clGetProgramBuildInfo :: Program -> DeviceID -> ProgramBuildInfo -> IO CLProgramBuildInfoRetval
clGetProgramBuildInfo program devID c@(ProgramBuildInfo param_name) = do
    (x,_) <- wrapGetInfo $ raw_clGetProgramBuildInfo program devID param_name
    case () of
        ()
            | c == clProgramBuildStatus  -> peekOneInfo (ProgramBuildInfoRetvalBuildStatus . BuildStatus) x
            | c == clProgramBuildOptions -> peekStringInfo ProgramBuildInfoRetvalString x
            | c == clProgramBuildLog     -> peekStringInfo ProgramBuildInfoRetvalString x
            | otherwise                  -> badArgument "clGetProgramBuildInfo" c
