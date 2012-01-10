{-# LANGUAGE ExistentialQuantification #-}
{-| Some helper functions that may or may not be useful to anyone. -}
module System.OpenCL.Wrappers.Helpers
    (createSyncKernel
    ,createAsyncKernelWithParams
    ,buildProgram
    ,pushKernelParams
    ,pushKernelParams'
    ,errorCodeToString
    ,KernelParameter(..))
where

import System.OpenCL.Wrappers.Kernel
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.ProgramObject
import System.OpenCL.Wrappers.EventObject
import System.OpenCL.Wrappers.FlushFinish
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

data KernelParameter = forall s. Storable s => KParam s

pushKernelParams' :: Kernel -> CLuint -> [KernelParameter] -> IO (Maybe ErrorCode)
pushKernelParams' kernel argNum ((KParam x):xs) = 
    withArray [x] (\y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)) >>=
        maybe (pushKernelParams' kernel (argNum + 1) xs) (return.Just)
pushKernelParams' _ _ _ = return Nothing

pushKernelParams :: Storable b => Kernel -> CLuint -> [b] -> IO (Maybe ErrorCode)
pushKernelParams kernel argNum (x:xs) = 
    withArray [x] (\y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)) >>=
        maybe (pushKernelParams kernel (argNum + 1) xs) (return.Just)
pushKernelParams _ _ _ = return Nothing

syncKernelFun :: forall b. Storable b => CLuint -> Kernel -> CommandQueue -> [CLsizei] -> [CLsizei] -> [b] -> IO (Maybe ErrorCode)
syncKernelFun _ kernel queue a b [] =
        clEnqueueNDRangeKernel queue kernel a b [] >>=
            either (return.Just) (\ev -> clReleaseEvent ev >>=
                maybe (clFinish queue >>= maybe (return Nothing) (return.Just)) (return.Just))
syncKernelFun argNum kernel queue a b (x:xs) =
        withArray [x] (\y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)) >>=
            maybe (syncKernelFun (argNum + 1) kernel queue a b xs) (return.Just)

createSyncKernel :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> IO (Either ErrorCode ([b] -> IO (Maybe ErrorCode)))
createSyncKernel program queue initFun globalWorkRange localWorkRange =
        clCreateKernel program initFun >>=
            either (return.Left) (\k -> return.Right $ syncKernelFun 0 k queue (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange))

createAsyncKernelWithParams :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> [b] -> IO (Either ErrorCode ([Event] -> IO (Either ErrorCode Event)))
createAsyncKernelWithParams program queue initFun globalWorkRange localWorkRange params =
        clCreateKernel program initFun >>=
            either (return.Left) (\k -> pushKernelParams k 0 params >>=
                maybe (return.Right $ clEnqueueNDRangeKernel queue k (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange)) (return.Left)) 

buildProgram :: String -> String -> Context -> DeviceID -> IO (Either (ErrorCode, String) Program)
buildProgram source opts context dID =
    clCreateProgramWithSource context source >>=
        either (\x -> return $ Left (x, "")) (\program -> clBuildProgram program [dID] opts Nothing nullPtr >>=
            maybe (return $ Right program) (\x -> do
                y <- fmap Left $ reportBuildFailure program dID x
                _ <- clReleaseProgram program
                return y))

reportBuildFailure :: Program -> DeviceID -> ErrorCode -> IO (ErrorCode,String)
reportBuildFailure program dID eCode = clGetProgramBuildInfo program dID clProgramBuildLog >>=
        either (\x -> return (x,"")) (\x -> case x of
            (ProgramBuildInfoRetvalString s) -> return (eCode,s)
            _                                -> undefined) 

errorCodeToString :: ErrorCode -> String
errorCodeToString e
  | e == clSuccess = "Success"
  | e == clDeviceNotFound = "Device not found"
  | e == clDeviceNotAvailable = "Device not available"
  | e == clCompilerNotAvailable = "Compiler not available"
  | e == clMemObjectAllocationFailure = "Memory object allocation failure"
  | e == clOutOfResources = "Out of resources"
  | e == clOutOfHostMemory = "Out of host memory"
  | e == clProfilingInfoNotAvailable = "Profiling information not available"
  | e == clMemCopyOverlap = "Memory copy overlap"
  | e == clImageFormatMismatch = "Image format mismatch"
  | e == clImageFormatNotSupported = "Image format not supported"
  | e == clMapFailure = "Map failure"
  | e == clInvalidValue = "Invalid value"
  | e == clInvalidDeviceType = "Invalid device type"
  | e == clInvalidPlatform = "Invalid platform"
  | e == clInvalidDevice = "Invalid device"
  | e == clInvalidContext = "Invalid context"
  | e == clInvalidQueueProperties = "Invalid queue properties"
  | e == clInvalidCommandQueue = "Invalid command queue"
  | e == clInvalidHostPtr = "Invalid host pointer"
  | e == clInvalidImageFormatDescriptor = "Invalid image format descriptor"
  | e == clInvalidImageSize = "Invalid image size"
  | e == clInvalidSampler = "Invalid sampler"
  | e == clInvalidBinary = "Invalid binary"
  | e == clInvalidBuildOptions = "Invalid build options"
  | e == clInvalidProgram = "Invalid program"
  | e == clInvalidProgramExecutable = "Invalid program executable"
  | e == clInvalidKernelName = "Invalid kernel name"
  | e == clInvalidArgIndex = "Invalid argument index"
  | e == clInvalidArgValue = "Invalid argument value"
  | e == clInvalidArgSize = "Invalid argument size"
  | e == clInvalidKernelArgs = "Invalid kernel arguments"
  | e == clInvalidWorkDimension = "Invalid work dimension"
  | e == clInvalidWorkGroupSize = "Invalid work group size"
  | e == clInvalidWorkItemSize = "Invalid work item size"
  | e == clInvalidGlobalOffset = "Invalid global offset"
  | e == clInvalidEventWaitList = "Invalid event wait list"
  | e == clInvalidEvent = "Invalid event"
  | e == clInvalidOperation = "Invalid operation"
  | e == clInvalidGLObject = "Invalid OpenGL object"
  | e == clInvalidBufferSize = "Invalid buffer size"
  | e == clInvalidMipLevel = "Invalid MIP level"
  | otherwise = "Unknown error"

