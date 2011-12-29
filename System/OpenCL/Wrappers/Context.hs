module System.OpenCL.Wrappers.Context 
    (clCreateContext
    ,clCreateContextFromType
    ,clRetainContext
    ,clReleaseContext
    ,clGetContextInfo
    )
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign.Ptr(Ptr, nullPtr, nullFunPtr,ptrToIntPtr)
import Foreign.Marshal.Array(withArray)


clCreateContext :: [ContextProperties] -> [DeviceID] -> Maybe ContextCallback -> Ptr () -> IO Context
clCreateContext props devices pfn_notify user_dat =
    withArrayNull0 (ContextProperties$ptrToIntPtr nullPtr) props $ \propertiesP -> withArray devices $ \devicesP -> do
        fptr <- maybe (return nullFunPtr) wrapContextCallback pfn_notify
        wrapErrorResult $ raw_clCreateContext propertiesP (fromIntegral devicesN) devicesP fptr user_dat
    where devicesN = length devices
          
clCreateContextFromType :: [ContextProperties] -> DeviceType -> Maybe ContextCallback -> Ptr () -> IO Context
clCreateContextFromType props (DeviceType device_type) pfn_notify user_data = withArrayNull0(ContextProperties$ptrToIntPtr nullPtr) props $ \propertiesP -> do
    fptr <- maybe (return nullFunPtr) wrapContextCallback pfn_notify
    wrapErrorResult $ raw_clCreateContextFromType propertiesP device_type fptr user_data
    
clRetainContext :: Context -> IO ()
clRetainContext ctx = wrapError (raw_clRetainContext ctx)

clReleaseContext :: Context -> IO ()
clReleaseContext ctx = wrapError (raw_clReleaseContext ctx)

clGetContextInfo :: Context -> ContextInfo -> IO CLContextInfoRetval
clGetContextInfo ctx c@(ContextInfo param_name) = do
    (x,size) <- wrapGetInfo (raw_clGetContextInfo ctx param_name)
    case () of
        ()
            | c == clContextReferenceCount -> peekOneInfo ContextInfoRetvalCLuint x
            | c == clContextDevices        -> peekManyInfo ContextInfoRetvalDeviceIDList x size
            | c == clContextProperties     -> peekManyInfo ContextInfoRetvalContextPropertiesList x size
            | otherwise                    -> badArgument "clGetContextInfo" c
