{-|
    Conforms to section 4.3 of the OpenCL 1.0 specification
-}
module System.OpenCL.Wrappers.Context 
    (clCreateContext
    ,clCreateContextFromType
    ,clRetainContext
    ,clReleaseContext
    ,clGetContextInfo
    )
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Foreign.C
import Control.Applicative
import Data.Bits
import Data.Maybe


clCreateContext :: [ContextProperties] -> [DeviceID] -> (Maybe ContextCallback) -> Ptr () -> IO (Either ErrorCode Context)
clCreateContext properties devices pfn_notify user_dat =
    allocaArray (propertiesN+1) $ \propertiesP -> allocaArray devicesN $ \devicesP -> do
        pokeArray0 nullPtr propertiesP properties
        pokeArray devicesP devices
        fptr <- maybe (return nullFunPtr) wrapContextCallback pfn_notify
        wrapErrorEither $ raw_clCreateContext propertiesP (fromIntegral devicesN) devicesP fptr user_dat             
    where propertiesN = length properties
          devicesN = length devices
          
clCreateContextFromType :: [ContextProperties] -> DeviceType -> (Maybe ContextCallback) -> Ptr () -> IO (Either ErrorCode Context)
clCreateContextFromType properties (DeviceType device_type) pfn_notify user_data = allocaArray (propertiesN+1) $ \propertiesP -> do
    pokeArray0 nullPtr propertiesP properties
    fptr <- maybe (return nullFunPtr) wrapContextCallback pfn_notify
    wrapErrorEither $ raw_clCreateContextFromType propertiesP device_type fptr user_data
    where propertiesN = length properties 
    
clRetainContext :: Context -> IO (Maybe ErrorCode)
clRetainContext ctx = wrapError (raw_clRetainContext ctx)

clReleaseContext :: Context -> IO (Maybe ErrorCode)
clReleaseContext ctx = wrapError (raw_clReleaseContext ctx)

clGetContextInfo :: Context -> ContextInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetContextInfo ctx (ContextInfo param_name) = wrapGetInfo (raw_clGetContextInfo ctx param_name)



