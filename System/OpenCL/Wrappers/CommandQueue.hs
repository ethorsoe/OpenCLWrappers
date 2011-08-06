{-| 
    Corresponds to section 5.1, Command Queues of the OpenCL 1.0 specifications.
-}
module System.OpenCL.Wrappers.CommandQueue 
    (clCreateCommandQueue
    ,clRetainCommandQueue
    ,clReleaseCommandQueue
    ,clGetCommandQueueInfo
    ,clSetCommandQueueProperty)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative
import Data.Bits
import Data.Maybe


clCreateCommandQueue :: Context -> DeviceID -> CommandQueueProperties -> IO (Either ErrorCode CommandQueue)
clCreateCommandQueue ctx devid (CommandQueueProperties properties) = 
    wrapErrorEither $ raw_clCreateCommandQueue ctx devid properties 

clRetainCommandQueue :: CommandQueue -> IO (Maybe ErrorCode)
clRetainCommandQueue queue = wrapError (raw_clRetainCommandQueue queue)

clReleaseCommandQueue :: CommandQueue -> IO (Maybe ErrorCode)
clReleaseCommandQueue queue = wrapError (raw_clReleaseCommandQueue queue)

clGetCommandQueueInfo :: CommandQueue -> CommandQueueInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetCommandQueueInfo ctx (CommandQueueInfo param_name) = wrapGetInfo (raw_clGetCommandQueueInfo ctx param_name)

clSetCommandQueueProperty :: CommandQueue -> CommandQueueProperties -> Bool -> IO (Either ErrorCode CommandQueueProperties)
clSetCommandQueueProperty queue (CommandQueueProperties properties) enable = alloca $ \old_properties -> do 
    err <- ErrorCode <$> raw_clSetCommandQueueProperty queue properties (if enable then clTrue else clFalse) old_properties
    if err == clSuccess 
        then Right . CommandQueueProperties <$> peek old_properties
        else return . Left $ err
        

