module System.OpenCL.Wrappers.CommandQueue 
    (clCreateCommandQueue
    ,clRetainCommandQueue
    ,clReleaseCommandQueue
    ,clGetCommandQueueInfo
    ,clSetCommandQueueProperty)
where

import Data.Bits
import Data.List
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable(peek)


clCreateCommandQueue :: Context -> DeviceID -> [CommandQueueProperties] -> IO CommandQueue
clCreateCommandQueue ctx devid props =
    wrapErrorResult $ raw_clCreateCommandQueue ctx devid properties
    where properties = foldl' (.|.) 0 [ prop | CommandQueueProperties prop <- props ]

clRetainCommandQueue :: CommandQueue -> IO ()
clRetainCommandQueue queue = wrapError (raw_clRetainCommandQueue queue)

clReleaseCommandQueue :: CommandQueue -> IO ()
clReleaseCommandQueue queue = wrapError (raw_clReleaseCommandQueue queue)

clGetCommandQueueInfo :: CommandQueue -> CommandQueueInfo -> IO CLCommandQueueInfoRetval
clGetCommandQueueInfo ctx c@(CommandQueueInfo param_name) = do
    (x,_) <- wrapGetInfo (raw_clGetCommandQueueInfo ctx param_name)
    case () of
        ()
            | c == clQueueContext        -> peekOneInfo CommandQueueInfoRetvalContext x
            | c == clQueueDevice         -> peekOneInfo CommandQueueInfoRetvalDeviceID x
            | c == clQueueReferenceCount -> peekOneInfo CommandQueueInfoRetvalCLuint x
            | c == clQueueProperties     -> peekOneInfo CommandQueueInfoRetvalCommandQueueProperties x
            | otherwise                  -> badArgument "clGetCommandQueueInfo" c

{-# DEPRECATED clSetCommandQueueProperty "Deprecated in C api" #-}
clSetCommandQueueProperty :: CommandQueue -> CommandQueueProperties -> Bool -> IO CommandQueueProperties
clSetCommandQueueProperty queue (CommandQueueProperties properties) enable = alloca $ \old_properties -> do
    wrapError $ raw_clSetCommandQueueProperty queue properties (if enable then clTrue else clFalse) old_properties
    fmap CommandQueueProperties $ peek old_properties
