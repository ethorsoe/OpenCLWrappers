module System.OpenCL.Wrappers.FlushFinish 
    (clFlush
    ,clFinish)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw


clFlush :: CommandQueue -> IO ()
clFlush queue = wrapError $ raw_clFlush queue

clFinish :: CommandQueue -> IO ()
clFinish queue = wrapError $ raw_clFinish queue
