module System.OpenCL.Wrappers.PlatformInfo (
    clGetPlatformIDs
  , clGetPlatformInfo
  ) where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw


clGetPlatformIDs :: IO [PlatformID]
clGetPlatformIDs = wrapGetNumElements raw_clGetPlatformIDs

clGetPlatformInfo :: PlatformID -> PlatformInfo -> IO CLPlatformInfoRetval
clGetPlatformInfo mem (PlatformInfo param_name) = do
    (x,_) <- wrapGetInfo (raw_clGetPlatformInfo mem param_name)
    peekStringInfo PlatformInfoRetvalString x
