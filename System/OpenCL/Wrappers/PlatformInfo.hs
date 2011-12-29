module System.OpenCL.Wrappers.PlatformInfo (
    clGetPlatformIDs
  , clGetPlatformInfo
  ) where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw


clGetPlatformIDs :: IO (Either ErrorCode [PlatformID])
clGetPlatformIDs = wrapGetNumElements raw_clGetPlatformIDs

clGetPlatformInfo :: PlatformID -> PlatformInfo -> IO (Either ErrorCode CLPlatformInfoRetval)
clGetPlatformInfo mem (PlatformInfo param_name) = wrapGetInfo (raw_clGetPlatformInfo mem param_name) >>=
    either (return.Left) (\(x,_) -> fmap Right $ peekStringInfo PlatformInfoRetvalString x)
