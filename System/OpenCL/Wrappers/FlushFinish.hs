{-| Conforms to section 5.10 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.FlushFinish 
    (clFlush
    ,clFinish)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative

clFlush :: CommandQueue -> IO (Maybe ErrorCode)
clFlush queue = wrapError $ raw_clFlush queue

clFinish :: CommandQueue -> IO (Maybe ErrorCode)
clFinish queue = wrapError $ raw_clFinish queue

