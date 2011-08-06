{-| Module for querying extensions -}
module System.OpenCL.Wrappers.Etc 
    (clGetExtensionFunctionAddress)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Foreign.C
import Control.Applicative

clGetExtensionFunctionAddress :: String -> IO (Ptr ())
clGetExtensionFunctionAddress str = withCString str raw_clGetExtensionFunctionAddress
