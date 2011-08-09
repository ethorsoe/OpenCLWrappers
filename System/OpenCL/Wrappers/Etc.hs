{-| Module for querying extensions -}
module System.OpenCL.Wrappers.Etc 
    (clGetExtensionFunctionAddress)
where

import System.OpenCL.Wrappers.Raw
import Foreign.Ptr(Ptr)
import Foreign.C.String(withCString)


clGetExtensionFunctionAddress :: String -> IO (Ptr ())
clGetExtensionFunctionAddress str = withCString str raw_clGetExtensionFunctionAddress
