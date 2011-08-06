{-# LANGUAGE ScopedTypeVariables #-}
{-| Conforms to section 4.1 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.PlatformInfo (
    clGetPlatformIDs
  , clGetPlatformInfo
  ) where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Foreign.C
import Control.Applicative
import Data.Maybe


clGetPlatformIDs :: CLuint -> IO (Either ErrorCode [PlatformID])
clGetPlatformIDs num_entries = alloca $ \(platforms::Ptr PlatformID) -> alloca $ \(num_platforms::Ptr CLuint) -> do
  errcode <- ErrorCode <$> raw_clGetPlatformIDs (fromIntegral num_entries) platforms num_platforms
  if errcode == clSuccess
      then Right <$> (peek num_platforms >>= \num_platformsN -> peekArray (fromIntegral num_platformsN) platforms)
      else return $ Left errcode
      
      
clGetPlatformInfo :: PlatformID -> PlatformInfo -> CLsizei -> Ptr () -> IO (Either ErrorCode CLsizei)
clGetPlatformInfo mem (PlatformInfo param_name) param_value_size param_value = alloca $ \param_value_size_ret -> do
    err <- wrapError $ raw_clGetPlatformInfo mem param_name param_value_size param_value param_value_size_ret
    if err == Nothing
        then peek param_value_size_ret >>= return . Right 
        else return . Left . fromJust $ err
