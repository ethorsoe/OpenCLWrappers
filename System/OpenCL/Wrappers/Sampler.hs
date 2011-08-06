{-| Conforms to section 5.3 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.Sampler
    (clCreateSampler
    ,clRetainSampler
    ,clReleaseSampler
    ,clGetSamplerInfo)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Foreign.C
import Control.Applicative
import Data.Maybe


clCreateSampler :: Context -> Bool -> AddressingMode -> FilterMode -> IO (Either ErrorCode Sampler)
clCreateSampler ctx normalized_coords (AddressingMode addressing_mode) (FilterMode filter_mode) = 
    wrapErrorEither $ raw_clCreateSampler ctx (if normalized_coords then clTrue else clFalse) addressing_mode filter_mode

clRetainSampler :: Sampler -> IO (Maybe ErrorCode) 
clRetainSampler sampler = wrapError $ raw_clRetainSampler sampler

clReleaseSampler :: Sampler -> IO (Maybe ErrorCode) 
clReleaseSampler sampler = wrapError $ raw_clReleaseSampler sampler

clGetSamplerInfo :: Sampler -> SamplerInfo -> CLsizei -> Ptr () -> IO (Either ErrorCode CLsizei)
clGetSamplerInfo mem (SamplerInfo param_name) param_value_size param_value = alloca $ \param_value_size_ret -> do
    err <- wrapError $ raw_clGetSamplerInfo mem param_name param_value_size param_value param_value_size_ret
    if err == Nothing
        then peek param_value_size_ret >>= return . Right 
        else return . Left . fromJust $ err
