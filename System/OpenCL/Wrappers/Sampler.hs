module System.OpenCL.Wrappers.Sampler
    (clCreateSampler
    ,clRetainSampler
    ,clReleaseSampler
    ,clGetSamplerInfo)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Data.Maybe


clCreateSampler :: Context -> Bool -> AddressingMode -> FilterMode -> IO (Either ErrorCode Sampler)
clCreateSampler ctx normalized_coords (AddressingMode addressing_mode) (FilterMode filter_mode) = 
    wrapErrorEither $ raw_clCreateSampler ctx (if normalized_coords then clTrue else clFalse) addressing_mode filter_mode

clRetainSampler :: Sampler -> IO (Maybe ErrorCode) 
clRetainSampler sampler = wrapError $ raw_clRetainSampler sampler

clReleaseSampler :: Sampler -> IO (Maybe ErrorCode) 
clReleaseSampler sampler = wrapError $ raw_clReleaseSampler sampler

clGetSamplerInfo :: Sampler -> SamplerInfo -> IO (Either ErrorCode CLSamplerInfoRetval)
clGetSamplerInfo sampler (SamplerInfo param_name) = (wrapGetInfo $ raw_clGetSamplerInfo sampler param_name) >>=
    either (return.Left) (\(x,_) -> fmap Right $ let c = (SamplerInfo param_name) in case () of 
        ()
            | c == clSamplerReferenceCount   -> peekOneInfo SamplerInfoRetvalCLuint x
            | c == clSamplerContext          -> peekOneInfo SamplerInfoRetvalContext x
            | c == clSamplerAddressingMode   -> peekOneInfo SamplerInfoRetvalAddressingMode x
            | c == clSamplerFilterMode       -> peekOneInfo SamplerInfoRetvalFilterMode x
            | c == clSamplerNormalizedCoords -> peekOneInfo SamplerInfoRetvalCLbool x
            | otherwise                      -> undefined)

