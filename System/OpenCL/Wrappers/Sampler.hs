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


clCreateSampler :: Context -> Bool -> AddressingMode -> FilterMode -> IO Sampler
clCreateSampler ctx normalized_coords (AddressingMode addressing_mode) (FilterMode filter_mode) = 
    wrapErrorResult $ raw_clCreateSampler ctx (if normalized_coords then clTrue else clFalse) addressing_mode filter_mode

clRetainSampler :: Sampler -> IO ()
clRetainSampler sampler = wrapError $ raw_clRetainSampler sampler

clReleaseSampler :: Sampler -> IO ()
clReleaseSampler sampler = wrapError $ raw_clReleaseSampler sampler

clGetSamplerInfo :: Sampler -> SamplerInfo -> IO CLSamplerInfoRetval
clGetSamplerInfo sampler c@(SamplerInfo param_name) = do
    (x,size) <- wrapGetInfo $ raw_clGetSamplerInfo sampler param_name
    case () of
        ()
            | c == clSamplerReferenceCount   -> peekOneInfo SamplerInfoRetvalCLuint x
            | c == clSamplerContext          -> peekOneInfo SamplerInfoRetvalContext x
            | c == clSamplerAddressingMode   -> peekOneInfo SamplerInfoRetvalAddressingMode x
            | c == clSamplerFilterMode       -> peekOneInfo SamplerInfoRetvalFilterMode x
            | c == clSamplerNormalizedCoords -> peekOneInfo SamplerInfoRetvalCLbool x

