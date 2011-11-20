module System.OpenCL.Wrappers.Kernel 
    (clCreateKernel
    ,clCreateKernelsInProgram
    ,clRetainKernel
    ,clReleaseKernel
    ,clGetKernelInfo
    ,clSetKernelArg
    ,clGetKernelWorkGroupInfo
    ,clEnqueueNDRangeKernel
    ,clEnqueueTask
    ,clEnqueueNativeKernel)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Foreign.C
import Control.Applicative
import Data.Maybe


clCreateKernel program init_name = withCString init_name (\x -> wrapErrorResult $ raw_clCreateKernel program x)

clCreateKernelsInProgram :: Program -> CLuint -> IO [Kernel]
clCreateKernelsInProgram program num_kernels = allocaArray (fromIntegral num_kernels) $ \kernels -> alloca $ \num_kernels_ret -> do
    wrapError $ raw_clCreateKernelsInProgram program num_kernels kernels num_kernels_ret
    nkr <- peek num_kernels_ret
    peekArray (fromIntegral nkr) kernels

clRetainKernel :: Kernel -> IO ()
clRetainKernel kernel = wrapError $ raw_clRetainKernel kernel

clReleaseKernel :: Kernel -> IO ()
clReleaseKernel kernel = wrapError $ raw_clRetainKernel kernel

clSetKernelArg :: Kernel -> CLuint -> CLsizei -> Ptr () -> IO ()
clSetKernelArg kernel arg_index arg_size arg_value = 
    wrapError $ raw_clSetKernelArg kernel arg_index arg_size arg_value

clGetKernelInfo :: Kernel -> KernelInfo -> IO CLKernelInfoRetval
clGetKernelInfo kernel c@(KernelInfo param_name) = do
    (x,size) <- wrapGetInfo $ raw_clGetKernelInfo kernel param_name
    case () of
        ()
            | c == clKernelFunctionName   -> peekStringInfo KernelInfoRetvalString x
            | c == clKernelNumArgs        -> peekOneInfo KernelInfoRetvalCLuint x
            | c == clKernelReferenceCount -> peekOneInfo KernelInfoRetvalCLuint x
            | c == clKernelContext        -> peekOneInfo KernelInfoRetvalContext x
            | c == clKernelProgram        -> peekOneInfo KernelInfoRetvalProgram x

clGetKernelWorkGroupInfo :: Kernel -> DeviceID -> KernelWorkGroupInfo -> IO CLKernelWorkGroupInfoRetval
clGetKernelWorkGroupInfo kernel device c@(KernelWorkGroupInfo param_name) = do
    (x,size) <- wrapGetInfo $ raw_clGetKernelWorkGroupInfo kernel device param_name
    case () of
        ()
            | c == clKernelWorkGroupSize        -> peekOneInfo KernelWorkGroupInfoRetvalCLsizei x
            | c == clKernelCompileWorkGroupSize -> peekManyInfo KernelWorkGroupInfoRetvalCLsizeiList x size
            | c == clKernelLocalMemSize         -> peekOneInfo KernelWorkGroupInfoRetvalCLulong x

clEnqueueNDRangeKernel :: CommandQueue -> Kernel -> [CLsizei] -> [CLsizei] -> [Event] -> IO Event
clEnqueueNDRangeKernel queue kernel global_work_sizeL local_work_sizeL event_wait_listL = 
    withArray global_work_sizeL $ \global_work_size ->
    withArrayNull local_work_sizeL $ \local_work_size ->
    withArrayNull event_wait_listL $ \event_wait_list ->
    alloca $ \event -> do
        wrapError $ raw_clEnqueueNDRangeKernel queue kernel (fromIntegral work_dim) nullPtr global_work_size local_work_size (fromIntegral num_events_in_wait_list) event_wait_list event
        peek event
    where work_dim = length global_work_sizeL
          num_events_in_wait_list = length event_wait_listL
        
clEnqueueTask :: CommandQueue -> Kernel -> [Event] -> IO Event
clEnqueueTask queue kernel event_wait_listL = 
    allocaArray num_events_in_wait_list $ \event_wait_list ->
    alloca $ \event -> do
        pokeArray event_wait_list event_wait_listL
        wrapError $ raw_clEnqueueTask queue kernel (fromIntegral num_events_in_wait_list) event_wait_list event
        peek event
    where num_events_in_wait_list = length event_wait_listL

clEnqueueNativeKernel :: NativeKernelCallback -> Ptr () -> CLsizei -> [Mem] -> [Ptr ()] -> [Event] -> IO Event
clEnqueueNativeKernel user_funcF args cb_args mem_listL args_mem_locL event_wait_listL = 
    allocaArray num_events_in_wait_list $ \event_wait_list ->
    allocaArray num_mem_objects $ \mem_list ->
    allocaArray (length args_mem_locL) $ \args_mem_loc -> 
    alloca $ \event -> do
        user_func <- wrapNativeKernelCallback user_funcF
        pokeArray event_wait_list event_wait_listL
        pokeArray mem_list mem_listL
        pokeArray args_mem_loc args_mem_locL
        raw_clEnqueueNativeKernel user_func args cb_args (fromIntegral num_mem_objects) mem_list args_mem_loc (fromIntegral num_events_in_wait_list) event_wait_list event
        peek event
    where num_events_in_wait_list = length event_wait_listL
          num_mem_objects = length mem_listL
