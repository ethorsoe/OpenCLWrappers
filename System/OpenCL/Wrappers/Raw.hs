{-# LANGUAGE ForeignFunctionInterface #-}
module System.OpenCL.Wrappers.Raw
    (raw_clCreateCommandQueue
    ,raw_clRetainCommandQueue
    ,raw_clReleaseCommandQueue
    ,raw_clGetCommandQueueInfo
    ,raw_clSetCommandQueueProperty
    ,raw_clCreateContext
    ,raw_clCreateContextFromType
    ,raw_clRetainContext
    ,raw_clReleaseContext
    ,raw_clGetContextInfo
    ,raw_clGetDeviceIDs
    ,raw_clGetDeviceInfo
    ,raw_clGetExtensionFunctionAddress
    ,raw_clWaitForEvents
    ,raw_clGetEventInfo
    ,raw_clRetainEvent
    ,raw_clReleaseEvent
    ,raw_clGetEventProfilingInfo
    ,raw_clFlush
    ,raw_clFinish
    ,raw_clCreateKernel
    ,raw_clCreateKernelsInProgram
    ,raw_clRetainKernel
    ,raw_clReleaseKernel
    ,raw_clSetKernelArg
    ,raw_clGetKernelInfo
    ,raw_clGetKernelWorkGroupInfo
    ,raw_clEnqueueNDRangeKernel
    ,raw_clEnqueueTask
    ,raw_clEnqueueNativeKernel
    ,raw_clCreateBuffer
    ,raw_clCreateImage2D
    ,raw_clCreateImage3D
    ,raw_clRetainMemObject
    ,raw_clReleaseMemObject
    ,raw_clGetSupportedImageFormats
    ,raw_clGetMemObjectInfo
    ,raw_clGetImageInfo
    ,raw_clEnqueueReadBuffer
    ,raw_clEnqueueWriteBuffer
    ,raw_clEnqueueCopyBuffer
    ,raw_clEnqueueReadImage
    ,raw_clEnqueueWriteImage
    ,raw_clEnqueueCopyImage
    ,raw_clEnqueueCopyImageToBuffer
    ,raw_clEnqueueCopyBufferToImage
    ,raw_clEnqueueMapBuffer
    ,raw_clEnqueueMapImage
    ,raw_clEnqueueUnmapMemObject
    ,raw_clEnqueueMarker
    ,raw_clEnqueueWaitForEvents
    ,raw_clEnqueueBarrier
    ,raw_clGetPlatformIDs
    ,raw_clGetPlatformInfo
    ,raw_clCreateProgramWithSource
    ,raw_clCreateProgramWithBinary
    ,raw_clRetainProgram
    ,raw_clReleaseProgram
    ,raw_clBuildProgram
    ,raw_clUnloadCompiler
    ,raw_clGetProgramInfo
    ,raw_clGetProgramBuildInfo
    ,raw_clCreateSampler
    ,raw_clRetainSampler
    ,raw_clReleaseSampler
    ,raw_clGetSamplerInfo
    --Callback functions
    ,wrapContextCallback
    ,wrapNativeKernelCallback
    ,wrapBuildProgramCallback
    )
where

import System.OpenCL.Wrappers.Types
--import System.OpenCL.Wrappers.Errors
import Foreign
import Foreign.C

foreign import ccall "wrapper" wrapContextCallback :: ContextCallback -> IO (FunPtr ContextCallback)
foreign import ccall "wrapper" wrapNativeKernelCallback :: NativeKernelCallback -> IO (FunPtr NativeKernelCallback)
foreign import ccall "wrapper" wrapBuildProgramCallback :: BuildProgramCallback -> IO (FunPtr BuildProgramCallback)


foreign import ccall "clCreateCommandQueue" raw_clCreateCommandQueue :: Context -> DeviceID -> CLbitfield -> Ptr CLint -> IO CommandQueue
foreign import ccall "clRetainCommandQueue" raw_clRetainCommandQueue :: CommandQueue -> IO CLint
foreign import ccall "clReleaseCommandQueue" raw_clReleaseCommandQueue :: CommandQueue -> IO CLint
foreign import ccall "clGetCommandQueueInfo" raw_clGetCommandQueueInfo :: CommandQueue -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clSetCommandQueueProperty" raw_clSetCommandQueueProperty :: CommandQueue -> CLbitfield -> CLbool -> Ptr CLbitfield -> IO CLint
foreign import ccall "clCreateContext" raw_clCreateContext :: Ptr (Ptr CLint) -> CLuint -> Ptr DeviceID -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context
foreign import ccall "clCreateContextFromType" raw_clCreateContextFromType :: Ptr ContextProperties -> CLbitfield -> FunPtr ContextCallback -> Ptr a -> Ptr CLint -> IO Context
foreign import ccall "clRetainContext" raw_clRetainContext :: Context -> IO CLint
foreign import ccall "clReleaseContext" raw_clReleaseContext :: Context -> IO CLint
foreign import ccall "clGetContextInfo" raw_clGetContextInfo :: Context -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clGetDeviceIDs" raw_clGetDeviceIDs :: PlatformID -> CLbitfield -> CLuint -> Ptr DeviceID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetDeviceInfo" raw_clGetDeviceInfo :: DeviceID -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clGetExtensionFunctionAddress" raw_clGetExtensionFunctionAddress :: CString -> IO (Ptr ())
foreign import ccall "clWaitForEvents" raw_clWaitForEvents :: CLuint -> Ptr Event -> IO CLint
foreign import ccall "clGetEventInfo" raw_clGetEventInfo :: Event -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clRetainEvent" raw_clRetainEvent :: Event -> IO CLint 
foreign import ccall "clReleaseEvent" raw_clReleaseEvent :: Event -> IO CLint 
foreign import ccall "clGetEventProfilingInfo" raw_clGetEventProfilingInfo :: Event -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clFlush" raw_clFlush :: CommandQueue -> IO CLint
foreign import ccall "clFinish" raw_clFinish :: CommandQueue -> IO CLint
foreign import ccall "clCreateKernel" raw_clCreateKernel :: Program -> CString -> Ptr CLint -> IO Kernel 
foreign import ccall "clCreateKernelsInProgram" raw_clCreateKernelsInProgram :: Program -> CLuint -> Ptr Kernel -> Ptr CLuint -> IO CLint 
foreign import ccall "clRetainKernel" raw_clRetainKernel :: Kernel -> IO CLint 
foreign import ccall "clReleaseKernel" raw_clReleaseKernel :: Kernel -> IO CLint 
foreign import ccall "clSetKernelArg" raw_clSetKernelArg :: Kernel -> CLuint -> CLsizei -> Ptr () -> IO CLint
foreign import ccall "clGetKernelInfo" raw_clGetKernelInfo :: Kernel -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clGetKernelWorkGroupInfo" raw_clGetKernelWorkGroupInfo :: Kernel -> DeviceID -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clEnqueueNDRangeKernel" raw_clEnqueueNDRangeKernel :: CommandQueue -> Kernel -> CLuint -> Ptr CLsizei -> Ptr CLsizei -> Ptr CLsizei -> CLuint -> Ptr Event  -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueTask" raw_clEnqueueTask :: CommandQueue -> Kernel -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueNativeKernel" raw_clEnqueueNativeKernel :: FunPtr NativeKernelCallback -> Ptr () -> CLsizei -> CLuint -> Ptr Mem -> Ptr (Ptr ()) -> CLuint -> Ptr Event -> Ptr Event -> IO CLint 
foreign import ccall "clCreateBuffer" raw_clCreateBuffer :: Context -> CLbitfield -> CLsizei -> Ptr () -> Ptr CLint -> IO Mem
foreign import ccall "clCreateImage2D" raw_clCreateImage2D :: Context -> CLbitfield -> Ptr CLuint -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> Ptr CLint -> IO Mem
foreign import ccall "clCreateImage3D" raw_clCreateImage3D :: Context -> CLbitfield -> Ptr CLuint -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> Ptr CLint -> IO Mem
foreign import ccall "clRetainMemObject" raw_clRetainMemObject :: Mem -> IO CLint
foreign import ccall "clReleaseMemObject" raw_clReleaseMemObject :: Mem -> IO CLint
foreign import ccall "clGetSupportedImageFormats" raw_clGetSupportedImageFormats :: Context -> CLbitfield -> CLuint -> CLuint -> Ptr CLuint -> Ptr CLuint -> IO CLint
foreign import ccall "clGetMemObjectInfo" raw_clGetMemObjectInfo :: Mem -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clGetImageInfo" raw_clGetImageInfo :: Mem -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clEnqueueReadBuffer" raw_clEnqueueReadBuffer :: CommandQueue -> Mem -> CLbool -> CLsizei -> CLsizei -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueWriteBuffer" raw_clEnqueueWriteBuffer :: CommandQueue -> Mem -> CLbool -> CLsizei -> CLsizei -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueCopyBuffer" raw_clEnqueueCopyBuffer :: CommandQueue -> Mem -> Mem -> CLsizei -> CLsizei -> CLsizei -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueReadImage" raw_clEnqueueReadImage :: CommandQueue -> Mem -> CLbool -> Ptr CLsizei -> Ptr CLsizei -> CLsizei -> CLsizei -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueWriteImage" raw_clEnqueueWriteImage :: CommandQueue -> Mem -> CLbool -> Ptr CLsizei -> Ptr CLsizei -> CLsizei -> CLsizei -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueCopyImage" raw_clEnqueueCopyImage :: CommandQueue -> Mem -> Mem -> Ptr CLsizei -> Ptr CLsizei -> Ptr CLsizei -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueCopyImageToBuffer" raw_clEnqueueCopyImageToBuffer :: CommandQueue -> Mem -> Mem -> Ptr CLsizei -> Ptr CLsizei -> CLsizei -> CLuint -> Ptr Event -> Ptr Event -> IO CLint 
foreign import ccall "clEnqueueCopyBufferToImage" raw_clEnqueueCopyBufferToImage :: CommandQueue -> Mem -> Mem -> CLsizei -> Ptr CLsizei -> Ptr CLsizei -> CLuint -> Ptr Event -> Ptr Event -> IO CLint 
foreign import ccall "clEnqueueMapBuffer" raw_clEnqueueMapBuffer :: CommandQueue -> Mem -> CLbool -> CLbitfield -> CLsizei -> CLsizei -> CLuint -> Ptr Event -> Ptr Event -> Ptr CLint -> IO (Ptr ())
foreign import ccall "clEnqueueMapImage" raw_clEnqueueMapImage :: CommandQueue -> Mem -> CLbool -> CLbitfield -> Ptr CLsizei -> Ptr CLsizei -> Ptr CLsizei -> Ptr CLsizei -> CLuint -> Ptr Event -> Ptr Event -> Ptr CLint -> IO (Ptr ())
foreign import ccall "clEnqueueUnmapMemObject" raw_clEnqueueUnmapMemObject :: CommandQueue -> Mem -> Ptr () -> CLuint -> Ptr Event -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueMarker" raw_clEnqueueMarker :: CommandQueue -> Ptr Event -> IO CLint 
foreign import ccall "clEnqueueWaitForEvents" raw_clEnqueueWaitForEvents :: CommandQueue -> CLuint -> Ptr Event -> IO CLint
foreign import ccall "clEnqueueBarrier" raw_clEnqueueBarrier :: CommandQueue -> IO CLint 
foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: CLuint -> Ptr PlatformID -> Ptr CLuint -> IO CLint
foreign import ccall "clGetPlatformInfo" raw_clGetPlatformInfo :: PlatformID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint 
foreign import ccall "clCreateProgramWithSource" raw_clCreateProgramWithSource :: Context -> CLuint -> Ptr CString -> Ptr CLsizei -> Ptr CLint -> IO Program
foreign import ccall "clCreateProgramWithBinary" raw_clCreateProgramWithBinary :: Context -> CLuint -> Ptr DeviceID -> Ptr CLsizei -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO Program
foreign import ccall "clRetainProgram" raw_clRetainProgram :: Program -> IO CLint
foreign import ccall "clReleaseProgram" raw_clReleaseProgram :: Program -> IO CLint
foreign import ccall "clBuildProgram" raw_clBuildProgram :: Program -> CLuint -> Ptr DeviceID -> CString -> FunPtr BuildProgramCallback -> Ptr () -> IO CLint
foreign import ccall "clUnloadCompiler" raw_clUnloadCompiler :: IO CLint
foreign import ccall "clGetProgramInfo" raw_clGetProgramInfo :: Program -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clGetProgramBuildInfo" raw_clGetProgramBuildInfo :: Program -> DeviceID -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
foreign import ccall "clCreateSampler" raw_clCreateSampler :: Context -> CLbool -> CLuint -> CLuint -> Ptr CLint -> IO Sampler
foreign import ccall "clRetainSampler" raw_clRetainSampler :: Sampler -> IO CLint
foreign import ccall "clReleaseSampler" raw_clReleaseSampler :: Sampler -> IO CLint
foreign import ccall "clGetSamplerInfo" raw_clGetSamplerInfo :: Sampler -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
