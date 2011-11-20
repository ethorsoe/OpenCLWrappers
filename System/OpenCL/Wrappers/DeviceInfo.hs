module System.OpenCL.Wrappers.DeviceInfo
    (clGetDeviceIDs
    ,clGetDeviceInfo)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw


clGetDeviceIDs :: PlatformID -> DeviceType -> IO [DeviceID]
clGetDeviceIDs platform (DeviceType device_type) = wrapGetNumElements $ raw_clGetDeviceIDs platform device_type
      
clGetDeviceInfo :: DeviceID -> DeviceInfo -> IO CLDeviceInfoRetval
clGetDeviceInfo obj (DeviceInfo param_name) = do
    (x,size) <- wrapGetInfo $ raw_clGetDeviceInfo obj param_name
    let c= DeviceInfo param_name
    case () of
        ()
            | c == clDeviceAddressBits                -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceAvailable                  -> peekOneInfo DeviceInfoRetvalCLbool x
            | c == clDeviceCompilerAvailable          -> peekOneInfo DeviceInfoRetvalCLbool x
            | c == clDeviceDoubleFPConfig             -> peekOneInfo DeviceInfoRetvalDeviceFPConfig x
            | c == clDeviceEndianLittle               -> peekOneInfo DeviceInfoRetvalCLbool x
            | c == clDeviceErrorCorrectionSupport     -> peekOneInfo DeviceInfoRetvalCLbool x
            | c == clDeviceExecutionCapabilities      -> peekOneInfo DeviceInfoRetvalDeviceExecCapabilities x
            | c == clDeviceExtensions                 -> peekStringInfo DeviceInfoRetvalString x
            | c == clDeviceGlobalMemCacheSize         -> peekOneInfo DeviceInfoRetvalCLulong x
            | c == clDeviceGlobalMemCacheType         -> peekOneInfo DeviceInfoRetvalDeviceMemCacheType x
            | c == clDeviceGlobalMemCacheLineSize     -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceGlobalMemSize              -> peekOneInfo DeviceInfoRetvalCLulong x
            | c == clDeviceHalfFPConfig               -> peekOneInfo DeviceInfoRetvalDeviceFPConfig x
            | c == clDeviceImageSupport               -> peekOneInfo DeviceInfoRetvalCLbool x
            | c == clDeviceImage2DMaxHeight           -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceImage2DMaxWidth            -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceImage3DMaxDepth            -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceImage3DMaxHeight           -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceImage3DMaxWidth            -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceLocalMemSize               -> peekOneInfo DeviceInfoRetvalCLulong x
            | c == clDeviceLocalMemType               -> peekOneInfo DeviceInfoRetvalDeviceLocalMemType x
            | c == clDeviceMaxClockFrequency          -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxComputeUnits            -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxConstantArgs            -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxConstantBufferSize      -> peekOneInfo DeviceInfoRetvalCLulong x
            | c == clDeviceMaxMemAllocSize            -> peekOneInfo DeviceInfoRetvalCLulong x
            | c == clDeviceMaxParameterSize           -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceMaxReadImageArgs           -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxSamplers                -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxWorkGroupSize           -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceMaxWorkItemDimensions      -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMaxWorkItemSizes           -> peekManyInfo DeviceInfoRetvalCLsizeiList x size
            | c == clDeviceMaxWriteImageArgs          -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMemBaseAddrAlign           -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceMinDataTypeAlignSize       -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceName                       -> peekStringInfo DeviceInfoRetvalString x
            | c == clDevicePlatform                   -> peekOneInfo DeviceInfoRetvalPlatformID x
            | c == clDevicePreferredVectorWidthChar   -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDevicePreferredVectorWidthShort  -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDevicePreferredVectorWidthInt    -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDevicePreferredVectorWidthLong   -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDevicePreferredVectorWidthFloat  -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDevicePreferredVectorWidthDouble -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceProfile                    -> peekStringInfo DeviceInfoRetvalString x
            | c == clDeviceProfilingTimerResolution   -> peekOneInfo DeviceInfoRetvalCLsizei x
            | c == clDeviceQueueProperties            -> peekOneInfo DeviceInfoRetvalCommandQueueProperties x
            | c == clDeviceSingleFPConfig             -> peekOneInfo DeviceInfoRetvalDeviceFPConfig x
            | c == clDeviceType                       -> peekOneInfo DeviceInfoRetvalDeviceType x
            | c == clDeviceVendor                     -> peekStringInfo DeviceInfoRetvalString x
            | c == clDeviceVendorID                   -> peekOneInfo DeviceInfoRetvalCLuint x
            | c == clDeviceVersion                    -> peekStringInfo DeviceInfoRetvalString x
            | c == clDriverVersion                    -> peekStringInfo DeviceInfoRetvalString x
