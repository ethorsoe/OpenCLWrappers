{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-| Declaration of types, bounds and constants -}
module System.OpenCL.Wrappers.Types where

import Foreign.C.Types
import Foreign.C.String(CString)
import Foreign

data PlatformIDc = PlatformIDc
data DeviceIDc = DeviceIDc
data Contextc = Contextc
data CommandQueuec = CommandQueuec
data Memc = Memc
data Programc = Programc
data Kernelc = Kernelc
data Eventc = Eventc
data Samplerc = Samplerc
data ImageFormatc = ImageFormatc

type PlatformID = Ptr PlatformIDc
type DeviceID = Ptr DeviceIDc
type Context = Ptr Contextc
type CommandQueue = Ptr CommandQueuec
type Mem = Ptr Memc
type Program = Ptr Programc
type Event = Ptr Eventc
type Sampler = Ptr Samplerc
type Kernel = Ptr Kernelc

type CLsizei = CSize
type CLint = CInt
type CLuint = CUInt
type CLbool = CLuint
type CLulong = CULong
type CLbitfield = CLulong
type ImageFormatp = Ptr ImageFormat

type ImageFormat = (ChannelOrder,ChannelType)
type ImageDims = (CLsizei,CLsizei,CLsizei)

newtype ContextProperties = ContextProperties IntPtr
    deriving (Eq,Storable)
newtype ChannelOrder = ChannelOrder CLuint
    deriving (Eq)
newtype ChannelType = ChannelType CLuint
    deriving (Eq)
newtype DeviceType = DeviceType CLbitfield
    deriving (Eq,Storable)
newtype ContextInfo = ContextInfo CLuint
    deriving (Eq)
newtype CommandQueueProperties = CommandQueueProperties CLbitfield
    deriving (Eq,Storable)
newtype CommandQueueInfo = CommandQueueInfo CLuint
    deriving (Eq)
newtype ErrorCode = ErrorCode CLint deriving (Eq,Ord,Show,Read)
newtype EventInfo = EventInfo CLuint
    deriving (Eq)
newtype ProfilingInfo = ProfilingInfo CLuint
    deriving (Eq)
newtype KernelInfo = KernelInfo CLuint
    deriving (Eq)
newtype KernelWorkGroupInfo = KernelWorkGroupInfo CLuint
    deriving (Eq)
newtype MapFlags = MapFlags CLbitfield
newtype MemFlags = MemFlags CLbitfield
    deriving (Eq,Storable)
newtype MemObjectType = MemObjectType CLuint
    deriving (Eq,Storable)
newtype MemInfo = MemInfo CLuint
    deriving (Eq)
newtype PlatformInfo = PlatformInfo CLuint
    deriving (Eq)
newtype SamplerInfo = SamplerInfo CLuint
    deriving (Eq)
newtype AddressingMode = AddressingMode CLuint
    deriving (Eq,Storable)
newtype FilterMode = FilterMode CLuint
    deriving (Eq,Storable)
newtype ProgramInfo = ProgramInfo CLuint
    deriving (Eq)
newtype ProgramBuildInfo = ProgramBuildInfo CLuint
    deriving (Eq)
newtype BuildStatus = BuildStatus CLint
    deriving (Eq)
newtype DeviceInfo = DeviceInfo CLuint
    deriving (Eq)
newtype DeviceFPConfig = DeviceFPConfig CLbitfield
    deriving (Eq,Storable)
newtype CommandType = CommandType CLuint
    deriving (Eq,Storable)
newtype DeviceExecCapabilities = DeviceExecCapabilities CLbitfield
    deriving (Eq,Storable)
newtype DeviceMemCacheType = DeviceMemCacheType CLuint
    deriving (Eq,Storable)
newtype DeviceLocalMemType = DeviceLocalMemType CLuint
    deriving (Eq,Storable)

data CLKernelInfoRetval = KernelInfoRetvalString String | KernelInfoRetvalCLuint CLuint | KernelInfoRetvalContext Context | KernelInfoRetvalProgram Program
    deriving(Eq)
data CLKernelWorkGroupInfoRetval = KernelWorkGroupInfoRetvalCLsizei CLsizei | KernelWorkGroupInfoRetvalCLsizeiList [CLsizei] | KernelWorkGroupInfoRetvalCLulong CLulong
    deriving(Eq)
data CLImageInfoRetval = ImageInfoRetvalCLsizei CLsizei | ImageInfoRetvalImageFormat ImageFormat | ImageInfoRetvalPtr (Ptr ())
    deriving(Eq)
data CLMemObjectInfoRetval = MemObjectInfoRetvalMemObjectType MemObjectType | MemObjectInfoRetvalMemFlags MemFlags | MemObjectInfoRetvalCLsizei CLsizei | MemObjectInfoRetvalPtr (Ptr ()) | MemObjectInfoRetvalCLuint CLuint | MemObjectInfoRetvalContext Context | MemObjectInfoRetvalMem Mem
    deriving(Eq)
data CLEventInfoRetval = EventInfoRetvalCommandQueue CommandQueue | EventInfoRetvalContext Context| EventInfoRetvalCommandType CommandType | EventInfoRetvalCLint CLint | EventInfoRetvalCLuint CLuint
    deriving(Eq)
data CLEventProfilingInfoRetval = EventProfilingInfoRetvalCLulong CLulong
    deriving(Eq)
data CLContextInfoRetval = ContextInfoRetvalCLuint CLuint | ContextInfoRetvalDeviceIDList [DeviceID] | ContextInfoRetvalContextPropertiesList [ContextProperties]
    deriving(Eq)
data CLCommandQueueInfoRetval = CommandQueueInfoRetvalContext Context | CommandQueueInfoRetvalDeviceID DeviceID | CommandQueueInfoRetvalCLuint CLuint | CommandQueueInfoRetvalCommandQueueProperties CommandQueueProperties
    deriving(Eq)
data CLDeviceInfoRetval = DeviceInfoRetvalString String | DeviceInfoRetvalCLuint CLuint | DeviceInfoRetvalCLbool CLbool | DeviceInfoRetvalDeviceFPConfig DeviceFPConfig | DeviceInfoRetvalDeviceExecCapabilities DeviceExecCapabilities | DeviceInfoRetvalCLulong CLulong | DeviceInfoRetvalDeviceMemCacheType DeviceMemCacheType | DeviceInfoRetvalCLsizei CLsizei | DeviceInfoRetvalDeviceLocalMemType DeviceLocalMemType | DeviceInfoRetvalCLsizeiList [CLsizei] | DeviceInfoRetvalPlatformID PlatformID | DeviceInfoRetvalCommandQueueProperties CommandQueueProperties | DeviceInfoRetvalDeviceType DeviceType
    deriving(Eq)
data CLProgramInfoRetval = ProgramInfoRetvalCLUint CLuint | ProgramInfoRetvalContext Context | ProgramInfoRetvalDeviceIDList [DeviceID] | ProgramInfoRetvalString String | ProgramInfoRetvalPtrList [Ptr ()] | ProgramInfoRetvalCLsizeiList [CLsizei]
    deriving(Eq)
data CLProgramBuildInfoRetval = ProgramBuildInfoRetvalBuildStatus BuildStatus | ProgramBuildInfoRetvalString String
    deriving(Eq)
data CLPlatformInfoRetval = PlatformInfoRetvalString String
    deriving(Eq)
data CLSamplerInfoRetval = SamplerInfoRetvalCLuint CLuint | SamplerInfoRetvalContext Context | SamplerInfoRetvalAddressingMode AddressingMode | SamplerInfoRetvalFilterMode FilterMode | SamplerInfoRetvalCLbool CLbool
    deriving(Eq)

type ContextCallback = (CString -> Ptr () -> CLsizei -> Ptr () -> IO ())
type NativeKernelCallback = Ptr () -> IO ()
type BuildProgramCallback = Program -> Ptr () -> IO ()

clQueueOutOfOrderExecModeEnable :: CommandQueueProperties 
clQueueOutOfOrderExecModeEnable = CommandQueueProperties (1`shiftL`0)

clQueueProfilingEnable :: CommandQueueProperties 
clQueueProfilingEnable = CommandQueueProperties (1`shiftL`1)

clQueueContext :: CommandQueueInfo 
clQueueContext = CommandQueueInfo 0x1090

clQueueDevice :: CommandQueueInfo 
clQueueDevice = CommandQueueInfo 0x1091

clQueueReferenceCount :: CommandQueueInfo 
clQueueReferenceCount = CommandQueueInfo 0x1092

clQueueProperties :: CommandQueueInfo 
clQueueProperties = CommandQueueInfo 0x1093

clDeviceType :: DeviceInfo 
clDeviceType = DeviceInfo 0x1000 

clDeviceVendorID :: DeviceInfo 
clDeviceVendorID = DeviceInfo 0x1001

clDeviceMaxComputeUnits :: DeviceInfo 
clDeviceMaxComputeUnits = DeviceInfo 0x1002

clDeviceMaxWorkItemDimensions :: DeviceInfo 
clDeviceMaxWorkItemDimensions = DeviceInfo 0x1003

clDeviceMaxWorkGroupSize :: DeviceInfo 
clDeviceMaxWorkGroupSize = DeviceInfo 0x1004

clDeviceMaxWorkItemSizes :: DeviceInfo 
clDeviceMaxWorkItemSizes = DeviceInfo 0x1005

clDevicePreferredVectorWidthChar :: DeviceInfo 
clDevicePreferredVectorWidthChar = DeviceInfo 0x1006

clDevicePreferredVectorWidthShort :: DeviceInfo 
clDevicePreferredVectorWidthShort = DeviceInfo 0x1007

clDevicePreferredVectorWidthInt :: DeviceInfo 
clDevicePreferredVectorWidthInt = DeviceInfo 0x1008

clDevicePreferredVectorWidthLong :: DeviceInfo 
clDevicePreferredVectorWidthLong = DeviceInfo 0x1009

clDevicePreferredVectorWidthFloat :: DeviceInfo 
clDevicePreferredVectorWidthFloat = DeviceInfo 0x100A

clDevicePreferredVectorWidthDouble :: DeviceInfo 
clDevicePreferredVectorWidthDouble = DeviceInfo 0x100B

clDeviceMaxClockFrequency :: DeviceInfo 
clDeviceMaxClockFrequency = DeviceInfo 0x100C

clDeviceAddressBits :: DeviceInfo 
clDeviceAddressBits = DeviceInfo 0x100D

clDeviceMaxReadImageArgs :: DeviceInfo 
clDeviceMaxReadImageArgs = DeviceInfo 0x100E 

clDeviceMaxWriteImageArgs :: DeviceInfo 
clDeviceMaxWriteImageArgs = DeviceInfo 0x100F

clDeviceMaxMemAllocSize :: DeviceInfo 
clDeviceMaxMemAllocSize = DeviceInfo 0x1010

clDeviceImage2DMaxWidth :: DeviceInfo 
clDeviceImage2DMaxWidth = DeviceInfo 0x1011

clDeviceImage2DMaxHeight :: DeviceInfo 
clDeviceImage2DMaxHeight = DeviceInfo 0x1012

clDeviceImage3DMaxWidth :: DeviceInfo 
clDeviceImage3DMaxWidth = DeviceInfo 0x1013

clDeviceImage3DMaxHeight :: DeviceInfo 
clDeviceImage3DMaxHeight = DeviceInfo 0x1014

clDeviceImage3DMaxDepth :: DeviceInfo 
clDeviceImage3DMaxDepth = DeviceInfo 0x1015

clDeviceImageSupport :: DeviceInfo 
clDeviceImageSupport = DeviceInfo 0x1016

clDeviceMaxParameterSize :: DeviceInfo 
clDeviceMaxParameterSize = DeviceInfo 0x1017

clDeviceMaxSamplers :: DeviceInfo 
clDeviceMaxSamplers = DeviceInfo 0x1018

clDeviceMemBaseAddrAlign :: DeviceInfo 
clDeviceMemBaseAddrAlign = DeviceInfo 0x1019

clDeviceMinDataTypeAlignSize :: DeviceInfo 
clDeviceMinDataTypeAlignSize = DeviceInfo 0x101A

clDeviceSingleFPConfig :: DeviceInfo 
clDeviceSingleFPConfig = DeviceInfo 0x101B

clDeviceGlobalMemCacheType :: DeviceInfo 
clDeviceGlobalMemCacheType = DeviceInfo 0x101C

clDeviceGlobalMemCacheLineSize :: DeviceInfo 
clDeviceGlobalMemCacheLineSize = DeviceInfo 0x101D

clDeviceGlobalMemCacheSize :: DeviceInfo 
clDeviceGlobalMemCacheSize = DeviceInfo 0x101E

clDeviceGlobalMemSize :: DeviceInfo 
clDeviceGlobalMemSize = DeviceInfo 0x101F

clDeviceMaxConstantBufferSize :: DeviceInfo 
clDeviceMaxConstantBufferSize = DeviceInfo 0x1020

clDeviceMaxConstantArgs :: DeviceInfo 
clDeviceMaxConstantArgs = DeviceInfo 0x1021

clDeviceLocalMemType :: DeviceInfo 
clDeviceLocalMemType = DeviceInfo 0x1022

clDeviceLocalMemSize :: DeviceInfo 
clDeviceLocalMemSize = DeviceInfo 0x1023

clDeviceErrorCorrectionSupport :: DeviceInfo 
clDeviceErrorCorrectionSupport = DeviceInfo 0x1024

clDeviceProfilingTimerResolution :: DeviceInfo 
clDeviceProfilingTimerResolution = DeviceInfo 0x1025

clDeviceEndianLittle :: DeviceInfo 
clDeviceEndianLittle = DeviceInfo 0x1026

clDeviceAvailable :: DeviceInfo 
clDeviceAvailable = DeviceInfo 0x1027

clDeviceCompilerAvailable :: DeviceInfo 
clDeviceCompilerAvailable = DeviceInfo 0x1028

clDeviceExecutionCapabilities :: DeviceInfo 
clDeviceExecutionCapabilities = DeviceInfo 0x1029

clDeviceQueueProperties :: DeviceInfo 
clDeviceQueueProperties = DeviceInfo 0x102A

clDeviceName :: DeviceInfo 
clDeviceName = DeviceInfo 0x102B

clDeviceVendor :: DeviceInfo 
clDeviceVendor = DeviceInfo 0x102C

clDriverVersion :: DeviceInfo 
clDriverVersion = DeviceInfo 0x102D

clDeviceProfile :: DeviceInfo 
clDeviceProfile = DeviceInfo 0x102E

clDeviceVersion :: DeviceInfo 
clDeviceVersion = DeviceInfo 0x102F

clDeviceExtensions :: DeviceInfo 
clDeviceExtensions = DeviceInfo 0x1030

clDevicePlatform :: DeviceInfo 
clDevicePlatform = DeviceInfo 0x1031

clDeviceDoubleFPConfig :: DeviceInfo
clDeviceDoubleFPConfig = DeviceInfo 0x1032

clDeviceHalfFPConfig :: DeviceInfo
clDeviceHalfFPConfig = DeviceInfo 0x1033


clFPDenorm :: DeviceFPConfig 
clFPDenorm = DeviceFPConfig (1`shiftL`0)

clFPInfNan :: DeviceFPConfig 
clFPInfNan = DeviceFPConfig (1`shiftL`1)

clFPRoundToNearest :: DeviceFPConfig 
clFPRoundToNearest = DeviceFPConfig (1`shiftL`2)

clFPRoundToZero :: DeviceFPConfig 
clFPRoundToZero = DeviceFPConfig (1`shiftL`3)

clFPRoundToInf :: DeviceFPConfig 
clFPRoundToInf = DeviceFPConfig (1`shiftL`4)

clFPFMA :: DeviceFPConfig 
clFPFMA = DeviceFPConfig (1`shiftL`5)



clEventCommandQueue  :: EventInfo 
clEventCommandQueue  = EventInfo 0x11D0

clEventCommandType :: EventInfo 
clEventCommandType = EventInfo 0x11D1

clEventReferenceCount :: EventInfo 
clEventReferenceCount = EventInfo 0x11D2

clEventCommandExecutionStatus :: EventInfo 
clEventCommandExecutionStatus = EventInfo 0x11D3


clProfilingCommandQueued :: ProfilingInfo 
clProfilingCommandQueued = ProfilingInfo 0x1280

clProfilingCommandSubmit :: ProfilingInfo 
clProfilingCommandSubmit = ProfilingInfo 0x1281

clProfilingCommandStart :: ProfilingInfo 
clProfilingCommandStart = ProfilingInfo 0x1282

clProfilingCommandEnd  :: ProfilingInfo 
clProfilingCommandEnd  = ProfilingInfo 0x1283

clFalse :: CLbool
clFalse = 0 :: CLbool

clTrue :: CLbool
clTrue = 1 :: CLbool


clDeviceTypeDefault :: DeviceType 
clDeviceTypeDefault = DeviceType (1`shiftL`0)

clDeviceTypeCPU :: DeviceType 
clDeviceTypeCPU = DeviceType (1`shiftL`1)

clDeviceTypeGPU :: DeviceType 
clDeviceTypeGPU = DeviceType (1`shiftL`2)

clDeviceTypeAccelerator :: DeviceType 
clDeviceTypeAccelerator = DeviceType (1`shiftL`3)

clDeviceTypeAll :: DeviceType 
clDeviceTypeAll = DeviceType 0xFFFFFFFF


clContextReferenceCount :: ContextInfo 
clContextReferenceCount = ContextInfo 0x1080

clContextDevices :: ContextInfo 
clContextDevices = ContextInfo 0x1081

clContextProperties :: ContextInfo 
clContextProperties = ContextInfo 0x1082

clContextPlatform :: ContextProperties
clContextPlatform = ContextProperties 0x1084


clKernelFunctionName  :: KernelInfo 
clKernelFunctionName  = KernelInfo 0x1190

clKernelNumArgs :: KernelInfo 
clKernelNumArgs = KernelInfo 0x1191

clKernelReferenceCount :: KernelInfo 
clKernelReferenceCount = KernelInfo 0x1192

clKernelContext :: KernelInfo 
clKernelContext = KernelInfo 0x1193

clKernelProgram :: KernelInfo 
clKernelProgram = KernelInfo 0x1194


clKernelWorkGroupSize :: KernelWorkGroupInfo 
clKernelWorkGroupSize = KernelWorkGroupInfo 0x11B0

clKernelCompileWorkGroupSize :: KernelWorkGroupInfo 
clKernelCompileWorkGroupSize = KernelWorkGroupInfo 0x11B1

clKernelLocalMemSize :: KernelWorkGroupInfo 
clKernelLocalMemSize = KernelWorkGroupInfo 0x11B2



clMemReadWrite :: MemFlags 
clMemReadWrite = MemFlags (1 `shiftL` 0)

clMemWriteOnly :: MemFlags 
clMemWriteOnly = MemFlags (1 `shiftL` 1)

clMemReadOnly :: MemFlags 
clMemReadOnly = MemFlags (1 `shiftL` 2)

clMemUseHostPtr :: MemFlags 
clMemUseHostPtr = MemFlags (1 `shiftL` 3)

clMemAllocHostPtr :: MemFlags 
clMemAllocHostPtr = MemFlags (1 `shiftL` 4)

clMemCopyHostPtr :: MemFlags 
clMemCopyHostPtr = MemFlags (1 `shiftL` 5)


clR :: ChannelOrder 
clR = ChannelOrder 0x10B0

clA :: ChannelOrder 
clA = ChannelOrder 0x10B1

clRG :: ChannelOrder 
clRG = ChannelOrder 0x10B2

clRA :: ChannelOrder 
clRA = ChannelOrder 0x10B3

clRGB :: ChannelOrder 
clRGB = ChannelOrder 0x10B4

clRGBA :: ChannelOrder 
clRGBA = ChannelOrder 0x10B5

clBGRA :: ChannelOrder 
clBGRA = ChannelOrder 0x10B6

clARGB :: ChannelOrder 
clARGB = ChannelOrder 0x10B7

clIntensity :: ChannelOrder 
clIntensity = ChannelOrder 0x10B8

clLuminance :: ChannelOrder 
clLuminance = ChannelOrder 0x10B9


clSNormInt8  :: ChannelType 
clSNormInt8  = ChannelType 0x10D0

clSNormInt16 :: ChannelType 
clSNormInt16 = ChannelType 0x10D1

clUNormInt8 :: ChannelType 
clUNormInt8 = ChannelType 0x10D2

clUNormInt16 :: ChannelType 
clUNormInt16 = ChannelType 0x10D3

clUNormShort565 :: ChannelType 
clUNormShort565 = ChannelType 0x10D4

clUNormShort555 :: ChannelType 
clUNormShort555 = ChannelType 0x10D5

clUNormInt101010 :: ChannelType 
clUNormInt101010 = ChannelType 0x10D6

clSignedInt8 :: ChannelType 
clSignedInt8 = ChannelType 0x10D7

clSignedInt16 :: ChannelType 
clSignedInt16 = ChannelType 0x10D8

clSignedInt32 :: ChannelType 
clSignedInt32 = ChannelType 0x10D9

clUnsignedInt8 :: ChannelType 
clUnsignedInt8 = ChannelType 0x10DA

clUnsignedInt16 :: ChannelType 
clUnsignedInt16 = ChannelType 0x10DB

clUnsignedInt32 :: ChannelType 
clUnsignedInt32 = ChannelType 0x10DC

clHalfFloat :: ChannelType 
clHalfFloat = ChannelType 0x10DD

clFloat  :: ChannelType 
clFloat  = ChannelType 0x10DE


clMemObjectBuffer :: MemObjectType 
clMemObjectBuffer = MemObjectType 0x10F0

clMemObjectImage2D :: MemObjectType 
clMemObjectImage2D = MemObjectType 0x10F1

clMemObjectImage3D :: MemObjectType 
clMemObjectImage3D = MemObjectType 0x10F2


clMemType :: MemInfo 
clMemType = MemInfo 0x1100

clMemFlags :: MemInfo 
clMemFlags = MemInfo 0x1101

clMemSize :: MemInfo 
clMemSize = MemInfo 0x1102

clMemHostPtr :: MemInfo 
clMemHostPtr = MemInfo 0x1103

clMemMapCount :: MemInfo 
clMemMapCount = MemInfo 0x1104

clMemReferenceCount :: MemInfo 
clMemReferenceCount = MemInfo 0x1105

clMemContext :: MemInfo 
clMemContext = MemInfo 0x1106


clImageFormat :: MemInfo 
clImageFormat = MemInfo 0x1110

clImageElementSize :: MemInfo 
clImageElementSize = MemInfo 0x1111

clImageRowPitch :: MemInfo 
clImageRowPitch = MemInfo 0x1112

clImageSlicePitch :: MemInfo 
clImageSlicePitch = MemInfo 0x1113

clImageWidth :: MemInfo 
clImageWidth = MemInfo 0x1114

clImageHeight :: MemInfo 
clImageHeight = MemInfo 0x1115

clImageDepth :: MemInfo 
clImageDepth = MemInfo 0x1116


clMapRead :: MapFlags 
clMapRead = MapFlags (1 `shiftL` 0)

clMapWrite :: MapFlags 
clMapWrite = MapFlags (1 `shiftL` 1)


clPlatformProfile :: PlatformInfo
clPlatformProfile = PlatformInfo 0x0900 

clPlatformVersion :: PlatformInfo
clPlatformVersion = PlatformInfo 0x0901

clPlatformName :: PlatformInfo
clPlatformName = PlatformInfo 0x0902

clPlatformVendor :: PlatformInfo
clPlatformVendor = PlatformInfo 0x0903


clPlatformExtensions :: PlatformInfo
clPlatformExtensions = PlatformInfo 0x0904

clProgramReferenceCount :: ProgramInfo 
clProgramReferenceCount = ProgramInfo 0x1160

clProgramContext :: ProgramInfo 
clProgramContext = ProgramInfo 0x1161

clProgramNumDevices :: ProgramInfo 
clProgramNumDevices = ProgramInfo 0x1162

clProgramDevices :: ProgramInfo 
clProgramDevices = ProgramInfo 0x1163

clProgramSource :: ProgramInfo 
clProgramSource = ProgramInfo 0x1164

clProgramBinarySizes :: ProgramInfo 
clProgramBinarySizes = ProgramInfo 0x1165

clProgramBinaries :: ProgramInfo 
clProgramBinaries = ProgramInfo 0x1166


clProgramBuildStatus :: ProgramBuildInfo 
clProgramBuildStatus = ProgramBuildInfo 0x1181

clProgramBuildOptions :: ProgramBuildInfo 
clProgramBuildOptions = ProgramBuildInfo 0x1182

clProgramBuildLog :: ProgramBuildInfo 
clProgramBuildLog = ProgramBuildInfo 0x1183                    


clBuildSuccess :: BuildStatus 
clBuildSuccess = BuildStatus 0

clBuildNone :: BuildStatus 
clBuildNone = BuildStatus (-1)

clBuildError :: BuildStatus 
clBuildError = BuildStatus (-2)

clBuildInProgress :: BuildStatus 
clBuildInProgress = BuildStatus (-3) 



clAddressNone :: AddressingMode 
clAddressNone = AddressingMode 0x1130

clAddressClampToEdge :: AddressingMode 
clAddressClampToEdge = AddressingMode 0x1131

clAddressClamp :: AddressingMode 
clAddressClamp = AddressingMode 0x1132

clAddressRepeat :: AddressingMode 
clAddressRepeat = AddressingMode 0x1133

clFilterNearest :: FilterMode
clFilterNearest = FilterMode 0x1140

clFilterLinear :: FilterMode
clFilterLinear = FilterMode 0x1141


clSamplerReferenceCount :: SamplerInfo 
clSamplerReferenceCount = SamplerInfo 0x1150

clSamplerContext :: SamplerInfo 
clSamplerContext = SamplerInfo 0x1151

clSamplerNormalizedCoords :: SamplerInfo 
clSamplerNormalizedCoords = SamplerInfo 0x1152

clSamplerAddressingMode :: SamplerInfo 
clSamplerAddressingMode = SamplerInfo 0x1153

clSamplerFilterMode :: SamplerInfo 
clSamplerFilterMode = SamplerInfo 0x1154

