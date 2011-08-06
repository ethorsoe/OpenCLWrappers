{-# LANGUAGE ScopedTypeVariables #-}
{-| Conforms to section 4.2 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.DeviceInfo
    (clGetDeviceIDs
    ,clGetDeviceInfo)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative
import Data.Bits


clGetDeviceIDs :: PlatformID -> DeviceType -> CLuint -> IO (Either ErrorCode [DeviceID])
clGetDeviceIDs platform (DeviceType device_type) num_entries = alloca $ \(devices::Ptr DeviceID) -> alloca $ \(num_devices::Ptr CLuint) -> do
  errcode <- ErrorCode <$> raw_clGetDeviceIDs platform device_type num_entries devices num_devices
  if errcode == clSuccess
      then Right <$> (peek num_devices >>= \num_devicesN -> peekArray (fromIntegral num_devicesN) devices)
      else return $ Left errcode
      
clGetDeviceInfo :: DeviceID -> DeviceInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetDeviceInfo obj (DeviceInfo param_name) = wrapGetInfo (raw_clGetDeviceInfo obj param_name)

