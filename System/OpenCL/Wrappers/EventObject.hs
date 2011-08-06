{-| Conforms to section 5.7 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.EventObject 
    (clWaitForEvents
    ,clGetEventInfo
    ,clRetainEvent
    ,clReleaseEvent
    ,clGetEventProfilingInfo)
where 

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative


clWaitForEvents :: [Event] -> IO (Maybe ErrorCode)
clWaitForEvents evts = allocaArray nEvents $ \eventP -> pokeArray eventP evts >> (wrapError $ raw_clWaitForEvents (fromIntegral nEvents) eventP)
    where nEvents = length evts
                            
clGetEventInfo :: Event -> EventInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetEventInfo obj (EventInfo param_name) = wrapGetInfo (raw_clGetEventInfo obj param_name)

clRetainEvent :: Event -> IO (Maybe ErrorCode)
clRetainEvent evt = wrapError $ raw_clRetainEvent evt

clReleaseEvent :: Event -> IO (Maybe ErrorCode)
clReleaseEvent evt = wrapError $ raw_clReleaseEvent evt 

clGetEventProfilingInfo :: Event -> ProfilingInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetEventProfilingInfo obj (ProfilingInfo param_name) = wrapGetInfo (raw_clGetEventProfilingInfo obj param_name)
