module System.OpenCL.Wrappers.EventObject 
    (clWaitForEvents
    ,clGetEventInfo
    ,clRetainEvent
    ,clReleaseEvent
    ,clGetEventProfilingInfo)
where 

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign.Marshal.Array(withArray)


clWaitForEvents :: [Event] -> IO (Maybe ErrorCode)
clWaitForEvents evts = withArray evts (\eventP -> wrapError $ raw_clWaitForEvents (fromIntegral nEvents) eventP)
    where nEvents = length evts
                            
clGetEventInfo :: Event -> EventInfo -> IO (Either ErrorCode CLEventInfoRetval)
clGetEventInfo obj (EventInfo param_name) = wrapGetInfo (raw_clGetEventInfo obj param_name) >>=
    either (return.Left) (\(x,_) -> fmap Right $ let c = (EventInfo param_name) in case () of 
        ()
            | c == clEventCommandQueue           -> peekOneInfo EventInfoRetvalCommandQueue x
            | c == clEventCommandType            -> peekOneInfo EventInfoRetvalCommandType x
            | c == clEventCommandExecutionStatus -> peekOneInfo EventInfoRetvalCLint x
            | c == clEventReferenceCount         -> peekOneInfo EventInfoRetvalCLuint x
            | otherwise                          -> undefined)

clRetainEvent :: Event -> IO (Maybe ErrorCode)
clRetainEvent evt = wrapError $ raw_clRetainEvent evt

clReleaseEvent :: Event -> IO (Maybe ErrorCode)
clReleaseEvent evt = wrapError $ raw_clReleaseEvent evt 

clGetEventProfilingInfo :: Event -> ProfilingInfo -> IO (Either ErrorCode CLEventProfilingInfoRetval)
clGetEventProfilingInfo obj (ProfilingInfo param_name) = wrapGetInfo (raw_clGetEventProfilingInfo obj param_name) >>=
    either (return.Left) (\(x,_) -> fmap Right $ let c = (ProfilingInfo param_name) in case () of 
        ()
            | c == clProfilingCommandQueued -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandSubmit -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandStart  -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandEnd    -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | otherwise                     -> undefined)
