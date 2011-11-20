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


clWaitForEvents :: [Event] -> IO ()
clWaitForEvents evts = withArray evts (\eventP -> wrapError $ raw_clWaitForEvents (fromIntegral nEvents) eventP)
    where nEvents = length evts
                            
clGetEventInfo :: Event -> EventInfo -> IO CLEventInfoRetval
clGetEventInfo obj (EventInfo param_name) = do
    (x,size) <- wrapGetInfo (raw_clGetEventInfo obj param_name)
    case EventInfo param_name of
        c
            | c == clEventCommandQueue           -> peekOneInfo EventInfoRetvalCommandQueue x
            | c == clEventCommandType            -> peekOneInfo EventInfoRetvalCommandType x
            | c == clEventCommandExecutionStatus -> peekOneInfo EventInfoRetvalCLint x
            | c == clEventReferenceCount         -> peekOneInfo EventInfoRetvalCLuint x

clRetainEvent :: Event -> IO ()
clRetainEvent evt = wrapError $ raw_clRetainEvent evt

clReleaseEvent :: Event -> IO ()
clReleaseEvent evt = wrapError $ raw_clReleaseEvent evt 

clGetEventProfilingInfo :: Event -> ProfilingInfo -> IO CLEventProfilingInfoRetval
clGetEventProfilingInfo obj (ProfilingInfo param_name) = do
    (x,size) <- wrapGetInfo (raw_clGetEventProfilingInfo obj param_name)
    case ProfilingInfo param_name of
      c
            | c == clProfilingCommandQueued -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandSubmit -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandStart  -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandEnd    -> peekOneInfo EventProfilingInfoRetvalCLulong x
