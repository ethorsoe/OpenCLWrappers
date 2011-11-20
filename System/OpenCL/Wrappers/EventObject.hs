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
clWaitForEvents evts = withArray evts $ \eventP -> wrapError $ raw_clWaitForEvents (fromIntegral nEvents) eventP
    where nEvents = length evts
                            
clGetEventInfo :: Event -> EventInfo -> IO CLEventInfoRetval
clGetEventInfo obj c@(EventInfo param_name) = do
    (x,_) <- wrapGetInfo (raw_clGetEventInfo obj param_name)
    case () of
        ()
            | c == clEventCommandQueue           -> peekOneInfo EventInfoRetvalCommandQueue x
            | c == clEventCommandType            -> peekOneInfo EventInfoRetvalCommandType x
            | c == clEventCommandExecutionStatus -> peekOneInfo EventInfoRetvalCLint x
            | c == clEventReferenceCount         -> peekOneInfo EventInfoRetvalCLuint x
            | otherwise                          -> badArgument "clGetEventInfo" c

clRetainEvent :: Event -> IO ()
clRetainEvent evt = wrapError $ raw_clRetainEvent evt

clReleaseEvent :: Event -> IO ()
clReleaseEvent evt = wrapError $ raw_clReleaseEvent evt 

clGetEventProfilingInfo :: Event -> ProfilingInfo -> IO CLEventProfilingInfoRetval
clGetEventProfilingInfo obj c@(ProfilingInfo param_name) = do
    (x,_) <- wrapGetInfo (raw_clGetEventProfilingInfo obj param_name)
    case () of
      ()
            | c == clProfilingCommandQueued -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandSubmit -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandStart  -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | c == clProfilingCommandEnd    -> peekOneInfo EventProfilingInfoRetvalCLulong x
            | otherwise                     -> badArgument "clGetEventProfilingInfo" c
