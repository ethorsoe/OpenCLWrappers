module System.OpenCL.Wrappers.OutOfOrder
    (clEnqueueMarker
    ,clEnqueueWaitForEvents
    ,clEnqueueBarrier)
where 

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign(withArray,peek,alloca)

clEnqueueMarker :: CommandQueue -> IO (Either ErrorCode Event)
clEnqueueMarker queue = alloca (\eventP ->
    wrapError (raw_clEnqueueMarker queue eventP) >>=
        maybe (fmap Right $ peek eventP) (return.Left))
    
clEnqueueWaitForEvents :: CommandQueue -> [Event] -> IO (Maybe ErrorCode)
clEnqueueWaitForEvents queue events =
    withArray events (\eventsP ->
        wrapError $ raw_clEnqueueWaitForEvents queue (fromIntegral num_events) eventsP)
    where num_events = length events

clEnqueueBarrier :: CommandQueue -> IO (Maybe ErrorCode) 
clEnqueueBarrier queue = wrapError $ raw_clEnqueueBarrier queue
