module System.OpenCL.Wrappers.OutOfOrder
    (clEnqueueMarker
    ,clEnqueueWaitForEvents
    ,clEnqueueBarrier)
where 

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign(withArray,peek,alloca)

clEnqueueMarker :: CommandQueue -> IO Event
clEnqueueMarker queue = alloca $ \eventP -> do
    raw_clEnqueueMarker queue eventP
    peek eventP
    
clEnqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
clEnqueueWaitForEvents queue events =
    withArray events $ \eventsP ->
        wrapError $ raw_clEnqueueWaitForEvents queue (fromIntegral num_events) eventsP
    where num_events = length events

clEnqueueBarrier :: CommandQueue -> IO ()
clEnqueueBarrier queue = wrapError $ raw_clEnqueueBarrier queue
