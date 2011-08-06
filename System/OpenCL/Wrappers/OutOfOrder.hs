{-| Conforms to section 5.8 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.OutOfOrder
    (clEnqueueMarker
    ,clEnqueueWaitForEvents
    ,clEnqueueBarrier)
where 

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative
import Data.Maybe

clEnqueueMarker :: CommandQueue -> IO (Either ErrorCode Event)
clEnqueueMarker queue = alloca $ \eventP -> do
    err <- wrapError $ raw_clEnqueueMarker queue eventP
    if err == Nothing 
        then Right <$> peek eventP
        else return $ Left . fromJust $  err
    
clEnqueueWaitForEvents :: CommandQueue -> [Event] -> IO (Maybe ErrorCode)
clEnqueueWaitForEvents queue events = 
    allocaArray num_events $ \eventsP -> do
        pokeArray eventsP events
        wrapError $ raw_clEnqueueWaitForEvents queue (fromIntegral num_events) eventsP 
    where num_events = length events

clEnqueueBarrier :: CommandQueue -> IO (Maybe ErrorCode) 
clEnqueueBarrier queue = wrapError $ raw_clEnqueueBarrier queue
