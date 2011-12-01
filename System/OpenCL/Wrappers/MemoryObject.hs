module System.OpenCL.Wrappers.MemoryObject 
    (clCreateBuffer
    ,clCreateImage2D
    ,clCreateImage3D
    ,clRetainMemObject
    ,clReleaseMemObject
    ,clGetSupportedImageFormats
    ,clGetMemObjectInfo
    ,clGetImageInfo
    ,clEnqueueReadBuffer
    ,clEnqueueWriteBuffer
    ,clEnqueueCopyBuffer
    ,clEnqueueReadImage
    ,clEnqueueWriteImage
    ,clEnqueueCopyImage
    ,clEnqueueCopyImageToBuffer
    ,clEnqueueCopyBufferToImage
    ,clEnqueueMapBuffer
    ,clEnqueueMapImage
    ,clEnqueueUnmapMemObject)
where

import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign


clCreateBuffer :: Context -> MemFlags -> CLsizei -> Ptr () -> IO Mem
clCreateBuffer ctx (MemFlags flags) size host_ptr = wrapErrorResult $ raw_clCreateBuffer ctx flags size host_ptr 
              
clCreateImage2D :: Context -> MemFlags -> ImageFormat -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> IO Mem
clCreateImage2D ctx (MemFlags memflags) (ChannelOrder corder, ChannelType ctype) image_width image_height image_row_pitch host_ptr = allocaArray 2 $ \image_format -> do
    pokeArray image_format [corder,ctype] 
    wrapErrorResult $ raw_clCreateImage2D ctx memflags image_format image_width image_height image_row_pitch host_ptr
                        
clCreateImage3D :: Context -> MemFlags -> ImageFormat -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> IO Mem
clCreateImage3D ctx (MemFlags memflags) (ChannelOrder corder, ChannelType ctype) image_width image_height image_depth image_row_pitch image_slice_pitch host_ptr = allocaArray 2 $ \image_format -> do
    pokeArray image_format [corder,ctype] 
    wrapErrorResult $ raw_clCreateImage3D ctx memflags image_format image_width image_height image_depth image_row_pitch image_slice_pitch host_ptr 
                        
clRetainMemObject :: Mem -> IO ()
clRetainMemObject mem = wrapError $ raw_clRetainMemObject mem

clReleaseMemObject :: Mem -> IO ()
clReleaseMemObject mem = wrapError $ raw_clReleaseMemObject mem
                                    
clGetSupportedImageFormats :: Context -> MemFlags -> MemObjectType -> IO [ImageFormat]
clGetSupportedImageFormats ctx (MemFlags flags) (MemObjectType image_type) = allocaArray 512 $ \image_formats -> alloca $ \num_image_formats -> do
    wrapError $ raw_clGetSupportedImageFormats ctx flags image_type 512 image_formats num_image_formats
    num_image_formatsN <- peek num_image_formats
    image_formatsN <- peekArray (fromIntegral num_image_formatsN*2) image_formats
    let sift (a:b:cs) = (ChannelOrder a,ChannelType b) : sift cs
        sift [] = []
        sift _  = undefined --peekArray returned a list of uneven length
    return $ sift image_formatsN

clGetMemObjectInfo :: Mem -> MemInfo -> IO CLMemObjectInfoRetval
clGetMemObjectInfo mem c@(MemInfo param_name) = do
    (x,_) <- wrapGetInfo $ raw_clGetMemObjectInfo mem param_name
    case () of
        ()
            | c == clMemType           -> peekOneInfo MemObjectInfoRetvalMemObjectType x
            | c == clMemFlags          -> peekOneInfo MemObjectInfoRetvalMemFlags x
            | c == clMemSize           -> peekOneInfo MemObjectInfoRetvalCLsizei x
            | c == clMemHostPtr        -> peekOneInfo MemObjectInfoRetvalPtr x
            | c == clMemMapCount       -> peekOneInfo MemObjectInfoRetvalCLuint x
            | c == clMemReferenceCount -> peekOneInfo MemObjectInfoRetvalCLuint x
            | c == clMemContext        -> peekOneInfo MemObjectInfoRetvalContext x
            | otherwise                -> badArgument "clGetMemObjectInfo" c

clGetImageInfo :: Mem -> MemInfo -> IO CLImageInfoRetval
clGetImageInfo mem c@(MemInfo param_name) = do
    (x,_) <- wrapGetInfo $ raw_clGetImageInfo mem param_name
    case () of
        ()
            | c == clImageElementSize -> peekOneInfo ImageInfoRetvalCLsizei x
            | c == clImageRowPitch    -> peekOneInfo ImageInfoRetvalCLsizei x
            | c == clImageSlicePitch  -> peekOneInfo ImageInfoRetvalCLsizei x
            | c == clImageWidth       -> peekOneInfo ImageInfoRetvalCLsizei x
            | c == clImageHeight      -> peekOneInfo ImageInfoRetvalCLsizei x
            | c == clImageDepth       -> peekOneInfo ImageInfoRetvalCLsizei x
            | otherwise               -> badArgument "clGetImageInfo" c
        
enqueue :: (CommandQueue -> CLuint -> Ptr Event -> Ptr Event -> IO CLint) -> CommandQueue -> [Event] -> IO Event
enqueue fn queue events = alloca $ \event -> withArrayNull events $ \event_wait_list -> do
    wrapError $ fn queue (fromIntegral events_in_wait_list) event_wait_list event
    peek event
    where events_in_wait_list = length events
    
clEnqueueReadBuffer :: Mem -> Bool -> CLsizei -> CLsizei -> Ptr () -> CommandQueue -> [Event] -> IO Event
clEnqueueReadBuffer buffer blocking_read offset cb ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> 
                raw_clEnqueueReadBuffer 
                    command_queue 
                    buffer 
                    (if blocking_read then clTrue else clFalse) 
                    offset 
                    cb 
                    ptr 
                    num_events_in_wait_list 
                    event_wait_list 
                    event)
                            
clEnqueueWriteBuffer :: Mem -> Bool -> CLsizei -> CLsizei -> Ptr () -> CommandQueue -> [Event] -> IO Event
clEnqueueWriteBuffer buffer blocking_write offset cb ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> 
                raw_clEnqueueWriteBuffer command_queue buffer (if blocking_write then clTrue else clFalse) offset cb ptr num_events_in_wait_list event_wait_list event)

clEnqueueCopyBuffer :: Mem -> Mem -> CLsizei -> CLsizei -> CLsizei -> CommandQueue -> [Event] -> IO Event
clEnqueueCopyBuffer src_buffer dst_buffer src_offset dst_offset cb = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> 
                raw_clEnqueueCopyBuffer command_queue src_buffer dst_buffer src_offset dst_offset cb num_events_in_wait_list event_wait_list event)

clEnqueueReadImage :: Mem -> Bool -> ImageDims -> ImageDims -> CLsizei -> CLsizei -> Ptr () -> CommandQueue -> [Event] -> IO Event
clEnqueueReadImage image blocking_read (oa,ob,oc) (ra,rb,rc) row_pitch slice_pitch ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray origin [oa,ob,oc]
                raw_clEnqueueReadImage command_queue image (if blocking_read then clTrue else clFalse) origin region row_pitch slice_pitch ptr num_events_in_wait_list event_wait_list event) 

clEnqueueWriteImage :: Mem -> Bool -> ImageDims -> ImageDims -> CLsizei -> CLsizei -> Ptr () -> CommandQueue -> [Event] -> IO Event
clEnqueueWriteImage image blocking_read (oa,ob,oc) (ra,rb,rc) row_pitch slice_pitch ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray origin [oa,ob,oc]
                raw_clEnqueueWriteImage command_queue image (if blocking_read then clTrue else clFalse) origin region row_pitch slice_pitch ptr num_events_in_wait_list event_wait_list event)                 

clEnqueueCopyImage :: Mem -> Mem -> ImageDims -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO Event
clEnqueueCopyImage src_image dst_image (soa,sob,soc) (doa,dob,doc) (ra,rb,rc) = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \src_origin -> allocaArray 3 $ \dst_origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray src_origin [soa,sob,soc]
                pokeArray dst_origin [doa,dob,doc]
                raw_clEnqueueCopyImage command_queue src_image dst_image src_origin dst_origin region num_events_in_wait_list event_wait_list event)

                           
clEnqueueCopyImageToBuffer :: Mem -> Mem -> ImageDims -> ImageDims -> CLsizei -> CommandQueue -> [Event] -> IO Event
clEnqueueCopyImageToBuffer src_image dst_buffer (soa,sob,soc) (ra,rb,rc) dst_offset = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \src_origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray src_origin [soa,sob,soc]
                raw_clEnqueueCopyImageToBuffer 
                    command_queue 
                    src_image 
                    dst_buffer 
                    src_origin 
                    region 
                    dst_offset 
                    num_events_in_wait_list 
                    event_wait_list 
                    event)


clEnqueueCopyBufferToImage :: Mem -> Mem -> CLsizei -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO Event
clEnqueueCopyBufferToImage src_buffer dst_image src_offset (soa,sob,soc) (ra,rb,rc) = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \dst_origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray dst_origin [soa,sob,soc]
                raw_clEnqueueCopyBufferToImage 
                    command_queue 
                    src_buffer 
                    dst_image 
                    src_offset 
                    dst_origin 
                    region 
                    num_events_in_wait_list 
                    event_wait_list 
                    event)


clEnqueueMapBuffer :: Mem -> Bool -> MapFlags -> CLsizei -> CLsizei -> CommandQueue -> [Event] -> IO (Ptr (),Event)
clEnqueueMapBuffer buffer blocking_map (MapFlags map_flags) offset cb command_queue events = 
    allocaArray num_events_in_wait_list $ \event_wait_list -> alloca $ \eventP -> do
        ptr <- wrapErrorResult $ raw_clEnqueueMapBuffer 
            command_queue 
            buffer 
            (if blocking_map then clTrue else clFalse) 
            map_flags 
            offset 
            cb 
            (fromIntegral num_events_in_wait_list) 
            event_wait_list
            eventP
        event <- peek eventP
        return (ptr,event)
    where num_events_in_wait_list = length events

clEnqueueMapImage :: Mem -> Bool -> MapFlags -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO (Ptr (),CLsizei,CLsizei,Event)
clEnqueueMapImage buffer blocking_map (MapFlags map_flags) (oa,ob,oc) (ra,rb,rc) command_queue events = 
    allocaArray num_events_in_wait_list $ \event_wait_list -> 
    alloca $ \event -> 
    allocaArray 3 $ \region -> 
    allocaArray 3 $ \origin -> 
    alloca $ \image_row_pitch -> 
    alloca $ \image_slice_pitch -> do
        pokeArray origin [oa,ob,oc]
        pokeArray region [ra,rb,rc]        
        ptr <- wrapErrorResult $ raw_clEnqueueMapImage
            command_queue 
            buffer 
            (if blocking_map then clTrue else clFalse) 
            map_flags 
            origin 
            region 
            image_row_pitch 
            image_slice_pitch 
            (fromIntegral num_events_in_wait_list) 
            event_wait_list 
            event
        event' <- peek event
        image_row_pitch' <- peek image_row_pitch
        image_slice_pitch' <- peek image_slice_pitch
        return (ptr,image_row_pitch',image_slice_pitch', event') 
        where num_events_in_wait_list = length events
                  
clEnqueueUnmapMemObject :: Mem -> Ptr () -> CommandQueue -> [Event] -> IO Event
clEnqueueUnmapMemObject mem mapped_ptr = enqueue
    (\command_queue num_events_in_wait_list event_wait_list event -> 
        raw_clEnqueueUnmapMemObject command_queue mem mapped_ptr num_events_in_wait_list event_wait_list event)

