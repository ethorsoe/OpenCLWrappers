{-| Conforms to section 5.2 of the OpenCL 1.0 specification -}
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
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Utils
import System.OpenCL.Wrappers.Raw
import Foreign
import Control.Applicative
import Data.Maybe
import Data.Bits


clCreateBuffer :: Context -> MemFlags -> CLsizei -> Ptr () -> IO (Either ErrorCode Mem)
clCreateBuffer ctx (MemFlags flags) size host_ptr = wrapErrorEither $ raw_clCreateBuffer ctx flags size host_ptr 
              
clCreateImage2D :: Context -> MemFlags -> ImageFormat -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> IO (Either ErrorCode Mem)
clCreateImage2D ctx (MemFlags memflags) (ChannelOrder corder, ChannelType ctype) image_width image_height image_row_pitch host_ptr = allocaArray 2 $ \image_format -> do
    pokeArray image_format [corder,ctype] 
    wrapErrorEither $ raw_clCreateImage2D ctx memflags image_format image_width image_height image_row_pitch host_ptr
                        
clCreateImage3D :: Context -> MemFlags -> ImageFormat -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> CLsizei -> Ptr () -> IO (Either ErrorCode Mem)
clCreateImage3D ctx (MemFlags memflags) (ChannelOrder corder, ChannelType ctype) image_width image_height image_depth image_row_pitch image_slice_pitch host_ptr = allocaArray 2 $ \image_format -> do
    pokeArray image_format [corder,ctype] 
    wrapErrorEither $ raw_clCreateImage3D ctx memflags image_format image_width image_height image_depth image_row_pitch image_slice_pitch host_ptr 
                        
clRetainMemObject :: Mem -> IO (Maybe ErrorCode) 
clRetainMemObject mem = wrapError $ raw_clRetainMemObject mem

clReleaseMemObject :: Mem -> IO (Maybe ErrorCode) 
clReleaseMemObject mem = wrapError $ raw_clReleaseMemObject mem
                                    
clGetSupportedImageFormats :: Context -> MemFlags -> MemObjectType -> IO (Either ErrorCode [ImageFormat])
clGetSupportedImageFormats ctx (MemFlags flags) (MemObjectType image_type) = allocaArray 512 $ \image_formats -> alloca $ \num_image_formats -> do
    err <- wrapError $ raw_clGetSupportedImageFormats ctx flags image_type 512 image_formats num_image_formats
    maybe (do num_image_formatsN <- peek num_image_formats
              image_formatsN <- peekArray (fromIntegral num_image_formatsN*2) image_formats
              let sift (a:b:cs) = (ChannelOrder a,ChannelType b) : sift cs
                  sift [] = [] 
              return . Right $ sift image_formatsN )
          (return . Left) 
          err

clGetMemObjectInfo :: Mem -> MemInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetMemObjectInfo mem (MemInfo param_name) = wrapGetInfo (raw_clGetMemObjectInfo mem param_name)

clGetImageInfo :: Mem -> MemInfo -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetImageInfo mem (MemInfo param_name) = wrapGetInfo (raw_clGetImageInfo mem param_name)
        
enqueue :: (CommandQueue -> CLuint -> Ptr Event -> Ptr Event -> IO CLint) -> CommandQueue -> [Event] -> IO (Either ErrorCode Event)      
enqueue fn queue events = alloca $ \event -> withArrayNull events $ \event_wait_list -> do
    err <- wrapError $ fn queue (fromIntegral events_in_wait_list) event_wait_list event
    if err == Nothing 
        then Right <$> peek event 
        else return (Left . fromJust $ err)
    where events_in_wait_list = length events
    
    
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
                            

clEnqueueWriteBuffer buffer blocking_write offset cb ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> 
                raw_clEnqueueWriteBuffer command_queue buffer (if blocking_write then clTrue else clFalse) offset cb ptr num_events_in_wait_list event_wait_list event)  


clEnqueueCopyBuffer src_buffer dst_buffer src_offset dst_offset cb = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> 
                raw_clEnqueueCopyBuffer command_queue src_buffer dst_buffer src_offset dst_offset cb num_events_in_wait_list event_wait_list event)                       

type ImageDims = (CLsizei,CLsizei,CLsizei)
clEnqueueReadImage image blocking_read (oa,ob,oc) (ra,rb,rc) row_pitch slice_pitch ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray origin [oa,ob,oc]
                raw_clEnqueueReadImage command_queue image (if blocking_read then clTrue else clFalse) origin region row_pitch slice_pitch ptr num_events_in_wait_list event_wait_list event) 
                    
clEnqueueWriteImage image blocking_read (oa,ob,oc) (ra,rb,rc) row_pitch slice_pitch ptr = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray origin [oa,ob,oc]
                raw_clEnqueueWriteImage command_queue image (if blocking_read then clTrue else clFalse) origin region row_pitch slice_pitch ptr num_events_in_wait_list event_wait_list event)                 

clEnqueueCopyImage :: Mem -> Mem -> ImageDims -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO (Either ErrorCode Event)  
clEnqueueCopyImage src_image dst_image (soa,sob,soc) (doa,dob,doc) (ra,rb,rc) = 
    enqueue (\command_queue num_events_in_wait_list event_wait_list event -> allocaArray 3 $ \src_origin -> allocaArray 3 $ \dst_origin -> allocaArray 3 $ \region -> do 
                pokeArray region [ra,rb,rc]
                pokeArray src_origin [soa,sob,soc]
                pokeArray dst_origin [doa,dob,doc]
                raw_clEnqueueCopyImage command_queue src_image dst_image src_origin dst_origin region num_events_in_wait_list event_wait_list event)

                           
clEnqueueCopyImageToBuffer :: Mem -> Mem -> ImageDims -> ImageDims -> CLsizei -> CommandQueue -> [Event] -> IO (Either ErrorCode Event)  
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


clEnqueueCopyBufferToImage :: Mem -> Mem -> CLsizei -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO (Either ErrorCode Event)  
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


clEnqueueMapBuffer :: Mem -> Bool -> MapFlags -> CLsizei -> CLsizei -> CommandQueue -> [Event] -> IO (Either ErrorCode (Ptr (),Event))
clEnqueueMapBuffer buffer blocking_map (MapFlags map_flags) offset cb command_queue events = 
    allocaArray num_events_in_wait_list $ \event_wait_list -> alloca $ \event -> do
        ret <- wrapErrorEither $ raw_clEnqueueMapBuffer 
            command_queue 
            buffer 
            (if blocking_map then clTrue else clFalse) 
            map_flags 
            offset 
            cb 
            (fromIntegral num_events_in_wait_list) 
            event_wait_list 
            event
        case ret of 
            Left err -> return (Left err)
            Right ptr -> peek event >>= \event -> return $ Right (ptr,event)
    where num_events_in_wait_list = length events

clEnqueueMapImage :: Mem -> Bool -> MapFlags -> ImageDims -> ImageDims -> CommandQueue -> [Event] -> IO (Either ErrorCode (Ptr (),CLsizei,CLsizei,Event))
clEnqueueMapImage buffer blocking_map (MapFlags map_flags) (oa,ob,oc) (ra,rb,rc) command_queue events = 
    allocaArray num_events_in_wait_list $ \event_wait_list -> 
    alloca $ \event -> 
    allocaArray 3 $ \region -> 
    allocaArray 3 $ \origin -> 
    alloca $ \image_row_pitch -> 
    alloca $ \image_slice_pitch -> do
        pokeArray origin [oa,ob,oc]
        pokeArray region [ra,rb,rc]        
        ret <- wrapErrorEither $ raw_clEnqueueMapImage
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
        case ret of 
            Left err -> return (Left err)
            Right ptr -> do 
                event' <- peek event
                image_row_pitch' <- peek image_row_pitch
                image_slice_pitch' <- peek image_slice_pitch
                return  $ Right (ptr,image_row_pitch',image_slice_pitch', event') 
        where num_events_in_wait_list = length events
                  

clEnqueueUnmapMemObject mem mapped_ptr = enqueue
    (\command_queue num_events_in_wait_list event_wait_list event -> 
        raw_clEnqueueUnmapMemObject command_queue mem mapped_ptr num_events_in_wait_list event_wait_list event)


