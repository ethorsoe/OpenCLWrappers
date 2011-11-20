{-| OpenCL utility functions for improving FFI wrapper code. -}
module System.OpenCL.Wrappers.Utils where

import Prelude hiding(catch)
import Foreign
import Foreign.C
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Types
import Control.Applicative
import Data.Maybe
import Control.Monad.Cont
import Control.Exception

wrapError :: IO CLint -> IO ()
wrapError action = do
    err <- ErrorCode <$> action
    when (err /= clSuccess) $ throwIO (CLError err)

wrapErrorResult :: (Ptr CLint -> IO a) -> IO a
wrapErrorResult action = alloca $ \errorP -> do
    ret <- action errorP
    err <- ErrorCode <$> peek errorP
    if err == clSuccess
        then return ret
        else throwIO (CLError err)

wrapGetInfo :: (CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint) -> IO (ForeignPtr (), CLsizei)
wrapGetInfo raw_infoFn = alloca $ \value_size_ret -> do
    raw_infoFn 0 nullPtr value_size_ret
    retsize <- peek value_size_ret
    param_data <- (mallocForeignPtrBytes . fromIntegral $ retsize) :: IO (ForeignPtr ())
    wrapError . withForeignPtr param_data $ \param_dataP -> raw_infoFn retsize param_dataP nullPtr
    return (param_data, retsize)

wrapGetNumElements :: Storable a => (CLuint -> Ptr a -> Ptr CLuint -> IO CLint) -> IO [a]
wrapGetNumElements raw_Fn = alloca $ \value_size_ret -> do
    raw_Fn 0 nullPtr value_size_ret
    retsize <- peek value_size_ret
    allocaArray (fromIntegral retsize) $ \param_dataP -> do
        wrapError $ raw_Fn retsize param_dataP nullPtr
        peekArray (fromIntegral retsize) param_dataP

withArrayNull0 a as = withArrayNull $ as ++ [a]

withArrayNull :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayNull as f = if null as
                           then f nullPtr
                           else withArray as f

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (sequence (map cont xs))

withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act = nest (map withCString strings)
                                     (\rs -> withArray0 nullPtr rs act)

peekOneInfo :: Storable a => (a -> b) -> ForeignPtr () -> IO b
peekOneInfo f x = withForeignPtr x (\y -> fmap f (peek $ castPtr y))

peekManyInfo :: Storable a => ([a] -> b) -> ForeignPtr () -> CLsizei -> IO b
peekManyInfo f x size = do
    c <- return undefined
    a <- withForeignPtr x (\y -> (peekArray ( div (fromIntegral size) $ sizeOf c) $ castPtr y))
    return (c:a)
    return $ f a

peekStringInfo :: (String -> b) -> ForeignPtr () -> IO b
peekStringInfo f x = withForeignPtr x (\y -> fmap f (peekCString $ castPtr y))
