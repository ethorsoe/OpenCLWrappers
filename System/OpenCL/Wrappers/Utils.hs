{-| OpenCL utility functions for improving FFI wrapper code. -}
module System.OpenCL.Wrappers.Utils where

import Foreign
import Foreign.C
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Types
import Control.Applicative
import Data.Maybe
import Control.Monad.Cont
import Data.Bits((.|.))
import Unsafe.Coerce(unsafeCoerce)

wrapError :: IO CLint -> IO (Maybe ErrorCode)
wrapError thunk = thunk >>= \errcode -> if ErrorCode errcode == clSuccess then return Nothing else return . Just . ErrorCode $ errcode

wrapErrorEither :: (Ptr CLint -> IO a) -> IO (Either ErrorCode a)
wrapErrorEither thunk = alloca $ \errorP -> do
    ret <- thunk errorP
    err <- ErrorCode <$> peek errorP
    if err == clSuccess
        then return . Right $ ret
        else return . Left $ err 
                
wrapGetInfo :: (CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint) -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
wrapGetInfo raw_infoFn = alloca $ \value_size_ret ->
    wrapError (raw_infoFn 0 nullPtr value_size_ret) >>=
        maybe (do retsize <- peek value_size_ret
                  param_data <- (mallocForeignPtrBytes . fromIntegral $ retsize) :: IO (ForeignPtr ())
                  wrapError (withForeignPtr param_data $ \param_dataP -> raw_infoFn retsize param_dataP nullPtr) >>=
                      maybe (return (Right (param_data,retsize))) (return.Left))
                  (return.Left)

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

combineOr :: [a] -> a
combineOr x = unsafeCoerce $ foldl (\x y -> x .|. unsafeCoerce y) (0 :: CLuint) x
