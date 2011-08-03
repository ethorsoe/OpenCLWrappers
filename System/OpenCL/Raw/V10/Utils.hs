{-| OpenCL utility functions for improving FFI wrapper code. -}
module System.OpenCL.Raw.V10.Utils where

import Foreign
import Foreign.C
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Types
import Control.Applicative
import Data.Maybe
import Control.Monad.Cont

wrapError :: IO CLint -> IO (Maybe ErrorCode)
wrapError thunk = thunk >>= \errcode -> if ErrorCode errcode == clSuccess then return Nothing else return . Just . ErrorCode $ errcode

wrapErrorEither :: (Ptr CLint -> IO a) -> IO (Either ErrorCode a)
wrapErrorEither thunk = alloca $ \errorP -> do
    ret <- thunk errorP
    err <- ErrorCode <$> peek errorP
    if err == clSuccess
        then return . Right $ ret
        else return . Left $ err 
                
wrapGetInfo :: (CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint) -> IO (Either ErrorCode (ForeignPtr ()))
wrapGetInfo raw_infoFn = alloca $ \value_size_ret ->
    wrapError (raw_infoFn 0 nullPtr value_size_ret) >>=
        maybe (do retsize <- peek value_size_ret
                  param_data <- (mallocForeignPtrBytes . fromIntegral $ retsize) :: IO (ForeignPtr ())
                  wrapError (withForeignPtr param_data $ \param_dataP -> raw_infoFn retsize param_dataP nullPtr) >>=
                      maybe (return (Right param_data)) (return.Left))
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
