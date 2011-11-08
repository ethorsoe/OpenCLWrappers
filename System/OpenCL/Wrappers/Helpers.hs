{-# LANGUAGE ExistentialQuantification #-}
{-| Conforms to section 5.5 of the OpenCL 1.0 specification -}
module System.OpenCL.Wrappers.Helpers
    (createSyncKernel
    ,createAsyncKernelWithParams
    ,buildProgram)
where

import System.OpenCL.Wrappers.Kernel
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.ProgramObject
import System.OpenCL.Wrappers.FlushFinish
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

pushParams :: forall b. Storable b => Kernel -> CLuint -> [b] -> IO (Maybe ErrorCode)
pushParams kernel argNum (x:xs) = 
    withArray [x] (\y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)) >>=
        maybe (pushParams kernel (argNum + 1) xs) (return.Just)
pushParams _ _ _ = return Nothing

syncKernelFun :: forall b. Storable b => CLuint -> Kernel -> CommandQueue -> [CLsizei] -> [CLsizei] -> [b] -> IO (Maybe ErrorCode)
syncKernelFun _ kernel queue a b [] =
        clEnqueueNDRangeKernel queue kernel a b [] >>=
            either (return.Just) (\_ -> clFinish queue >>= maybe (return Nothing) (return.Just))
syncKernelFun argNum kernel queue a b (x:xs) =
        withArray [x] (\y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)) >>=
            maybe (syncKernelFun (argNum + 1) kernel queue a b xs) (return.Just)

createSyncKernel :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> IO (Either ErrorCode ([b] -> IO (Maybe ErrorCode)))
createSyncKernel program queue initFun globalWorkRange localWorkRange =
        clCreateKernel program initFun >>=
            either (return.Left) (\k -> return.Right $ syncKernelFun 0 k queue (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange))

createAsyncKernelWithParams :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> [b] -> IO (Either ErrorCode ([Event] -> IO (Either ErrorCode Event)))
createAsyncKernelWithParams program queue initFun globalWorkRange localWorkRange params =
        clCreateKernel program initFun >>=
            either (return.Left) (\k -> pushParams k 0 params >>=
                maybe (return.Right $ clEnqueueNDRangeKernel queue k (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange)) (return.Left)) 

buildProgram :: String -> String -> Context -> DeviceID -> IO (Either (ErrorCode, String) Program)
buildProgram source opts context dID =
    clCreateProgramWithSource context source >>=
        either (\x -> return $ Left (x, "")) (\program -> clBuildProgram program [dID] opts Nothing nullPtr >>=
            maybe (return $ Right program) (\x -> do
                y <- fmap Left $ reportBuildFailure program dID x
                _ <- clReleaseProgram program
                return y))

reportBuildFailure :: Program -> DeviceID -> ErrorCode -> IO (ErrorCode,String)
reportBuildFailure program dID eCode = clGetProgramBuildInfo program dID clProgramBuildLog >>=
        either (\x -> return (x,"")) (\x -> case x of
            (ProgramBuildInfoRetvalString s) -> return (eCode,s)
            _                                -> undefined) 

