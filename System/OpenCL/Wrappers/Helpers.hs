{-# LANGUAGE ExistentialQuantification #-}
{-| Some helper functions that may or may not be useful to anyone. -}
module System.OpenCL.Wrappers.Helpers
    (createSyncKernel
    ,createAsyncKernelWithParams
    ,buildProgram
    ,pushKernelParams)
where

import Prelude hiding(catch)
import System.OpenCL.Wrappers.Kernel
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.ProgramObject
import System.OpenCL.Wrappers.FlushFinish
import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative
import Control.Exception

pushKernelParams :: forall b. Storable b => Kernel -> CLuint -> [b] -> IO ()
pushKernelParams kernel argNum (x:xs) = do
    withArray [x] $ \y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)
    pushKernelParams kernel (argNum + 1) xs
pushKernelParams _ _ _ = return ()

syncKernelFun :: forall b. Storable b => CLuint -> Kernel -> CommandQueue -> [CLsizei] -> [CLsizei] -> [b] -> IO ()
syncKernelFun _ kernel queue a b [] = do
    clEnqueueNDRangeKernel queue kernel a b []
    clFinish queue
syncKernelFun argNum kernel queue a b (x:xs) = do
    withArray [x] $ \y -> clSetKernelArg kernel argNum (fromIntegral.sizeOf $ x) (castPtr y)
    syncKernelFun (argNum + 1) kernel queue a b xs

createSyncKernel :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> IO ([b] -> IO ())
createSyncKernel program queue initFun globalWorkRange localWorkRange = do
    k <- clCreateKernel program initFun
    return $ syncKernelFun 0 k queue (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange)

createAsyncKernelWithParams :: forall b. Storable b => Program -> CommandQueue -> String -> [Int] -> [Int] -> [b] -> IO ([Event] -> IO Event)
createAsyncKernelWithParams program queue initFun globalWorkRange localWorkRange params = do
    k <- clCreateKernel program initFun
    pushKernelParams k 0 params
    return $ clEnqueueNDRangeKernel queue k (map fromIntegral globalWorkRange) (map fromIntegral localWorkRange)

buildProgram :: String -> String -> Context -> DeviceID -> IO Program
buildProgram source opts context dID = do
    program <- clCreateProgramWithSource context source
    clBuildProgram program [dID] opts Nothing nullPtr `catch` \(CLError eCode) -> do
        err <- reportBuildFailure program dID eCode
        clReleaseProgram program
        throwIO err
    return program

reportBuildFailure :: Program -> DeviceID -> ErrorCode -> IO SomeException
reportBuildFailure program dID eCode =
    handle (\err -> return $ toException (err :: SomeCLException)) $ do
        ProgramBuildInfoRetvalString s <- clGetProgramBuildInfo program dID clProgramBuildLog
        return $ toException (CLBuildError eCode s)
