{-|
    The OpenCL system for open heterogenous data parallel supercomputing.
    
    A fork of the FFI wrapper OpenCLRaw by Jeff Heard of the Renaissance Computing Institute.
    
    From the introduction: 
    
    OpenCL (Open Computing Language) is an open royalty-free standard for general purpose 
    parallel programming across CPUs, GPUs and other processors, giving software developers 
    portable and efficient access to the power of these heterogeneous processing platforms.   

    OpenCL supports a wide range of applications, ranging from embedded and consumer software 
    to HPC solutions, through a low-level, high-performance, portable abstraction.  By creating an 
    efficient, close-to-the-metal programming interface, OpenCL will form the foundation layer of a 
    parallel computing ecosystem of platform-independent tools, middleware and applications.  
    OpenCL is particularly suited to play an increasingly significant role in emerging interactive 
    graphics applications that combine general parallel compute algorithms with graphics rendering 
    pipelines. 

    OpenCL consists of an API for coordinating parallel computation across
    heterogeneous processors; and a cross-platform programming language with a well- 
    specified computation environment.  The OpenCL standard: 

    * Supports both data- and task-based parallel programming models 
    
    * Utilizes a subset of ISO C99 with extensions for parallelism 
    
    * Defines consistent numerical requirements based on IEEE 754 
    
    * Defines a configuration profile for handheld and embedded devices 
    
    * Efficiently interoperates with OpenGL, OpenGL ES and other graphics APIs
    
    _General Notes on the differences between Haskell and the OpenCL-C implementation_
    
    * Side-effectful procedures capable of returning an error code only return a Maybe ErrorCode, with Nothing returned upon success
    
    * Procedures which follow the pattern of returning a pointer to an object and taking a final parameter as an error code instead
      return Either ErrorCode @ObjectType@ 
      
    * Enumerations and constants are replaced by newtypes for the sake of type-safety.
-}
module System.OpenCL.Wrappers 
    (module System.OpenCL.Wrappers.CommandQueue
    ,module System.OpenCL.Wrappers.Context
    ,module System.OpenCL.Wrappers.DeviceInfo
    ,module System.OpenCL.Wrappers.Errors
    ,module System.OpenCL.Wrappers.Etc
    ,module System.OpenCL.Wrappers.EventObject
    ,module System.OpenCL.Wrappers.FlushFinish
    ,module System.OpenCL.Wrappers.Kernel
    ,module System.OpenCL.Wrappers.MemoryObject
    ,module System.OpenCL.Wrappers.OutOfOrder
    ,module System.OpenCL.Wrappers.PlatformInfo
    ,module System.OpenCL.Wrappers.ProgramObject
    ,module System.OpenCL.Wrappers.Sampler
    ,module System.OpenCL.Wrappers.Types    
    ,module System.OpenCL.Wrappers.Raw)    
where

import System.OpenCL.Wrappers.CommandQueue
import System.OpenCL.Wrappers.Context
import System.OpenCL.Wrappers.DeviceInfo
import System.OpenCL.Wrappers.Errors
import System.OpenCL.Wrappers.Etc
import System.OpenCL.Wrappers.EventObject
import System.OpenCL.Wrappers.FlushFinish
import System.OpenCL.Wrappers.Kernel
import System.OpenCL.Wrappers.MemoryObject
import System.OpenCL.Wrappers.OutOfOrder
import System.OpenCL.Wrappers.PlatformInfo
import System.OpenCL.Wrappers.ProgramObject
import System.OpenCL.Wrappers.Sampler
import System.OpenCL.Wrappers.Types
import System.OpenCL.Wrappers.Raw
