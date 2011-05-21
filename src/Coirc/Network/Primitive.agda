module Coirc.Network.Primitive where
{-# IMPORT Coirc.Network.FFI #-}
{-# IMPORT System.IO #-}
open import Data.String
open import IO.Primitive
open import Foreign.Haskell

postulate Handle : Set
{-# COMPILED_TYPE Handle System.IO.Handle #-}

postulate hConnect : String → IO Handle
{-# COMPILED hConnect Coirc.Network.FFI.hConnect #-}

postulate hPutStr : Handle → String → IO Unit
{-# COMPILED hPutStr System.IO.hPutStr #-}

postulate hGetLine : Handle → IO String
{-# COMPILED hGetLine System.IO.hGetLine #-}
  
