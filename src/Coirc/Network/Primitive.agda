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

postulate hPrint : Handle → String → IO Unit
{-# COMPILED hPrint System.IO.hPrint #-}

postulate hGetLine : Handle → IO String
{-# COMPILED hGetLine System.IO.hGetLine #-}
  
