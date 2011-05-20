module Coirc.Network.Primitive where
{-# IMPORT Coirc.Network.FFI #-}
{-# IMPORT System.IO #-}
open import Data.String
open import IO.Primitive
open import Foreign.Haskell

postulate Handle : Set
{-# COMPILED_TYPE Handle Handle #-}

postulate hConnect : String → IO Handle
{-# COMPILED hConnect hConnect #-}

postulate hPrint : Handle → String → IO Unit
{-# COMPILED hPrint hPrint #-}

postulate hGetLine : Handle → IO String
{-# COMPILED hGetLine hGetLine #-}
  
