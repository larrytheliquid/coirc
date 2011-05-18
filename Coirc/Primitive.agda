module Coirc.Primitive where
{-# IMPORT Coirc.FFI #-}
{-# IMPORT System.IO #-}
open import Data.String
open import IO.Primitive

postulate Handle : Set
{-# COMPILED_TYPE Handle Handle #-}

postulate connectTo : String → IO Handle
{-# COMPILED connectTo connectTo #-}

postulate hPrint : Handle → String → IO Handle
{-# COMPILED hPrint hPrint #-}

postulate hGetLine : Handle → IO String
{-# COMPILED hGetLine hGetLine #-}
  
