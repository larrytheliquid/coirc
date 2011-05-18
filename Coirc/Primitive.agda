module Coirc.Primitive where
{-# IMPORT Coirc.FFI #-}
{-# IMPORT System.IO #-}
open import Data.String
open import IO.Primitive

postulate Handle : Set
{-# COMPILED_TYPE Handle System.IO.Handle #-}

postulate connectTo : String → IO Handle
{-# COMPILED connectTo Coirc.FFI.connectTo #-}
  
