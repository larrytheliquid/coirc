module Coirc.Network where
open import Coirc
open import Data.String
open import Data.Colist
open import IO.Primitive
open import Coirc.Network.Primitive

runBot : {bot : Bot} → Handle → (Colist From → Colist To) → IO String
runBot h f = {!!}
