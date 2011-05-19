module Coirc.Network where
open import Function
open import Coinduction
open import Data.String
open import Data.Maybe
open import Data.Colist
open import IO.Primitive
open import Coirc
open import Coirc.Parser
open import Coirc.Network.Primitive

abstract
  hGetFrom : Handle → IO (Colist From)
  hGetFrom h = hGetLine h >>= f where
    f : String → IO (Colist From)
    f s with parse-From (toList s)
    ... | nothing = hGetFrom h
    ... | just x = hGetFrom h >>= λ xs → return (x ∷ ♯ xs)

-- runBot : {bot : Bot} → Handle → (Colist From → Colist To) → IO String
-- runBot h f = {!!}
