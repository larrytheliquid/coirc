module Coirc where
open import Coinduction
open import Data.String
open import Data.Maybe
open import Data.Colist hiding (_++_)

data SP (A B : Set) : Set where
  get : (A → SP A B) → SP A B
  put : B → ∞ (SP A B) → SP A B

⟦_⟧SP : ∀ {A B} → SP A B → Colist A → Colist B
⟦ get f ⟧SP [] = []
⟦ get f ⟧SP (a ∷ as) = ⟦ f a ⟧SP (♭ as)
⟦ put b x ⟧SP as = b ∷ ♯ ⟦ ♭ x ⟧SP as

data Event : Set where
  notice mode numeric ping : Event

data Action : Set where
  print : (text : String) → Action
  pong : (nick : String) → Action

Bot = SP Event Action

