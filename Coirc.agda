module Coirc where
open import Coinduction
open import Data.String
open import Data.Maybe
open import Data.Colist hiding (_++_)

server = "irc.freenode.org"
port = 6667
nick = "foobot-name"

data SP (A B : Set) : Set where
  get : (A → SP A B) → SP A B
  put : B → ∞ (SP A B) → SP A B

⟦_⟧SP : ∀ {A B} → SP A B → Colist A → Colist B
⟦ get f ⟧SP [] = []
⟦ get f ⟧SP (a ∷ as) = ⟦ f a ⟧SP (♭ as)
⟦ put b x ⟧SP as = b ∷ ♯ ⟦ ♭ x ⟧SP as

data From : Set where
  notice : (from text : String) → From
  ping : (server : String) → From
  message : (from text : String) → From

data To : Set where
  print : (text : String) → To
  pong : To
  message : (to text : String) → To

Bot = SP From To

