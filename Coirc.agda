module Coirc where
open import Coinduction
open import Data.String
open import Data.Maybe
open import Data.Stream hiding (_++_)

data SP (A B : Set) : Set where
  get : (A → SP A B) → SP A B
  put : B → ∞ (SP A B) → SP A B

⟦_⟧SP : ∀ {A B} → SP A B → Stream A → Stream B
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

Irc : Set
Irc = SP From To

bot : Irc
bot = get f where
  f : From → Irc
  f (notice from text) = put (print ("Notice from: " ++ from ++ "\n Notice text: " ++ text))
    (♯ bot)
  f (ping server) = put (print ("Ping from: " ++ server))
    (♯ put pong (♯ bot))
  f (message from text) = put (print ("Message from: " ++ from ++ "\n Message text: " ++ text))
    (♯ put (message from "Greetings from your friendly pointed coalgebra!") (♯ bot))

