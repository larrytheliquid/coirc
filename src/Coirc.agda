module Coirc where
open import Coinduction
open import IO
open import Data.String
open import Data.Maybe

data SP (A B : Set) : Set where
  get : (A → SP A B) → SP A B
  put : B → ∞ (SP A B) → SP A B

data Event : Set where
  notice mode numeric ping : Event

data Action : Set where
  print : (text : String) → Action
  nick : (name : String) → Action
  user : (name real : String) → Action
  pong : (name : String) → Action
  quit : (text : String) → Action

Bot = SP Event Action

