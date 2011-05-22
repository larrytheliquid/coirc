module Coirc where
open import Coinduction
open import IO
open import Data.String
open import Data.Maybe

data Connection : Set where
  unregistered registered : Connection

data Event : Connection → Set where
  welcome : (text : String) → Event registered
  notice : ∀ {conn} → Event conn
  mode numeric ping : Event registered
  privmsg : (source text : String) → Event registered

data Action : (pre post : Connection) → Set where
  print : ∀ {conn} (text : String) → Action conn conn
  nick : ∀ {conn} (name : String) → Action conn conn
  user : (name real : String) → Action unregistered registered
  pong : (name : String) → Action registered registered
  join part : (channel : String) → Action registered registered
  privmsg : (target text : String) → Action registered registered
  quit : ∀ {conn} (text : String) → Action conn unregistered

data Bot : (pre post : Connection) → Set where
  get : ∀ {pre post} → (Event pre → Bot pre post) → Bot pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (Bot mid post) → Bot pre post



