module Coirc where
open import Coinduction
open import IO
open import Data.String
open import Data.Maybe

data Connection : Set where
  unregistered registration-requested registered disconnected : Connection

data Event : (pre post : Connection) → Set where
  welcome : (text : String) → Event registration-requested registered
  notice : ∀ {conn} → Event conn conn
  mode numeric ping : Event registered registered
  privmsg : (source text : String) → Event registered registered
  error : ∀ {conn} (text : String) → Event conn disconnected

data Action : (pre post : Connection) → Set where
  print : ∀ {conn} (text : String) → Action conn conn
  nick : ∀ {conn} (name : String) → Action conn conn
  user : (name real : String) → Action unregistered registration-requested
  pong : (name : String) → Action registered registered
  join part : (channel : String) → Action registered registered
  privmsg : (target text : String) → Action registered registered
  quit : ∀ {conn} (text : String) → Action conn disconnected

data Bot : (pre post : Connection) → Set where
  get : ∀ {pre mid post} → (Event pre mid → Bot mid post) → Bot pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (Bot mid post) → Bot pre post



