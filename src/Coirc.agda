module Coirc where
open import Coinduction
open import IO
open import Data.String
open import Data.Maybe

data Connection : Set where
  unregistered requested registered disconnected : Connection

data Channel : Set where
  outside inside : Channel

infixr 4 _,_
record State : Set where
  constructor _,_
  field
    connection : Connection
    channel : Channel

data Event : (pre post : State) → Set where
  welcome : ∀ {chan} (text : String) → Event (requested , chan) (registered , chan)
  notice : ∀ {state} → Event state state
  mode numeric ping : ∀ {chan} → Event (registered , chan) (registered , chan)
  privmsg : ∀ {chan} (source text : String) → Event (registered , chan) (registered , chan)
  error : ∀ {state} (text : String) → Event state (disconnected , outside)

data Action : (pre post : State) → Set where
  print : ∀ {state} (text : String) → Action state state
  nick : ∀ {state} (name : String) → Action state state
  user : ∀ {chan} (name real : String) → Action (unregistered , chan) (requested , chan)
  pong : ∀ {chan} (name : String) → Action (registered , chan) (registered , chan)
  join : (channel : String) → Action (registered , outside) (registered , inside)
  part : (channel : String) → Action (registered , inside) (registered , outside)
  privmsg : ∀ {chan} (target text : String) → Action (registered , chan) (registered , chan)
  quit : ∀ {state} (text : String) → Action state (disconnected , outside)

data Bot : (pre post : State) → Set where
  get : ∀ {pre mid post} → (Event pre mid → Bot mid post) → Bot pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (Bot mid post) → Bot pre post



