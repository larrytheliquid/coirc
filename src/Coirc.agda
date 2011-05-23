module Coirc where
open import Coinduction
open import IO
open import Data.String
open import Data.Maybe
open import Data.List hiding (drop)
open import Data.List.Any
open Membership-≡

data Connection : Set where
  unregistered requested registered disconnected : Connection

data Channel : Set where
  requested joined : String → Channel

Channels = List Channel

drop : {x : Channel}{xs : Channels} → x ∈ xs → Channels
drop (here {xs = xs} px) = xs
drop (there {x = x} pxs) = x ∷ drop pxs

grant-join : ∀ {x} {xs : Channels} → requested x ∈ xs → Channels
grant-join (here {requested x} {xs = xs} px) = joined x ∷ xs
grant-join (here {joined y} ())
grant-join (there {x = x} pxs) = x ∷ grant-join pxs

infixr 4 _,_
record State : Set where
  constructor _,_
  field
    connection : Connection
    channels : Channels

data Event : (pre post : State) → Set where
  welcome : ∀ {chans} (text : String) → Event (requested , chans) (registered , chans)
  notice : ∀ {state} → Event state state
  mode numeric ping : ∀ {chans} → Event (registered , chans) (registered , chans)
  privmsg : ∀ {chans} (source text : String) → Event (registered , chans) (registered , chans)
  error : ∀ {state} (text : String) → Event state (disconnected , [])
  join : ∀ {chans} (chan : String) (p : requested chan ∈ chans) →
    Event (registered , chans) (registered , grant-join p)
  channel-full : ∀ {chans} (chan : String) (p : requested chan ∈ chans) →
    Event (registered , chans) (registered , drop p)

data Action : (pre post : State) → Set where
  print : ∀ {state} (text : String) → Action state state
  nick : ∀ {state} (name : String) → Action state state
  user : ∀ {chans} (name real : String) → Action (unregistered , chans) (requested , chans)
  pong : ∀ {chans} (name : String) → Action (registered , chans) (registered , chans)
  join : ∀ {chans} {chan-state : String → Channel} (chan : String) → chan-state chan ∉ chans →
    Action (registered , chans) (registered , requested chan ∷ chans)
  part : ∀ {chans} (chan : String) (p : joined chan ∈ chans) → Action (registered , chans) (registered , drop p)
  privmsg : ∀ {chans} (target text : String) → Action (registered , chans) (registered , chans)
  quit : ∀ {state} (text : String) → Action state (disconnected , [])

data Bot : (pre post : State) → Set where
  get : ∀ {pre mid post} → (Event pre mid → Bot mid post) → Bot pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (Bot mid post) → Bot pre post



