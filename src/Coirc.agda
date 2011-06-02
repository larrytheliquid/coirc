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

data Request : (pre post : State) → Set where
  nick : ∀ {state} (name : String) → Request state state
  user : ∀ {chans} (name real : String) → Request (unregistered , chans) (requested , chans)

  pong : ∀ {chans} (name : String) → Request (registered , chans) (registered , chans)

  join : ∀ {chans} {chan-state : String → Channel} (chan : String) → chan-state chan ∉ chans →
    Request (registered , chans) (registered , requested chan ∷ chans)
  part : ∀ {chans} (chan : String) (p : joined chan ∈ chans) → Request (registered , chans) (registered , drop p)

  privmsg : ∀ {chans} (target text : String) → Request (registered , chans) (registered , chans)

  quit : ∀ {state} (text : String) → Request state (disconnected , [])

data Response : (pre post : State) → Set where
  notice : ∀ {state} → Response state state

  welcome : ∀ {chans} (text : String) → Response (requested , chans) (registered , chans)

  mode numeric ping : ∀ {chans} → Response (registered , chans) (registered , chans)

  privmsg : ∀ {chans} (source text : String) → Response (registered , chans) (registered , chans)

  error : ∀ {state} (text : String) → Response state (disconnected , [])

  join : ∀ {chans} (chan : String) (p : requested chan ∈ chans) →
    Response (registered , chans) (registered , grant-join p)

  channel-full : ∀ {chans} (chan : String) (p : requested chan ∈ chans) →
    Response (registered , chans) (registered , drop p)

data Session : (pre post : State) → Set where
  get : ∀ {pre mid post} → (Request pre mid → Session mid post) → Session pre post
  put : ∀ {pre mid post} → Response pre mid → ∞ (Session mid post) → Session pre post



