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
State = Connection

data Request : (pre post : State) → Set where
  nick : ∀ {state} (name : String) → Request state state
  user : (name real : String) → Request unregistered requested

  pong : (name : String) → Request registered registered

  privmsg : (target text : String) → Request registered registered

  quit : ∀ {state} (text : String) → Request state disconnected

data Response : (pre post : State) → Set where
  notice : ∀ {state} → Response state state

  welcome : (text : String) → Response requested registered

  mode numeric ping : Response registered registered

  privmsg : (source text : String) → Response registered registered

  error : ∀ {state} (text : String) → Response state disconnected

data Session : (pre post : State) → Set where
  get : ∀ {pre mid post} → (Request pre mid → Session mid post) → Session pre post
  put : ∀ {pre mid post} → Response pre mid → ∞ (Session mid post) → Session pre post



