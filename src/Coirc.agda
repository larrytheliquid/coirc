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

data User : Set where
  requested joined : String → User

Users = List User

drop : {x : User}{xs : Users} → x ∈ xs → Users
drop (here {xs = xs} px) = xs
drop (there {x = x} pxs) = x ∷ drop pxs

grant-join : ∀ {x} {xs : Users} → requested x ∈ xs → Users
grant-join (here {requested x} {xs = xs} px) = joined x ∷ xs
grant-join (here {joined y} ())
grant-join (there {x = x} pxs) = x ∷ grant-join pxs

infixr 4 _,_
record State : Set where
  constructor _,_
  field
    connection : Connection
    users : Users

data Request : (pre post : State) → Set where
  nick : ∀ {state} (name : String) → Request state state
  user : ∀ {users} (name real : String) → Request (unregistered , users) (requested , users)

  pong : ∀ {users} (name : String) → Request (registered , users) (registered , users)

  join : ∀ {users} {chan-state : String → User} (chan : String) → chan-state chan ∉ users →
    Request (registered , users) (registered , requested chan ∷ users)
  part : ∀ {users} (chan : String) (p : joined chan ∈ users) → Request (registered , users) (registered , drop p)

  privmsg : ∀ {users} (target text : String) → Request (registered , users) (registered , users)

  quit : ∀ {state} (text : String) → Request state (disconnected , [])

data Response : (pre post : State) → Set where
  notice : ∀ {state} → Response state state

  welcome : ∀ {users} (text : String) → Response (requested , users) (registered , users)

  mode numeric ping : ∀ {users} → Response (registered , users) (registered , users)

  privmsg : ∀ {users} (source text : String) → Response (registered , users) (registered , users)

  error : ∀ {state} (text : String) → Response state (disconnected , [])

  join : ∀ {users} (chan : String) (p : requested chan ∈ users) →
    Response (registered , users) (registered , grant-join p)

  channel-full : ∀ {users} (chan : String) (p : requested chan ∈ users) →
    Response (registered , users) (registered , drop p)

data Session : (pre post : State) → Set where
  get : ∀ {pre mid post} → (Request pre mid → Session mid post) → Session pre post
  put : ∀ {pre mid post} → Response pre mid → ∞ (Session mid post) → Session pre post



