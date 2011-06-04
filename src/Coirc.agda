module Coirc where
open import Coinduction
open import IO
open import Data.Nat
open import Data.Fin
open import Data.String
open import Data.Maybe
open import Data.Vec
open import Data.Sum

data Connection : Set where
  unregistered closed : Connection
  requested-nick : (nickname : String) → Connection
  requested-user : (user realname : String) → Connection
  requested welcomed versioned birthed moded registered :
    (nickname user realname : String) → Connection

Connections : ℕ → Set
Connections n = Vec Connection n

data Event {n : ℕ} : (pre post : Connections n) → Set where
  nick : ∀ {conns} (i : Fin n) (nickname : String) →
    conns [ i ]= unregistered →
    Event conns (conns [ i ]≔ requested-nick nickname)

  user : ∀ {conns} (i : Fin n) (user realname : String) →
    conns [ i ]= unregistered →
    Event conns (conns [ i ]≔ requested-user user realname)

  connect : ∀ {conns} (i : Fin n) →
    conns [ i ]= closed →
    Event conns (conns [ i ]≔ unregistered)

  privmsg : ∀ {conns user realname} (nickname text : String) →
    registered nickname user realname ∈ conns →
    Event conns conns

  quit : ∀ {conns} (i : Fin n) (text : String) → Event conns (conns [ i ]≔ closed)

data Action {n : ℕ} : (pre post : Connections n) → Set where
  notice : ∀ {conns} →
    unregistered ∈ conns →
    Action conns conns

  welcome : ∀ {conns nickname user realname} (i : Fin n) (text : String) →
    requested nickname user realname ∈ conns →
    Action conns (conns [ i ]≔ welcomed nickname user realname)

  yourhost : ∀ {conns nickname user realname} (i : Fin n) (text : String) →
    welcomed nickname user realname ∈ conns →
    Action conns (conns [ i ]≔ versioned nickname user realname)

  created : ∀ {conns nickname user realname} (i : Fin n) (text : String) →
    versioned nickname user realname ∈ conns →
    Action conns (conns [ i ]≔ birthed nickname user realname)

  myinfo : ∀ {conns nickname user realname} (i : Fin n) (text : String) →
    birthed nickname user realname ∈ conns →
    Action conns (conns [ i ]≔ moded nickname user realname)

  luser : ∀ {conns nickname user realname} (i : Fin n) (text : String) →
    moded nickname user realname ∈ conns →
    Action conns (conns [ i ]≔ registered nickname user realname)

data SP {n : ℕ} : (pre post : Connections n) → Set where
  get : ∀ {pre mid post} → (Event pre mid → SP mid post) → SP pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (SP mid post) → SP pre post



