module Coirc where
open import Coinduction
open import IO
open import Data.Nat
open import Data.Fin
open import Data.String
open import Data.Maybe
open import Data.Vec
open import Data.Sum

∈-to-Fin : ∀ {A : Set} {x n} {xs : Vec A n} → x ∈ xs → Fin n
∈-to-Fin here = zero
∈-to-Fin (there p) = suc (∈-to-Fin p)

data Connection : Set where
  unregistered closed : Connection
  requested-nick : (nickname : String) → Connection
  requested-user : (username realname : String) → Connection
  requested welcomed versioned birthed moded registered :
    (nickname username realname : String) → Connection

Connections : ℕ → Set
Connections n = Vec Connection n

data RawEvent : Set where
  nick : (nickname : String) → RawEvent
  user : (username realname : String) → RawEvent
  connect : RawEvent
  privmsg : (nickname text : String) → RawEvent
  quit : (text : String) → RawEvent

data Event {n : ℕ} : (pre post : Connections n) → Set where
  nick : ∀ {conns} (i : Fin n) (nickname : String) →
    conns [ i ]= unregistered →
    Event conns (conns [ i ]≔ requested-nick nickname)

  user : ∀ {conns} (i : Fin n) (username realname : String) →
    conns [ i ]= unregistered →
    Event conns (conns [ i ]≔ requested-user username realname)

  connect : ∀ {conns} (i : Fin n) →
    conns [ i ]= closed →
    Event conns (conns [ i ]≔ unregistered)

  privmsg : ∀ {conns username realname} (nickname text : String) →
    registered nickname username realname ∈ conns →
    Event conns conns

  quit : ∀ {conns} (i : Fin n) (text : String) → Event conns (conns [ i ]≔ closed)

data Action {n : ℕ} : (pre post : Connections n) → Set where
  notice : ∀ {conns} (text : String) →
    unregistered ∈ conns →
    Action conns conns

  welcome : ∀ {conns nickname username realname} (i : Fin n) (text : String) →
    requested nickname username realname ∈ conns →
    Action conns (conns [ i ]≔ welcomed nickname username realname)

  yourhost : ∀ {conns nickname username realname} (i : Fin n) (text : String) →
    welcomed nickname username realname ∈ conns →
    Action conns (conns [ i ]≔ versioned nickname username realname)

  created : ∀ {conns nickname username realname} (i : Fin n) (text : String) →
    versioned nickname username realname ∈ conns →
    Action conns (conns [ i ]≔ birthed nickname username realname)

  myinfo : ∀ {conns nickname username realname} (i : Fin n) (text : String) →
    birthed nickname username realname ∈ conns →
    Action conns (conns [ i ]≔ moded nickname username realname)

  luser : ∀ {conns nickname username realname} (i : Fin n) (text : String) →
    moded nickname username realname ∈ conns →
    Action conns (conns [ i ]≔ registered nickname username realname)

data SP {n : ℕ} : (pre post : Connections n) → Set where
  get : ∀ {pre mid post} → (Event pre mid → SP mid post) → SP pre post
  put : ∀ {pre mid post} → Action pre mid → ∞ (SP mid post) → SP pre post



