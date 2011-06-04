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
  unregistered disconnected : Connection
  requested-nick : (nickname : String) → Connection
  requested-user : (user realname : String) → Connection
  requested registered : (nickname user realname : String) → Connection

Connections : ℕ → Set
Connections n = Vec Connection n

data Request {n : ℕ} : (pre post : Connections n) → Set where
  nick : ∀ {conns} (i : Fin n) (nickname : String) →
    -- TODO: nick not taken, before or after user?
    conns [ i ]= unregistered →
    Request conns (conns [ i ]≔ requested-nick nickname)

  user : ∀ {conns} (i : Fin n) (user realname : String) →
    conns [ i ]= unregistered →
    Request conns (conns [ i ]≔ requested-user user realname)

  -- pong : ∀ {users} (name : String) → Request (registered , users) (registered , users)

  connect : ∀ {conns} (i : Fin n) →
    conns [ i ]= disconnected →
    Request conns (conns [ i ]≔ unregistered)

  privmsg : ∀ {conns user realname} (nickname text : String) →
    registered nickname user realname ∈ conns →
    Request conns conns

  quit : ∀ {conns} (i : Fin n) (text : String) → Request conns (conns [ i ]≔ disconnected)

-- data Response : (pre post : State) → Set where
--   notice : ∀ {state} → Response state state

--   welcome : ∀ {users} (text : String) → Response (requested , users) (registered , users)

--   mode numeric ping : ∀ {users} → Response (registered , users) (registered , users)

--   privmsg : ∀ {users} (source text : String) → Response (registered , users) (registered , users)

--   error : ∀ {state} (text : String) → Response state (disconnected , [])

--   join : ∀ {users} (chan : String) (p : requested chan ∈ users) →
--     Response (registered , users) (registered , grant-join p)

--   channel-full : ∀ {users} (chan : String) (p : requested chan ∈ users) →
--     Response (registered , users) (registered , drop p)

-- data Session : (pre post : State) → Set where
--   get : ∀ {pre mid post} → (Request pre mid → Session mid post) → Session pre post
--   put : ∀ {pre mid post} → Response pre mid → ∞ (Session mid post) → Session pre post



