module Coirc.Network where
open import Function
open import Coinduction
open import Data.Unit
open import Data.Nat
open import Data.String
open import Data.Maybe
open import Data.Vec hiding (_>>=_; _++_)
open import IO
open import Coirc
-- open import Coirc.Parser
import Coirc.Network.Primitive as Prim

private
  Handle = Prim.Handle

  Handles : ℕ → Set
  Handles n = Vec Handle n

  hConnect : String → IO Handle
  hConnect s = lift (Prim.hConnect s)

  hGetLine : Handle → IO String
  hGetLine h =
    ♯ lift (Prim.hGetLine h) >>= λ s →
    ♯ (♯ putStrLn s >>
       ♯ return (s ++ "\n"))

  hPutStr : Handle → String → IO ⊤
  hPutStr h s = ♯ lift (Prim.hPutStr h s) >> ♯ return tt

  hSend : Handle → String → IO ⊤
  hSend h s =
    ♯ putStrLn ("> " ++ s) >>
    ♯ hPutStr h (s ++ "\r\n")

  -- getEvent : Handle → IO (Maybe Event)
  -- getEvent h = ♯ hGetLine h >>= (λ x → ♯ f x) where
  --   f : String → IO (Maybe Event)
  --   f = return ∘ parse-Event ∘ toList

  runAction : ∀ {n} {pre post : Connections n} → Handles n → Action pre post → IO ⊤
  runAction hs (notice text p) =
    hSend h ("NOTICE * :*** " ++ text) where
    h = lookup (∈-to-Fin p) hs
  runAction hs (welcome {nickname = nickname} i text p) =
    hSend h ("001 " ++ nickname ++ " :" ++ text) where
    h = lookup (∈-to-Fin p) hs
  runAction hs (yourhost {nickname = nickname} i text p) =
    hSend h ("002 " ++ nickname ++ " :" ++ text) where
    h = lookup (∈-to-Fin p) hs
  runAction hs (created {nickname = nickname} i text p) =
    hSend h ("003 " ++ nickname ++ " :" ++ text) where
    h = lookup (∈-to-Fin p) hs
  runAction hs (myinfo {nickname = nickname} i text p) =
    hSend h ("004 " ++ nickname ++ " " ++ text) where
    h = lookup (∈-to-Fin p) hs
  runAction hs (luser {nickname = nickname} i text p) =
    hSend h ("251 " ++ nickname ++ " :" ++ text) where
    h = lookup (∈-to-Fin p) hs

--   runSP : Handle → Bot → IO ⊤
--   runSP h (get f) =
--     ♯ getEvent h >>= λ e? →
--     ♯ g e?
--     where
--     g : Maybe Event → IO ⊤
--     g nothing =
--       ♯ putStrLn "<disconnect>" >>
--       ♯ return tt
--     g (just e) = runSP h (f e)
--   runSP h (put a sp) =
--     ♯ runAction h a >>
--     ♯ runSP h (♭ sp)

-- runBot : Bot → String → IO ⊤
-- runBot bot server =
--   ♯ hConnect server >>= λ h → 
--   ♯ runSP h bot
