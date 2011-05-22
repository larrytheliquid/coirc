module Coirc.Network where
open import Function
open import Coinduction
open import Data.Unit
open import Data.String
open import Data.Maybe
open import IO
open import Coirc
open import Coirc.Parser
import Coirc.Network.Primitive as Prim

private
  Handle = Prim.Handle

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

  getEvent : Handle → IO (Maybe Event)
  getEvent h = ♯ hGetLine h >>= (λ x → ♯ f x) where
    f : String → IO (Maybe Event)
    f = return ∘ parse-Event ∘ toList

  runAction : Handle → Action → IO ⊤
  runAction h (print text) =
    putStrLn text
  runAction h (nick name) =
    hSend h ("NICK " ++ name)
  runAction h (user name real) =
    hSend h ("USER " ++ name ++ " 0 * :" ++ real)
  runAction h (pong name) =
    hSend h ("PONG " ++ name)
  runAction h (privmsg target text) =
    hSend h ("PRIVMSG " ++ target ++ " :" ++ text)
  runAction h (quit text) =
    hSend h ("QUIT :" ++ text)

  runSP : Handle → Bot → IO ⊤
  runSP h (get f) =
    ♯ getEvent h >>= λ e? →
    ♯ g e?
    where
    g : Maybe Event → IO ⊤
    g nothing =
      ♯ putStrLn "<disconnect>" >>
      ♯ return tt
    g (just e) = runSP h (f e)
  runSP h (put a sp) =
    ♯ runAction h a >>
    ♯ runSP h (♭ sp)

runBot : Bot → String → IO ⊤
runBot bot server =
  ♯ hConnect server >>= λ h → 
  ♯ runSP h bot
