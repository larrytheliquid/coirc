module Coirc.Network where
open import Function
open import Coinduction
open import Data.Unit
open import Data.String
open import Data.Maybe
open import Data.Colist hiding (_++_)
open import IO
open import Coirc
open import Coirc.Parser
import Coirc.Network.Primitive as Prim

private
  Handle = Prim.Handle

  hConnect : String → IO Handle
  hConnect s = lift (Prim.hConnect s)

  hGetLine : Handle → IO String
  hGetLine h = lift (Prim.hGetLine h)

  hPrint : Handle → String → IO ⊤
  hPrint h s = ♯ lift (Prim.hPrint h s) >> ♯ return tt

  hSend : Handle → String → IO ⊤
  hSend h s =
    ♯ putStrLn ("> " ++ s) >>
    ♯ hPrint h (s ++ "\r\n")

  hRegister : Handle → (nick real : String) → IO ⊤
  hRegister h nick real =
    ♯ hSend h ("NICK " ++ nick) >>
    ♯ hSend h ("USER " ++ nick ++ " 0 * :" ++ real)

  hGetEvents : Handle → IO (Colist Event)
  hGetEvents h = ♯ hGetLine h >>= (λ x → ♯ f x) where
    f : String → IO (Colist Event)
    f s with parse-Event (toList s)
    ... | nothing = return []
    ... | just x = ♯ hGetEvents h >>= λ xs → ♯ return (x ∷ ♯ xs)

  runActions : Handle → Colist Action → IO ⊤
  runActions h [] = return tt
  runActions h (print text ∷ xs) =
    ♯ putStrLn text >>
    ♯ runActions h (♭ xs)
  runActions h (pong nick ∷ xs) =
    ♯ hSend h ("PONG " ++ nick) >>
    ♯ runActions h (♭ xs)

  runSP : Handle → (Colist Event → Colist Action) → IO ⊤
  runSP h sp =
    ♯ hGetEvents h >>= λ xs → 
    ♯ runActions h (sp xs)

runBot : (server nick real : String) → Bot → IO ⊤
runBot server nick real bot =
  ♯ hConnect server >>= λ h → 
  ♯ (♯ hRegister h nick real >>
     ♯ runSP h ⟦ bot ⟧SP)
