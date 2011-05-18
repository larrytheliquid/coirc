module Coirc.Bot where
open import Coirc
open import Coinduction
open import Data.String

bot : Bot
bot = get f where
  f : From → Bot
  f (notice from text) = put (print ("Notice from: " ++ from ++ "\n Notice text: " ++ text))
    (♯ bot)
  f (ping server) = put (print ("Ping from: " ++ server))
    (♯ put pong (♯ bot))
  f (message from text) = put (print ("Message from: " ++ from ++ "\n Message text: " ++ text))
    (♯ put (message from "Greetings from your friendly pointed coalgebra!") (♯ bot))
