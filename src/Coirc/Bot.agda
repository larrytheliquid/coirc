module Coirc.Bot where
open import Coirc
open import Coirc.Network
open import Coinduction
open import IO
open import Data.String

server = "irc.freenode.org"
name = "coalgbot"
real = "Coalgebra Bot"

bot : Bot
bot =
  put (nick name)
  (♯ put (user name real) 
  (♯ loop)) where
  loop = get f where
    f : Event → Bot
    f notice = put (print "<notice>") (♯ loop)
    f numeric = put (print "<numeric-reply>") (♯ loop)
    f mode = put (print "<mode>") (♯ loop)
    f ping = put (print "<ping/pong>") (♯ put (pong name) (♯ loop))

main = run (runBot bot server)

