module Coirc.Bot where
open import Coirc
-- open import Coirc.Network
open import Coinduction
open import IO
open import Data.String

server = "irc.freenode.org"
name = "coalgbot"
real = "Coalgebra Bot"

foo : Bot unregistered unregistered
foo = put (print "foo") (♯ foo)

bot : Bot unregistered registered
bot =
  put (nick name)
  (♯ put (user name real) 
  (♯ loop)) where
  loop : Bot registered registered
  loop = get f where
    f : Event registered → Bot registered registered
    f notice = put (print "<notice>") (♯ loop)
    f (welcome text) = put (print ("<welcome>: " ++ text)) (♯ loop)
    f numeric = put (print "<numeric-reply>") (♯ loop)
    f mode = put (print "<mode>") (♯ loop)
    f ping = put (print "<ping/pong>") (♯ put (pong name) (♯ loop))
    f (privmsg source text) =
      put (print ("<privmsg>: " ++ text))
      (♯ put (privmsg source "interesting, tell me more!")
      (♯ loop))

-- main = run (runBot bot server)

