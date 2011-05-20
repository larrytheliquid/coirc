module Coirc.Bot where
open import Coirc
open import Coinduction
open import Data.String

server = "irc.freenode.org"
nick = "foobot-name"

bot : Bot
bot = get f where
  f : Event → Bot
  f notice = put (print "<notice>") (♯ bot)
  f numeric = put (print "<numeric-reply>") (♯ bot)
  f mode = put (print "<mode>") (♯ bot)
  f ping = put (print "<ping/pong>") (♯ put (pong nick) (♯ bot))

