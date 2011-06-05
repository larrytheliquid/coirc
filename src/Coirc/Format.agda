module Coirc.Format where
open import Data.Empty
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.String
open import Data.List
open import Data.Sum
open import Data.Product
open import Coirc

infixr 3 _∣_
infixr 1 _>>_ _>>-_ _>>=_

within? : Char → ℕ → ℕ → Bool
within? c start end = toBool lower ∧ toBool higher
  where
  target = toNat c
  lower  = suc target ∸ start
  higher = suc end ∸ target
  toBool : ℕ → Bool
  toBool zero    = false
  toBool (suc _) = true

data DarRange (start end : ℕ) : Bool → Set where
  dar : (c : Char) → DarRange start end (within? c start end)

data Dar : ℕ → Set where
  dar : (c : Char) → Dar (toNat c)

data U : Set where
  CHAR : U
  DAR : ℕ → U
  DAR-RANGE : ℕ → ℕ → U
  `*! `*sp `*crlf : U

El : U → Set
El CHAR = Char
El (DAR n) = Dar n
El (DAR-RANGE n m) = DarRange n m true
El `*! = String
El `*sp = String
El `*crlf = String

mutual
  data Format : Set where
    Fail End : Format
    As : RawEvent → Format
    Base : U → Format
    Skip Or And : Format → Format → Format
    Use : (f : Format) → (⟦ f ⟧ → Format) → Format

  ⟦_⟧ : Format → Set
  ⟦ Fail ⟧ = ⊥
  ⟦ End ⟧ = ⊤
  ⟦ As _ ⟧ = RawEvent
  ⟦ Base u ⟧ = El u
  ⟦ Skip _ f ⟧ = ⟦ f ⟧
  ⟦ Or f₁ f₂ ⟧ = ⟦ f₁ ⟧ ⊎ ⟦ f₂ ⟧
  ⟦ And f₁ f₂ ⟧ = ⟦ f₁ ⟧ × ⟦ f₂ ⟧
  ⟦ Use f₁ f₂ ⟧ = Σ ⟦ f₁ ⟧ λ x → ⟦ f₂ x ⟧

_>>_ : Format → Format → Format
f₁ >> f₂ = Skip f₁ f₂

_>>-_ : Format → Format → Format
x >>- y = And x y

_>>=_ : (f : Format) → (⟦ f ⟧ → Format) → Format
x >>= y = Use x y

_∣_ : Format → Format → Format
x ∣ y = Or x y

char : Char → Format
char c = Base (DAR (toNat c))

str : String → Format
str s = chars (toList s)
  where
  chars : List Char → Format
  chars [] = End
  chars (x ∷ xs) = char x >>- chars xs

digit = Base (DAR-RANGE (toNat '0') (toNat '9'))
cl    = char ':'
sp    = char ' '
cr    = char '\r'
lf    = char '\n'
crlf  = cr >>- lf

Nickname : Format
Nickname =
  str "NICK" >>
  sp >>
  Base `*crlf >>= λ nickname →
  crlf >>
  As (nick nickname)

User : Format
User =
  str "USER" >>
  sp >>
  Base `*sp >>= λ username →
  sp >>
  digit >> -- mode
  char '*' >> sp >> cl >>
  Base `*crlf >>= λ realname →
  crlf >>
  As (user username realname)

Privmsg =
  str "PRIVMSG" >>
  sp >>
  Base `*sp >>= λ nickname →
  sp >> cl >>
  Base `*crlf >>= λ text →
  crlf >>
  As (privmsg nickname text)

Quit =
  str "QUIT" >>
  sp >> cl >>
  Base `*crlf >>= λ text →
  crlf >>
  As (quit text)
