module Coirc.Parser where
open import Data.Unit
open import Data.Bool
open import Data.Nat
open import Data.Char
open import Data.String
open import Data.Maybe
open import Data.List hiding (_++_)
open import Data.Sum
open import Data.Product
open import Relation.Nullary
open import Coirc
open import Coirc.Format

readTo-sp : List Char → Maybe (String × List Char)
readTo-sp [] = nothing
readTo-sp (' ' ∷ xs) = just ("" , ' ' ∷ xs)
readTo-sp (x ∷ xs) with readTo-sp xs
... | nothing = nothing
... | just (a , ys) = just ( fromList [ x ] ++ a  , ys )

readTo-crlf : List Char → Maybe (String × List Char)
readTo-crlf [] = nothing
readTo-crlf ('\r' ∷ '\n' ∷ xs) = just ("" , '\r' ∷ '\n' ∷ xs)
readTo-crlf (x ∷ xs) with readTo-crlf xs
... | nothing = nothing
... | just (a , ys) = just ( fromList (x ∷ []) ++ a  , ys )

read : (u : U) → List Char → Maybe (El u × List Char)

read CHAR (x ∷ xs) = just (x , xs)

read (DAR n) (x ∷ xs) with Data.Nat._≟_ n (toNat x)
... | no _  = nothing
... | yes p rewrite p = just (dar x , xs)

read (DAR-RANGE n m) (x ∷ xs) with Data.Bool._≟_ true (within? x n m)
... | no _ = nothing
... | yes p rewrite p = just (dar x , xs)

read `*sp xs = readTo-sp xs
read `*crlf xs = readTo-crlf xs

read _ [] = nothing


parse : (f : Format) → List Char → Maybe (⟦ f ⟧ × List Char)
parse Fail _ = nothing
parse End xs = just (tt , xs)
parse (As val) xs = just (val , xs)
parse (Base u) xs = read u xs
parse (Skip f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (_ , ys) = parse f₂ ys
parse (Or f₁ f₂) xs with parse f₁ xs
... | just (a , ys) = just (inj₁ a , ys)
... | nothing with parse f₂ xs
... | just (a , ys) = just (inj₂ a , ys)
... | nothing = nothing
parse (And f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (a , ys) with parse f₂ ys
... | nothing = nothing
... | just (b , zs) = just ((a , b) , zs)
parse (Use f₁ f₂) xs with parse f₁ xs
... | nothing = nothing
... | just (a , ys) with parse (f₂ a) ys
... | nothing = nothing
... | just (b , zs) = just ((a , b) , zs)

parse-Notice : List Char → Maybe ⟦ Notice ⟧
parse-Notice xs with parse Notice xs
... | nothing = nothing
... | just (result , _) = just result

parse-NumericReply : List Char → Maybe ⟦ NumericReply ⟧
parse-NumericReply xs with parse NumericReply xs
... | nothing = nothing
... | just (x , _) = just x

parse-Mode : List Char → Maybe ⟦ Mode ⟧
parse-Mode xs with parse Mode xs
... | nothing = nothing
... | just (x , _) = just x

parse-Ping : List Char → Maybe ⟦ Ping ⟧
parse-Ping xs with parse Ping xs
... | nothing = nothing
... | just (x , _) = just x

parse-Event : List Char → Maybe Event
parse-Event xs with parse-Notice xs
... | just x = just x
... | nothing with parse-NumericReply xs
... | just x = just x
... | nothing with parse-Mode xs
... | just x = just x
... | nothing with parse-Ping xs
... | just x = just x
... | nothing = nothing
