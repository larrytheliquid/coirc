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
open import Coirc.Format

readTo-SP : List Char → Maybe (String × List Char)
readTo-SP [] = nothing
readTo-SP (' ' ∷ xs) = just ("" , ' ' ∷ xs)
readTo-SP (x ∷ xs) with readTo-SP xs
... | nothing = nothing
... | just (a , ys) = just ( fromList [ x ] ++ a  , ys )

readTo-CRLF : List Char → Maybe (String × List Char)
readTo-CRLF [] = nothing
readTo-CRLF ('\r' ∷ '\n' ∷ xs) = just ("" , '\r' ∷ '\n' ∷ xs)
readTo-CRLF (x ∷ xs) with readTo-CRLF xs
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

read `*SP xs = readTo-SP xs
read `*CRFL xs = readTo-CRLF xs

read _ [] = nothing


parse : (f : Format) → List Char → Maybe (⟦ f ⟧ × List Char)
parse Fail _ = nothing
parse End xs = just (tt , xs)
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
