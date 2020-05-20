module Nat where

foreign import kind Nat
foreign import kind Boolean

foreign import data Zero :: Nat
foreign import data Succ :: Nat → Nat
foreign import data True :: Boolean
foreign import data False :: Boolean

data NProxy (a :: Nat) = NProxy
data BProxy (b :: Boolean) = BProxy

type Two = Succ (Succ Zero)
type Three = Succ (Succ (Succ Zero))

class IsEven (n :: Nat) (b :: Boolean) | n -> b
class IsOdd  (n :: Nat) (b :: Boolean) | n -> b

instance isEvenZero :: IsEven Zero True
instance isEvenSucc :: IsOdd n b ⇒ IsEven (Succ n) b


instance isOddZero :: IsOdd Zero False
instance isOddSucc :: IsEven n b ⇒ IsOdd (Succ n) b

test3 :: BProxy True
test3 = BProxy :: ∀ b. IsEven Two b => BProxy b

-- Can't get compiled
-- test4 :: BProxy True
-- test4 = BProxy :: ∀ b. IsEven Three b => BProxy b
