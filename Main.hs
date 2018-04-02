{-# LANGUAGE Rank2Types #-}

import Prelude hiding (const, succ, pred, fst, snd)

type LambdaInt = forall a. (a -> a) -> a -> a

zero f x = x
succ n f x = f (n f x)
pred n = snd (n g s)
    where s   = pair false zero
          g p = pair true (p (\f s -> f succ id s))

          -- s = (False, zero)
          -- g (False, f) = (True, f)
          -- g (True, f)  = (True, succ f)

-- from Wikipedia (slower):
-- pred n f x = n (\g h -> h (g f)) (\_ -> x) (\u -> u)

pair l r f = f l r
fst p = p (\l r -> l)
snd p = p (\l r -> r)

true  t f = t
false t f = f

const x _ = x

isZero n = n (const false) true

ackermann :: LambdaInt -> LambdaInt -> LambdaInt
ackermann x y = isZero x (succ y) $
                isZero y (ackermann (pred x) (succ zero)) $
                ackermann (pred x) (ackermann x (pred y))


main = print $ ackermann (succ . succ .  succ $ zero) (succ . succ . succ . succ $ zero) (+1) 0
