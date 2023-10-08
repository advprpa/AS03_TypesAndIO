-------------------------------------------------------------------------------
-- Part 1
-------------------------------------------------------------------------------

-- Show that `(Bool,a)` is isomorphic to `Either a a`
-- Algebra: 2 * a = a + a

f :: (Bool,a) -> Either a a
f (True, a) = Left a
f (False, a) = Right a

g :: Either a a -> (Bool, a)
g (Left a) = (True, a)
g (Right a) = (False, a) 

{- Show that `(a -> b -> c)` is isomorphic to `(a, b) -> c`

  a -> b -> c  =  a -> (b -> c) -- (->) associates to the right
  Cardinalities of (a -> (b -> c)): (|c| ^ |b|) ^ |a| == |c| ^ (|b| * |a|) == |c| ^ (|a| * |b|)
  Cardinalities of ((a, b) -> c):    |c| ^ (|a| * |b|)
  Both types have the same number of inhabitants.
-}

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(a,b) -> f a b

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \a -> \b -> f (a,b)

-------------------------------------------------------------------------------
-- Part 2
-------------------------------------------------------------------------------
-- Avoid boolean blindness by using algebra:
-- 2 * x = x + x
data Result a  = Result Bool a
data Result' a = Bad a | Good a

unblind :: Result a -> Result' a
unblind (Result False a) = Bad a   
unblind (Result True a)  = Good a

blind :: Result' a -> Result a
blind (Bad a) = (Result False a)   
blind (Good a) = (Result True a) 


-------------------------------------------------------------------------------
-- Part 3. Folds
-------------------------------------------------------------------------------
data Expr
    = Val Int
    | Add Expr Expr
    | Mul Expr Expr
    deriving Show

foldExpr :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr v _ _ (Val i) = v i
foldExpr v a m (Add l r) = a (foldExpr v a m l) (foldExpr v a m r)
foldExpr v a m (Mul l r) = m (foldExpr v a m l) (foldExpr v a m r)

eval :: Expr -> Int
eval = foldExpr id (+) (*)

toString :: Expr -> String
toString = foldExpr show (\l r -> concat ["(",l,"+",r,")"]) (\l r -> concat ["(",l,"*",r,")"])

e1 = eval (Add (Val 1) (Mul (Val 2) (Val 3)))


-- Recursive types:
data List a = Nil | Cons a (List a)
newtype L a = L (Either () (a, L a)) -- we need newtype because `type` is expanded statically.

{- 
L = 1 + a * L
  = 1 + a * (1 + a * L)
  = 1 + a + a^2 * (1 + a * L)
  = 1 + a + a^2 + a^3 * (1 + a * L)
  ...
  = 1 + a + a^2 + a^3 + a^4 + ....
-}
