import Data.List
import Data.Maybe

-- main = do
-- term <- getLine
-- putStrLn (show (parse term))

data Term 
  = S | K | App Term Term
  deriving Show

parse :: [Char] -> Term
parse str
  = parse' str [[]]
  where
    parse' :: [Char] -> [[Term]] -> Term
    parse' [] [[t]] 
      = t
    parse' ('(' : str) stacks
      = parse' str ([] : stacks)
    parse' (')' : str) ([t] : stack : stacks)
      | length stack > 0
        = parse' str (((App (head stack) t) : (tail stack)) : stacks)
      | otherwise 
        = parse' str ([t] : stacks)
    parse' ('S' : str) (stack : stacks)
      | length stack > 0
        = parse' str (((App (head stack) S) : (tail stack)) : stacks)
      | otherwise
        = parse' str ([S] : stacks)
    parse' ('K' : str) (stack : stacks)
      | length stack > 0
        = parse' str (((App (head stack) K) : (tail stack)) : stacks)
      | otherwise
        = parse' str ([K] : stacks)
    parse' _ _ 
      = error "Parse error: bad structure"

-- Type variables are identified by an Int
data Type 
  = Var Int | Function Type Type
  deriving (Show, Eq)

-- Represent substitutions in the natural way: mapping types to types
-- WARNING - user's responsibility to ensure this is a function 
type Sub
  = [(Type, Type)]

apply :: Sub -> Type -> Type
apply s (Var n)
  = fromMaybe (Var n) (lookup (Var n) s) 
apply s (Function t t')
  = Function (apply s t) (apply s t')

-- compose s t == st (function composition)
-- Pre: s, t are functions
compose :: Sub -> Sub -> Sub
compose s t 
  = (zip (map fst t) (map (apply s) (map snd t))) ++ remainder
  where 
    remainder 
      = deleteFirstsBy (\(a,b) (c,d) -> a == c) s t

-- check whether a type variable is contained in a type
contained_in :: Int -> Type -> Bool
contained_in n (Var k)
  = n == k
contained_in n (Function t t')
  = (contained_in n t) || (contained_in n t')

-- principal type algorithm
pt :: Term -> Type 
pt 
  = normalise . fst . pt' 
  where 
    -- Returned Int is the largest allocated type variable identifier. Needed
    -- to handle `freshness'. Additionally, indexing identifiers from 1 here.
    pt' :: Term -> (Type, Int)
    pt' S
      = ((Function (Function (Var 1) (Function (Var 2) (Var 3))) (Function (Function (Var 1) (Var 2)) (Function (Var 1) (Var 3)))), 3)
    pt' K 
      = ((Function (Var 1) (Function (Var 2) (Var 1))), 2)
    pt' (App p q)
      = (apply s (Var k), k)
      where
        (t, n)
          = pt' p
        (t', n')
          = pt' q
        k 
          = n + n' + 1
        s 
          = unify t (Function (freshen n t') (Var k))

-- adds n to every type variable ID in the type
freshen :: Int -> Type -> Type
freshen n (Var k)
  = Var (n + k)
freshen n (Function t t')
  = Function (freshen n t) (freshen n t')

-- type unification algorithm
unify :: Type -> Type -> Sub
unify (Var m) (Var n)
  | m == n
    = []
unify (Var m) t
  | not (contained_in m t)
    = [((Var m), t)]
  | otherwise 
    = error "Ununifiable"
unify t (Var m)
  = unify (Var m) t
unify (Function t t') (Function u u')
  = compose s2 s1
  where
    s1 
      = unify t u
    s2
      = unify (apply s1 t') (apply s1 u')

-- normalise a type; set type variable IDs to count consecutively from 1
normalise :: Type -> Type
normalise t 
  = apply s t
  where 
    vars
      = type_vars t
    s
      = zip vars (map (\n -> Var n) [1..])
    type_vars (Var n)
      = [(Var n)]
    type_vars (Function t' t'')
      = nub ((type_vars t') ++ (type_vars t''))
