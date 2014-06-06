import Data.List
import Data.Maybe

main = do
term <- getLine
putStrLn (show (parse term))

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

-- principal type algorithm
-- pt :: Term -> Type 

-- type unification algorithm
-- unify :: Type -> Type -> Sub
