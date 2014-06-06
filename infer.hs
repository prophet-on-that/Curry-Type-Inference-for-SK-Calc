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
