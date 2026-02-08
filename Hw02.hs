{--
  CSCI 312 Homework #2

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw02.html
--}

module Hw02 where
import System.Win32 (xBUTTON1)

data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp

-- Put your code here -------------------------

-- print a parseable expression
-- For example, show (Num 5) should yield the string "Num 5"
-- show (Neg (Plus (Num 1) (Num 1))) should yield the string "Neg (Plus (Num 1) (Num 1))"
instance Show ArithExp where
  show :: ArithExp -> String
  show n = case n of
    Num x -> "Num " ++ show x
    Plus a b -> "Plus (" ++ show a ++ ") (" ++ show b ++ ")"
    Times a b -> "Times (" ++ show a ++ ") (" ++ show b ++ ")"
    Neg a -> "Neg (" ++ show a ++ ")"


instance Eq ArithExp where
  (==) :: ArithExp -> ArithExp -> Bool
  Num x == Num y = x == y
  Plus a1 b1 == Plus a2 b2 = a1 == a2 && b1 == b2
  Times a1 b1 == Times a2 b2 = a1 == a2 && b1 == b2
  Neg a1 == Neg a2 = a1 == a2
  _ == _ = False

-- interpreter, which takes an arithmetic expression and evaluates it to a number.
--  eval (Plus (Num 42) (Neg (Num 42))) should yield 0
eval :: ArithExp -> Int
eval n = case n of
  Num x -> x
  Plus a b -> eval a + eval b
  Times a b -> eval a * eval b
  Neg a -> - (eval a)

data ArithExp' =
    Num' Int
  | Plus' ArithExp' ArithExp'
  | Sub' ArithExp' ArithExp'
  | Times' ArithExp' ArithExp'
  | Neg' ArithExp'
  deriving Show

eval' :: ArithExp' -> Int
eval' = eval . translate

translate :: ArithExp' -> ArithExp
translate n = case n of
  Num' n -> Num n
  Plus' a b -> Plus (translate a) (translate b)
  Sub' a b -> Plus (translate a) (Neg (translate b))
  Times' a b -> Times (translate a) (translate b)
  Neg' a -> Neg (translate a)


--  non-standard Eq instance for ArithExp', where e1 == e2 iff they evaluate to the same number,
-- e.g., (Num' 2) == (Plus' (Num' 1) (Num' 1)) should return `True.
instance Eq ArithExp' where
  (==) :: ArithExp' -> ArithExp' -> Bool
  Num' x == Num' y = x == y
  eq3 == eq4 = eval' (eq3) == eval' (eq4)

--  e1 < e2 iff e1 evaluates to a lower number than e2, etc.
instance Ord ArithExp' where
  compare :: ArithExp' -> ArithExp' -> Ordering
  compare e1 e2 = compare (eval' e1) (eval' e2)


-- Tests: un-comment as you go ---------------

main = do

    putStrLn "Problem 1: arithmetic expressions -----------------------------------\n"

    putStr "\n(a) Should be Num 5: "
    print $ Num 5
    putStr "(a) Should be Neg (Plus (Num 1) (Num 1)): "
    print $ Neg (Plus (Num 1) (Num 1))

    putStr "\n(b) Should be True: " 
    print$ (Num 3) == (Num 3)
    putStr "(b) Should be False: " 
    print$ (Num 3) == (Num 4)
    putStr "(b) Should be True: " 
    print$ (Plus (Num 3) (Num 4)) == (Plus (Num 3) (Num 4))
    putStr "(b) Should be False: " 
    print $ (Plus (Num 3) (Num 4)) == (Num 7)

    putStr "\n(c) Should be 5: "
    print $ eval (Plus (Num 1) (Num 4))
    putStr "(c) Should be 0: "
    print $ eval (Plus (Num 42) (Neg (Num 42)))

    putStr "\n(d) Should be 2: "
    print $ eval' (Sub' (Num' 5) (Num' 3))

    putStr "(e) Should be False: " 
    print $ (Num' 2) == (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) == (Num' 3)

    putStr "(e) Should be False: " 
    print $ (Num' 2) > (Num' 3)
    putStr "(e) Should be True: " 
    print $ (Plus' (Num' 1) (Num' 2)) < (Times' (Num' 2) (Num' 3))

    {-
    putStrLn "\nProblem 2: Functors ------------------------------------------------\n"
    putStr "\n(a) Should be ( (4) 6 (8) ): " 
    print $ fmap (\n -> 2 * n)(Node (Node Empty 2 Empty) 3 (Node Empty 4 Empty))
    putStr "\n(b) Should be Branch [Leaf 2,Leaf 3]: "
    print $ Branch [(Leaf 2), (Leaf 3)]
    putStr "\n(b) Should be: Branch [Leaf 1,Branch [Leaf 4,Leaf 9]]: "
    print $ fmap (\x -> x*x) (Branch [Leaf 1, (Branch [(Leaf 2), (Leaf 3)])])
    putStrLn ""


    -}
