{--
  CSCI 312 Homework #2

  Adpated from https://cs.pomona.edu/~michael/courses/csci131s18/hw/Hw02.html
--}

module Hw02 where

data ArithExp =
    Num Int
  | Plus ArithExp ArithExp
  | Times ArithExp ArithExp
  | Neg ArithExp


-- Put your code here -------------------------


-- Tests: un-comment as you go ---------------

main = do

    putStrLn "Problem 1: arithmetic expressions -----------------------------------\n"

    putStr "\n(a) Should be Num 5: "
    print $ Num 5
    putStr "(a) Should be Neg (Plus (Num 1) (Num 1)): "
    print $ (Neg (Plus (Num 1) (Num 1)))

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

    putStrLn "\nProblem 2: Functors ------------------------------------------------\n"
    putStr "\n(a) Should be ( (4) 6 (8) ): " 
    print $ fmap (\n -> 2 * n)(Node (Node Empty 2 Empty) 3 (Node Empty 4 Empty))
    putStr "\n(b) Should be Branch [Leaf 2,Leaf 3]: "
    print $ Branch [(Leaf 2), (Leaf 3)]
    putStr "\n(b) Should be: Branch [Leaf 1,Branch [Leaf 4,Leaf 9]]: "
    print $ fmap (\x -> x*x) (Branch [Leaf 1, (Branch [(Leaf 2), (Leaf 3)])])
    putStrLn ""
