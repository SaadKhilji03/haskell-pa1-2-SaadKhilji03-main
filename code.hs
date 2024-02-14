import Test.Hspec

-- Use the following data types for the questions below
data Tree a = Nil | TreeNode (Tree a) a (Tree a) deriving (Show, Eq)

data LinkedList a = Null | ListNode a (LinkedList a) deriving (Show, Eq)

-- Category: Medium
-- Question 1
letterCombinations :: String -> [String]
letterCombinations digits = pickingCombinations (customMap mapToString digits)

mapToString :: Char -> String
mapToString '2' = "abc"
mapToString '3' = "def"
mapToString '4' = "ghi"
mapToString '5' = "jkl"
mapToString '6' = "mno"
mapToString '7' = "pqrs"
mapToString '8' = "tuv"
mapToString '9' = "wxyz"
mapToString _   = ""

customMap :: (a -> b) -> [a] -> [b]
customMap _ [] = []
customMap f (x:xs) = f x : customMap f xs

customConcatMap :: (a -> [b]) -> [a] -> [b]
customConcatMap _ [] = []
customConcatMap f (x:xs) = f x ++ customConcatMap f xs

pickingCombinations :: [String] -> [String]
pickingCombinations [] = []
pickingCombinations [s] = customMap (:[]) s
pickingCombinations (s:ss) = customConcatMap (combineCombinations ss) s

combineCombinations :: [String] -> Char -> [String]
combineCombinations strings char = customMap (char:) (pickingCombinations strings)

-- Question 2
fallingSquares :: [[Int]] -> [Int]
fallingSquares = undefined

-- Question 3
breadthFirstConv :: [Int] -> [Int]
breadthFirstConv input = breadthFirstTraversal (buildTree input)

customInit :: [Int] -> [Int]
customInit [_] = []
customInit (x:xs) = x : customInit xs

customLengthFunction :: [Int] -> Int
customLengthFunction [] = 0
customLengthFunction (_:xs) = 1 + customLengthFunction xs

getLastElement :: [Int] -> Int
getLastElement [x] = x
getLastElement (_:xs) = getLastElement xs

buildTree :: [Int] -> Tree Int
buildTree [] = Nil
buildTree postorder = 
    let n = length postorder
        root = getLastElement postorder
        leftSize = calculateLeftSubtreeSize n
        (leftPostorder, rightPostorder) = customSplitAt leftSize (customInit postorder)
    in TreeNode (buildTree leftPostorder) root (buildTree rightPostorder)

calculateLeftSubtreeSize :: Int -> Int
calculateLeftSubtreeSize n = 
    let height = floor (logBase 2 (fromIntegral n))
        maxLastLevel = 2 ^ height
        lastLevelNodes = n - (maxLastLevel - 1)
        leftLastLevelNodes = min (maxLastLevel `div` 2) lastLevelNodes
    in (maxLastLevel - 1) `div` 2 + leftLastLevelNodes

customSplitAt :: Int -> [a] -> ([a], [a])
customSplitAt 0 xs = ([], xs)
customSplitAt _ [] = ([], [])
customSplitAt n (x:xs) = 
    let (left, right) = customSplitAt (n - 1) xs
    in (x:left, right)

breadthFirstTraversal :: Tree a -> [a]
breadthFirstTraversal tree = bfs [tree]
    where
        bfs [] = []
        bfs (Nil:xs) = bfs xs
        bfs ((TreeNode left val right):xs) = val : bfs (xs ++ [left, right])


-- Category: Hard
-- Attempy any 3 questions from this category
-- Question 1
validNumber :: String -> Bool
validNumber = undefined

-- Question 2
swappingNodes :: Tree Int -> Tree Int
swappingNodes = undefined

-- Question 3
calculator :: String -> Int
calculator = undefined

-- Question 4
pathSumSync :: Tree Int -> Tree Int -> Tree Int
pathSumSync = undefined


-- Main Function
main :: IO ()
main =
  hspec $ do
    -- Test Letter Combinations
    describe "letterCombinations" $ do
      it "should return a list of all possible letter combinations" $ do
        letterCombinations "2" `shouldBe` ["a", "b", "c"]
        letterCombinations "23" `shouldBe` ["ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf"]
        letterCombinations "234" `shouldBe` ["adg", "adh", "adi", "aeg", "aeh", "aei", "afg", "afh", "afi", "bdg", "bdh", "bdi", "beg", "beh", "bei", "bfg", "bfh", "bfi", "cdg", "cdh", "cdi", "ceg", "ceh", "cei", "cfg", "cfh", "cfi"]
        letterCombinations "447" `shouldBe` ["ggp", "ggq", "ggr", "ggs", "ghp", "ghq", "ghr", "ghs", "gip", "giq", "gir", "gis", "hgp", "hgq", "hgr", "hgs", "hhp", "hhq", "hhr", "hhs", "hip", "hiq", "hir", "his", "igp", "igq", "igr", "igs", "ihp", "ihq", "ihr", "ihs", "iip", "iiq", "iir", "iis"]
        letterCombinations "2447" `shouldBe` ["aggp", "aggq", "aggr", "aggs", "aghp", "aghq", "aghr", "aghs", "agip", "agiq", "agir", "agis", "ahgp", "ahgq", "ahgr", "ahgs", "ahhp", "ahhq", "ahhr", "ahhs", "ahip", "ahiq", "ahir", "ahis", "aigp", "aigq", "aigr", "aigs", "aihp", "aihq", "aihr", "aihs", "aiip", "aiiq", "aiir", "aiis","bggp", "bggq", "bggr", "bggs", "bghp", "bghq", "bghr", "bghs", "bgip", "bgiq", "bgir", "bgis", "bhgp", "bhgq", "bhgr", "bhgs", "bhhp", "bhhq", "bhhr", "bhhs", "bhip", "bhiq", "bhir", "bhis", "bigp", "bigq", "bigr", "bigs", "bihp", "bihq", "bihr", "bihs", "biip", "biiq", "biir", "biis", "cggp", "cggq", "cggr", "cggs", "cghp", "cghq", "cghr", "cghs", "cgip", "cgiq", "cgir", "cgis", "chgp", "chgq", "chgr", "chgs", "chhp", "chhq", "chhr", "chhs", "chip", "chiq", "chir", "chis", "cigp", "cigq", "cigr", "cigs", "cihp", "cihq", "cihr", "cihs", "ciip", "ciiq", "ciir", "ciis"]
    
    -- Test Falling Squares
    describe "fallingSquares" $ do
      it "should return a list of heights for each square" $ do
        fallingSquares [[1, 2], [2, 3], [6, 1]] `shouldBe` [2, 5, 1]
        fallingSquares [[100, 100], [200, 100]] `shouldBe` [100, 100]
        fallingSquares [[1, 5], [2, 2], [7, 5]] `shouldBe` [5, 7, 5]
        fallingSquares [[1, 2], [3, 1], [2, 4], [2, 3]] `shouldBe` [2, 1, 6, 9]
        fallingSquares [[2, 2], [4, 3], [2, 4]] `shouldBe` [2, 3, 7]
        fallingSquares [[1, 1], [1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1],[1, 1]] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
        fallingSquares [[9,1], [9,2], [1,3], [7,10], [8,1]] `shouldBe` [1,3,3,13,14] 
        fallingSquares [[2,2], [1,18], [6,6], [11, 3]] `shouldBe` [2,20,26,29]
        fallingSquares [[2,4], [1,16], [26,6], [27, 3]] `shouldBe` [4,20,6,9]

    -- Test Breadth First Conversion
    describe "breadthFirstConv" $ do
      it "should return a list of integers in breadth first order" $ do
        breadthFirstConv [4, 2, 3, 1] `shouldBe` [1, 2, 3, 4]
        breadthFirstConv [4, 5, 2, 6, 3, 1] `shouldBe` [1, 2, 3, 4, 5, 6]
        breadthFirstConv [8, 9, 4, 5, 2, 6, 7, 3, 1] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9]
        breadthFirstConv [16, 8, 9, 4, 10, 11, 5, 2, 12, 13, 6, 14, 15, 7, 3, 1] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
        breadthFirstConv [32, 33, 16, 34, 35, 17, 8, 36, 18, 19, 9, 4, 20, 21, 10, 22, 23, 11, 5, 2, 24, 25, 12, 26, 27, 13, 6, 28, 29, 14, 30, 31, 15, 7, 3, 1] `shouldBe` [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36]
        breadthFirstConv [1, 3, 2, 5, 6, 4, 8, 10, 9, 7] `shouldBe` [7, 4, 9, 2, 6, 8, 10, 1, 3, 5]
        breadthFirstConv [8, 1, 0, 3, 5, 0, 8] `shouldBe` [8,0,0,8,1,3,5]
        breadthFirstConv [100001, 1, 9, 6, 23, 0, 24, 102, 22] `shouldBe` [22,23,102,9,6,0,24,100001,1]

    -- Test Valid Number
    describe "validNumber" $ do
      it "Should return True if a number is valid, otherwise, False" $ do
        validNumber "0" `shouldBe` True
        validNumber " 0.1 " `shouldBe` True
        validNumber "abc" `shouldBe` False
        validNumber "1 a" `shouldBe` False
        validNumber "02.10" `shouldBe` True
        validNumber " -90   " `shouldBe` True
        validNumber " -2.001" `shouldBe` True
        validNumber " +2.001" `shouldBe` True
        validNumber " 2.001e" `shouldBe` False
        validNumber " 2.001e-1" `shouldBe` False
        validNumber " +90" `shouldBe` True
        validNumber " -90.   " `shouldBe` True
        validNumber "+18.900.1" `shouldBe` False
        validNumber "+05.90-1" `shouldBe` False
        validNumber "-+5" `shouldBe` False
        validNumber "18.a" `shouldBe` False
        validNumber "0.-100000000000000000000000000000000000000000000000000000000.00000000000000000000000005" `shouldBe` False
        validNumber "18.4665115217351684634816848641348451345868a48651355" `shouldBe` False
        validNumber "18.4665145615333316454816848641348451345868448651355" `shouldBe` True

    -- Test Swapping Nodes
    describe "swappingNodes" $ do
      it "should return the tree with nodes swapped" $ do
        swappingNodes (Nil :: Tree Int) `shouldBe` Nil
        swappingNodes (TreeNode (TreeNode Nil 1 Nil) 2 Nil) `shouldBe` TreeNode (TreeNode Nil 2 Nil) 1 Nil
        swappingNodes (TreeNode (TreeNode Nil 3 Nil) 2 (TreeNode Nil 9 (TreeNode Nil 11 Nil))) `shouldBe` TreeNode (TreeNode Nil 9 (TreeNode Nil 11 Nil)) 2 (TreeNode Nil 3 Nil)
        swappingNodes (TreeNode (TreeNode Nil 10 Nil) 3 (TreeNode Nil 8 (TreeNode Nil 5 Nil))) `shouldBe` TreeNode (TreeNode Nil 5 Nil) 3 (TreeNode Nil 8 (TreeNode Nil 10 Nil))
        swappingNodes (TreeNode (TreeNode Nil 10 Nil) 16 (TreeNode Nil 8 (TreeNode Nil 5 Nil))) `shouldBe` TreeNode (TreeNode Nil 5 Nil) 8 (TreeNode Nil 16 (TreeNode Nil 10 Nil))
        swappingNodes (TreeNode (TreeNode Nil 17 Nil) 11 (TreeNode (TreeNode Nil 21 Nil) 13 (TreeNode (TreeNode Nil 23 Nil) 5 Nil))) `shouldBe` TreeNode (TreeNode Nil 17 Nil) 11 (TreeNode (TreeNode Nil 21 Nil) 13 (TreeNode (TreeNode Nil 23 Nil) 5 Nil))
        swappingNodes (TreeNode (TreeNode (TreeNode Nil 17 Nil) 22 (TreeNode (TreeNode Nil 7 Nil) 23 Nil)) 13 (TreeNode (TreeNode (TreeNode Nil 29 Nil) 21 Nil) 11 (TreeNode Nil 5 Nil))) `shouldBe` TreeNode (TreeNode (TreeNode Nil 7 Nil) 11 (TreeNode Nil 5 Nil)) 13 (TreeNode (TreeNode Nil 17 Nil) 22 (TreeNode (TreeNode (TreeNode Nil 29 Nil) 21 Nil) 23 Nil))
        swappingNodes (TreeNode (TreeNode (TreeNode Nil 37 (TreeNode Nil 17 (TreeNode Nil 6 Nil))) 10 (TreeNode Nil 19 Nil)) 11 (TreeNode (TreeNode Nil 13 (TreeNode (TreeNode Nil 59 (TreeNode (TreeNode Nil 23 Nil) 32 (TreeNode (TreeNode Nil 31 Nil) 36 Nil))) 5 Nil)) 8 (TreeNode Nil 14 Nil)))
          `shouldBe` TreeNode (TreeNode (TreeNode Nil 59 (TreeNode (TreeNode Nil 13 (TreeNode (TreeNode Nil 37 (TreeNode Nil 17 (TreeNode (TreeNode Nil 31 Nil) 36 Nil))) 10 (TreeNode Nil 19 Nil))) 8 (TreeNode Nil 14 Nil))) 5 Nil) 11 (TreeNode (TreeNode Nil 23 Nil) 32 (TreeNode Nil 6 Nil))
    -- This last test case is for the case-1 given in the manual

    -- Test Basic Calculator
    describe "calculator" $ do
      it "should return an integer represented by a mathematical expression" $ do
        calculator "2 + 9 / 3" `shouldBe` 5
        calculator "(3 + 12) / 5" `shouldBe` 3
        calculator "2 -  1 " `shouldBe` 1
        calculator "- (2 *  ( 9 - 195 / (7 + 8)))" `shouldBe` 8
        calculator " - (1 + 5 /3 )" `shouldBe` (-2)
        calculator " 3 - (5 +  12  / (2 + 24/ (6 * 2) + 1/ (18 - 2)))" `shouldBe` (-5)
        calculator " ( 1 + ( 3 + (5 + 3) / 2 + ( 12 + 6 * 6 / (5 +  1)) / ( 19 / (3 - 1) ) ) )" `shouldBe` 10

    -- Test Path Sum Synchronizer
    describe "pathSumSync" $ do
      it "should return modified tree" $ do
        pathSumSync (Nil :: Tree Int) (Nil :: Tree Int) `shouldBe` Nil
        pathSumSync (TreeNode (TreeNode Nil 6 Nil) 2 (TreeNode Nil 3 Nil)) (TreeNode Nil 2 Nil) `shouldBe` TreeNode Nil 2 Nil
        pathSumSync (TreeNode (TreeNode Nil 6 Nil) 2 (TreeNode Nil 3 Nil)) (TreeNode Nil 3 Nil) `shouldBe` Nil
        pathSumSync (TreeNode (TreeNode Nil 6 Nil) 2 (TreeNode Nil 3 Nil)) (Nil :: Tree Int) `shouldBe` Nil
        pathSumSync (TreeNode (TreeNode (TreeNode Nil 11 Nil) 6 Nil) 2 (TreeNode Nil 5 (TreeNode (TreeNode Nil (-2) Nil) (-7) (TreeNode Nil 0 Nil)))) (TreeNode Nil 0 Nil) `shouldBe` TreeNode Nil 2 (TreeNode Nil 5 (TreeNode Nil (-7) (TreeNode Nil 0 Nil)))
        pathSumSync (TreeNode (TreeNode (TreeNode Nil 11 Nil) 6 (TreeNode Nil 7 Nil)) 2 (TreeNode (TreeNode Nil 4 Nil) 5 (TreeNode (TreeNode (TreeNode Nil (-1) Nil) (-2) Nil) (-7) (TreeNode Nil 0 Nil)))) (TreeNode (TreeNode (TreeNode Nil 5 Nil) 2 Nil) 1 (TreeNode (TreeNode Nil (-7) Nil) 3 (TreeNode Nil 9 Nil))) `shouldBe` TreeNode (TreeNode Nil 6 Nil) 2 (TreeNode Nil 5 (TreeNode (TreeNode (TreeNode Nil (-1) Nil) (-2) Nil) (-7) Nil))
        pathSumSync (TreeNode (TreeNode (TreeNode Nil 11 Nil) 6 (TreeNode Nil 7 Nil)) 2 (TreeNode (TreeNode Nil 4 Nil) 5 (TreeNode (TreeNode (TreeNode Nil (-1) Nil) (-2) Nil) (-7) (TreeNode Nil 0 Nil)))) (TreeNode (TreeNode (TreeNode Nil 6 Nil) 2 Nil) 1 (TreeNode (TreeNode Nil (-6) Nil) 3 (TreeNode Nil 7 Nil))) `shouldBe` TreeNode Nil 2 (TreeNode (TreeNode Nil 4 Nil) 5 (TreeNode (TreeNode Nil (-2) Nil) (-7) Nil))
        pathSumSync (TreeNode (TreeNode (TreeNode Nil 11 Nil) 6 (TreeNode Nil 7 Nil)) 2 (TreeNode (TreeNode Nil 4 Nil) 5 (TreeNode (TreeNode (TreeNode Nil (-1) Nil) (-2) Nil) (-7) (TreeNode Nil 0 Nil)))) (TreeNode (TreeNode (TreeNode (TreeNode Nil 6 Nil) 6 Nil) 2 Nil) 1 (TreeNode (TreeNode Nil (-2) Nil) 3 (TreeNode Nil 9 Nil))) `shouldBe` TreeNode (TreeNode Nil 6 (TreeNode Nil 7 Nil)) 2 Nil
