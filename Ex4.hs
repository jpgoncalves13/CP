import BTree
import Cp
import Cp2223data
import Exp
import FTree
import List
import ListUtils
import LTree
import Nat
import NEList
--import Probability
import RelCalc
import Rose
import Show
import St
import Svg
import Data.List 
import Show

type Team = String
type Group = [Team]
type Match = (Team, Team)



groups :: [Group]
groups = [["Qatar", "Ecuador", "Senegal", "Netherlands"],
          ["England", "Iran", "USA", "Wales"],
          ["Argentina", "Saudi Arabia", "Mexico", "Poland"],
          ["France", "Denmark", "Tunisia", "Australia"],
          ["Spain", "Germany", "Japan", "Costa Rica"],
          ["Belgium", "Canada", "Morocco", "Croatia"],
          ["Brazil", "Serbia", "Switzerland", "Cameroon"],
          ["Portugal", "Ghana", "Uruguay", "Korea Republic"]]

groupA = [("Qatar","Ecuador"),("Qatar","Senegal"),("Qatar","Netherlands"),("Ecuador","Senegal"),("Ecuador","Netherlands"),("Senegal","Netherlands")]
groupB = [("England","Iran"),("England","USA"),("England","Wales"),("Iran","USA"),("Iran","Wales"),("USA","Wales")]
groupC = [("Argentina","Saudi Arabia"),("Argentina","Mexico"),("Argentina","Poland"),("Saudi Arabia","Mexico"),("Saudi Arabia","Poland"),("Mexico","Poland")]
groupD = [("France","Denmark"),("France","Tunisia"),("France","Australia"),("Denmark","Tunisia"),("Denmark","Australia"),("Tunisia","Australia")]
groupE = [("Spain","Germany"),("Spain","Japan"),("Spain","Costa Rica"),("Germany","Japan"),("Germany","Costa Rica"),("Japan","Costa Rica")]
groupF = [("Belgium","Canada"),("Belgium","Morocco"),("Belgium","Croatia"),("Canada","Morocco"),("Canada","Croatia"),("Morocco","Croatia")]
groupG = [("Brazil","Serbia"),("Brazil","Switzerland"),("Brazil","Cameroon"),("Serbia","Switzerland"),("Serbia","Cameroon"),("Switzerland","Cameroon")]
groupH = [("Portugal","Ghana"),("Portugal","Uruguay"),("Portugal","Korea Republic"),("Ghana","Uruguay"),("Ghana","Korea Republic"),("Uruguay","Korea Republic")]

rankings = [("Argentina", 4.8),
            ("Australia", 4.0),
            ("Belgium", 5.0),
            ("Brazil", 5.0),
            ("Cameroon", 4.0),
            ("Canada", 4.0),
            ("Costa Rica", 4.1),
            ("Croatia", 4.4),
            ("Denmark", 4.5),
            ("Ecuador", 4.0),
            ("England", 4.7),
            ("France", 4.8),
            ("Germany", 4.5),
            ("Ghana", 3.8),
            ("Iran", 4.2),
            ("Japan", 4.2),
            ("Korea Republic", 4.2),
            ("Mexico", 4.5),
            ("Morocco", 4.2),
            ("Netherlands", 4.6),
            ("Poland", 4.2),
            ("Portugal", 4.6),
            ("Qatar", 3.9),
            ("Saudi Arabia", 3.9),
            ("Senegal", 4.3),
            ("Serbia", 4.2),
            ("Spain", 4.7),
            ("Switzerland", 4.4),
            ("Tunisia", 4.1),
            ("USA", 4.4),
            ("Uruguay", 4.5),
            ("Wales", 4.3)]


--winner :: Team
--winner = wcup groups
--wcup = knockoutStage · groupStage

--generateMatches = pairup

--generateMatch :: Team -> Team -> Match
--generateMatch a b = (a,b)

createTuples :: b -> [b] -> [(b,b)]
createTuples x l = map (\y -> (x, y)) l

pairup :: [Team] -> [(Team, Team)]
pairup [] = []
pairup (x:xs) = createTuples x xs ++ pairup xs 

generateMatches :: Group -> [Match]
generateMatches [] = []
generateMatches (x:xs) = pairup (x:xs) ++ generateMatches xs

collect' :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
collect' x = nub [(k, [d' | (k', d') <- x, k' == k]) | (k, d) <- x]

consolidate :: (Num d, Eq d, Eq b) => [(b, d)] -> [(b, d)]
consolidate = map (id >< sum) . collect'

consolidate' :: (Eq a, Num b) => [(a, b)] -> [(a, b)]
consolidate' =  cataList (either c1 c2)
c1 = nil
c2 ((a,b), []) = [(a,b)]
c2 ((a,b), (a1,b1):t) = if a == a1 then (a1,b1+b) : t
                        else (a1,b1) : c2 ((a,b),t)

--collect :: (Eq a, Eq b) -> [(a, b)] -> [(a, [b])]
--collect x = nub [k -> [d' | (k', d') <- x, k' == k] | (k, d) <- x]

rank x = 4 ** (pap' rankings x - 3.8)

gsCriteria :: Match -> Maybe Team
gsCriteria = s . split (id >< id) (rank >< rank) where
             s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
                if d > 0.5 then Just s1
                else if d < -0.5 then Just s2
                else Nothing

--koCriteria :: Match -> Maybe Team
--koCriteria = s · 〈id  id, rank >< rank〉 where
--             s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
--             if d ≡ 0 then s1
--             else if d > 0 then s1 else s2

matchResult :: (Match -> Maybe Team) -> Match -> [(Team, Int)]
matchResult gsCriteria (t1,t2) = if gsCriteria (t1,t2) == Just t1 then [(t1,3),(t2,0)]
                                 else if gsCriteria (t1,t2) == Just t2 then [(t1,0),(t2,3)]
                                 else [(t1,1),(t2,1)]

best n = map p1 . take n . reverse . presort' p2

groupWinners gsCriteria = best 2 . consolidate . (>>= matchResult gsCriteria) 

genGroupStageMatches :: [Group] -> [[Match]]
genGroupStageMatches = map generateMatches

simulateGroupStage :: [[Match]] -> [[Team]]
simulateGroupStage = map (groupWinners gsCriteria)

arrangement :: [[Team]] -> [Team]
arrangement = (>>=swapTeams) . chunksOf 4 where
swapTeams [[a1, a2 ], [b1, b2 ], [c1, c2 ], [d1, d2 ]] = [a1, b2, c1, d2, b1, a2, d1, c2 ]


--data LTree a = Leaf a | Fork (LTree a, LTree a) deriving (Show, Eq, Ord)

--inLTree :: Either a (LTree a, LTree a) -> LTree a
--inLTree = either Leaf Fork

--outLTree :: LTree a -> Either a (LTree a,LTree a)
--outLTree (Leaf a)       = i1 a
--outLTree (Fork (t1,t2)) = i2 (t1,t2)

--anaLTree f = inLTree . (recLTree (anaLTree f) ) . f

--recLTree f = baseLTree id f          -- that is:  id -|- (f >< f)

glt :: Eq a => [a] -> Either a ([a], [a])
glt [x] = i1 x
glt l = i2 (take n l,drop n l)
        where n = div (length l) 2

printLTree :: Show a => LTree a -> IO ()
printLTree (Leaf x) = putStrLn (show x)
printLTree (Fork (t1, t2)) = do
  putStrLn "Fork"
  printLTree t1
  printLTree t2

initKnockoutStage :: [[Team]] -> LTree Team
initKnockoutStage = (anaLTree glt) . arrangement

groupStage :: [Group] -> LTree Team
groupStage = initKnockoutStage . simulateGroupStage . genGroupStageMatches










