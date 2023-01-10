{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import Cp
import Nat
import List
import NEList
import Exp
import Cp2223data
import Probability
import BTree
import Cp2223data
import Exp
import FTree
import List
import ListUtils
import LTree
import Rose
import Show
import St
import Svg
import Data.List
import DynFlags (xFlags)



gene :: [String] -> Either String (String, [[String]])
gene = (id -|- (id >< (f2 . map (drop 4)))) . out


f2 :: [[Char]] -> [[[Char]]]
f2 [] = []
f2 (h:t) = (h : lr) : f2 (drop (length lr) t)
        where lr = takeWhile ((==) "    " . take 4) t


tax = anaExp gene



g1 :: a -> [[a]]
g1 x = [[x]]

-- Exp S S <---- S + S x (Exp S S)*
--
--   |                 |
--   v                 v
--
-- (S*)* <------ S + S x (S*)*




g2 :: Foldable t => (a, t [[a]]) -> [[a]]
g2 (x,l) = [x] : fmap (x:) (concat l)


gene1 :: Either a (a, [[[a]]]) -> [[a]]
gene1 = either g1 g2


post :: Exp a a -> [[a]]
post = cataExp gene1

--------------------------------------------------------------------------------------------------------------------------------

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


rankings = [
        ("Argentina", 4.8),
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


winner :: Team
winner = wcup groups
wcup = knockoutStage . groupStage

pwinner :: Dist Team
pwinner = pwcup groups
pwcup = pknockoutStage .! pgroupStage


-- pwinner :: Dist Team
-- pwinner = mbin f x >>= pknockoutStage where
-- f (x, y) = initKnockoutStage (x ++ y)
-- x = ⟨g · take 4, g · drop 4⟩ groups
-- g = psimulateGroupStage · genGroupStageMatches

--------------------------------------------------


knockoutStage = cataLTree (either id koCriteria)
pknockoutStage = mcataLTree' [return, pkoCriteria]

--------------------------------------------------


groupStage :: [Group] -> LTree Team
groupStage = initKnockoutStage . simulateGroupStage . genGroupStageMatches

pgroupStage = pinitKnockoutStage .! psimulateGroupStage . genGroupStageMatches

--------------------------------------------------


genGroupStageMatches :: [Group] -> [[Match]]
genGroupStageMatches = map generateMatches

--------------------------------------------------


simulateGroupStage :: [[Match]] -> [[Team]]
simulateGroupStage = map (groupWinners gsCriteria)

psimulateGroupStage = trim . map (pgroupWinners pgsCriteria)

--------------------------------------------------
groupWinners :: (Match -> Maybe Team) -> [Match] -> [Team]
groupWinners criteria = best 2 . consolidate . (>>=matchResult criteria)

pgroupWinners :: (Match -> Dist (Maybe Team)) -> [Match] -> Dist [Team]
pgroupWinners criteria = best 2 . consolidate . (>>=pmatchResult criteria)

--------------------------------------------------


glt [x] = i1 x
glt l = i2 (splitAt n l)
        where n = div (length l) 2

initKnockoutStage = anaLTree glt . arrangement
arrangement = (>>=swapTeams) . chunksOf 4
        where swapTeams [[a1, a2 ], [b1, b2 ], [c1, c2 ], [d1, d2 ]] = [a1, b2, c1, d2, b1, a2, d1, c2]

-- pinitKnockoutStage


--------------------------------------------------


matchResult :: (Match -> Maybe Team) -> Match -> [(Team, Int)]
matchResult criteria (t1,t2)
  | criteria (t1,t2) == Just t1 = [(t1,3),(t2,0)]
  | criteria (t1,t2) == Just t2 = [(t1,0),(t2,3)]
  | otherwise = [(t1,1),(t2,1)]


pmatchResult :: (Match -> Dist (Maybe Team)) -> Match -> Dist [Team]
pmatchResult criteria (t1,t2) = do { D a <- criteria (t1,t2); x <- sortP a;
                                 if x == Nothing then return [(t1,1),(t2,1)];
                                 else if x == t1 then return [(t1,3),(t2,0)];
                                 else return [(t1,0),(t2,3)];}


--------------------------------------------------

pgsCriteria = s . split (id >< id)  (rank >< rank)
        where s ((s1, s2), (r1, r2)) = if abs (r1 - r2) > 0.5 then fmap Just (pkoCriteria (s1, s2))
                                       else f (s1, s2)
              f = D . ((Nothing, 0.5):) . map (Just >< (/2)) . unD . pkoCriteria

gsCriteria = s . split (id >< id) (rank >< rank)
         where s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
                if d > 0.5 then Just s1
                else if d < -0.5 then Just s2
                else Nothing


generateMatches = pairup

pairup :: [Team] -> [(Team, Team)]
pairup [] = []
pairup (x:xs) = createTuples x xs ++ pairup xs

createTuples :: b -> [b] -> [(b,b)]
createTuples x = map (\y -> (x, y))












pkoCriteria (e1, e2) = D [(e1, 1 - r2 / (r1 + r2)), (e2, 1 - r1 / (r1 + r2))]
         where r1 = rank e1
               r2 = rank e2

koCriteria = s . split (id >< id) (rank >< rank)
        where s ((s1, s2), (r1, r2)) = let d = r1 - r2 in
                if (d == 0) || (d > 0) then s1 else s2


rank :: [Char] -> Float
rank x = 4 ** (pap rankings x - 3.8)

trim :: Ord a => [Dist a] -> Dist [a]
trim = top 5 . sequence . map (filterP . norm)
          where filterP (D x) = D [(a, p) | (a, p) <- x, p > 0.0001]
                top n = vec2Dist . take n . reverse . presort p2 . unD
                vec2Dist x = D [(a, n / t) | (a, n) <- x]
                        where t = sum (map p2 x)

mcataLTree' g = k
        where k (Leaf a) = g1 a
              k (Fork (x, y)) = mmbin g2 (k x, k y)
              g1 = g . i1
              g2 = g . i2

mmbin :: Monad m => ((a, b) -> m c) -> (m a, m b) -> m c
mmbin f (a, b) = do {x <- a; y <- b; f (x, y)}

best n = map p1 . take n . reverse . presort p2

consolidate :: (Num d, Eq d, Eq b) => [(b, d)] -> [(b, d)]
consolidate = map (id >< sum) . collect

collect :: (Eq a, Eq b) => [(a, b)] -> [(a, [b])]
collect x = nub [ k |-> [ d' | (k',d') <- x , k'==k ] | (k,d) <- x ]

mbin :: Monad m => ((a, b) -> c) -> (m a, m b) -> m c
mbin = mmbin . (return.)

rcons (x, a) = x ++ [a]


