module Data where


import Data.List



type Id = String

type Endpoint = Bool

type Dim = [[Int]]

data Term = Face Id | Abs Term | App Term Dim -- | Hole Int
  deriving (Eq)

data Cube = Path Cube Term Term | Point
  deriving (Eq , Show)

type Decl = (Id,Cube)
type Tele   = [Decl]


data Result = Dir Term | Comp Term [(Term,Term)]
  deriving (Show)

-- instance Eq Term where
--     Abs (App u [[1]]) == v = u == v
--     -- Abs (Abs (App (App u [[2]]) [[1]])) == v = u == v
--     -- TODO GENERALIZE
--     (Abs u) == (Abs v) = u == v
--     (App u i) == (App v j) = u == v && i == j
--     (Face m) == (Face n) = m == n
--     _ == _ = False

-- TODO pretty print disjunctive normal forms
-- instance Show Dim where
--   show [] = ""

instance Show Term where
  show (Face name) = name
  show (Abs u) = "\\" ++ show (depth u + 1) ++ "." ++ show u
  show (App u i) = "(" ++ show u ++ "<" ++ show i ++ ">)"


instance Ord Term where
  -- compare (Face a) (Face b) = compare a b
  -- compare (Abs a) (Abs b) = compare a b
  -- compare (App u i) (App v j) = compare u v
  (Face a) <= (Face b) = a <= b
  (Abs u) <= (Abs v) = u <= v
  (App u i) <= (App v j) = u <= v
  (Face a) <= t = True
  (Abs u) <= t = u <= t
  (App u i) <= t = u <= t


-- instance Show Cube where
--   show Point = "."
--   show (Path Point u v) = show u ++ "--->" ++ show v
--   show (Path (Path Point u0 u1) v0 v1) = let lm = length (show u0) in "\n" ++
--     (replicate (lm + 6 - (length (show v1)) `div` 2) ' ') ++ show v1 ++ "\n" ++
--     (replicate lm ' ') ++ "+---------->+\n" ++
--     (replicate lm ' ') ++ "^           ^\n" ++
--     (replicate lm ' ') ++ "|           |\n" ++
--                show u0 ++ "|           |" ++ show u1 ++ "\n" ++
--     (replicate lm ' ') ++ "|           |\n" ++
--     (replicate lm ' ') ++ "|           |\n" ++
--     (replicate lm ' ') ++ "+---------->+\n" ++
--     (replicate (lm + 6 - (length (show v0)) `div` 2) ' ') ++ show v0
--   show (Path (Path (Path Point t0 t1) u0 u1) v0 v1) =
--         "   +--------------+" ++
--         "  /|             /|" ++
--         " / |            / |" ++
--         "*--------------*  |" ++
--         "|  |           |  |" ++
--         "|  |           |  |" ++
--         "|  |           |  |" ++
--         "|  +-----------|--+" ++
--         "| /            | /" ++
--         "|/             |/" ++
--         "*--------------*"

dim :: Cube -> Int
dim Point = 0
dim (Path c u v) = dim c + 1

depth :: Term -> Int
depth (Face name) = 0
depth (Abs t) = 1 + depth t
depth (App t i) = 0


-- TODO use these functions to fix the index mess?
decDimT :: Term -> Term
decDimT (Face name) = Face name
decDimT (Abs t) = Abs (decDimT t)
decDimT (App t dim) = App (decDimT t) (map (map (\i -> i-1)) dim)
decDim :: Cube -> Cube
decDim Point = Point
decDim (Path c u v) = Path (decDim c) (decDimT u) (decDimT v)



inDnf :: Dim -> Bool
inDnf phi = all (\c -> all (\d -> c == d || c \\ d /= []) phi) phi

redDnf :: Dim -> Dim
redDnf phi = filter (\c -> not (any (\d -> c /= d && d `isSubsequenceOf` c) norm)) norm
  where norm = (nub $ map sort phi)

-- Generates all formulas in dnf? Idea: disjunctive normal normal forms are
-- those which do not have two clauses where one subsumes the other
-- (e.g., P or (P and Q))
formulas :: [Int] -> [Dim]
formulas is = filter inDnf (subsets (subsets is))
  where
  subsets :: [a] -> [[a]]
  subsets [ i ] = [[ i ]]
  subsets (i : is) = let r = subsets is in
    [[i]] ++ r ++ map (i:) r



getTFace :: Term -> Id
getTFace (Face name) = name
getTFace (Abs t) = getTFace t
getTFace (App t r) = getTFace t


getCFaces :: Cube -> [Id]
getCFaces Point = []
getCFaces (Path c u v) = nub $ getCFaces c ++ [ getTFace u , getTFace v ]



