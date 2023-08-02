module Data where


import Data.List



type Id = String

type Endpoint = Bool

type Var = Int

type Dim = [[Var]]

data Term = Face Id | Abs Term | App Term Dim
  deriving (Eq, Show)

data Cube = Path Cube Term Term | Point
  deriving (Eq , Show)

type Decl = (Id,Cube)
type Tele   = [Decl]


data Result = Dir Term | Comp Term [(Term,Term)]
  deriving (Eq, Show)

-- instance Eq Term where
--   u == v = (normalize u) == (normalize v)
  -- Abs (App u [[1]]) == v = u == v
  -- Abs (Abs (App (App u [[2]]) [[1]])) == v = u == v
  -- -- TODO GENERALIZE
  -- (Abs u) == (Abs v) = u == v
  -- (App u i) == (App v j) = u == v && i == j
  -- (Face m) == (Face n) = m == n
  -- _ == _ = False

-- TODO pretty print disjunctive normal forms
-- instance Show Dim where
--   show [] = ""

-- instance Show Term where
--   show (Face name) = name
--   show (Abs u) = "\\" ++ show (depth u + 1) ++ "." ++ show u
--   show (App u i) = "(" ++ show u ++ "<" ++ show i ++ ">)"


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


toInd :: Endpoint -> Int
toInd False = 0
toInd True = 1

dimAsString = ["i","j","k","l","m","n"]


dim :: Cube -> Int
dim Point = 0
dim (Path c u v) = dim c + 1

instance Ord Cube where
  c <= d = dim c <= dim d


depth :: Term -> Int
depth (Face name) = 0
depth (Abs t) = 1 + depth t
depth (App t i) = 0


varOccurs :: Int -> Term -> Bool
varOccurs i (Face name) = False
varOccurs i (Abs t) = varOccurs i t
varOccurs i (App t r) = varOccurs i t || i `elem` concat r


subst :: Term -> Int -> Dim -> Term
subst (Face name) i r = Face name
subst (Abs t) i r = Abs (subst t i r)
subst (App t s) i r = App (subst t i r) (subst' s i r)
  where
  subst' :: Dim -> Int -> Dim -> Dim
  subst' s i r = redDnf $ concat $ map (\c -> if i `elem` c
                                 then map (++ delete i c) r
                                 else [c]
                                  ) s


substC :: Cube -> Int -> Dim -> Cube
substC Point _ _ = Point
substC (Path c u v) i r = Path (substC c i r) (subst u i r) (subst v i r)


normalize :: Term -> Term
normalize (Face name) = Face name
-- eta reductions
normalize (Abs (App t [[1]])) = if varOccurs 1 t
  then Abs (App (normalize t) [[1]])
  else normalize (decDimT t)
normalize (Abs (Abs (App (App t [[2]]) [[1]]))) = if varOccurs 1 t || varOccurs 2 t
    then (Abs (Abs (App (App (normalize t) [[2]]) [[1]])))
    else normalize (decDimT (decDimT t))
-- TODO GENERALIZE
-- beta reductions
normalize (App (Abs t) r) = normalize (subst t 1 r)
normalize (Abs t) = Abs (normalize t)
normalize t = t


normalizeC :: Cube -> Cube
normalizeC Point = Point
normalizeC (Path c u v) = Path (normalizeC c) (normalize u) (normalize v)



-- TODO use these functions to fix the index mess?
decDimT :: Term -> Term
decDimT (Face name) = Face name
decDimT (Abs t) = Abs (decDimT t)
decDimT (App t dim) = App (decDimT t) (map (map (\i -> i-1)) dim)

decDim :: Cube -> Cube
decDim Point = Point
decDim (Path c u v) = Path (decDim c) (decDimT u) (decDimT v)


decDimC :: Cube -> Cube
decDimC Point = Point
decDimC (Path Point u v) = Path Point (decUnbound u) (decUnbound v)
decDimC (Path c u v) = Path (decDimC c) (iterate decUnbound u !! dim c) (iterate decUnbound v !! dim c)



decUnbound :: Term -> Term
decUnbound t = decUnbound' t 0
  where
  decUnbound' (Face name) i = Face name
  decUnbound' (Abs t) i = Abs $ decUnbound' t (i + 1)
  decUnbound' (App t r) i = App (decUnbound' t i) (map (map (\j -> if j <= i then j else j-1)) r)


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



unpeelAbs :: Term -> (Int, Term)
unpeelAbs (Face name) = (0, Face name)
unpeelAbs (Abs t) = let (n , t') = unpeelAbs t in (n+1, t')
unpeelAbs (App t r) = (0, App t r)
