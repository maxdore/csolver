-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe



type Id = String

data Dim = Var Int | And Dim Dim | Or Dim Dim
  deriving (Eq)

data Term = Face Id | Abs Term | App Term Dim

data Cube = Path Cube Term Term | Point
  deriving (Eq , Show)

type Decl = (Id,Cube)
type Tele   = [Decl]


instance Eq Term where
    Abs (App u (Var 1)) == v = u == v
    Abs (Abs (App (App u (Var 2)) (Var 1))) == v = u == v
    -- TODO ALSO QUOTIENT OUT \2.\1.sq<2><1> = sq
    -- Abs (Abs (App (App t (Var 2)) (Var 1)))
    (Abs u) == (Abs v) = u == v
    (App u i) == (App v j) = u == v && i == j
    (Face m) == (Face n) = m == n
    _ == _ = False


instance Show Dim where
  show (Var i) = show i
  show (And i j) = show i ++ " and " ++ show j
  show (Or i j) = show i ++ " or " ++ show j

instance Show Term where
  show (Face name) = name
  show (Abs u) = "\\" ++ show (depth u + 1) ++  "." ++ show u
  show (App u i) = show u ++ "<" ++ show i ++ ">"

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



type Solving a = ReaderT SEnv (ExceptT String IO) a

data SEnv =
  SEnv { context :: Tele
       , goal    :: Cube
       -- , verbose :: Bool
       } deriving (Eq)



trace :: String -> Solving ()
trace s = do
  -- b <- asks verbose
  -- when b $ liftIO (putStrLn s)
  liftIO (putStrLn s)


lookupDef :: Id -> Solving Cube
lookupDef name = do
  context <- asks context
  case lookup name context of
    Just c -> return c
    Nothing -> throwError $ "Could not find definition of " ++ name


subst :: Term -> Int -> Bool -> Solving Term
subst (Face name) k e = return $ Face name
subst (Abs t) k e = subst t k e >>= (\r -> return $ Abs r)
subst (App t (Var i)) k e = do
  r <- subst t k e
  if i == k
    then do
      u <- getType r
      case u of (Path _ u0 u1) -> return (if e then u1 else u0)
    else return $ App r (Var i)


getType :: Term -> Solving Cube
getType (Face name) = lookupDef name
getType (Abs t) = do
  let numvars = depth t + 1
  u <- subst t numvars False
  v <- subst t numvars True
  nu <- getType t
  return $ Path nu u v
getType (App t i) = getType t >>= (\r -> case r of (Path c _ _) -> return c)


hasType :: Term -> Cube -> Solving Bool
hasType t d = getType t >>= (\c -> return $ c == d)





checkBoundaries :: Cube -> Solving ()
checkBoundaries (Point) = return ()
checkBoundaries (Path c u v) = do
  c0 <- evalTy c False
  c1 <- evalTy c True
  ut <- getType u
  vt <- getType v
  if c0 /= ut
    then throwError $ "Boundary does not match: " ++ show ut ++ " is not " ++ show c0
  else if c1 /= vt
    then throwError $ "Boundary does not match: " ++ show vt ++ " is not " ++ show c1
    else return ()
  where
  evalTy :: Cube -> Bool -> Solving Cube
  evalTy Point e = return Point
  evalTy (Path c u v) e = do
    let numvars = depth u + 1
    eu <- subst u numvars e
    ev <- subst v numvars e
    return $ Path c eu ev


subsets :: [Int] -> [[Int]]
subsets [ i ] = [[ i ]]
subsets (i : is) = let r = subsets is in
  [[i]] ++ r ++ map (i:) r


formulas :: [Int] -> [Dim]
formulas [i] = [ Var i ]
formulas (i : is) = let r = formulas is in
  r ++ [ And (Var i) phi | phi <- r ]

-- [ Var i | i <- is ] ++ [ And (Var i) (Var j) | i <- is, j <- is ]


match :: Decl -> Cube -> Solving [Term]
match (name,x) c = do
  -- trace $ "Trying to match " ++ show x ++ " with " ++ show c
  let vars = [ Var i | i <- [1 .. dim c]]

  let options = map (`absn` (dim c)) (appn (Face name) vars (dim x))

  mapM (\t -> getType t >>= (\ty -> trace $ (show t) ++ " : " ++ show ty)) options

  filterM (`hasType` c) options

    where
    absn :: Term -> Int -> Term
    absn t 0 = t
    absn t n = Abs (absn t (n - 1))

    appn :: Term -> [Dim] -> Int -> [Term]
    appn t is 0 = [t]
    appn t is n = [ App u i | u <- appn t is (n-1) , i <- is]




solve :: Solving [Term]
solve = do
  trace "CONTEXT"
  context <- asks context
  mapM (\(name , u) -> trace $ name ++ " : " ++ show u) context
  mapM (\(name , u) -> checkBoundaries u) context

  trace "GOAL"
  goal <- asks goal
  trace $ show goal
  checkBoundaries goal

  res <- mapM (\ d -> match d goal) context
  return $ concat res


runSolve :: SEnv -> IO (Either String [Term])
runSolve env = runExceptT $ runReaderT solve env


runTyper :: Tele -> Term -> IO (Either String Cube)
runTyper ctxt t = runExceptT $ runReaderT (getType t) (SEnv ctxt Point)





-- Basic examples
degEnv = SEnv [ ("x" , Point) ] (Path Point (Face "x") (Face "x"))


-- Interval example
intCtxt :: Tele
intCtxt = [
    ("zero" ,     Point)
  , ("one" ,      Point)
  , ("seg" ,      Path Point (Face "zero") (Face "one"))
           ]


-- SOL \.\. App 1 seg
app1goal = SEnv intCtxt (Path (Path Point (Face "zero") (Face "one")) (Face "seg") (Face "seg"))

-- SOL \.\. App 2 seg
app2goal = SEnv intCtxt (Path (Path Point (App (Face "seg") (Var 1)) (App (Face "seg") (Var 1))) (Abs (Face "zero")) (Abs (Face "one")))

-- SOL \.\. App (And 1 2) seg
andGoal = SEnv intCtxt (Path (Path Point (Face "zero") (App (Face "seg") (Var 1))) (Abs (Face "zero")) (Face "seg"))

-- SOL \.\. App (Or 1 2) seg
orGoal = SEnv intCtxt ((Path (Path Point (App (Face "seg") (Var 1)) (Face "one")) (Face "seg")) (Abs (Face "one")))

-- SOL \. App 1 seg
trivGoal = SEnv intCtxt (Path Point (Face "zero") (Face "one"))

-- SOL hcomp
invGoal = SEnv intCtxt (Path Point (Face "one") (Face "zero"))




sqCtxt :: Tele
sqCtxt = [
    ("a" ,     Point)
  , ("b" ,      Point)
  , ("c" ,      Point)
  , ("d" ,      Point)
  , ("p" ,      Path Point (Face "a") (Face "b"))
  , ("q" ,      Path Point (Face "a") (Face "c"))
  , ("r" ,      Path Point (Face "b") (Face "d"))
  , ("s" ,      Path Point (Face "c") (Face "d"))
  , ("sq" ,     Path (Path Point (App (Face "q") (Var 1)) (App (Face "r") (Var 1))) (Face "p") (Face "s"))
           ]

-- Path Point a d
lsq11 = Abs (App (App (Face "sq") (Var 1)) (Var 1))

-- Path (Path Point a d) \1.sq<1><1> \1.sq<1><1>
llsq11 = Abs (Abs (App (App (Face "sq") (Var 1)) (Var 1)))

-- Path (Path Point sq<1><1> sq<1><1>) \1.a \1.d
-- TODO RESULT HAS sq<2><2>
llsq22 = Abs (Abs (App (App (Face "sq") (Var 2)) (Var 2)))




-- SOL \2.\1. sq (2 ∧ 1) 2
sqands = SEnv sqCtxt (Path (Path Point (App (Face "p") (Var 1)) (App (App (Face "sq") (Var 1)) (Var 1))) (Abs (Face "a")) (Face "r"))



-- SOL \.\.\. sq<2><1>
sq21goal = SEnv sqCtxt (Path (Path (Path Point (App (Face "q") (Var 1)) (App (Face "r") (Var 1))) (Face "p") (Face "s")) (Face "sq") (Face "sq"))

-- SOL \2.\1.sq<2><2>
sq22 = SEnv sqCtxt (Path (Path Point
                          (App (App (Face "sq") (Var 1)) (Var 1))
                          (App (App (Face "sq") (Var 1)) (Var 1)))
                     (Abs (Face "a"))
                     (Abs (Face "d")))



-- Sset example

ssetCtxt :: Tele
ssetCtxt = [
    ("vert x" ,     Point)
  , ("vert y" ,     Point)
  , ("vert z" ,     Point)
  , ("edge f" ,      Path Point (Face "vert x") (Face "vert y"))
  , ("edge g" ,      Path Point (Face "vert y") (Face "vert z"))
  , ("edge h" ,      Path Point (Face "vert x") (Face "vert z"))
  , ("triangle phi" , Path (Path Point (App (Face "edge h") (Var 1)) (App (Face "edge g") (Var 1))) (Face "edge f") (Abs (Face "vert z")))
           ]

ssetGoal :: Cube
ssetGoal = Path (Path Point (App (Face "edge f") (Var 1)) (App (Face "edge h") (Var 1))) (Abs (Face "vert x")) (Face "edge g")

ssetEnv :: SEnv
ssetEnv = SEnv ssetCtxt ssetGoal





-- Comp example

compCtxt :: Tele
compCtxt = [
    ("w" , Point)
  , ("x" , Point)
  , ("y" , Point)
  , ("z" , Point)
  , ("p" , Path Point (Face "w") (Face "x"))
  , ("q" , Path Point (Face "x") (Face "y"))
  , ("r" , Path Point (Face "y") (Face "z"))
           ]

compGoal :: Cube
compGoal = Path Point (Face "w") (Face "z")

-- Goal: A
-- ———— Boundary ——————————————————————————————————————————————
-- i = i0 ⊢ w
-- i = i1 ⊢ z
-- ————————————————————————————————————————————————————————————
-- i    : I
-- r    : y ≡ z
-- q    : x ≡ y
-- p    : w ≡ x
-- z    : A   (not in scope)
-- y    : A   (not in scope)
-- x    : A   (not in scope)
-- w    : A   (not in scope)
-- A  : Set (not in scope)




-- CAN WE REPRESENT CONG??
-- cong : (f : (a : A) → B a) (p : x ≡ y) →
--        PathP (λ i → B (p i)) (f x) (f y)
-- cong f p i = f (p i)



-- pair :: Context
-- pair = [ Path (TTerm "A") "snd k" "snd l" ]

-- i : I
-- q : snd k ≡ snd l
-- p : fst k ≡ fst l
-- l : A × B
-- k : A × B
-- B : Set   (not in scope)
-- A : Set   (not in scope)





someFunc :: IO ()
someFunc = putStrLn "someFunc"
