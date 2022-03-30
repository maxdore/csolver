{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
    ( someFunc
    ) where

import Control.Monad
import Control.Monad.Except
-- import Control.Monad.Reader
import Control.Monad.State
-- import Control.Monad.List
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map ((!), Map)
-- import qualified Data.IntSet as IntSet
-- import Data.IntSet (IntSet)
import Data.Set (Set)
import qualified Data.Set as Set

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (pure True) b

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = foldr ((||^) . p) (pure False)



type Id = String

type Endpoint = Bool

type Dim = [[Int]]

data Term = Face Id | Abs Term | App Term Dim -- | Hole Int

data Cube = Path Cube Term Term | Point
  deriving (Eq , Show)

type Decl = (Id,Cube)
type Tele   = [Decl]


instance Eq Term where
    Abs (App u [[1]]) == v = u == v
    Abs (Abs (App (App u [[2]]) [[1]])) == v = u == v
    -- TODO GENERALIZE
    (Abs u) == (Abs v) = u == v
    (App u i) == (App v j) = u == v && i == j
    (Face m) == (Face n) = m == n
    _ == _ = False

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



type Solving s a = StateT (SEnv s) (ExceptT String IO) a

data SEnv s =
  SEnv { context :: Tele
       , goal    :: Cube
       -- , verbose :: Bool -- for later
       , varSupply :: Int
       , varMap :: VarMap s -- Map Int (Set Term)
       }

-- type VarSupply s = FDVar s
data VarInfo a = VarInfo { delayedConstraints :: Solving a (), values :: Set Term }
type VarMap a = Map Int (VarInfo a)
-- data FDState s = FDState { varSupply :: VarSupply s, varMap :: VarMap s }



trace :: String -> Solving s ()
trace s = do
  -- b <- asks verbose
  -- when b $ liftIO (putStrLn s)
  liftIO (putStrLn s)



lookupDef :: Id -> Solving s Cube
lookupDef name = do
  context <- gets context
  case lookup name context of
    Just c -> return c
    Nothing -> throwError $ "Could not find definition of " ++ name


-- call it @ or something?
subst :: Term -> Int -> Bool -> Solving s Term
subst (Face name) k e = return $ Face name
subst (Abs t) i e = subst t i e >>= (\r -> return $ Abs r)
-- subst (Abs t) i e = subst t i e
subst (App t dim) i e = do
  r <- subst t i e
  if e
    then do
      let ndim = map (delete i) dim
      if [] `elem` ndim
        then do
          u <- infer r
          trace $ "SUBST" ++ show ndim
          trace $ show r ++ " : " ++ show u
          case u of (Path _ u0 u1) -> return u1
        else return $ App r ndim
        -- else return r
    else do
      let ndim = filter (\c -> not (i `elem` c)) dim
      if ndim == []
        then do
          u <- infer r
          case u of (Path _ u0 u1) -> return u0
        else return $ App r ndim
        -- else return r

apply :: Term -> Bool -> Solving s Term
apply (Abs t) = subst t (depth t)


infer :: Term -> Solving s Cube
infer (Face name) = lookupDef name
infer (Abs t) = do
  ty <- infer t
  let numvars = depth t + 1
  u <- subst t numvars False
  v <- subst t numvars True
  return $ Path ty u v
infer (App t i) = infer t >>= (\r -> case r of (Path c _ _) -> return c) -- nonono


hasType :: Term -> Cube -> Solving s Bool
hasType t d = infer t >>= (\c -> return $ c == d)


endpointsAgree :: Term -> Endpoint -> Term -> Endpoint -> Solving s Bool
endpointsAgree (Abs u) i (Abs v) j = do
  ui <- subst u (depth u + 1) i
  vi <- subst v (depth v + 1) j
  trace $ "ENDPOINTS: " ++ show u ++ " ON " ++ show i ++ " AND " ++ show v ++ " ON " ++ show j ++ " : " ++ show (ui) ++ " vs " ++ show vi
  return $ ui == vi

evalTy :: Cube -> Bool -> Solving s Cube
evalTy Point e = return Point
evalTy (Path c u v) e = do
  let numvars = depth u + 1
  eu <- subst u numvars e
  ev <- subst v numvars e
  return $ Path c eu ev

checkBoundaries :: Cube -> Solving s ()
checkBoundaries (Point) = return ()
checkBoundaries (Path c u v) = do
  c0 <- evalTy c False
  c1 <- evalTy c True
  ut <- infer u
  vt <- infer v
  if c0 /= ut
    then throwError $ "Boundary does not match: " ++ show ut ++ " is not " ++ show c0
  else if c1 /= vt
    then throwError $ "Boundary does not match: " ++ show vt ++ " is not " ++ show c1
    else return ()



-- Generates all formulas in dnf? Idea: disjunctive normal normal forms are
-- those which do not have two clauses where one subsumes the other
-- (e.g., P or (P and Q))
formulas :: [Int] -> [Dim]
formulas is = filter (\ phi -> all (\c -> all (\d -> c == d || c \\ d /= []) phi) phi) (subsets (subsets is))
  where
  subsets :: [a] -> [[a]]
  subsets [ i ] = [[ i ]]
  subsets (i : is) = let r = subsets is in
    [[i]] ++ r ++ map (i:) r


-- For a given declaration generate all possible terms that could fit in a cube
fitin :: Decl -> Int -> Solving s [Term]
fitin (name,x) cdim = return $ map (`absn` cdim) (appn (Face name) (formulas [1 .. cdim]) (dim x))
    where
    absn :: Term -> Int -> Term
    absn t 0 = t
    absn t n = Abs (absn t (n - 1))
    appn :: Term -> [Dim] -> Int -> [Term]
    appn t is 0 = [t]
    appn t is n = [ App u i | u <- appn t is (n-1) , i <- is]





newVar :: [Term] -> Solving s Int
newVar domain = do
    v <- nextVar
    v `isOneOf` domain
    return v
    where
        -- nextVar :: Solving
        nextVar = do
            s <- get
            let v = varSupply s
            put $ s { varSupply = (v + 1) }
            return v
        -- isOneOf :: Solving -> [Term] -> Solving
        x `isOneOf` domain =
            modify $ \s ->
                let vm = varMap s
                    vi = VarInfo {
                        delayedConstraints = return (),
                        values = Set.fromList domain}
                in
                s { varMap = Map.insert x vi vm }

lookupDom :: Int -> Solving s (Set Term)
lookupDom x = do
    s <- get
    return . values $ varMap s ! x


update :: Int -> Set Term -> Solving s ()
update x i = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    put $ s { varMap = Map.insert x (vi { delayedConstraints = return (), values = i }) vm }
    -- put $ s { varMap = Map.insert x (vi { values = i }) vm }
    delayedConstraints vi


addConstraint :: Int -> Solving s () -> Solving s ()
addConstraint x constraint = do
    s <- get
    let vm = varMap s
    let vi = vm ! x
    let cs = delayedConstraints vi
    put $ s { varMap =
        Map.insert x (vi { delayedConstraints = cs >> constraint }) vm }

type BinaryConstraint s = Int -> Int -> Solving s ()
addBinaryConstraint :: BinaryConstraint s -> BinaryConstraint s
addBinaryConstraint f x y = do
    let constraint  = f x y
    constraint
    addConstraint x constraint
    addConstraint y constraint



endpoints :: Endpoint -> Endpoint -> Int -> Int -> Solving s ()
endpoints i j = addBinaryConstraint $ \x y -> do
    xv <- lookupDom x
    yv <- lookupDom y
    xv' <- filterM (\x' -> anyM (\y' -> endpointsAgree x' i y' j) (Set.toList yv)) (Set.toList xv)
    yv' <- filterM (\y' -> anyM (\x' -> endpointsAgree x' i y' j) (Set.toList xv)) (Set.toList yv)
    guard $ not $ Set.null (Set.fromList xv')
    guard $ not $ Set.null (Set.fromList yv')
    when (xv' /= Set.toList xv) $ update x (Set.fromList xv')
    when (yv' /= Set.toList yv) $ update y (Set.fromList yv')



labelling :: [Int] -> Solving s [Term]
labelling = mapM label where
    label :: Int -> Solving s Term
    label var = do
        vals <- lookupDom var
        trace $ show var ++ " : " ++ show vals
        let val = (Set.toList vals) !! 0
        -- val <- Solving . lift $ Set.toList vals
        -- var `hasValue` val
        return val




comp :: Cube -> [Term] -> Solving s [Term]
comp (Path Point a b) shapes = do
  lshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
                                            Path Point _ u -> return $ a == u
                                            _ -> return False)) shapes
  rshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
                                            Path Point _ u -> return $ b == u
                                            _ -> return False)) shapes
  (trace . show) lshapes
  (trace . show) shapes
  (trace . show) rshapes

  xa <- newVar lshapes
  xy <- newVar shapes
  yb <- newVar rshapes

  endpoints False False xy xa
  endpoints True False xy yb

  res <- labelling [xa , xy , yb]

  trace "RESULT"
  (trace . show) res
  return []


comp (Path (Path Point q r) p s) shapes = do
  -- pshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
  --   Path (Path Point _ _) u _ -> return $ p == u
  --   _ -> return False)) shapes

  qty <- infer (Abs q)
  trace $ "Q: " ++ show (Abs q) ++ " : " ++ show qty
  qshapes <- filterM (\s -> do
    bound <- apply s False
    sty <- infer bound
    trace $ show s ++ " : " ++ show sty
    return $ sty == qty) shapes

  -- rshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
  --   Path (Path Point _ _) u _ -> return $ r == u
  --   _ -> return False)) shapes
  -- sshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
  --   Path (Path Point _ _) _ u -> return $ s == u
  --   _ -> return False)) shapes

  -- (trace . show) pshapes
  (trace . show) qshapes
  -- (trace . show) rshapes
  -- (trace . show) sshapes

  -- psides <- newVar pshapes
  -- qsides <- newVar qshapes
  -- rsides <- newVar rshapes
  -- ssides <- newVar sshapes
  -- back <- newVar shapes

  -- endpoints True False xy yb


  return []

rightBoundary :: Term -> Endpoint -> Solving s ()
-- rightBoundary (Path (Path Point q r) p s) = do
rightBoundary u e = do
  ty <- infer u
  trace $ show u ++ " : " ++ show ty
  boundary <- apply u e
  ty' <- infer boundary
  trace $ show boundary ++ " : " ++ show ty'
  return ()



solve :: Solving s [Term]
solve = do
  trace "CONTEXT"
  context <- gets context
  mapM (\(name , u) -> trace $ name ++ " : " ++ show u) context
  mapM (\(name , u) -> checkBoundaries u) context

  trace "GOAL"
  goal <- gets goal
  trace $ show goal
  checkBoundaries goal

  shapes <- do
    allshapes <- mapM (\ d -> fitin d (dim goal)) context
    return $ concat allshapes

  trace "SHAPES"
  mapM (\t -> infer t >>= (\ty -> trace $ (show t) ++ " : " ++ show ty)) shapes

  res <- filterM (`hasType` goal) shapes

  trace "DIRECT FIT"
  trace $ show res

  if res /= []
    then return res
    else comp goal shapes


runSolve :: SEnv s -> IO (Either String ([Term],SEnv s))
runSolve env = runExceptT $ runStateT solve env

runInfer :: Tele -> Term -> IO (Either String (Cube,SEnv s))
runInfer ctxt t = do
  res <- runExceptT $ runStateT (infer t) (SEnv ctxt Point 0 Map.empty)
  case res of
    Left err -> do
      putStrLn $ "ERROR: " ++ err
    Right (ty , _)->
      putStrLn $ show ty
  return res

runTest :: Tele -> Term -> Endpoint -> IO (Either String ((),SEnv s))
runTest ctxt t e = runExceptT $ runStateT (rightBoundary t e) (SEnv ctxt Point 0 Map.empty)



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
app1goal = SEnv intCtxt (Path (Path Point (Face "zero") (Face "one")) (Face "seg") (Face "seg")) 0 Map.empty

-- SOL \.\. App 2 seg
app2goal = SEnv intCtxt (Path (Path Point (App (Face "seg") [[1]]) (App (Face "seg") [[1]])) (Abs (Face "zero")) (Abs (Face "one")))

-- SOL \.\. App (And 1 2) seg
andGoal = SEnv intCtxt (Path (Path Point (Face "zero") (App (Face "seg") [[1]])) (Abs (Face "zero")) (Face "seg"))

-- SOL \.\. App (Or 1 2) seg
orGoal = SEnv intCtxt ((Path (Path Point (App (Face "seg") [[1]]) (Face "one")) (Face "seg")) (Abs (Face "one")))

-- SOL \. App 1 seg
trivGoal = SEnv intCtxt (Path Point (Face "zero") (Face "one"))

-- SOL hcomp
invGoal = SEnv intCtxt (Path Point (Face "one") (Face "zero")) 0 Map.empty


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
  , ("sq" ,     Path (Path Point (App (Face "q") [[1]]) (App (Face "r") [[1]])) (Face "p") (Face "s"))
           ]

-- Path Point a d
lsq11 = Abs (App (App (Face "sq") [[1]]) [[1]])

-- Path (Path Point a d) \1.sq<1><1> \1.sq<1><1>
llsq11 = Abs (Abs (App (App (Face "sq") [[1]]) [[1]]))

-- Path (Path Point sq<1><1> sq<1><1>) \1.a \1.d
-- TODO RESULT HAS sq<2><2>
llsq22 = Abs (Abs (App (App (Face "sq") [[2]]) [[2]]))

-- Path (Path Point p<1> sq<1><1>) \1.a r
llsq1and22 = Abs (Abs (App (App (Face "sq") [[1,2]]) [[2]]))

-- Path (Path Point p<1> d) \1.sq<1><1> r
llsq11or2 = Abs (Abs (App (App (Face "sq") [[1]]) [[1],[2]]))

-- Path (Path (Path Point q<[[1]]> r<[[1]]>) p s) sq sq
lllsq21 = Abs (Abs (Abs (App (App (Face "sq") [[2]]) [[1]])))

-- Path (λ i → Path (λ j → Path Point (p (i ∨ j)) d) (λ k → sq k (i ∨ k)) r) (λ j k → sq k (j ∨ k)) λ _ → r
-- Path (Path (Path Point p<[[2],[3]]> d) \1.sq<[[1]]><[[1],[3]]> \1.r<[[1]]>) \2.\1.sq<[[1]]><[[1],[2]]> \2.\1.r<[[1]]>
lllsqor = (Abs (Abs (Abs (App (App (Face "sq") [[1]]) [[1],[2],[3]]))))

-- SOL \2.\1. sq (2 ∧ 1) 2
sqands = SEnv sqCtxt (Path (Path Point (App (Face "p") [[1]]) (App (App (Face "sq") [[1]]) [[1]])) (Abs (Face "a")) (Face "r"))

-- SOL \2.\1.sq<2><2>
sq22 = SEnv sqCtxt (Path (Path Point
                          (App (App (Face "sq") [[1]]) [[1]])
                          (App (App (Face "sq") [[1]]) [[1]]))
                     (Abs (Face "a"))
                     (Abs (Face "d")))

-- SOL \.\.\. sq<2><1>
sq21 = SEnv sqCtxt (Path (Path (Path Point (App (Face "q") [[1]]) (App (Face "r") [[1]])) (Face "p") (Face "s")) (Face "sq") (Face "sq"))
-- This works??!?




-- Sset example

ssetCtxt :: Tele
ssetCtxt = [
    ("x" ,     Point)
  , ("y" ,     Point)
  , ("z" ,     Point)
  , ("f" ,      Path Point (Face "x") (Face "y"))
  , ("g" ,      Path Point (Face "y") (Face "z"))
  , ("h" ,      Path Point (Face "x") (Face "z"))
  , ("phi" , Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))
           ]

lowerT = SEnv ssetCtxt (Path (Path Point (App (Face "f") [[1]]) (App (Face "h") [[1]])) (Abs (Face "x")) (Face "g")) 0 Map.empty






-- Comp example

compCtxt :: Tele
compCtxt = [
    ("w" , Point)
  , ("x" , Point)
  , ("y" , Point)
  , ("z" , Point)
  , ("p" , Path Point (Face "x") (Face "w"))
  , ("q" , Path Point (Face "x") (Face "y"))
  , ("r" , Path Point (Face "y") (Face "z"))
           ]

compGoal = SEnv compCtxt (Path Point (Face "w") (Face "z")) 0 Map.empty


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

