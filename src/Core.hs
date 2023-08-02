{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Core where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import Data.List
import qualified Data.Map as Map
import Data.Map ((!), Map)
import Data.Set (Set)
import qualified Data.Set as Set

import Data
import Lib

type Solving s a = StateT (SEnv s) (ExceptT String IO) a

data SEnv s =
  SEnv { context :: Tele
       , goal    :: Cube
       , varSupply :: Int
       , varMap :: VarMap s
       , verbose :: Bool
       , allSol :: Bool
       }

mkSEnv ctxt goal = SEnv ctxt goal 0 Map.empty True -- False

data VarInfo a = VarInfo { delayedConstraints :: Solving a (), values :: Set Term }
type VarMap a = Map Int (VarInfo a)

trace :: String -> Solving s ()
trace s = do
  b <- gets verbose
  when b $ liftIO (putStrLn s)



lookupDef :: Id -> Solving s Cube
lookupDef name = do
  context <- gets context
  case lookup name context of
    Just c -> return c
    Nothing -> throwError $ "Could not find definition of " ++ name


-- call it @ or something?
eval :: Term -> Int -> Bool -> Solving s Term
eval (Face name) k e = return $ Face name
eval (Abs t) i e = eval t i e >>= (\r -> return $ Abs r)
eval (App t dim) i e = do
  r <- eval t i e
  if e
    then do
      let ndim = redDnf $ map (delete i) dim
      if [] `elem` ndim
        then do
          u <- infer' r
          -- trace $ "EVAL" ++ show ndim
          -- trace $ show r ++ " : " ++ show u
          case u of (Path _ u v) -> return v
        else return $ App r ndim
        -- else return r
    else do
      let ndim = redDnf $ filter (\c -> not (i `elem` c)) dim
      if ndim == []
        then do
          u <- infer' r
          case u of (Path _ u v) -> return u
        else return $ App r ndim
        -- else return r

apply :: Term -> Bool -> Solving s Term
apply (Abs t) = eval t (depth t)





infer :: Term -> Solving s Cube
infer t = infer' t >>= return . normalizeC . decDimC


-- TODO also normalize formulas after type inference, e.g.,
-- Abs (Abs (Abs (App (Face "f") [[1,2],[1,3]]))) : Path (Path (Path Point (Face "x") (App (Face "f") [[1],[2]])) (Abs (App (Face "f") [[1,2]])) (Abs (App (Face "f") [[1],[1,2]]))) (Abs (Abs (App (Face "f") [[1,2]]))) (Abs (Abs (App (Face "f") [[1,2],[1]])))
infer' (Face name) = lookupDef name
infer' (Abs t) = do
  ty <- infer' t
  let numvars = depth t + 1
  u <- eval t numvars False
  v <- eval t numvars True
  return $ Path ty u v
  -- return $ Path (decDimC ty) u v
infer' (App t i) = do
  ty <- infer' t
  case ty of
    (Path c _ _) -> return $ substC c 1 i
    _ -> throwError "Applied non-path to interval variable"


hasType :: Term -> Cube -> Solving s Bool
hasType t d = infer t >>= (\c -> return $ c == d)



evalTy :: Cube -> Int -> Bool -> Solving s Cube
evalTy Point i e = return Point
evalTy (Path c u v) i e = do
  ec <- evalTy c (i + 1) e
  eu <- eval u i e
  ev <- eval v i e
  return $ normalizeC $ Path ec eu ev

wellFormedCube :: Cube -> Solving s ()
wellFormedCube (Point) = return ()
wellFormedCube (Path c u v) = do
  return ()
  -- wellFormedCube c TODO
  -- let numvars = dim c
  -- c0 <- evalTy c 1 False -- >>= return . decDim
  -- c1 <- evalTy c 1 True -- >>= return . decDim
  -- ut <- infer' u
  -- vt <- infer' v
  -- if c0 /= ut
  --   then throwError $ "BOUNDARY OF " ++ show (Path c u v) ++  "\n" ++ show ut ++ "\nis not\n" ++ show c0
  -- else if c1 /= vt
  --   then throwError $ "BOUNDARY OF " ++ show (Path c u v) ++  "\n" ++ show vt ++ "\nis not\n" ++ show c1
  --   else return ()




absn :: Term -> Int -> Term
absn t 0 = t
absn t n = Abs (absn t (n - 1))

appn :: Term -> [Dim] -> Int -> [Term]
appn t is 0 = [t]
appn t is n = [ App u i | u <- appn t is (n-1) , i <- is]


-- For a given declaration generate all possible terms that could fit in a cube
fitin :: Decl -> Int -> Solving s [Term]
fitin (name,x) cdim = return $ map (`absn` cdim) (appn (Face name) (formulas [1 .. cdim]) (dim x))


-- endpointsAgree :: Term -> Endpoint -> Term -> Endpoint -> Solving s Bool
-- endpointsAgree (Abs u) i (Abs v) j = do
--   ui <- eval u (depth u + 1) i
--   vi <- eval v (depth v + 1) j
--   trace $ "ENDPOINTS: " ++ show u ++ " ON " ++ show i ++ " AND " ++ show v ++ " ON " ++ show j ++ " : " ++ show (ui) ++ " vs " ++ show vi ++ " is " ++ show (ui == vi)
--   return $ ui == vi


boundariesAgree  :: Term -> Var -> Endpoint -> Term -> Var -> Endpoint -> Solving s Bool
boundariesAgree u i e v j e' = do
  ub <- getBoundary u i e
  vb <- getBoundary v j e'
  -- ue <- eval u i e
  -- ve <- eval v j e'
  -- ub <- infer ue
  -- vb <- infer ve
  -- ub <- eval u i e
  -- vb <- eval v j e'
  -- when (ub == vb) $
  --   trace $ "BOUNDARY: " ++ show u ++ " | " ++ show i ++ " @ " ++ show e ++ " = " ++ show ub
  --     ++ " ?=? " ++ show v ++ " | " ++ show j ++ " @ " ++ show e' ++ " = " ++ show vb -- ++ ":\n " ++ show (ub == vb)
  return (ub == vb)


getBoundary :: Term -> Int -> Endpoint -> Solving s Term
getBoundary t i e = do
  ty <- infer t
  getBoundaryC ty i e 0 >>= return . normalize . decUnbound

getBoundaryC :: Cube -> Int -> Endpoint -> Int -> Solving s Term
getBoundaryC (Path c u v) i e d =
  if i == 1
    then return (normalize (absn (if e then v else u) d))
    else getBoundaryC c (i-1) e (d + 1)



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



boundaries :: Var -> Endpoint -> Var -> Endpoint -> Int -> Int -> Solving s ()
boundaries i e j e' = addBinaryConstraint $ \x y -> do
    xv <- lookupDom x
    yv <- lookupDom y
    xv' <- filterM (\x' -> anyM (\y' -> boundariesAgree x' i e y' j e') (Set.toList yv)) (Set.toList xv)
    yv' <- filterM (\y' -> anyM (\x' -> boundariesAgree x' i e y' j e') (Set.toList xv)) (Set.toList yv)
    guard $ not $ Set.null (Set.fromList xv')
    guard $ not $ Set.null (Set.fromList yv')
    when (xv' /= Set.toList xv) $ update x (Set.fromList xv')
    when (yv' /= Set.toList yv) $ update y (Set.fromList yv')


hasValue :: Var -> Term -> Solving s ()
var `hasValue` val = do
    vals <- lookupDom var
    guard $ val `Set.member` vals
    let i = Set.singleton val
    when (i /= vals) $ update var i


oneSolution :: [Int] -> Solving s [Term]
oneSolution = mapM label where
    label :: Int -> Solving s Term
    label var = do
        vals <- lookupDom var
        let val = (Set.toList vals) !! 0
        var `hasValue` val
        return val

        -- trace $ show var ++ " : " ++ show vals

        -- let ind = if (length (Set.toList vals) > 1) then 1 else 0
        -- let val = (Set.toList vals) !! ind
        -- var `hasValue` val
        -- return val
  
        -- sndSol <- gets allSol
        -- if sndSol && length (Set.toList vals) > 1
        --   then do
        --         trace "GETTING SECONDS"
        --         -- s <- get
        --         -- put $ s { allSol = False }
        --         let val = (Set.toList vals) !! 1
        --         var `hasValue` val
        --         return val
        --   else do
        --         trace "GETTING FIRSTS"
        --         let val = (Set.toList vals) !! 0
        --         var `hasValue` val
        --         return val

        -- val <- Solving . lift $ Set.toList vals


allSolutions :: [Int] -> Solving s [[Term]]
allSolutions [var] = do
        vals <- lookupDom var
        mapM (\val -> return [val]) (Set.toList vals)
allSolutions (var:vs) = do
        vals <- lookupDom var
        rs <- mapM (\val -> do
                       var `hasValue` val
                       sol <- allSolutions vs
                       update var vals
                       return $ map (val:) sol
                   ) (Set.toList vals)
        return $ concat rs
        -- return val



comp :: Cube -> [Term] -> Solving s [Result]
comp c shapes = do
  let dims = [1..(dim c)]
  trace $ show c
  sides0 <- mapM (\i -> do
                     cb <- getBoundaryC c i False 0
                     trace $ show i ++ " with boundary " ++ show cb
                     (filterM (\ s -> do
                                     sb <- getBoundary s (dim c) True
                                     trace $ show s ++ " has bound " ++ show sb ++ ": " ++ show (sb == cb)
                                     return $ sb == cb
                                     ) shapes) >>= newVar) dims
  sides1 <- mapM (\i -> do
                     cb <- getBoundaryC c i True 0
                     (filterM (\ s -> do
                                     sb <- getBoundary s (dim c) True
                                     return $ sb == cb
                                     ) shapes) >>= newVar) dims
  back <- newVar shapes

  trace "DOMAINS BEFORE CONSTRAINTS"
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! s) ++ "0: " ++ show d) sides0
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! (s - dim c)) ++ "1: " ++ show d) sides1

  mapM (\i -> boundaries i False (dim c) False back (sides0 !! (i-1))) dims
  mapM (\i -> boundaries i True (dim c) False back (sides1 !! (i-1))) dims

  trace "DOMAINS AFTER BACK CONSTRAINTS"
  lookupDom back >>= trace . show
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! s) ++ "0: " ++ show d) sides0
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! (s - dim c)) ++ "1: " ++ show d) sides1

  -- mapM (\e -> mapM (\e' -> mapM (\i -> mapM (\j ->
  --      boundaries (i + toInd e) e' (j - toInd e') e ((if e then sides1 else sides0) !! (i-1)) ((if e' then sides1 else sides0) !! (j-1)))
  --                                 [i + 1 .. dim c]) dims) [False , True]) [False , True]

  let k = 1
  let l = 2
  boundaries k False l False (sides0 !! (1-1)) (sides0 !! (2-1))
  boundaries l False l True (sides1 !! (1-1)) (sides0 !! (2-1))
  boundaries k True k False (sides0 !! (1-1)) (sides1 !! (2-1))
  boundaries l True k True (sides1 !! (1-1)) (sides1 !! (2-1))

  let m = 2
  let n = 3
  boundaries m False n False (sides0 !! (1-1)) (sides0 !! (3-1))
  boundaries n False n True (sides1 !! (1-1)) (sides0 !! (3-1))
  boundaries m True m False (sides0 !! (1-1)) (sides1 !! (3-1))
  boundaries n True m True (sides1 !! (1-1)) (sides1 !! (3-1))


  let p = 1
  let q = 3
  boundaries p False q False (sides0 !! (2-1)) (sides0 !! (3-1))
  boundaries q False q True (sides1 !! (2-1)) (sides0 !! (3-1))
  boundaries p True p False (sides0 !! (2-1)) (sides1 !! (3-1))
  boundaries q True p True (sides1 !! (2-1)) (sides1 !! (3-1))


  trace "DOMAINS AFTER SIDE CONSTRAINTS"
  lookupDom back >>= trace . show
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! s) ++ "0: " ++ show d) sides0
  mapM (\s -> lookupDom s >>= \d -> trace $ (dimAsString !! (s - dim c)) ++ "1: " ++ show d) sides1


  isAll <- gets allSol
  if False -- isAll
    then do
      res <- allSolutions (back : sides0 ++ sides1)
      return (map (\r -> Comp (r !! 0) (map (\i -> (r !! i , r !! (dim c + i))) dims)) res)
    else do
      r <- oneSolution (back : sides0 ++ sides1)
      return [Comp (r !! 0) (map (\i -> (r !! i , r !! (dim c + i))) dims)]





solve :: Solving s [Result]
solve = do
  trace "CONTEXT"
  context <- gets context
  mapM (\(name , u) -> trace $ name ++ " : " ++ show u) context
  mapM (\(name , u) -> wellFormedCube u) context

  trace "GOAL"
  goal' <- gets goal
  let goal = normalizeC goal'
  trace $ show goal
  wellFormedCube goal

  shapes <- do
    allshapes <- mapM (\ d -> fitin d (dim goal)) context
    return $ concat allshapes

  trace "SHAPES"
  mapM (\t -> infer t >>= (\ty -> trace $ (show t) ++ " : " ++ show ty)) shapes

  res <- filterM (`hasType` goal) shapes

  if res /= []
    then do
      trace "FOUND DIRECT SOLUTIONS"
      trace $ show res
      isAll <- gets allSol
      if isAll 
        then return $ map Dir res
        else return $ [Dir (res !! 0)]
    else do
      trace "NO DIRECT FIT FOUND, SEARCHING FOR HIGHER CUBES"
      hres <- comp goal shapes
      trace $ show hres
      return hres

