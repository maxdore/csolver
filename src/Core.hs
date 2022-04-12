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
       }

mkSEnv ctxt goal = SEnv ctxt goal 0 Map.empty True

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
      let ndim = map (delete i) dim
      if [] `elem` ndim
        then do
          u <- infer r
          -- trace $ "EVAL" ++ show ndim
          -- trace $ show r ++ " : " ++ show u
          case u of (Path _ u v) -> return v
        else return $ App r ndim
        -- else return r
    else do
      let ndim = filter (\c -> not (i `elem` c)) dim
      if ndim == []
        then do
          u <- infer r
          case u of (Path _ u v) -> return u
        else return $ App r ndim
        -- else return r

apply :: Term -> Bool -> Solving s Term
apply (Abs t) = eval t (depth t)



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


infer :: Term -> Solving s Cube
infer (Face name) = lookupDef name
infer (Abs t) = do
  ty <- infer t
  let numvars = depth t + 1
  u <- eval t numvars False
  v <- eval t numvars True
  return $ Path ty u v
  -- return $ Path (decDim ty) u v
infer (App t i) = do
  ty <- infer t
  case ty of
    (Path c _ _) -> return $ substC c 1 i
    _ -> throwError "Applied non-path to interval variable"


hasType :: Term -> Cube -> Solving s Bool
hasType t d = infer t >>= (\c -> return $ c == d)


endpointsAgree :: Term -> Endpoint -> Term -> Endpoint -> Solving s Bool
endpointsAgree (Abs u) i (Abs v) j = do
  ui <- eval u (depth u + 1) i
  vi <- eval v (depth v + 1) j
  -- trace $ "ENDPOINTS: " ++ show u ++ " ON " ++ show i ++ " AND " ++ show v ++ " ON " ++ show j ++ " : " ++ show (ui) ++ " vs " ++ show vi
  return $ ui == vi

evalTy :: Cube -> Bool -> Solving s Cube
evalTy Point e = return Point
evalTy (Path c u v) e = do
  let numvars = depth u + 1
  eu <- eval u numvars e
  ev <- eval v numvars e
  return $ Path c eu ev

wellFormedCube :: Cube -> Solving s ()
wellFormedCube (Point) = return ()
wellFormedCube (Path c u v) = do
  c0 <- evalTy c False
  c1 <- evalTy c True
  ut <- infer u
  vt <- infer v
  if c0 /= ut
    then throwError $ "Boundary does not match: " ++ show ut ++ " is not " ++ show c0
  else if c1 /= vt
    then throwError $ "Boundary does not match: " ++ show vt ++ " is not " ++ show c1
    else return ()




absn :: Term -> Int -> Term
absn t 0 = t
absn t n = Abs (absn t (n - 1))

appn :: Term -> [Dim] -> Int -> [Term]
appn t is 0 = [t]
appn t is n = [ App u i | u <- appn t is (n-1) , i <- is]


-- For a given declaration generate all possible terms that could fit in a cube
fitin :: Decl -> Int -> Solving s [Term]
fitin (name,x) cdim = return $ map (`absn` cdim) (appn (Face name) (formulas [1 .. cdim]) (dim x))


getBoundary :: Term -> Int -> Endpoint -> Solving s Term
getBoundary t i e = do
  ty <- infer t
  getBoundaryC ty i e 0 >>= return . decUnbound

getBoundaryC :: Cube -> Int -> Endpoint -> Int -> Solving s Term
getBoundaryC (Path c u v) i e d = if i == 1
      then if e then return (absn v d) else return (absn u d)
      -- then if e then return (absn (decDimT v) d) else return (absn (decDimT u) d)
           -- then do
           --   trace $ show v
           --   infer v
           -- do
           --   trace $ show (absn u d)
           --   infer (absn u d)
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




comp :: Cube -> [Term] -> Solving s (Maybe Result)

comp c shapes = do
  let dims = [1..(dim c)]
  trace $ show c

  sides0 <- mapM (\i -> do
                     cb <- getBoundaryC c i False 0
                     -- trace $ show i ++ " with boundary " ++ show cb
                     (filterM (\ s -> do
                                     sb <- getBoundary s i True
                                     -- trace $ show s ++ ":\n" ++ show sb ++ "\n" ++ show cb ++ "\n" ++ show (sb == cb)
                                     return $ sb == cb
                                     ) shapes) >>= newVar) dims
  sides1 <- mapM (\i -> do
                     cb <- getBoundaryC c i True 0
                     -- trace $ show i ++ " with boundary " ++ show cb
                     (filterM (\ s -> do
                                     sb <- getBoundary s i True
                                     -- trace $ show s ++ ":\n" ++ show sb ++ "\n" ++ show cb ++ "\n" ++ show (sb == cb)
                                     return $ sb == cb
                                     ) shapes) >>= newVar) dims
  back <- newVar shapes

  -- mapM (\s -> lookupDom s >>= trace . show) sides0
  -- mapM (\s -> lookupDom s >>= trace . show) sides1
  -- lookupDom back >>= trace . show

  mapM (\i -> endpoints False False back (sides0 !! (i-1))) dims
  mapM (\i -> endpoints True False back (sides1 !! (i-1))) dims

  res <- labelling (back : sides0 ++ sides1)
  trace "RESULT"
  -- (trace . show) res
  return $ Just $ Comp (res !! 0) (map (\i -> (res !! i , res !! (i*2))) dims)

  -- return Nothing


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
  return Nothing


comp (Path (Path Point q r) p s) shapes = do
  -- pshapes <- filterM (\t -> infer t >>= (\ty -> case ty of
  --   Path (Path Point _ _) u _ -> return $ p == u
  --   _ -> return False)) shapes

  qty <- infer (Abs q)
  trace $ "Q: " ++ show (Abs q) ++ " : " ++ show qty
  qshapes <- filterM (\s -> do
    bound <- apply s False
    sty <- infer bound
    trace $ show bound ++ " : " ++ show sty
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


  return Nothing






solve :: Solving s Result
solve = do
  trace "CONTEXT"
  context <- gets context
  mapM (\(name , u) -> trace $ name ++ " : " ++ show u) context
  mapM (\(name , u) -> wellFormedCube u) context

  trace "GOAL"
  goal <- gets goal
  trace $ show goal
  wellFormedCube goal

  shapes <- do
    allshapes <- mapM (\ d -> fitin d (dim goal)) context
    return $ concat allshapes

  trace "SHAPES"
  mapM (\t -> infer t >>= (\ty -> trace $ (show t) ++ " : " ++ show ty)) shapes

  res <- filterM (`hasType` goal) shapes

  -- trace "DIRECT FIT"
  -- trace $ show res

  if res /= []
    then return $ Dir (res !! 0)
    else do
      trace "NO DIRECT FIT FOUND, SEARCHING FOR HIGHER CUBES"
      hres <- comp goal shapes
      case hres of
        Just com -> return com
        Nothing -> throwError "No solution found"
