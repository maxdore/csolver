module Examples where

import qualified Data.Map as Map
import Data.Map ((!), Map)


import Core
import Data


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
app1goal = mkSEnv intCtxt (Path (Path Point (Face "zero") (Face "one")) (Face "seg") (Face "seg"))

-- SOL \.\. App 2 seg
app2goal = SEnv intCtxt (Path (Path Point (App (Face "seg") [[1]]) (App (Face "seg") [[1]])) (Abs (Face "zero")) (Abs (Face "one")))

-- SOL \.\. App (And 1 2) seg
andGoal = SEnv intCtxt (Path (Path Point (Face "zero") (App (Face "seg") [[1]])) (Abs (Face "zero")) (Face "seg"))

-- SOL \.\. App (Or 1 2) seg
orGoal = SEnv intCtxt ((Path (Path Point (App (Face "seg") [[1]]) (Face "one")) (Face "seg")) (Abs (Face "one")))

-- SOL \. App 1 seg
trivGoal = mkSEnv intCtxt (Path Point (Face "zero") (Face "one"))

-- SOL hcomp
invGoal = mkSEnv intCtxt (Path Point (Face "one") (Face "zero"))


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

-- sqapp13 : PathP (λ i → PathP (λ j → PathP (λ k → Sq) (p i) (s i)) (λ k → sq k i) λ k → sq k i) (λ _ → q) λ _ → r
lllsq13 = Abs (Abs (Abs (App (App (Face "sq") [[1]]) [[3]])))

-- Path (λ i → Path (λ j → Path Point (p (i ∨ j)) d) (λ k → sq k (i ∨ k)) r) (λ j k → sq k (j ∨ k)) λ _ → r
-- Path (Path (Path Point p<[[2],[3]]> d) \1.sq<[[1]]><[[1],[3]]> \1.r<[[1]]>) \2.\1.sq<[[1]]><[[1],[2]]> \2.\1.r<[[1]]>
lllsqor = (Abs (Abs (Abs (App (App (Face "sq") [[1]]) [[1],[2],[3]]))))

-- SOL \2.\1. sq (2 ∧ 1) 2
sqands = mkSEnv sqCtxt (Path (Path Point (App (Face "p") [[1]]) (App (App (Face "sq") [[1]]) [[1]])) (Abs (Face "a")) (Face "r"))

-- SOL \2.\1.sq<2><2>
sq22 = mkSEnv sqCtxt (Path (Path Point
                          (App (App (Face "sq") [[1]]) [[1]])
                          (App (App (Face "sq") [[1]]) [[1]]))
                     (Abs (Face "a"))
                     (Abs (Face "d")))

-- SOL \.\.\. sq<2><1>
sq21 = mkSEnv sqCtxt (Path (Path (Path Point (App (Face "q") [[1]]) (App (Face "r") [[1]])) (Face "p") (Face "s")) (Face "sq") (Face "sq"))
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
  -- , ("symh" ,      Path Point (Face "z") (Face "x"))
  , ("phi" , Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))
  -- , ("flipphi" , Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))
           ]

phi = mkSEnv ssetCtxt (Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))

-- SOL \2.\1.phi (1 ∨ 2) 2
phior = mkSEnv ssetCtxt (Path (Path Point (App (App (Face "phi") [[1]]) [[1]]) (Face "z")) (Face "h") (Abs (Face "z")))

-- SOL \2.\1. phi (1 ∨ 2) etaequiv \3.\2.\1. phi (2 ∨ 3) 1
phipart = mkSEnv ssetCtxt (Path
                            (Path
                              (Path Point
                               (App (Face "h") [[1],[2]])
                               (App (Face "g") [[1],[2]]))
                             (Abs (App (App (Face "phi") [[2]]) [[1]]))
                             (Abs (Face "z")))
                           (Face "phi")
                           (Abs (Abs (Face "z"))))

    -- test4 : PathP (λ i → PathP (λ j → Path Δ∣ X ∣ (edge h (i ∨ j)) (edge g (i ∨ j))) (λ j → triangle ϕ i j) λ _ → vert z) (triangle ϕ) λ _ _ → vert z
    -- test4 = λ i j → triangle ϕ (i ∨ j)

lowerT = mkSEnv ssetCtxt (Path (Path Point (App (Face "f") [[1]]) (App (Face "h") [[1]])) (Abs (Face "x")) (Face "g"))


fctCtxt :: Tele
fctCtxt = [
    ("x" , Point)
  , ("y" , Point)
  , ("z" , Point)
  , ("f" , Path Point (Face "x") (Face "y"))
  , ("g" , Path Point (Face "x") (Face "z"))
  , ("h" , Path Point (Face "y") (Face "z"))
  , ("alpha" , Path (Path Point (Face "x") (App (Face "h") [[1]])) (Face "f") (Face "g"))
               ]

coh2 = mkSEnv fctCtxt (Path (Path Point (App (Face "f") [[1]]) (Face "z")) (Face "g") (Face "h"))
-- PathP (λ i → link x y i ≡ f z) (link x z) (link y z)




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

compGoal = mkSEnv compCtxt (Path Point (Face "w") (Face "z"))


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
