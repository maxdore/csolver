
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map ((!), Map)


import Data
import Core
import Examples


checkSolver :: Tele -> Cube -> Result -> IO ()
checkSolver ctxt goal t = do
  res <- runExceptT $ runStateT solve (SEnv ctxt goal 0 Map.empty False False)
  case res of
    Left err -> do
      putStrLn $ "FAIL!" ++ err
    Right ([r] , _)-> do
      -- TODO WHY IS show COMPARISON NECESSARY????
      putStrLn $ if (show r == show t) then "OK" else "FAIL! GOAL\n" ++ show goal ++ "\n" ++ show r ++ "\nIS PRESENTED, BUT SHOULD BE\n" ++ show t

checkInfer :: Tele -> Term -> Cube -> IO ()
checkInfer ctxt t phi = do
  res <- runExceptT $ runStateT (infer t) (mkSEnv ctxt Point False)
  case res of
    Left err -> do
      putStrLn $ "FAIL!" ++ err
    Right (psi , _)-> do
      -- TODO WHY IS show COMPARISON NECESSARY????
      putStrLn $ if (phi == psi) then "OK" else "FAIL! INFER\n" ++ show t ++ " \n" ++ show phi ++ "\nIS PRESENTED, BUT SHOULD BE\n" ++ show psi

main :: IO ()
main = do
  checkInfer sset2ExtCtxt
    (Abs (Abs (Abs (App (Face "f") [[1,2],[1,3]]))))
    (Path (Path (Path Point (Face "x") (App (Face "f") [[1],[2]])) (Abs (App (Face "f") [[1,2]])) (Face "f")) (Abs (Abs (App (Face "f") [[1,2]]))) (Abs (Face "f")))

  -- TODO
  checkInfer sset2Ctxt (Abs (Abs ((App (Face "phi")[[1,2]]))))
    (Path (Path (Path Point (App (Face "f") [[1,2]]) (App (Face "h") [[1,2]])) (Abs (Face "x")) (App (Face "phi") [[1]])) (Abs (Abs (Face "x"))) (Face "phi"))

  checkSolver intCtxt
    (Path (Path Point (Face "zero") (Face "one")) (Face "seg") (Face "seg"))
    (Dir (Abs (Abs (App (Face "seg") [[1]]))))

  checkSolver intCtxt
    (Path (Path Point (App (Face "seg") [[1]]) (App (Face "seg") [[1]])) (Abs (Face "zero")) (Abs (Face "one")))
    (Dir (Abs (Abs (App (Face "seg") [[2]]))))

  checkSolver intCtxt
    (Path (Path Point (Face "zero") (App (Face "seg") [[1]])) (Abs (Face "zero")) (Face "seg"))
    (Dir (Abs (Abs (App (Face "seg") [[1,2]]))))

  checkSolver intCtxt
    ((Path (Path Point (App (Face "seg") [[1]]) (Face "one")) (Face "seg")) (Abs (Face "one")))
    (Dir (Abs (Abs (App (Face "seg") [[1],[2]]))))

  checkSolver sqCtxt
    (Path (Path Point (App (Face "p") [[1]]) (App (App (Face "sq") [[1]]) [[1]])) (Abs (Face "a")) (Face "r"))
    (Dir (Abs (Abs (App (App (Face "sq") [[1,2]]) [[2]]))))

  checkSolver sqCtxt
    (Path (Path Point (App (App (Face "sq") [[1]]) [[1]]) (App (App (Face "sq") [[1]]) [[1]])) (Abs (Face "a")) (Abs (Face "d")))
    (Dir (Abs (Abs (App (App (Face "sq") [[2]]) [[2]]))))

  checkSolver sqCtxt
    (Path (Path (Path Point (App (Face "q") [[1]]) (App (Face "r") [[1]])) (Face "p") (Face "s")) (Face "sq") (Face "sq"))
    (Dir (Abs (Abs (Abs (App (App (Face "sq") [[2]]) [[1]])))))

  checkSolver ssetCtxt
    (Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))
    (Dir (Abs (Abs (App (App (Face "phi") [[2]]) [[1]]))))

  checkSolver ssetCtxt
    (Path (Path Point (App (App (Face "phi") [[1]]) [[1]]) (Face "z")) (Face "h") (Abs (Face "z")))
    (Dir (Abs (Abs (App (App (Face "phi") [[1],[2]]) [[2]]))))

  checkSolver ssetCtxt
    (Path (Path (Path Point (App (Face "h") [[1],[2]]) (App (Face "g") [[1],[2]]))
                            (Abs (App (App (Face "phi") [[2]]) [[1]])) (Abs (Face "z")))
                            (Face "phi") (Abs (Abs (Face "z"))))
    (Dir (Abs (Abs (Abs (App (App (Face "phi") [[2],[3]]) [[1]])))))


  -- Compositions

  checkSolver intCtxt
    (Path Point (Face "one") (Face "zero"))
    (Comp (Abs (Face "zero")) [(Abs (App (Face "seg") [[1]]),Abs (Face "zero"))])

  checkSolver twoCtxt
    (Path Point (Face "x") (Face "z"))
    (Comp (Abs (App (Face "p") [[1]])) [(Abs (Face "x"),Abs (App (Face "q") [[1]]))])

  checkSolver compCtxt
    (Path Point (Face "w") (Face "z"))
    (Comp (Abs (App (Face "q") [[1]])) [(Abs (App (Face "p") [[1]]),Abs (App (Face "r") [[1]]))])

  checkSolver fctCtxt
    (Path (Path Point (App (Face "f") [[1]]) (Face "z")) (Face "g") (Face "h"))
    (Comp (Abs (Abs (App (App (Face "alpha") [[1]]) [[2]]))) [(Abs (Abs (App (Face "g") [[1,2]])),Abs (Abs (App (Face "h") [[2]]))),(Abs (Abs (App (Face "f") [[2]])),Abs (Abs (App (Face "g") [[1],[2]])))])

  checkSolver sset2Ctxt
    (Path (Path Point (App (Face "h") [[1]]) (App (Face "g") [[1]])) (Face "f") (Abs (Face "z")))
    (Comp (Abs (Abs (App (App (Face "phi") [[1]]) [[2]]))) [(Abs (Abs (App (Face "f") [[2]])),Abs (Abs (App (Face "h") [[1],[2]]))),(Abs (Abs (App (Face "h") [[1,2]])),Abs (Abs (App (Face "g") [[2]])))])

  return ()
