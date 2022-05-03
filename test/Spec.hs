
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map ((!), Map)


import Data
import Core
import Examples


checkSolver :: [Decl] -> Cube -> Result -> IO ()
checkSolver ctxt goal t = do
  res <- runExceptT $ runStateT solve (SEnv ctxt goal 0 Map.empty False)
  case res of
    Left err -> do
      putStrLn $ "FAIL!" ++ err
    Right (r , _)-> do
      -- TODO WHY IS show COMPARISON NECESSARY????
      putStrLn $ if (show r == show t) then "OK" else "FAIL! GOAL\n" ++ show goal ++ "\n" ++ show r ++ "\nIS PRESENTED, BUT SHOULD BE\n" ++ show t


main :: IO ()
main = do
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
    (Path (Path (Path Point (App (Face "h") [[1],[2]]) (App (Face "g") [[1],[2]]))
                            (Abs (App (App (Face "phi") [[2]]) [[1]])) (Abs (Face "z")))
                            (Face "phi") (Abs (Abs (Face "z"))))
    (Dir (Abs (Abs (Abs (App (App (Face "phi") [[2],[3]]) [[1]])))))


  -- Compositions

  checkSolver intCtxt
    (Path Point (Face "one") (Face "zero"))
    (Comp (Abs (Face "zero")) [(Abs (App (Face "seg") [[1]]),Abs (Face "zero"))])

  checkSolver compCtxt
    (Path Point (Face "w") (Face "z"))
    (Comp (Abs (App (Face "q") [[1]])) [(Abs (App (Face "p") [[1]]),Abs (App (Face "r") [[1]]))])

  checkSolver fctCtxt
    (Path (Path Point (App (Face "f") [[1]]) (Face "z")) (Face "g") (Face "h"))
    (Comp (Abs (Abs (App (App (Face "alpha") [[1]]) [[2]]))) [(Abs (Abs (App (Face "g") [[1,2]])),Abs (Abs (App (Face "h") [[2]]))),(Abs (Abs (App (Face "f") [[2]])),Abs (Abs (App (Face "g") [[1],[2]])))])


  return ()
