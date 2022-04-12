module Lib where


ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

(||^) :: Monad m => m Bool -> m Bool -> m Bool
(||^) a b = ifM a (pure True) b

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = foldr ((||^) . p) (pure False)










