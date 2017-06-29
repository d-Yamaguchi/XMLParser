{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import System.IO
import qualified XML as Tst
import qualified Data.STRef as Sr
import qualified Control.Monad.State as S
import Control.Applicative

main :: IO ()
main = print $ Tst.parseText testdate

{-}
charset180 = [True,False]

e10 :: ((Int) -> S.State Int (Bool))
e10 fpx' = do
  px' <- S.get
  return $ S.evalState (many9 0 (\p0'' -> do {p0' <- S.get ; S.put (p0' + 1) ; return (charset180 !!  p0')})) px'

many9 :: ((Int)->(((Int) -> S.State Int (Bool))) -> S.State Int (Bool))
many9 fpx' f' = do
    px' <- S.get
    let (cond, nexste) = (S.runState (f' px') px')
    return ((if (cond) then ((S.evalState (many9 (succ fpx') f') nexste)) else (fpx' > (0) )))

e0 = S.evalState (e10 0) 0
-}

testdate = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Don't forget me this weekend!</body></note>"
