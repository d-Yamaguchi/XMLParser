module Main where
import System.IO
import qualified Control.Monad.State as S
import qualified XML as X

main :: IO ()
main = readFile "./xmark5m.xml" >>= print . X.parseText

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

(<&&>) :: Integer -> [S.State Integer Bool] -> S.State Integer Bool
(<&&>) s [] = S.put s >> return True
(<&&>) s (x:xs) = let (a1,s1) = S.runState x s in
                    if a1
                      then s1 <&&> xs
                      else S.put s1 >> return False

(<||>) :: Integer -> [S.State Integer Bool] -> S.State Integer Bool
(<||>) s [] = S.put s >> return False
(<||>) s (x:xs) = let (a1,s1) = S.runState x s in
                    if a1
                      then S.put s1 >> return True
                      else s1 <||> xs

increment :: S.State Integer Bool
increment = do
  px <- S.get
  S.put $ succ px
  return True

runInc :: S.State Integer Bool
runInc = S.get >>= \x -> x <&&> replicate 5 increment

(<?>) :: (S.State Integer Bool,Integer) -> (S.State Integer a,S.State Integer a)  -> S.State Integer a
(<?>) (cond,s) (e1,e2) = let
  (a1,c1) = S.runState cond s
  in if a1
    then S.put c1 >> e1
    else S.put c1 >> e2

many3 :: (((Integer) -> S.State Integer (Bool))) -> S.State Integer (Bool)
many3 f' = do
      px' <- S.get
      (f' px', px') <?> ( many3 f', return False)

e2 :: Integer -> S.State Integer Bool
e2 fpx = do
  px <- S.get
  S.put $ succ px
  if px == 15
    then return False
    else return True

memo2 :: (Integer)->(((Integer) -> S.State Integer (Bool))) -> S.State Integer (Bool)
memo2 mpoint' f' = do
      px' <- S.get
      let (cond,s0) = ((S.runState (lookupM2 mpoint') px'))
      case (cond) of
        1 -> S.put s0 >> return (True)
        2 -> let (v,s1) = S.runState (f' px') s0 in S.put s1 >> e2 0
        _ -> S.put s0 >> return (False)

lookupM2:: Integer -> S.State Integer Integer
lookupM2 m = do
  px <- S.get
  S.put $ px * 2
  return $ px `mod` 3
