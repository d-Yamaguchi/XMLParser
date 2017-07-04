module XML(parseText) where
import Prelude hiding (length,log,take,drop)
import Data.Word
import qualified Data.ByteString.Char8 as C
import qualified Control.Monad.State as S
import Data.ByteString hiding (length,replicate)
import Data.Int
import Data.Maybe
import qualified Data.Vector as Ar
import qualified Data.IntMap as Map

(%) = mod

data AST = Node String SubTree
         | Leaf String String
         deriving (Eq,Show)

data SubTree = SubTree {sLabel :: String, sNode :: AST, sNext :: SubTree}
             | Notree
             deriving (Eq,Show)

memosize :: Int
memosize = 4
memoentries :: Int
memoentries = 257
charset173 :: Ar.Vector Bool
charset173 = Ar.fromList [False,False,False,False,False,False,False,False,False,True,True,False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
charset174 :: Ar.Vector Bool
charset174 = Ar.fromList [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
charset175 :: Ar.Vector Bool
charset175 = Ar.fromList [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
charset176 :: Ar.Vector Bool
charset176 = Ar.fromList [False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,True,True,False,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False,False]
choice177 :: Ar.Vector Word8
choice177 = Ar.fromList [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
charset178 :: Ar.Vector Bool
charset178 = Ar.fromList [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
charset179 :: Ar.Vector Bool
charset179 = Ar.fromList [False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,False,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
choice180 :: Ar.Vector Word8
choice180 = Ar.fromList [0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]
choice181 :: Ar.Vector Word8
choice181 = Ar.fromList [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2,0,0,0,0,0,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,2,0,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
nezsymbols :: Ar.Vector String
nezsymbols = Ar.fromList ["","key","Element","Name","Value","value","Attr","Text","CDATA","err"]
nezerror :: Int
nezerror = 9
nezvalues :: Ar.Vector ByteString
nezvalues = Ar.fromList [(C.pack "")]
nezvaluesizes :: Ar.Vector Int
nezvaluesizes = Ar.fromList [(0)]
data TreeLog = TreeLog {lop :: Int,lpos :: Int,ltree :: AST,lprev :: (Maybe (TreeLog))} deriving (Eq,Show)

data State = State {ntag :: Int,cnt :: Int,value :: ByteString,sprev :: (Maybe (State))} deriving (Eq,Show)

data MemoEntry = MemoEntry {key :: Int64,result :: Int,mpos :: Int,mtree :: AST,mstate :: (Maybe (State))} deriving (Eq,Show)

data NezParserContext = NezParserContext {inputs :: ByteString,length :: Int,pos :: Int,headpos :: Int,tree :: AST,treeLog :: (Maybe (TreeLog)),state :: (Maybe (State)),memos :: Ar.Vector (MemoEntry)} deriving (Eq,Show)

(<&&>) :: NezParserContext -> [S.State NezParserContext Bool] -> S.State NezParserContext Bool
(<&&>) s [] = S.put s >> return True
(<&&>) s (x:xs) = let (a1,s1) = S.runState x s in
                    if a1
                      then s1 <&&> xs
                      else S.put s1 >> return False

(<||>) :: NezParserContext -> [S.State NezParserContext Bool] -> S.State NezParserContext Bool
(<||>) s [] = S.put s >> return False
(<||>) s (x:xs) = let (a1,s1) = S.runState x s in
                    if a1
                      then S.put s1 >> return True
                      else s1 <||> xs

(<?>) :: (S.State NezParserContext Bool,NezParserContext) -> (S.State NezParserContext a,S.State NezParserContext a)  -> S.State NezParserContext a
(<?>) (cond,s) (e1,e2) = let
  (a1,c1) = S.runState cond s
  in if a1
    then S.put c1 >> e1
    else S.put c1 >> e2

nextbyte :: ((NezParserContext) -> S.State NezParserContext (Int))
nextbyte fpx' = do
  px' <- S.get
  let c' = (fromIntegral (toInteger  (((inputs px') `index` (pos px')))))
  S.put $ px' {pos = (pos px') + (1)}
  return (c')

next1 :: ((NezParserContext)->(Int) -> S.State NezParserContext (Bool))
next1 fpx' c' = do
  px' <- S.get
  let (v,s) = S.runState (nextbyte px') px'
  S.put s >> return (v == c')

backpos :: ((NezParserContext)->(Int) -> S.State (NezParserContext) Int)
backpos fpx' pos' = do
  px' <- S.get
  if ((headpos px') < pos') then
    S.put (px' {headpos = pos'}) >> return pos'
    else return (pos')

back1 :: ((NezParserContext)->(Int) -> S.State NezParserContext (Bool))
back1 fpx' pos' = do
  px' <- S.get
  let (v,s) = S.runState (backpos px' pos') px'
  S.put $ s {pos = v}
  return (True)

not1 :: ((NezParserContext)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
not1 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  (f' px', px') <?> (return False,back1 px' pos')

neof :: ((NezParserContext) -> S.State NezParserContext (Bool))
neof fpx' = do
  px' <- S.get
  return ((pos px') < (length px'))

movep :: ((NezParserContext)->(Int) -> S.State NezParserContext (Bool))
movep fpx' shift' = do
  px' <- S.get
  S.put $ px' {pos = (pos px') + shift'}
  return (True)

many1 :: ((NezParserContext)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
many1 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  (f' px',px') <?> (many1 px' f',back1 px' pos')

option1 :: ((NezParserContext)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
option1 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  px' <||> [f' px', back1 px' pos']

logT :: ((NezParserContext)->(Int)->(Int)->(AST) -> S.State NezParserContext (Bool))
logT fpx' lop' lpos' tree' = do
  px' <- S.get
  let treeLog' = (Just (TreeLog {lop=lop',lpos=lpos',ltree=tree',lprev=(treeLog px')}))
  S.put $ px' {treeLog = treeLog'}
  return (True)

linkT :: ((NezParserContext)->(Int)->(AST) -> S.State NezParserContext (Bool))
linkT fpx' nlabel' tree' = do
  px' <- S.get
  (logT px' (3) nlabel' tree')

backLink :: ((NezParserContext)->((Maybe (TreeLog)))->(Int)->(AST) -> S.State NezParserContext (Bool))
backLink fpx' treeLog' nlabel' tree' = do
  px' <- S.get
  let ltree' = (tree px')
  S.put $ px' {treeLog = treeLog', tree = tree'}
  (linkT px' nlabel' ltree')

link2 :: ((NezParserContext)->(Int)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
link2 fpx' nlabel' f' = do
  px' <- S.get
  let treeLog' = (treeLog px')
  let tree' = (tree px')
  px' <&&> [f' px', backLink px' treeLog' nlabel' tree']

back3 :: ((NezParserContext)->(Int)->((Maybe (TreeLog)))->(AST) -> S.State NezParserContext (Bool))
back3 fpx' pos' treeLog' tree' = do
  px' <- S.get
  S.put $ px' {pos = pos', treeLog = treeLog', tree = tree'}
  return (True)

many3 :: ((NezParserContext)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
many3 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  let treeLog' = (treeLog px')
  let tree' = (tree px')
  (f' px', px') <?> ( many3 px' f', back3 px' pos' treeLog' tree')

tagT :: ((NezParserContext)->(Int) -> S.State NezParserContext (Bool))
tagT fpx' ntag' = do
  px' <- S.get
  (logT px' (1) ntag' (tree px'))

gettag :: ((Int) -> S.State (NezParserContext) (String))
gettag ntag' = do
  return ((nezsymbols Ar.! ntag'))

getlabel :: ((Int) -> S.State (NezParserContext) (String))
getlabel nlabel' = do
  return ((nezsymbols Ar.! nlabel'))

getval :: ((Int) -> S.State (NezParserContext) (ByteString))
getval nvalue' = do
  return ((nezvalues Ar.! nvalue'))

getvalen :: ((Int) -> S.State (NezParserContext) (Int))
getvalen nvalue' = do
  return ((nezvaluesizes Ar.! nvalue'))

--recT :: ((NezParserContext)->(TreeLog)->(Int)->(Int)->(Int)->(Ar.Vector AST) -> S.State (NezParserContext) AST)
--recT fpx' tcur' ntag' nvalue' epos' subtrees' = do
--  px' <- S.get --no put function is called so, i didin't fix multiple evalstates.
--  return ((if ((lop tcur') == (0)) then ((if (nvalue' == (0)) then (newtree (S.evalState (gettag ntag') px') (inputs px') (lpos tcur') epos' subtrees') else (newtree (S.evalState (gettag ntag') px') (S.evalState (getval nvalue') px') (0) (S.evalState (getvalen nvalue') px') subtrees'))) else ((S.evalState (recT px' (fromJust ((lprev tcur'))) (if (ntag' == (0) && (lop tcur') == (1)) then ((lpos tcur')) else (ntag')) (if (nvalue' == (0) && (lop tcur') == (2)) then ((lpos tcur')) else (nvalue')) epos' (if ((lop tcur') == (3)) then (((Tree ((S.evalState (getlabel (lpos tcur')) px')) (Ar.singleton (ltree tcur'))) `Ar.cons` subtrees')) else (subtrees'))) px'))))

recT :: ((NezParserContext)->(TreeLog)->(Int)->(Int)->(Int)->(SubTree) -> S.State (NezParserContext) AST)
recT fpx' tcur' ntag' nvalue' epos' subtrees' = do
  px' <- S.get
  if lop tcur' == 0
    then if nvalue' == 0
      then return $ newtree (nezsymbols Ar.! ntag') (inputs px') (lpos tcur') epos' subtrees'
      else return $ newtree (nezsymbols Ar.! ntag') (nezvalues Ar.! nvalue') 0 (nezvaluesizes Ar.! nvalue') subtrees'
    else recT px' (fromJust (lprev tcur')) (if (ntag' == (0) && (lop tcur') == (1)) then ((lpos tcur')) else (ntag')) (if (nvalue' == (0) && (lop tcur') == (2)) then ((lpos tcur')) else (nvalue')) epos' (if ((lop tcur') == (3)) then SubTree (nezsymbols Ar.! (lpos tcur')) (ltree tcur') subtrees' else subtrees')

rLog :: ((TreeLog) -> S.State (NezParserContext) ((Maybe (TreeLog))))
rLog tcur' = do
  px' <- S.get
  (return (lop tcur' == 0),px') <?> (return (lprev tcur'),rLog (fromJust (lprev tcur')))

endT :: ((NezParserContext)->(Int)->(Int) -> S.State (NezParserContext) (Bool))
endT fpx' shift' ntag' = do
  px' <- S.get
  let (nTree,s1) = S.runState (recT px' (fromJust ((treeLog px'))) ntag' (0) ((pos px') + shift') Notree) px'
  let (nTreeLog,s2) = S.runState (rLog (fromJust ((treeLog px')))) s1
  S.put $ s2 {tree = nTree, treeLog = nTreeLog}
  return (True)

beginT :: ((NezParserContext)->(Int) -> S.State NezParserContext (Bool))
beginT fpx' shift' = do
  px' <- S.get
  (logT px' (0) ((pos px') + shift') (tree px'))

longkey :: ((Int64)->(Int) -> S.State NezParserContext (Int64))
longkey key' mpoint' = do
    px' <- S.get
    return ((key' * 64 + (fromIntegral (mpoint'))))

getMemo :: ((NezParserContext)->(Int64) -> S.State NezParserContext (MemoEntry))
getMemo fpx' key' = do
  px' <- S.get
  return (((memos px') Ar.! (fromIntegral (key' % 257))))

consumeM2 :: ((NezParserContext)->(MemoEntry) -> S.State NezParserContext (Int))
consumeM2 fpx' m' = do
  px' <- S.get
  S.put $ px' {pos = (mpos m'), tree = (mtree m')}
  return ((result m'))

lookupM2 :: ((NezParserContext)->(Int) -> S.State NezParserContext (Int))
lookupM2 fpx' mpoint' = do
  px' <- S.get
  let (key',s1) = (S.runState (longkey (fromIntegral ((pos px'))) mpoint') px')
  let (m',s2)  = (S.runState (getMemo px' key') s1)
  S.put s2
  if key m' == key' then (consumeM2 px' m') else return 2

storeM :: ((NezParserContext)->(Int)->(Int)->(Bool) -> S.State (NezParserContext) Bool)
storeM fpx' memoPoint' pos' matched' = do
  px' <- S.get
  let (key',s1) = (S.runState (longkey (fromIntegral pos') memoPoint') px')
  let (m', s2) = (S.runState (getMemo px' key') s1)
  let oldmemo = memos s2
  S.put $ s2 {memos = rewriteList oldmemo (key' % 257) (m' {key = key', result = if matched' then 1 else 0, mpos = if matched' then pos s2 else pos', mtree = tree s2, mstate = (state s2)})}
  return (matched')
  where
    rewriteList xs i v = let k = fromIntegral i in Ar.update xs (Ar.singleton (k,v))

memo2 :: ((NezParserContext)->(Int)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
memo2 fpx' mpoint' f' = do
  px' <- S.get
  let pos' = (pos px')
  let (cond,s0) = ((S.runState (lookupM2 px' mpoint') px'))
  case (cond) of
    1 -> S.put s0 >> return (True)
    2 -> let (v,s1) = S.runState (f' px') s0 in S.put s1 >> storeM px' mpoint' pos' v
    _ -> S.put s0 >> return (False)

getbyte :: ((NezParserContext) -> S.State NezParserContext (Int))
getbyte fpx' = do
  px' <- S.get
  return ((fromIntegral (toInteger  (((inputs px') `index` (pos px'))))))

many9 :: ((Int)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
many9 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  (f' px',px') <?> (many9 (succ fpx') f', back1 px' pos')

back7 :: ((NezParserContext)->(Int)->((Maybe (TreeLog)))->(AST)->((Maybe (State))) -> S.State NezParserContext (Bool))
back7 fpx' pos' treeLog' tree' state' = do
  px' <- S.get
  S.put $ px' {pos = pos', treeLog = treeLog', tree = tree', state = state'}
  return (True)

option7 :: ((NezParserContext)->(((NezParserContext) -> S.State NezParserContext (Bool))) -> S.State NezParserContext (Bool))
option7 fpx' f' = do
  px' <- S.get
  let pos' = (pos px')
  let treeLog' = (treeLog px')
  let tree' = (tree px')
  let state' = (state px')
  px' <||> [f' px', back7 px' pos' treeLog' tree' state']

-- !'<![CDATA['
e24 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e24 fpx' = do
  px' <- S.get
  not1 px' (\p0'' -> do {p0' <- S.get ; p0' <&&> [next1 p0' 60,next1 p0' 33,next1 p0' 91,next1 p0' 67,next1 p0' 68,next1 p0' 65,next1 p0' 84,next1 p0' 65,next1 p0' 91]})

-- !']]>'
e23 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e23 fpx' = do
  px' <- S.get
  not1 px' (\p0'' -> do {p0' <- S.get ; p0' <&&> [next1 p0' 93,next1 p0' 93,next1 p0' 62]})

-- ('<![CDATA[' xml:CDATA ']]>' xml:CDATA)?
e26 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e26 fpx' = do
  px' <- S.get
  option7 px' (\p0'' -> do {p0' <- S.get ; p0' <&&> [next1 p0' 60,next1 p0' 33,next1 p0' 91,next1 p0' 67,next1 p0' 68, next1 p0' 65,next1 p0' 84,next1 p0' 65, next1 p0' 91,xml_CDATA p0',next1 p0' 93,next1 p0' 93,next1 p0' 62,xml_CDATA p0']})

-- (!']]>' !'<![CDATA[' .)*
e25 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e25 fpx' = do
  px' <- S.get
  many1 px' (\p2'' -> do {p2' <- S.get ; p2' <&&> [e23 p2',e24 p2',neof p2',movep p2' 1] })

-- (!']]>' !'<![CDATA[' .)* ('<![CDATA[' xml:CDATA ']]>' xml:CDATA)?
xml_CDATA :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_CDATA fpx' = do
  px' <- S.get
  px' <&&> [e25 px',e26 px']

-- [\t-\n\r ]
xml_S :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_S fpx' = do
  px' <- S.get
  let (v,s) = S.runState (nextbyte px') px'
  S.put s
  return (charset173 Ar.! v)

-- xml:S*
e22 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e22 fpx' = do
  px' <- S.get
  many1 px' xml_S

-- [\---.0-:A-Z_a-z]*
e11 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e11 fpx' = do
  px' <- S.get
  many1 px' (\p0'' -> do {p0' <- S.get ; let (v1,p1') = S.runState (nextbyte p0') p0' in S.put p1' >> return (charset176 Ar.! v1) } )

-- [\x01-;=-\xff]+
e20 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e20 fpx' = do
  px' <- S.get
  many9 0 (\p0'' -> do {p0' <- S.get ; let (v1,p1') = S.runState (nextbyte p0') p0' in S.put p1' >> return (charset179 Ar.! v1)})

-- {[:A-Z_a-z] [\---.0-:A-Z_a-z]* #Name }
xml_Name :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_Name fpx' = do
  px' <- S.get
  memo2 px' 2 (\p1'' -> do {p1' <- S.get ; p1' <&&> [beginT p1' (0), S.get >>= \s -> let (v,p) = (S.runState (nextbyte s) s) in S.put p >> return (charset175 Ar.! v), e11 p1', (endT p1' (0) (3))]})

-- $key(xml:Name)
e7 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e7 fpx' = do
  px' <- S.get
  link2 px' 1 xml_Name

-- [\x01-!#-\xff]*
e14 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e14 fpx' = do
  px' <- S.get
  many1 px' (\p0'' -> do {p0' <- S.get ; let (v,s) = (S.runState (nextbyte p0') p0') in S.put s >> return ((charset178 Ar.! v))})

-- $value(('"' {[\x01-!#-\xff]* '"' #Value }))
e15 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e15 fpx' = do
  px' <- S.get
  link2 px' 5 (\px'' -> do {p1' <- S.get; p1' <&&> [next1 p1' 34, beginT p1' 0, e14 p1', next1 p1' 34, endT p1' (-1) 4]})

-- [\t-\n\r ]*
e3 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e3 fpx' = do
  px' <- S.get
  many1 px' (\p0'' -> do {p0' <- S.get; let (v,s) = S.runState (nextbyte p0') p0' in S.put s >> return (charset173 Ar.! v)})

-- {$key(xml:Name) [\t-\n\r ]* '=' [\t-\n\r ]* $value(('"' {[\x01-!#-\xff]* '"' #Value })) #Attr } [\t-\n\r ]*
xml_Attribute :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_Attribute fpx' = do
  px' <- S.get
  memo2 px' 3 (\p5'' -> do{p5' <- S.get; p5' <&&> [beginT p5' 0,e7 p5', e3 p5',next1 p5' 61, e3 p5', e15 p5', tagT p5' 6,endT p5' 0 0,e3 p5' ]})

-- $(xml:Attribute)
e8 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e8 fpx' = do
  px' <- S.get
  link2 px' (0) xml_Attribute

-- ($(xml:Attribute))*
e9 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e9 fpx' = do
  px' <- S.get
  many3 px' e8

-- <switch '!'->'![CDATA[' {xml:CDATA #CDATA ']]>' } xml:S* [:A-Z_a-z]->{$key(xml:Name) [\t-\n\r ]* ($(xml:Attribute))* <switch '/'->'/>' '>'->'>' [\t-\n\r ]* ($value(xml:Content) / xml:COMMENT)* '</' [:A-Z_a-z] [\---.0-:A-Z_a-z]* '>'> #Element } [\t-\n\r ]*>
e21 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e21 fpx' = do
  px' <- S.get
  let (v,s) = S.runState (getbyte px') px'
  case (choice181 Ar.! v) of
    1 -> s <&&> [movep px' 1,next1 px' 91,next1 px' 67,next1 px' 68, next1 px' 65,next1 px' 84,next1 px' 65, next1 px' 91, beginT px' 0, xml_CDATA px',tagT px' (8),next1 px' 93, next1 px' 93,next1 px' 62,endT px' (-3) 0, e22 px']
    2 -> s <&&> [beginT px' (-1), e7 px', e3 px', e9 px',e10 px', tagT px' 2, endT px' 0 0, e3 px']
    _ -> S.put s >> return (False)


-- <switch [\x01-;=-\xff]->{[\x01-;=-\xff]+ #Text } '<'->. <switch '!'->'![CDATA[' {xml:CDATA #CDATA ']]>' } xml:S* [:A-Z_a-z]->{$key(xml:Name) [\t-\n\r ]* ($(xml:Attribute))* <switch '/'->'/>' '>'->'>' [\t-\n\r ]* ($value(xml:Content) / xml:COMMENT)* '</' [:A-Z_a-z] [\---.0-:A-Z_a-z]* '>'> #Element } [\t-\n\r ]*>>
e17 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e17 fpx' = do
  px' <- S.get
  let (v,s) = S.runState (getbyte px') px'
  case (choice180 Ar.! v)  of
    1 -> s <&&> [beginT px' 0, e20 px', endT px' 0 7]
    2 -> s <&&> [movep px' 1, e21 px']
    _ -> S.put s >> return (False)


-- <switch [\x01-;=-\xff]->{[\x01-;=-\xff]+ #Text } '<'->. <switch '!'->'![CDATA[' {xml:CDATA #CDATA ']]>' } xml:S* [:A-Z_a-z]->{$key(xml:Name) [\t-\n\r ]* ($(xml:Attribute))* <switch '/'->'/>' '>'->'>' [\t-\n\r ]* ($value(xml:Content) / xml:COMMENT)* '</' [:A-Z_a-z] [\---.0-:A-Z_a-z]* '>'> #Element } [\t-\n\r ]*>>
xml_Content :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_Content fpx' = do
  px' <- S.get
  memo2 px' (4) e17

-- $value(xml:Content)
e16 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e16 fpx' = do
  px' <- S.get
  link2 px' (5) xml_Content

-- !'-->'
e18 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e18 fpx' = do
  px' <- S.get
  not1 px' (\p0'' -> do {p0' <- S.get ; p0' <&&> [next1 p0' 45,next1 p0' 45,next1 p0' 62]} )

-- (!'-->' .)*
e19 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e19 fpx' = do
  px' <- S.get
  many1 px' (\p1'' -> do{p1' <- S.get; p1' <&&> [e18 p1',neof p1',movep p1' 1]})

-- '<!--' (!'-->' .)* '-->' [\t-\n\r ]*
xml_COMMENT :: ((NezParserContext) -> S.State NezParserContext (Bool))
xml_COMMENT fpx' = do
  px' <- S.get
  px' <&&> [next1 px' 60, next1 px' 33, next1 px' 45, next1 px' 45, e19 px', next1 px' 45, next1 px' 45, next1 px' 62, e3 px']

-- $value(xml:Content) / xml:COMMENT
e12 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e12 fpx' = do
  px' <- S.get
  let pos' = (pos px')
  let treeLog' = (treeLog px')
  let tree' = (tree px')
  let (v,s) = S.runState (e16 px') px'
  S.put s
  if v
    then return True
    else s <&&> [back3 px' pos' treeLog' tree', xml_COMMENT px']

-- ($value(xml:Content) / xml:COMMENT)*
e13 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e13 fpx' = do
  px' <- S.get
  (many3 px' e12)

-- <switch '/'->'/>' '>'->'>' [\t-\n\r ]* ($value(xml:Content) / xml:COMMENT)* '</' [:A-Z_a-z] [\---.0-:A-Z_a-z]* '>'>
e10 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e10 fpx' = do
  px' <- S.get
  let (v,s) = S.runState (nextbyte px') px'
  case ((choice177 Ar.! v)) of
    1 -> S.put s >> (next1 px' 62)
    2 -> s <&&> [e3 px',e13 px',next1 px' 60,next1 px' 47,S.get >>= (\x -> let (v1,s1) = (S.runState (nextbyte x) x) in S.put s1 >> return (charset175 Ar.! v1)), e11 px', next1 px' 62]
    _ -> S.put s >> return (False)


-- [\x01-=?-\xff]*
e5 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e5 fpx' = do
  px' <- S.get
  many1 px' (\p0'' -> do {p0' <- S.get ; let (v,s) = S.runState (nextbyte p0') p0' in S.put s >> return ((charset174 Ar.! v))})

-- ('<!' [\x01-=?-\xff]* '>' [\t-\n\r ]*)?
e6 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e6 fpx' = do
  px' <- S.get
  option1 px' (\p2'' -> do {p2' <- S.get ; p2' <&&> [next1 p2' 60, next1 p2' 33, e5 p2', next1 p2' 62,e3 p2']})

-- !'?>'
e1 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e1 fpx' = do
  px' <- S.get
  not1 px' (\p0'' -> do {p0' <- S.get ; p0' <&&> [next1 p0' 63,next1 p0' 62]})

-- (!'?>' .)*
e2 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e2 fpx' = do
  px' <- S.get
  many1 px' (\p1'' -> do {p1' <- S.get ; p1' <&&> [(e1 p1'),(neof p1'),(movep p1' (1))]})

-- ('<?xml' (!'?>' .)* '?>' [\t-\n\r ]*)?
e4 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e4 fpx' = do
  px' <- S.get
  option1 px' (\p3'' -> do {p3' <- S.get ; p3' <&&> [(next1 p3' 60),(next1 p3' 63),(next1 p3' 120),(next1 p3' 109),(next1 p3' 108), (e2 p3') ,(next1 p3' 63),(next1 p3' 62),(e3 p3') ]})

-- ('<?xml' (!'?>' .)* '?>' [\t-\n\r ]*)? ('<!' [\x01-=?-\xff]* '>' [\t-\n\r ]*)? '<' {$key(xml:Name) [\t-\n\r ]* ($(xml:Attribute))* <switch '/'->'/>' '>'->'>' [\t-\n\r ]* ($value(xml:Content) / xml:COMMENT)* '</' [:A-Z_a-z] [\---.0-:A-Z_a-z]* '>'> #Element } [\t-\n\r ]*
e0 :: ((NezParserContext) -> S.State NezParserContext (Bool))
e0 fpx' = do
  px' <- S.get
  px' <&&> [e4 px', e6 px',next1 px' 60, beginT px' (-1), e7 px',e3 px',e9 px', e10 px', tagT px' 2, endT px' 0 0,e3 px']

parse :: (ByteString)->(Int) -> AST
parse inputs' length' = let tree' = newtree (Ar.head nezsymbols) inputs' 0 length' Notree in
                        let px' = NezParserContext {inputs=inputs'
                             ,length=length'
                             ,pos=0
                             ,headpos=0
                             ,tree=tree'
                             ,treeLog=(Just (TreeLog {lop=0,lpos=0,ltree=tree',lprev=Nothing}))
                             ,state=Nothing
                             ,memos=(newMemos tree' 257)} in
                        let result = S.runState (e0 px') px' in
                        if Prelude.fst result then tree (Prelude.snd result) else newtree (Prelude.show nezerror) inputs' (headpos (Prelude.snd result)) length' Notree

parseText :: String -> AST
parseText text' = let inputs' = (C.pack text') `C.snoc` '\NUL' in
                  let length' = C.length inputs' - 1 in
                  parse inputs' length'

newtree :: String -> ByteString -> Int -> Int -> SubTree -> AST
newtree tag inputs pos epos subs = if subs == Notree
  then Leaf tag (C.unpack $ (C.drop pos (C.take epos inputs)))
  else Node tag subs

newMemos tree' length' = Ar.replicate length' MemoEntry { key = -1, result = 0, mpos = 0, mtree = tree', mstate = Nothing}

testdata = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><note><to>Tove</to><from>Jani</from><heading>Reminder</heading><body>Don't forget me this weekend!</body></note>"
