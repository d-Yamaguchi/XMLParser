# Nezcc

```haskell
data TreeLog a = TreeLog {lop :: Int,lpos :: Int,ltree :: a,lprev :: (Maybe (TreeLog a))} deriving (Eq)

data State a = State {ntag :: Int,cnt :: Int,value :: ByteString,sprev :: (Maybe (State a))} deriving (Eq)

data MemoEntry a = MemoEntry {key :: Int64,result :: Int,mpos :: Int,mtree :: a,mstate :: (Maybe (State a))} deriving (Eq)

data NezParserContext a = NezParserContext {inputs :: ByteString,length :: Int,pos :: Int,headpos :: Int,tree :: a,treeLog :: (Maybe (TreeLog a)),state :: (Maybe (State a)),memos :: Ar.Vector (MemoEntry a)} deriving (Eq)



```


## 変更点
* array にはvectorを使う
* rLogだけpxを引数で引き回していない -> 呼び出し元のendTでは状態を更新しているため，S.getを用いて状態を取れば良い．

## 問題点
* newtreeがない
* recT logT endT 周り


## 要望
