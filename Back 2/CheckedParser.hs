{-# LANGUAGE TypeFamilies, GADTs, FlexibleInstances, MultiParamTypeClasses, EmptyDataDecls #-}
module CheckedParser where

import ParserCore

data Symbol = Symbol
data Fail = Fail
data Return a = Return a
data Choice n1 n2 = Choice n1 n2
data Bind n1 n2 = Bind n1 n2
data Look = Look

class ToParser p s a where
  toParser :: p -> Parser s a

data CheckedP :: * -> * -> * where
  CheckedP :: ToParser p s a => p -> CheckedP s a

--instance ToParser (CheckedP s a) s a where
  --toParser (CheckedP p) = toParser p
instance ToParser Symbol s s where
  toParser Symbol = symbol
instance ToParser Fail s a where
  toParser Fail = pfail
instance ToParser (Return a) s a where
  toParser (Return a) = return a
instance (ToParser n1 s a, ToParser n2 s a) => ToParser (Choice n1 n2) s a where
  toParser (Choice p q) = toParser p <|> toParser q
instance (ToParser n1 s a, ToParser n2 s b) => ToParser (Bind n1 (a -> n2)) s b where
  toParser (Bind p q) = toParser p >>= toParser . q
instance ToParser Look s [s] where
  toParser Look = look

class IsConsumer t

instance IsConsumer Symbol
instance IsConsumer Fail
instance IsConsumer n1 => IsConsumer (Choice n1 n2)
instance (IsConsumer n1) 
  => IsConsumer (Bind n1 (a -> n2))
instance (IsConsumer n2) 
  => IsConsumer (Bind n1 n2)

convertToParser 
  :: (ToParser p s a, IsConsumer p) 
  => p -> Parser s a
convertToParser x = toParser x

