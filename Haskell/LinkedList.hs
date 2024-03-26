import Prelude hiding (concat)

data LinkedList a = Node a (LinkedList a)
                  | End
                  deriving (Show, Eq)

cons :: a -> LinkedList a -> LinkedList a
cons value list = Node value list

concat :: LinkedList a -> LinkedList a -> LinkedList a
concat End                   otherList = otherList
concat (Node value End)      otherList = Node value otherList
concat (Node value nextNode) otherList = Node value (concat nextNode otherList)

instance Functor LinkedList where
  fmap f (End)                 = End
  fmap f (Node value nextNode) = Node (f value) (fmap f nextNode)

instance Applicative LinkedList where
  pure a = Node a End

  (<*>) (End)                        (End) = End
  (<*>) (Node function nextFunction) (Node value nextValue)
    = Node (function value) (nextFunction <*> nextValue)

instance Monad LinkedList where
  (>>=) (End) _ = End
  (>>=) (Node value nextNode) f = concat (f value) (nextNode >>= f)

instance Foldable LinkedList where
  foldr f b (End)                 = b
  foldr f b (Node value nextNode) = foldr f (f value b) nextNode

instance Traversable LinkedList where
  traverse f (Node value nextNode) = f $ concat value (traverse f nextNode)
  traverse f node                  = f node
