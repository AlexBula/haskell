module Mon where

infixl 5 ><

class Mon m where
  m1 :: m
  (><) :: m -> m -> m

