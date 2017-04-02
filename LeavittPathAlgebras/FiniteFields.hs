module FiniteFields where

data Z2 = Z2_0 | Z2_1   deriving (Eq, Ord)

instance Enum Z2 where
  fromEnum Z2_0 = 0
  fromEnum Z2_1 = 1
  toEnum n = case (mod n 2) of
    0 -> Z2_0
    1 -> Z2_1

instance Show Z2 where
  show Z2_0 = "0"
  show Z2_1 = "1"

instance Num Z2 where
  x + y = fromIntegral ((fromEnum x) + (fromEnum y))
  x * y = fromIntegral ((fromEnum x) * (fromEnum y))
  negate x = x
  abs x = x
  signum x = x
  fromInteger n = toEnum (fromIntegral n)

z2 = [Z2_0 .. Z2_1]

data Z3 = Z3_0 | Z3_1 | Z3_2   deriving (Eq, Ord)

instance Enum Z3 where
  fromEnum Z3_0 = 0
  fromEnum Z3_1 = 1
  fromEnum Z3_2 = 2
  toEnum n = case (mod n 3) of
    0 -> Z3_0
    1 -> Z3_1
    2 -> Z3_2

instance Show Z3 where
  show Z3_0 = "0"
  show Z3_1 = "1"
  show Z3_2 = "2"

instance Num Z3 where
  x + y = fromIntegral ((fromEnum x) + (fromEnum y))
  x * y = fromIntegral ((fromEnum x) * (fromEnum y))
  negate x = x
  abs x = x
  signum x = x
  fromInteger n = toEnum (fromIntegral n)

z3 = [Z3_0 .. Z3_2]
