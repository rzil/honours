module FiniteFields where

--
-- Z2
--

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
  negate x = toEnum (negate (fromEnum x))
  abs x = toEnum (abs (fromEnum x))
  signum x = toEnum (signum (fromEnum x))
  fromInteger n = toEnum (fromIntegral n)

z2 = [Z2_0 .. Z2_1]

--
-- Z3
--

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
  negate x = toEnum (negate (fromEnum x))
  abs x = toEnum (abs (fromEnum x))
  signum x = toEnum (signum (fromEnum x))
  fromInteger n = toEnum (fromIntegral n)

z3 = [Z3_0 .. Z3_2]

--
-- Z5
--

data Z5 = Z5_0 | Z5_1 | Z5_2 | Z5_3 | Z5_4   deriving (Eq, Ord)

instance Enum Z5 where
  fromEnum Z5_0 = 0
  fromEnum Z5_1 = 1
  fromEnum Z5_2 = 2
  fromEnum Z5_3 = 3
  fromEnum Z5_4 = 4
  toEnum n = case (mod n 5) of
    0 -> Z5_0
    1 -> Z5_1
    2 -> Z5_2
    3 -> Z5_3
    4 -> Z5_4

instance Show Z5 where
  show Z5_0 = "0"
  show Z5_1 = "1"
  show Z5_2 = "2"
  show Z5_3 = "3"
  show Z5_4 = "4"

instance Num Z5 where
  x + y = fromIntegral ((fromEnum x) + (fromEnum y))
  x * y = fromIntegral ((fromEnum x) * (fromEnum y))
  negate x = toEnum (negate (fromEnum x))
  abs x = toEnum (abs (fromEnum x))
  signum x = toEnum (signum (fromEnum x))
  fromInteger n = toEnum (fromIntegral n)

z5 = [Z5_0 .. Z5_4]

--
-- Z7
--

data Z7 = Z7_0 | Z7_1 | Z7_2 | Z7_3 | Z7_4 | Z7_5 | Z7_6   deriving (Eq, Ord)

instance Enum Z7 where
  fromEnum Z7_0 = 0
  fromEnum Z7_1 = 1
  fromEnum Z7_2 = 2
  fromEnum Z7_3 = 3
  fromEnum Z7_4 = 4
  fromEnum Z7_5 = 5
  fromEnum Z7_6 = 6
  toEnum n = case (mod n 7) of
    0 -> Z7_0
    1 -> Z7_1
    2 -> Z7_2
    3 -> Z7_3
    4 -> Z7_4
    5 -> Z7_5
    6 -> Z7_6

instance Show Z7 where
  show Z7_0 = "0"
  show Z7_1 = "1"
  show Z7_2 = "2"
  show Z7_3 = "3"
  show Z7_4 = "4"
  show Z7_5 = "5"
  show Z7_6 = "6"

instance Num Z7 where
  x + y = fromIntegral ((fromEnum x) + (fromEnum y))
  x * y = fromIntegral ((fromEnum x) * (fromEnum y))
  negate x = toEnum (negate (fromEnum x))
  abs x = toEnum (abs (fromEnum x))
  signum x = toEnum (signum (fromEnum x))
  fromInteger n = toEnum (fromIntegral n)

z7 = [Z7_0 .. Z7_6]
