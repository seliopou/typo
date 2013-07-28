module Language.Typo.Prelude
  ( prelude
  ) where


prelude :: String
prelude = "\
\-------------------------------------------------------------------------------\n\
\-- BEGIN PRELUDE --------------------------------------------------------------\n\
\-------------------------------------------------------------------------------\n\
\\n\
\{-# LANGUAGE NoImplicitPrelude #-}\n\
\{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}\n\
\{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}\n\
\{-# LANGUAGE OverlappingInstances #-}\n\
\\n\
\undefined = undefined\n\
\\n\
\data True\n\
\data False\n\
\\n\
\true = undefined :: True\n\
\false = undefined :: False\n\
\\n\
\class And a b c | a b -> c where\n\
\  and :: a -> b -> c\n\
\  and = undefined\n\
\\n\
\class Or a b c | a b -> c where\n\
\  or :: a -> b -> c\n\
\  or = undefined\n\
\\n\
\class Imp a b c | a b -> c where\n\
\  imp :: n -> b -> c\n\
\  imp = undefined\n\
\\n\
\class Cond c t f r | c t f -> r where\n\
\  cond :: c -> t -> f -> r\n\
\  cond = undefined\n\
\\n\
\instance And True True True\n\
\instance And True False False\n\
\instance And False True False\n\
\instance And False False False\n\
\\n\
\instance Or True True True\n\
\instance Or True False True\n\
\instance Or False True True\n\
\instance Or False False False\n\
\\n\
\instance Imp True True True\n\
\instance Imp True False False\n\
\instance Imp False True True\n\
\instance Imp False False True\n\
\\n\
\instance Cond True t f t\n\
\instance Cond False t f f\n\
\\n\
\data Z\n\
\data S n\n\
\\n\
\data Neg n\n\
\\n\
\data LT\n\
\data EQ\n\
\data GT\n\
\\n\
\class Compare n m c | n m -> c where\n\
\  compare :: n -> m -> c\n\
\  compare = undefined\n\
\\n\
\instance Compare Z       Z       EQ\n\
\instance Compare Z       (S m)   LT\n\
\instance Compare (S n)   Z       GT\n\
\instance Compare (Neg n) Z       LT\n\
\instance Compare Z       (Neg m) GT\n\
\instance Compare (Neg n) (S m)   LT\n\
\instance Compare (S n)   (Neg m) GT\n\
\instance Compare m n c => Compare (Neg n) (Neg m) c\n\
\instance Compare n m c => Compare (S n)   (S m)   c\n\
\\n\
\class Eq n m b | n m -> b where\n\
\  eq :: n -> m -> b\n\
\  eq = undefined\n\
\\n\
\class Eq' n m c b | n m c -> b where\n\
\  eq' :: n -> m -> c -> b\n\
\  eq' = undefined\n\
\\n\
\class Lt n m b | n m -> b where\n\
\  lt :: n -> m -> b\n\
\  lt = undefined\n\
\\n\
\class Lt' n m c b | n m c -> b where\n\
\  lt' :: n -> m -> c -> b\n\
\  lt' = undefined\n\
\\n\
\instance Eq' n m LT False\n\
\instance Eq' n m EQ True\n\
\instance Eq' n m GT False\n\
\\n\
\instance (Compare n m c, Eq' n m c b) => Eq n m b\n\
\\n\
\instance Lt' n m LT True\n\
\instance Lt' n m EQ False\n\
\instance Lt' n m GT False\n\
\\n\
\instance (Compare n m c, Lt' n m c b) => Lt n m b\n\
\\n\
\class Add n m k | n m -> k where\n\
\  add :: n -> m -> k\n\
\  add = undefined\n\
\\n\
\instance Add Z Z Z\n\
\instance Add n Z n\n\
\instance Add Z m m\n\
\\n\
\instance Add n m r => Add (Neg n) (Neg m) (Neg r)\n\
\instance Sub m n r => Add (Neg n) m       r\n\
\instance Sub n m r => Add n       (Neg m) r\n\
\instance Add n m r => Add (S n)   (S m)   (S (S r))\n\
\\n\
\class Sub n m k | n m -> k where\n\
\  sub :: n -> m -> k\n\
\  sub = undefined\n\
\\n\
\instance Sub Z Z       Z\n\
\instance Sub n Z       n\n\
\instance Sub Z (Neg m) m\n\
\instance Sub Z (S m)   (Neg (S m))\n\
\\n\
\instance Sub m n r => Sub (Neg n) (Neg m) r\n\
\instance Add n m r => Sub (Neg n) (S m)   (Neg (S r))\n\
\instance Add n m r => Sub (S n)   (Neg m) (S r)\n\
\instance Sub n m r => Sub (S n)   (S m)   r\n\
\\n\
\class Mul n m k | n m -> k where\n\
\  mul :: n -> m -> k\n\
\  mul = undefined\n\
\\n\
\instance Mul Z Z Z\n\
\instance Mul n Z Z\n\
\instance Mul Z m Z\n\
\\n\
\instance Mul n     m     r => Mul (Neg n) (Neg m) r\n\
\instance Mul (S n) m     r => Mul (S n)   (Neg m) (Neg r)\n\
\instance Mul n     (S m) r => Mul (Neg n) (S m)   (Neg r)\n\
\instance (Mul n m k, Add n k k', Add m k' r) => Mul (S n) (S m) (S r)\n\
\\n\
\class DivRem n m q r | n m -> q r where\n\
\  divRem :: n -> m -> (q, r)\n\
\  divRem = undefined\n\
\\n\
\class DivRemBranch0 b n m q r | b n m -> q r where\n\
\  divRemBranch0 :: b -> n -> m -> (q, r)\n\
\  divRemBranch0 = undefined\n\
\\n\
\instance DivRemBranch0 LT n m Z n\n\
\instance DivRemBranch0 EQ n m (S Z) Z\n\
\instance (Sub n m n', DivRem n' m q r) => DivRemBranch0 GT n m (S q) r\n\
\\n\
\instance (Compare n m c, DivRemBranch0 c n m q r) => DivRem n m q r\n\
\\n\
\class Div n m k | n m -> k where\n\
\  div :: n -> m -> k\n\
\  div = undefined\n\
\\n\
\class Rem n m r | n m -> r where\n\
\  rem :: n -> m -> r\n\
\  rem = undefined\n\
\\n\
\instance DivRem n m q r => Div n m q\n\
\instance DivRem n m q r => Rem n m r\n\
\\n\
\-------------------------------------------------------------------------------\n\
\-- END PRELUDE ----------------------------------------------------------------\n\
\-------------------------------------------------------------------------------\n\
\\n\
\"
