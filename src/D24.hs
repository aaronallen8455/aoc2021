module D24
  ( day24A
  , day24B
  , div'
  ) where

import           Control.Monad
import           Data.Foldable
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Map.Strict as M

import           Util
import           Debug.Trace

day24A :: BS.ByteString -> BS.ByteString
day24A (map parseInstr . BS.lines -> instr) =
  let p = runPgrm' instr
   in showBS p
-- last digit increments 321272407
-- -3224605498 -3212725448
-- tryCombo :: [Int] -> [Int]
-- tryCombo combo = do
--   x <- [1..9]
--   let inp = take 14 $ combo ++ x : repeat 1
--   case 

-- The last position is a linear variable. No recognizable pattern for others.

-- possible to run the program in reverse?

day24B :: BS.ByteString -> BS.ByteString
day24B = undefined

data Val
  = Var Char
  | Num Int
  deriving Show

parseInstr :: BS.ByteString -> Instr
parseInstr bs =
  case BS.splitAt 3 bs of
    ("inp", BS.unpack -> [_, v]) -> Inp v
    ("add", BS.words -> [BS.unpack -> [a], b]) -> Add a $ parseVal b
    ("mul", BS.words -> [BS.unpack -> [a], b]) -> Mul a $ parseVal b
    ("div", BS.words -> [BS.unpack -> [a], b]) -> Div a $ parseVal b
    ("mod", BS.words -> [BS.unpack -> [a], b]) -> Mod a $ parseVal b
    ("eql", BS.words -> [BS.unpack -> [a], b]) -> Eql a $ parseVal b

parseVal :: BS.ByteString -> Val
parseVal (BS.unpack -> x@(h:rest))
  | isAlpha h = Var h
  | otherwise = Num $ read x

-- data N
--   = N Int
--   | Add'' N N
--   | Mul'' N N
--   | NotEq N
--   | Div'' N N
--   | Mod'' N N

-- data R
--   = N Int
--   | NotEq R
--   | MultOf R R -- starting number, distance between multiples
--   | OneOf [R]

data R
  = OneOf [R]
  | N Int
  | MultOf Int Int -- starting number, distance between multiples
  | NotEq R

runInReverse :: [Instr] -> [Int]
runInReverse instrs = fst $ foldl' go ([], M.empty) (reverse instrs) where
  go (inp, vars) = \case
    Inp v -> (findMax (vars M.! v) : inp, vars)
    Add v b -> doOP v b subt
    Mul v b -> doOP v b divide
    Div v b -> doOP v b options -- need to do better due to truncation
    Mod v b -> doOP v b unMod
    Eql v b -> doOP v b unEql
    where
    doOP v b op =
      let v' = vars M.! v
          b' = case b of
                 Var x -> vars M.! x
                 Num n -> N n
       in (inp, M.insert v (op v' b') vars)
  unEql (N 1) b = b
  unEql (N 0) b = NotEq b -- what can be done here? Find the highest values of the inputs
  -- such that the result is not equal to b
  unEql _ _ = error "invalid equal" -- need to handle other cases?

  subt a b =
    case (a, b) of
      (N x, N y) -> N $ x - y
      (N x, MultOf bs d) -> MultOf (x - bs) d

      (OneOf xs, _) -> OneOf $ subt <$> xs <*> pure b
      (_, OneOf xs) -> OneOf $ subt a <$> xs

      (MultOf bs d, N x) -> MultOf (bs - x) d
      (MultOf bs1 d1, MultOf bs2 d2) -> MultOf (bs1 - bs2) d2

      (NotEq a, NotEq b) -> NotEq $ subt a b -- is this right?
      (NotEq a, _) -> NotEq $ subt a b
      (_, NotEq b) -> NotEq $ subt a b

  divide a b =
    case (a, b) of
      (N x, N y) -> N $ div x y
      (N x, MultOf bs d) ->
        let up = takeWhile (/=0) $ div x <$> [bs, bs + d..]
            down = takeWhile (/=0) $ div x <$> [bs -d, bs - 2 * d ..]
         in OneOf . map N . (0 :) $ up <> down

      -- Possible problem
      -- only find numbers that are actually divisible? Shouldn't need to
      (MultOf bs d, N x) ->
        let up = takeWhile (/=0) $ div <$> [bs, bs + d..] <*> [x]
            down = takeWhile (/=0) $ div' <$> [bs -d, bs - 2 * d ..] <*> [x]
         in OneOf . map N . (0 :) $ up <> down

      (OneOf xs, _) -> OneOf $ divide <$> xs <*> pure b
      (_, OneOf xs) -> OneOf $ divide a <$> xs

      (NotEq a, NotEq b) -> NotEq $ divide a b -- is this right?
      (NotEq a, _) -> NotEq $ divide a b
      (_, NotEq b) -> NotEq $ divide a b

  options a b = -- find the numbers that can be divided by b to get a
    case (a, b) of
      (N x, N y) ->
        let b = x * y
            opts = [b .. b + x - 1]
         in OneOf opts

      (N x, MultOf bs d) ->
        let up = takeWile (/=0) $ div' x <$> [bs, bs + d]
            down = takeWhile (/=0) $ div' <$> [bs - d, bs - 2*d..] <*> [x]
         in MultOf $ do
              c <- up <> down

-- to keep it simpler, could have a list of nums within some sane range.
-- will result in exponential blow up.

  options = undefined
  unMod = undefined

  findMax = undefined

-- could find the values of z that are able to succeed in final step, then
-- inductively go backwards to find acceptable values in prior blocks and we
-- have the complete list of possibles

-- still, determining z even with an expression of only operations and constants
-- seems difficult.

--   subtract' a b =
--     case (a, b) of
--       (EqTo a, EqTo b) -> EqTo $ a - b
--       (NotEq a, NotEq b) -> NotEq $ a - b
--       (EqTo a, NotEq b) -> NotEq $ a - b
--       (NotEq a, EqTo b) -> NotEq $ a - b
-- 
--       (EqTo a, MultOf x r) -> MultOf x _
-- 
--       (MultOf x1 r1, MultOf x2 r2) -> MultOf x1 (MultOf x2 $ subtract' r1 r2)
--       (MultOf x r, EqTo y) -> MultOf x (subtract' r $ EqTo y)
--       (MultOf x r, NotEq y) -> MultOf x (subtract' r $ NotEq y)

-- values in map should be expressions rather than concrete numbers

data Expr
  = Add' !Expr !Expr
  | Mul' !Expr !Expr
  | Div' !Expr !Expr
  | Mod' !Expr !Expr
  | Eql' !Expr !Expr
  | Inp' !Int
  | Const !Int
  deriving (Show, Eq)

simplify :: Expr -> Expr
simplify = \case
  Add' (simplify -> a) (simplify -> b)
    | Const 0 <- a -> b
    | Const 0 <- b -> a
    | otherwise -> doOP (+) Add' a b
  Mul' (simplify -> a) (simplify -> b)
    | Const 1 <- a -> b
    | Const 0 <- a -> Const 0
    | Const 1 <- b -> a
    | Const 0 <- b -> Const 0
    | otherwise -> doOP (*) Mul' a b
  Div' (simplify -> a) (simplify -> b)
    | Const 0 <- a -> Const 0
    | Const 1 <- b -> a
    | a == b -> Const 1
    | otherwise -> doOP div' Div' a b
  Mod' (simplify -> a) (simplify -> b)
    | Inp' _ <- a, Const x <- b, x > 9 -> a
    | otherwise -> doOP mod' Mod' a b
  Eql' (simplify -> a) (simplify -> b)
    | Inp' _ <- a, Const x <- b, x <= 0 || x > 9 -> Const 0
    | Inp' _ <- b, Const x <- a, x <= 0 || x > 9 -> Const 0
    | Inp' _ <- a, outOfVarRange b -> Const 0
    | Inp' _ <- b, outOfVarRange a -> Const 0
    | Inp' _ <- b, Const x <- a, x <= 0 || x > 9 -> Const 0
    | a == b -> Const 1
    | Const 0 <- a, cannotBe0 b -> Const 0
    | Const 0 <- b, cannotBe0 a -> Const 0
    | otherwise -> doOP (\x y -> if x == y then 1 else 0) Eql' a b
  Inp' n -> Inp' n
  Const x -> Const x
  where
    doOP op con a b =
      case (a, b) of
        (Const x, Const y) -> Const $ x `op` y
        (x, y) -> con x y
    cannotBe0 = \case
      Add' a b
        | cannotBe0 a, Const x <- b, x >= 0 -> True
        | cannotBe0 b, Const x <- a, x >= 0 -> True
        | cannotBe0 a, cannotBe0 b -> True
        | otherwise -> False
      Mul' a b -> cannotBe0 a && cannotBe0 b
      Div' a b -> False
      Mod' a b -> False
      Eql' a b -> False
      Const x -> x /= 0
      Inp' _ -> True
    outOfVarRange = \case
      Add' a b
        | Mod' _ _ <- a, outOfVarRange b -> True
        | Mod' _ _ <- b, outOfVarRange a -> True
        | Inp' _ <- a, outOfVarRange b -> True
        | outOfVarRange a, Const x <- b, x >= 0 -> True
        | Inp' _ <- b, outOfVarRange a -> True
        | outOfVarRange b, Const x <- a, x >= 0 -> True
        | otherwise -> False
      Mul' a b -> outOfVarRange a || outOfVarRange b
      Div' a b -> False
      Mod' a b -> False
      Eql' a b -> False
      Const x -> x >= 9 || x <= -9
      Inp' _ -> False

-- Maybe it's another stupid problem where I just need to brute force.

-- One side of the op is always going to be a constant?

-- determineVars :: Int -> M.Map Int Int -> Expr -> (Expr, M.Map Int Int)
-- determineVars target m = \case
--   Add' a b ->
--     let (maxA, ra) = maxAssignments a
--         (maxB, rb) = maxAssignments b
--      in if maxA > maxB
--            then determineVars (negate ra) _
--            else _
--     let c1 = determineVars
-- 
-- assignmentToInts :: M.Map Int Int -> [Int]
-- assignmentToInts = M.elems

-- the inputs go into w, so the other vars must be expressible in
-- terms of operations on w and constants. w doesn't change once
-- set by an input
-- Once we have the expression for z, can just solve the equation
-- for each input (1..9).

-- Once the expression has been build, can set it equal to 0, meaning that
-- for addition, the second term must be equal to the negation of the first,
-- so we can essentially discard the first, unless it results in a greater
-- input. Need to look at the input from the first and see if it's greater than
-- the second.
--
-- Need some way of "equalizing" two expressions.

runPgrm' :: [Instr] -> Expr
runPgrm' instrs =
  let r = foldl' go (M.fromList $ zip "xyzw" $ repeat (Const 0)) instrs
      go vars = \case
        Inp v ->
          case vars M.! v of
            Inp' x -> M.insert v (Inp' $ x + 1) vars
            _ -> M.insert v (Inp' 0) vars
        Add v b -> doOP vars Add' v b
        Mul v b -> doOP vars Mul' v b
        Div v b -> doOP vars Div' v b
        Mod v b -> doOP vars Mod' v b
        Eql v b -> doOP vars Eql' v b

      doOP vars op v b =
          case b of
            Var b' ->
              let v' = vars M.! v
                  b'' = vars M.! b'
                  ex = simplify $ op v' b''
               in M.insert v ex vars
            Num b' ->
              let v' = vars M.! v
                  ex = simplify $ op v' (Const b')
               in M.insert v ex vars
   in r M.! 'z'

runPgrm :: [Int] -> [Instr] -> Int
runPgrm inp instrs =
  let (_, r) = foldl' go (inp, M.fromList $ zip "xyzw" $ repeat 0) instrs
      go (inp, vars) = \case
        Inp v ->
          case inp of
            x:rest -> (rest, M.insert v x vars)
        Add v b -> doOP vars (+) v b
        Mul v b -> doOP vars (*) v b
        Div v b -> doOP vars div' v b
        Mod v b -> doOP vars mod' v b
        Eql v b -> doOP vars (\x y -> if x == y then 1 else 0) v b
      doOP vars op v b =
        let b' = case b of
                   Var c -> vars M.! c
                   Num v -> v
         in (inp, M.insert v ((vars M.! v) `op` b') vars)
   in r M.! 'z'

data Instr
  = Inp Char
  | Add Char Val
  | Mul Char Val
  | Div Char Val
  | Mod Char Val
  | Eql Char Val
  deriving Show

div' :: Int -> Int -> Int
div' a b
  | (a >= 0 && b > 0) || (a < 0 && b < 0) = div a b
  | otherwise = negate $ abs a `div` abs b

mod' :: Int -> Int -> Int
mod' a b =
  let x = div' a b
   in b - (x * a)
