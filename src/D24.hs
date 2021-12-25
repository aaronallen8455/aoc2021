module D24
  ( day24A
  , day24B
  ) where

import           Control.Monad
import           Data.Foldable
import qualified Data.ByteString.Char8 as BS
import           Data.Char
import qualified Data.Map.Strict as M
import           Data.Ord

import           Util
import           Debug.Trace
import qualified Data.Set as S
import           Data.Maybe
import qualified Data.IntMap.Strict as IM

day24A :: BS.ByteString -> BS.ByteString
day24A (map parseInstr . BS.lines -> instr) =
  let chunks = reverse $ splitOnInputs instr
      nums = foldl' (solve False) (IM.singleton 0 []) chunks
   in showBS nums

day24B :: BS.ByteString -> BS.ByteString
day24B (map parseInstr . BS.lines -> instr) =
  let chunks = reverse $ splitOnInputs instr
      nums = foldl' (solve True) (IM.singleton 0 []) chunks
   in showBS nums

solve :: Bool -> IM.IntMap [Int] -> [Instr] -> IM.IntMap [Int]
solve isDay2 m instr =
  let simpl = simplify $ runPgrm' instr
   in IM.fromListWith (if isDay2 then min else max) $ do
     w <- [1..9]
     (target, path) <- IM.toList m
     let simpl' = subInp w simpl
     cand <- estimate target simpl'
     guard $ evalExpr cand simpl' == target
     pure (cand, w:path)

evalExpr :: Int -> Expr -> Int
evalExpr z = \case
  Inp' _ -> error "impossible"
  ZVar -> z
  Add' x y -> evalExpr z x + evalExpr z y
  Mul' x y -> evalExpr z x * evalExpr z y
  Div' x y -> evalExpr z x `div'` evalExpr z y
  Mod' x y -> evalExpr z x `mod` evalExpr z y
  Eql' x y -> if evalExpr z x == evalExpr z y then 1 else 0
  Const x -> x

subInp :: Int -> Expr -> Expr
subInp inp = simplify . \case
  Inp' _ -> Const inp
  ZVar -> ZVar
  Add' x y -> Add' (subInp inp x) (subInp inp y)
  Mul' x y -> Mul' (subInp inp x) (subInp inp y)
  Div' x y -> Div' (subInp inp x) (subInp inp y)
  Mod' x y -> Mod' (subInp inp x) (subInp inp y)
  Eql' x y -> Eql' (subInp inp x) (subInp inp y)
  Const x -> Const x

estimate :: Int -> Expr -> [Int]
estimate target = \case
  ZVar -> [target]
  Inp' _ -> error "shouldn't happen"
  Add' x y -> do
    case handleEq y of
      [] -> do
        Const x <- handleEq x
        estimate (target - x) y
      ys -> do
        Const y <- ys
        estimate (target - y) x

  Mul' x y -> do
    case handleEq y of
      [] -> do
        Const x <- handleEq x
        estimate (div target x) y
      ys -> do
        Const y <- ys
        estimate (div target y) x

  Div' x y -> do
    case handleEq y of
      [] -> do
        Const x <- handleEq x
        let p = target * x
        ext <- [p.. p + (x - 1)]
        estimate ext y
      ys -> do
        Const y <- ys
        let p = target * y
        ext <- [p.. p + (y - 1)]
        estimate ext x

  Mod' x y -> error "shouldn't happen"

  Eql' _ _ -> [1, 0]

  _ -> error "death"

  where
  handleEq (Add' ex1 ex2) = do
    Const x <- handleEq ex1
    Const y <- handleEq ex2
    pure . Const $ x + y
  handleEq (Mul' ex1 ex2) = do
    Const x <- handleEq ex1
    Const y <- handleEq ex2
    pure . Const $ x * y
  handleEq (Eql' _ _) = [Const 1, Const 0]
  handleEq (Const x) = [Const x]
  handleEq _ = []

splitOnInputs :: [Instr] -> [[Instr]]
splitOnInputs = chunksOf 18

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

data Expr
  = Add' !Expr !Expr
  | Mul' !Expr !Expr
  | Div' !Expr !Expr
  | Mod' !Expr !Expr
  | Eql' !Expr !Expr
  | Inp' Int
  | Const !Int
  | ZVar
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
    | otherwise -> doOP mod Mod' a b
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
  ZVar -> ZVar
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
        | Inp' _ <- a, outOfVarRange b -> True
        | Inp' _ <- b, outOfVarRange a -> True
        | outOfVarRange a, Const x <- b, x >= 0 -> True
        | outOfVarRange b, Const x <- a, x >= 0 -> True
        | otherwise -> False
      Mul' a b -> outOfVarRange a || outOfVarRange b
      Div' a b -> False
      Mod' a b -> False
      Eql' a b -> False
      Const x -> x >= 9 || x <= -9
      Inp' _ -> False

runPgrm' :: [Instr] -> Expr
runPgrm' instrs =
  let r = foldl' go (M.insert 'z' ZVar . M.fromList $ zip "xyw" $ repeat (Const 0)) instrs
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
runPgrm = runPgrm_ 0

runPgrm_ :: Int -> [Int] -> [Instr] -> Int
runPgrm_ z inp instrs =
  let (_, r) = foldl' go (inp, M.fromList $ zip "xyzw" $ repeat z) instrs
      go (inp, vars) = \case
        Inp v ->
          case inp of
            x:rest -> (rest, M.insert v x vars)
        Add v b -> doOP (+) v b
        Mul v b -> doOP (*) v b
        Div v b -> doOP div' v b
        Mod v b -> doOP mod v b
        Eql v b -> doOP (\x y -> if x == y then 1 else 0) v b
        where
        doOP op v b =
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
