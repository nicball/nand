module Main where

import qualified Data.Map as Map
import Data.Map (Map, (!))
import Control.Monad (join, replicateM, forM_)
import Control.Applicative (liftA2)
import Control.Monad.State (State, MonadState, get, put, modify, execState)
import Data.Bits ((.&.), (.>>.))

newtype Wire = Wire { wireID :: Int }
  deriving (Eq, Ord)

data Signal = High | Low
  deriving (Eq, Show)

signalToBool :: Signal -> Bool
signalToBool High = True
signalToBool Low = False

boolToSignal :: Bool -> Signal
boolToSignal True = High
boolToSignal False = Low

type CircuitState = Map Wire Signal

data Component where
  Nand :: Wire -> Wire -> Wire -> Component
  One :: Wire -> Component
  Zero :: Wire -> Component
  Observer :: String -> Wire -> Component

simulate :: [Component] -> CircuitState -> Maybe CircuitState
simulate comps state = traverse resolve . foldr applyComp Map.empty $ comps where
  applyComp (Nand a b out) drives = Map.alter (addDrive value) out drives where
    value = boolToSignal $ not (signalToBool (state ! a) && signalToBool (state ! b))
  applyComp (One out) drives = Map.alter (addDrive High) out drives
  applyComp (Zero out) drives = Map.alter (addDrive Low) out drives
  applyComp (Observer _ _) drives = drives
  addDrive value (Just xs) = Just (value : xs)
  addDrive value Nothing = Just [value]

resolve :: [Signal] -> Maybe Signal
resolve [] = Nothing
resolve (s : sigs) | all (== s) sigs = Just s
                   | otherwise = Nothing

observations :: CircuitState -> [Component] -> [(String, Signal)]
observations state = foldr obs [] where
  obs (Observer tag wire) xs = (tag, state ! wire) : xs
  obs _ xs = xs

data CircuitBuilderState = CircuitBuilderState { wireCount :: Int, components :: [Component] }
newtype CircuitBuilder a = CircuitBuilder (State CircuitBuilderState a)
  deriving (Functor, Applicative, Monad, MonadState CircuitBuilderState)

createWire :: CircuitBuilder Wire
createWire = do
  s <- get
  let n = wireCount s
  put s { wireCount = n + 1 }
  pure (Wire n)

addComponent :: Component -> CircuitBuilder ()
addComponent c = modify (\s -> s { components = c : components s })

runCircuitBuilder :: CircuitBuilder a -> [Component]
runCircuitBuilder (CircuitBuilder s) = components . execState s . CircuitBuilderState 0 $ []

observe :: String -> Wire -> CircuitBuilder ()
observe tag w = addComponent (Observer tag w)

withOutput :: (Wire -> CircuitBuilder a) -> CircuitBuilder Wire
withOutput f = do
  out <- createWire
  f out
  pure out

nand :: Wire -> Wire -> CircuitBuilder Wire
nand a b = withOutput (addComponent . Nand a b)

one :: CircuitBuilder Wire
one = withOutput (addComponent . One)

zero :: CircuitBuilder Wire
zero = withOutput (addComponent . Zero)

not' :: Wire -> CircuitBuilder Wire
not' a = nand a =<< one

and' :: Wire -> Wire -> CircuitBuilder Wire
and' a b = not' =<< nand a b

or' :: Wire -> Wire -> CircuitBuilder Wire
or' a b = join $ liftA2 nand (not' a) (not' b)

xor :: Wire -> Wire -> CircuitBuilder Wire
xor a b = join $ liftA2 and' (nand a b) (or' a b)

nor :: Wire -> Wire -> CircuitBuilder Wire
nor a b = not' =<< or' a b

halfAdd :: Wire -> Wire -> CircuitBuilder (Wire, Wire)
halfAdd a b = do
  sum <- xor a b
  carry <- and' a b
  pure (sum, carry)

fullAdd :: Wire -> Wire -> Wire -> CircuitBuilder (Wire, Wire)
fullAdd a b carry = do
  (s1, c1) <- halfAdd a b
  (sum, c2) <- halfAdd s1 carry
  carryOut <- or' c1 c2
  pure (sum, carryOut)

type Byte = [Wire]

addByte :: Byte -> Byte -> Wire -> CircuitBuilder (Byte, Wire)
addByte a b carry = do
  (s, c) <- add8 (reverse a) (reverse b) carry
  pure (reverse s, c)
  where
    add1 [a] [b] c = do
      (s, c') <- fullAdd a b c
      pure ([s], c')
    widen addn (a : as) (b : bs) c = do
      (s, c') <- addn as bs c
      (sh, cout) <- fullAdd a b c'
      pure (sh : s, cout)
    add8 = (!! 7) . iterate widen $ add1

constByte :: Int -> CircuitBuilder Byte
constByte n = traverse (\i -> if (n .>>. i) .&. 1 == 1 then one else zero) [0 .. 7]

initialState :: [Component] -> CircuitState
initialState = foldr addConns Map.empty where
  addConns (Nand a b out)
    = Map.insert a Low
    . Map.insert b Low
    . Map.insert out Low
  addConns (One out) = Map.insert out Low
  addConns (Zero out) = Map.insert out Low
  addConns (Observer _ w) = Map.insert w Low

main :: IO ()
main = do
  let cc = runCircuitBuilder $ do
        a <- constByte 3
        b <- constByte 4
        (sum, _) <- addByte a b =<< zero
        forM_ (zip [0 ..] sum) $ \(n, w) -> do
          observe ("sum_" ++ show n) w
      Just state = (!! 100) . iterate (>>= simulate cc) . Just . initialState $ cc
  print (observations state cc)
  pure ()
