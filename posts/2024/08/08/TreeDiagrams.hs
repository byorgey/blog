import Data.List.NonEmpty qualified as NE
import Data.Tree
import Diagrams.Backend.Rasterific.CmdLine
import Diagrams.Prelude
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Layout.Tree
import Data.Map qualified as M
import Data.Map (Map, (!))
import TreeDecomposition hiding (main)
import Control.Monad.State

lf :: Tree ()
lf = Node () []

chain :: Int -> Tree ()
chain 0 = lf
chain n = Node () [chain (n-1)]

chainWith :: Int -> [(Int, Tree ())] -> Tree ()
chainWith 0 _ = lf
chainWith n [] = chain n
chainWith n ((k,t):ts)
  | n == k = Node () [chainWith (n-1) ts, t]
  | otherwise = Node () [chainWith (n-1) ((k,t):ts)]

spine :: Int -> Tree ()
spine 0 = lf
spine n = Node () [spine (n-1), lf]

t :: Tree Int
t = labelUnique $ Node () [chainWith 10 [(8, chainWith 5 [(3, chain 1)])], chainWith 9 [(6, chain 2)]]

labelUnique :: Tree () -> Tree Int
labelUnique t = evalState (traverse (const inc) t) 0
  where
    inc = get >>= \s -> put (s+1) >> return s

theTree :: Diagram B
theTree = mconcat
  [ renderTree (\n -> circle 0.5 # fc white) (~~) laidOutTree
  , drawPaths ps laidOutTree
  ]
  where
    ps = map (NE.toList . fmap snd) $ maxChainDecomposition t
    laidOutTree = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t

drawPaths :: Ord a => [[a]] -> Tree (a, P2 Double) -> Diagram B
drawPaths ps t = foldMap drawPath ps
  where
    locOf = M.fromList $ flatten t
    drawPath = lw none . fc lightblue . translate (0.5 ^& (-0.5)) . strokeP . expandTrail' (with & expandJoin .~ LineJoinRound & expandCap .~ LineCapRound) 1 . fromVertices . map (locOf!)

-- main = mainWith dia
