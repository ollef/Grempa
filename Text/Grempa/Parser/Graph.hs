module Text.Grempa.Parser.Graph where

import Control.Monad.State

import Data.GraphViz
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree

import qualified Text.Grempa.Typed
import qualified Text.Grempa.Test

import Text.Grempa.Parser.Table
import Text.Grempa.Parser.Dynamic

makeGraph :: Show s => [ReductionTree s] -> Gr (String, Color, Shape) Color
makeGraph = uncurry mkGraph . flip evalState 0 . makeGraph' Nothing

makeGraph' :: Show s
           => Maybe Int
           -> [ReductionTree s]
           -> State Int ( [(Int, (String, Color, Shape))]
                        , [(Int, Int, Color)]
                        )
makeGraph' _    []     = return ([], [])
makeGraph' from (a:as) = do
    n <- inc
    (nodes, edges) <- makeGraph' from as
    case a of
        RTTerm x -> do
            return
              ( nodes ++ [(n, (show x, red__, Circle))]
              , edges ++ maybe [] (\x -> [(x, n, red__)]) from
              )
        RTReduce rule prod rt -> do
            (nodes', edges') <- makeGraph' (Just n) rt
            return
              ( nodes ++ [(n, ("Reduce (" ++ show rule ++ ", " ++ show prod ++ ")", white, Ellipse))] ++ nodes'
              , edges ++ maybe [] (\x -> [(x, n, black)]) from ++ edges'
              )

inc = do n <- get; put (n + 1); return n

white = X11Color Transparent
red__ = X11Color Red
black = X11Color Black

previewGraphviz :: Show s => [ReductionTree s] -> IO ()
previewGraphviz = preview . makeGraph

-- | Skapar en sträng innehållande dot kommandon
-- jag brukar spara detta innehåll till en fil och sedan köra
-- dot -Tpng -O filnamen.dot
toGraphviz :: Show s => [ReductionTree s] -> String
toGraphviz = printDotGraph . toDotGraph


-- | Skapa en special dot-graph, har inte brytt mig för mycket om options
toDotGraph x = graphToDot True
                          (makeGraph x)
                          [GraphAttrs [ RankDir FromTop
                                      , Center True]]
                          nodeToAttr
                          edgeToAttr
  where 
    -- För varje nod körs denna funktion, här får man alltså tillgång till
    -- parametrarna man la in förut
    -- http://hackage.haskell.org/packages/archive/graphviz/2999.9.0.0/doc/html/Data-GraphViz-Attributes.html#t%3AAttribute
    -- kollar om den är mindre än två då det är mina special-tillstånd
    nodeToAttr (n, (name, col, shape)) =
                       [ Label (StrLabel name)
                       , FillColor col
                       , Shape shape
                       , Style [ SItem Filled [] ]
                       ]
    -- Samma fast för kanter
    edgeToAttr (from, to, label) = [Color [label]]

writeTest e = writeFile "test.dot" x
  where x = toGraphviz $ (:[]) $ fst $ Typed.evalGrammar $ runSLRG id Test.e e
