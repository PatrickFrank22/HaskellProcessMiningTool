{-# LANGUAGE OverloadedStrings #-}

{- 
--- This implementation is based on: ---
1. Attributes, 2022. https://graphviz.org/doc/info/attrs.html. Accessed on 17 Feb. 2024
2. Turtle.Tutorial, (n.d.). https://hackage.haskell.org/package/turtle-1.6.2/docs/Turtle-Tutorial.html. Accessed on 17 Feb. 2024.
3. Michael Burge. Visualizing Graphs in Haskell, 2017. https://www.michaelburge.us/2017/09/01/how-to-use-graphviz-in-haskell.html. Accessed on 17 Feb. 2024.
-}

module ProcessTreeVisualization.DrawProcessTree ( mainDrawProcessTree ) where

import qualified Data.GraphViz as G
import qualified Data.GraphViz.Attributes.Complete as G
import qualified Data.GraphViz.Types as G

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Turtle (proc, void, empty, format, fp)
import System.FilePath.Posix (dropExtension)
import qualified Data.Text as T

import Helperfunctions.HelperFunctions


dotToPng :: FilePath -> IO ()
dotToPng dotFile = do
    let pngFile = T.pack (System.FilePath.Posix.dropExtension dotFile) <> ".png"
        dotText = format fp dotFile
        pngText = format fp (T.unpack pngFile)
    void $ proc "dot" ["-Tpng", "-o", pngText, dotText] empty


data TreeNodeGraph a = ExclusiveChoiceGraph Int [TreeNodeGraph a] | 
                             SequenceGraph Int [TreeNodeGraph a] | 
                             ConcurrentGraph Int [TreeNodeGraph a] | 
                             LoopGraph Int [TreeNodeGraph a] |
                             ActivityGraph Int a deriving Show


transformTreeNode :: TreeNode a -> TreeNodeGraph a
transformTreeNode (Activity a) = (ActivityGraph 0 a)
transformTreeNode (ExclusiveChoice a) = (ExclusiveChoiceGraph 0 (transformTreeNodeList a))
transformTreeNode (Sequence a) = (SequenceGraph 0 (transformTreeNodeList a))
transformTreeNode (Concurrent a) = (ConcurrentGraph 0 (transformTreeNodeList a))
transformTreeNode (Loop a) = (LoopGraph 0 (transformTreeNodeList a))

transformTreeNodeList :: [TreeNode a] -> [TreeNodeGraph a]
transformTreeNodeList [] = []
transformTreeNodeList (x:xs) = (transformTreeNode x) : (transformTreeNodeList xs)


data VLabel = VLExclusiveChoice
            | VLSequence
            | VLConcurrent
            | VLLoop
            | VLActivity
            | VLActivityTau
            deriving (Show)
type V = (String, VLabel)

data ELabel = ELHardlink deriving (Show)

type E = (String, String, ELabel)

type FileGraph = ([V], [E])



createGraph :: TreeNodeGraph String -> IO FileGraph
createGraph a = do
  let a' = createGraphLabels a 1 
  return (createGraphNodes a', createGraphEdges a')


getLabels :: TreeNodeGraph String -> IO [(TL.Text, TL.Text)]
getLabels a = do
  let a' = createGraphLabels a 1
  return (createGraphIDs a')


createGraphIDs :: TreeNodeGraph String -> [(TL.Text, TL.Text)]
createGraphIDs (ActivityGraph c a) = [(TL.pack (show c), TL.pack (show a))]
createGraphIDs (SequenceGraph c a) = [(TL.pack (show c), TL.pack "Seq")] ++ (concatMap (\x -> createGraphIDs x) a)
createGraphIDs (ExclusiveChoiceGraph c a) = [(TL.pack (show c), TL.pack "XOR")] ++ (concatMap (\x -> createGraphIDs x) a)
createGraphIDs (ConcurrentGraph c a) = [(TL.pack (show c), TL.pack "Con")] ++ (concatMap (\x -> createGraphIDs x) a)
createGraphIDs (LoopGraph c a) = [(TL.pack (show c), TL.pack "Loop")] ++ (concatMap (\x -> createGraphIDs x) a)

createGraphLabels :: TreeNodeGraph String -> Int -> TreeNodeGraph String
createGraphLabels (ActivityGraph c a) count = (ActivityGraph count a)
createGraphLabels (SequenceGraph c a) count = (SequenceGraph count (createGraphLabels' a (count+1)))
createGraphLabels (ExclusiveChoiceGraph c a) count = (ExclusiveChoiceGraph count (createGraphLabels' a (count+1)))
createGraphLabels (ConcurrentGraph c a) count = (ConcurrentGraph count (createGraphLabels' a (count+1)))
createGraphLabels (LoopGraph c a) count = (LoopGraph count (createGraphLabels' a (count+1)))

createGraphLabels' :: [TreeNodeGraph String] -> Int -> [TreeNodeGraph String]
createGraphLabels' [] _ = []
createGraphLabels' (a:as) count = 
  let
  maxCount_ = maxCount (createGraphLabels a count) 
  in (createGraphLabels a count) : (createGraphLabels' as (maxCount_+1))


maxCount :: TreeNodeGraph String -> Int
maxCount (ActivityGraph c a) = c
maxCount (SequenceGraph c a) = maximum ([c] ++ (maxCount' a))
maxCount (ExclusiveChoiceGraph c a) = maximum ([c] ++ (maxCount' a))
maxCount (ConcurrentGraph c a) = maximum ([c] ++ (maxCount' a))
maxCount (LoopGraph c a) = maximum ([c] ++ (maxCount' a))

maxCount' :: [TreeNodeGraph String] -> [Int]
maxCount' [] = []
maxCount' (a:as) = (maxCount a) : (maxCount' as)


createGraphNodes :: TreeNodeGraph String -> [V]
createGraphNodes (ActivityGraph c "Tau") = [(show c, VLActivityTau)]
createGraphNodes (ActivityGraph c a) = [(show a, VLActivity)]
createGraphNodes (ExclusiveChoiceGraph c a) = [(show c, VLExclusiveChoice)] ++ (concatMap (\x -> createGraphNodes x) a)
createGraphNodes (SequenceGraph c a) = [(show c, VLSequence)] ++ (concatMap (\x -> createGraphNodes x) a)
createGraphNodes (ConcurrentGraph c a) = [(show c, VLConcurrent)] ++ (concatMap (\x -> createGraphNodes x) a)
createGraphNodes (LoopGraph c a) = [(show c, VLLoop)] ++ (concatMap (\x -> createGraphNodes x) a)


createGraphEdges :: TreeNodeGraph String -> [E]
createGraphEdges (ActivityGraph c a) = []
createGraphEdges (ExclusiveChoiceGraph c a) = (map (\x -> (show c, extractValues x, ELHardlink)) a) ++ (concatMap (\x -> createGraphEdges x) a)
createGraphEdges (SequenceGraph c a) = (map (\x -> (show c, extractValues x, ELHardlink)) a) ++ (concatMap (\x -> createGraphEdges x) a)
createGraphEdges (ConcurrentGraph c a) = (map (\x -> (show c, extractValues x, ELHardlink)) a) ++ (concatMap (\x -> createGraphEdges x) a)
createGraphEdges (LoopGraph c a) = (map (\x -> (show c, extractValues x, ELHardlink)) a) ++ (concatMap (\x -> createGraphEdges x) a)


extractValues :: TreeNodeGraph String -> String
extractValues (ActivityGraph c "Tau") = show c
extractValues (ActivityGraph c a) = show a
extractValues (ExclusiveChoiceGraph c a) = show c
extractValues (SequenceGraph c a) = show c 
extractValues (ConcurrentGraph c a) = show c 
extractValues (LoopGraph c a) = show c 


fileGraphParams :: G.GraphvizParams FilePath VLabel ELabel () VLabel
fileGraphParams = G.defaultParams {
  G.fmtNode = \(v, vl) -> case vl of
      VLExclusiveChoice -> shapeAttribute G.Circle <> colorAttribute (G.RGB 0 0 0) <> labelAttribute " " <> 
        imageAttribute "ProcessTreeVisualization/images/xor.png" <> labelWidth 0.9 <> labelHeight 0.9 <> 
        fixedSize G.SetNodeSize
      VLSequence -> shapeAttribute G.Circle <> colorAttribute (G.RGB 0 0 0) <> labelAttribute " " <> 
        imageAttribute "ProcessTreeVisualization/images/sequence.png" <> labelWidth 0.9 <> labelHeight 0.9 <> 
        fixedSize G.SetNodeSize
      VLConcurrent -> shapeAttribute G.Circle <> colorAttribute (G.RGB 0 0 0) <> labelAttribute " " <> 
        imageAttribute "ProcessTreeVisualization/images/concurrency.png" <> labelWidth 0.9 <> labelHeight 0.9 <> 
        fixedSize G.SetNodeSize
      VLLoop -> shapeAttribute G.Circle <> colorAttribute (G.RGB 0 0 0) <> labelAttribute " " <> 
        imageAttribute "ProcessTreeVisualization/images/loop.png" <> labelWidth 0.9 <> labelHeight 0.9 <> 
        fixedSize G.SetNodeSize
      VLActivity -> shapeAttribute G.BoxShape <> colorAttribute (G.RGB 0 0 0) <> fontSize 24
      VLActivityTau -> shapeAttribute G.BoxShape <> colorAttribute (G.RGB 70 70 70) <> bgColorAttribute (G.RGB 70 70 70) <> 
        labelAttribute "" <> imageAttribute "ProcessTreeVisualization/images/tau.png" <> labelWidth 0.6 <> labelHeight 0.6 <> 
        fixedSize G.SetNodeSize,
  G.fmtEdge = \(from, to, el) -> case el of
      ELHardlink -> colorAttribute (G.RGB 0 0 0)
  }  
  where
    colorAttribute color = [ G.Color $ G.toColorList [ color ] ]
    shapeAttribute shape = [ G.Shape shape ]
    labelAttribute label = [ G.Label $ G.StrLabel label ]
    bgColorAttribute color = [ G.FillColor $ G.toColorList [ color ] ]
    style filled = [ G.Style [G.SItem filled [] ] ]
    imageAttribute image = [ G.Image image ]
    labelWidth size = [ G.Width size ]
    labelHeight size = [ G.Height size ]
    fixedSize bool = [ G.FixedSize bool ]
    fontSize size = [ G.FontSize size ]


mainDrawProcessTree :: TreeNode String -> IO ()
mainDrawProcessTree processTree = do
  let processTreeTransformed = transformTreeNode processTree
  (vs, es) <- createGraph processTreeTransformed
  ids <- getLabels processTreeTransformed
  let dotGraph = G.graphElemsToDot fileGraphParams vs es :: G.DotGraph FilePath
      dotText = G.printDotGraph dotGraph :: TL.Text
  TL.writeFile "./Result/resultingProcessTree.dot" dotText
  dotToPng "./Result/resultingProcessTree.dot"
