-- | Visualise circuits using Graphviz
module Circuit.Dot
  ( arithCircuitToDot
  ) where

import Protolude

import qualified Data.Text as Text
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Circuit.Affine ()
import Circuit.Arithmetic (ArithCircuit(..), Gate(..), Wire(..), fetchVars)

arithCircuitToDot
  :: (Show f) => ArithCircuit f -> Text
arithCircuitToDot (ArithCircuit gates)
  = Text.unlines . wrapInDigraph . concatMap graphGate $ gates
  where
    wrapInDigraph x = ["digraph g {"] ++ x ++ ["}"]

    dotWire :: Wire -> Text
    dotWire = show . pretty

    dotArrow :: Text -> Text -> Text
    dotArrow s t = s <> " -> " <> t

    dotArrowLabel :: Text -> Text -> Text -> Text
    dotArrowLabel s t lbl = dotArrow s t <> " [label=\"" <> lbl <> "\"]"

    labelNode lblId lbl = lblId <> " [label=\"" <> lbl <> "\"]"

    pointNode lblId = lblId <> " [shape=point]"

    graphGate :: Show f => Gate Wire f -> [Text]
    graphGate (Mul lhs rhs output)
      = [ labelNode gateLabel "*"
        , labelNode lhsLabel (show $ pretty lhs)
        , dotArrow lhsLabel gateLabel
        , labelNode rhsLabel (show $ pretty rhs)
        , dotArrow rhsLabel gateLabel
        ] ++ inputs lhs lhsLabel
          ++ inputs rhs rhsLabel
      where
        lhsLabel = dotWire output <> "lhs"
        rhsLabel = dotWire output <> "rhs"
        gateLabel = dotWire output
        inputs circuit tgt
          = map ((\src -> dotArrowLabel src tgt (show $ pretty src))
                   . dotWire)
          $ fetchVars circuit
    graphGate (Equal i m output)
      = [ labelNode gateLabel "= 0 ? 0 : 1"
        , dotArrowLabel (dotWire i) gateLabel (dotWire i)
        , dotArrowLabel (dotWire m) gateLabel (dotWire m)
        ]
      where
        gateLabel = dotWire output
    graphGate (Split i outputs)
      = [ labelNode gateLabel "split"
        , dotArrowLabel (dotWire i) gateLabel (dotWire i)
        ] ++ map (pointNode . dotWire) outputs
          ++ map (\output -> dotArrow gateLabel (dotWire output)) outputs
      where
        gateLabel = Text.concat . fmap dotWire $ outputs
