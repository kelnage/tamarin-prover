-- |
-- Copyright   : (c) 2010-2012 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC only
--
-- Produce a graph of the expected execution model for a theory
module Theory.Tools.GraphExecution (
  -- * Produce a dot-graph string
    graphTheoryExec
  ) where

import           Data.List       (nub, groupBy, intercalate)
import qualified Data.Map        as M

import qualified Text.Dot        as D

import           Control.Monad.Reader

import qualified Extension.Data.Label as L

import           Theory

-- import           Debug.Trace

-- | Output a dot-graph of all the possible connections for a theory's rules
graphTheoryExec :: ClosedTheory -> String
graphTheoryExec thy = D.showDot (createExecutionGraph thy)

-- | Given a theory, take its rules and for every premise, check which rules have conclusions that can unify
-- with that premise. For each of those rules, add an edge to the graph.
-- TODO: allow rules to be automatically excluded from the graph based on a rule name pattern? (Such as reveal)
createExecutionGraph :: ClosedTheory -> D.Dot ()
createExecutionGraph thy = do
    let rules = getProtoRuleACs thy
    let nodeNames = map getRuleName rules
    -- For each rule, and every fact in that rules premises, find all conclusion facts that unify with the premise
    -- fact. Add dot-specific styling depending on fact type.
    let edges = map combineEdges $ groupBy isSameEdge $ nub [ (getRuleName i, getRuleName r, edgeInfo f)
                                                            | r <- rules
                                                            , f <- L.get rPrems r
                                                            , i <- possibleSourceRules thy f (L.get pracVariants (L.get rInfo r)) rules
                                                            ]

    -- Create the nodes and label them with the rule name
    -- TODO: add node-specific styling?
    nodeTab <- sequence [ do nd <- D.node [("label", n)]
                             return (n, nd)
                        | n <- nodeNames ]
    let fm = M.fromList nodeTab
    -- Create the edges between the nodes
    sequence_ [ D.edge (fm M.! src) (fm M.! dst) (toStyle sty) | (src,dst,sty) <- edges ]
    return ()
  where
    -- Given a premise fact for a graph edge, produce an edge info triple
    edgeInfo (Fact tag _) = case (factTagName tag, factTagMultiplicity tag) of
        ("In", _)          -> ("red", "dotted", "")
        (name, Persistent) -> ("blue", "dashed", name)
        (name, _)          -> ("black", "solid", name)

    -- Convert an edge info triple into a dot-style list of tuples
    toStyle (color, linetype, label) = [("color", color), ("type", linetype), ("label", label)]

    -- Compare a triple containing a src, dst, and an edge info and return true if they have the same src, dst and edge
    -- info (except the label)
    isSameEdge (src1, dst1, (col1, typ1, _)) (src2, dst2, (col2, typ2, _)) =
        src1 == src2 && dst1 == dst2 && col1 == col2 && typ1 == typ2

    -- Combine a list of src, dst, edge info triples into a single src, dst, edge info, with unique labels joined by
    -- newlines
    combineEdges es@((src, dst, (col, typ, _)):_) =
        (src, dst, (col, typ, intercalate "\n" $ nub (map getLabel es)))
      where
        getLabel (_, _, (_, _, lab)) = lab
    combineEdges [] = ("", "", ("", "", ""))

-- | Find all rules that could possibly unify with the premise fact
possibleSourceRules :: ClosedTheory -> LNFact -> Disj LNSubstVFresh -> [ProtoRuleAC] -> [ProtoRuleAC]
possibleSourceRules thy p psubs = filter $ \ru -> or [checkUnify (applyFrSubst psub p c) (applyFrSubst csub c p)
                                                     | c <- L.get rConcs ru
                                                     , psub <- getDisj psubs
                                                     , csub <- getRuleDisj ru]
  where
    getRuleDisj ru = getDisj (L.get pracVariants (L.get rInfo ru))
    applyFrSubst s t1 t2  = apply (freshToFreeAvoiding s t2) t1
    checkUnify t1 t2 = unifiable thy (renameAvoiding t1 t2) t2

-- | Check whether two rules unify. This allows In and Out facts to unify if their term argument unify.
unifiable :: ClosedTheory -> LNFact -> LNFact -> Bool
unifiable thy prem@(Fact ptag pts) conc@(Fact ctag cts) =
    case (factTagName ptag, factTagName ctag) of
      -- Allow In/Out facts to unify based on their term argument
      ("In", "Out") -> runMaude (unifiableLNTerms (head pts) (head cts))
      -- Nothing will unify with FreshFacts (which will only occur in the premises)
      ("Fr", _)     -> False
      -- Everything else must unify based on the fact tag and its term argument(s)
      (a, b)        -> if (a == b) then runMaude (unifiableLNFacts prem conc) else False
  where
    -- TODO: check whether this is creating a new instance of Maude for each unification
    runMaude = (`runReader` L.get pcMaudeHandle (getProofContext (getExistsLemma thy) thy))

-- | Find the first exists-trace lemma in the theory
-- TODO: check what happens when there is no exists-trace lemma?
getExistsLemma :: ClosedTheory -> Lemma IncrementalProof
getExistsLemma thy = head $ filter (\l -> (L.get lTraceQuantifier l) == ExistsTrace) $ getLemmas thy

