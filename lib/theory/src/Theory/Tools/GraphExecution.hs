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

import qualified Data.Set        as S
import           Data.List       (nub)
import qualified Data.Map        as M
import           Data.Monoid     (Any(..))

import qualified Text.Dot        as D

import           Control.Basics
import           Control.Monad.Reader
import           Control.Monad.State      (StateT, evalStateT)

import qualified Extension.Data.Label as L

import           Theory
import           Theory.Model
import           Term.Unification

-- import           Debug.Trace

-- | Output a dot-graph of all the possible connections for a theory's rules
graphTheoryExec :: ClosedTheory -> String
graphTheoryExec thy = D.showDot (createExecutionGraph thy)

-- | Given a theory, take its rules and for every premise, check which rules have conclusions that can unify
-- with that premise. For each of those rules, and an edge to the graph.
-- TODO: allow rules to be automatically excluded from the graph based on a rule name pattern? (Such as reveal)
createExecutionGraph :: ClosedTheory -> D.Dot ()
createExecutionGraph thy = do
    let rules = getProtoRuleEs thy
    let nodeNames = map getRuleName rules
    -- For each rule, and every fact in that rules premises, find all conclusion facts that unify with the premise
    -- fact. Add dot-specific styling depending on fact type.
    let edges = nub [(getRuleName i, getRuleName r, edgeStyle f) | r <- rules
                                                                 , f <- (L.get rPrems r)
                                                                 , i <- possibleSourceRules thy f rules]
    -- Create the nodes and label them with the rule name
    -- TODO: add node-specific styling?
    nodeTab <- sequence [ do nd <- D.node [("label", n)]
                             return (n, nd)
                        | n <- nodeNames ]
    let fm = M.fromList nodeTab
    -- Create the edges between the nodes
    sequence_ [ D.edge (fm M.! src) (fm M.! dst) sty | (src,dst,sty) <- edges ]
    return ()
  where
    edgeStyle (Fact tag ts) = case (factTagName tag, factTagMultiplicity tag) of
        ("In", _)          -> [("color", "red"), ("style", "dotted")]
        (_,    Persistent) -> [("color", "blue"), ("style", "dashed")]
        _                  -> [("color", "black")]

-- | Find all rules that could possibly unify with the premise fact
possibleSourceRules :: ClosedTheory -> LNFact -> [ProtoRuleE] -> [ProtoRuleE]
possibleSourceRules thy p = filter $ \ru -> any (\c -> unifiable thy p c) (L.get rConcs ru)

-- | Check whether two rules unify. This allows In and Out facts to unify if their term argument unify.
unifiable :: ClosedTheory -> LNFact -> LNFact -> Bool
unifiable thy prem@(Fact ptag pts) conc@(Fact ctag cts) =
    case (factTagName ptag, factTagName ctag) of
      -- Allow In/Out facts to unify based on their term argument
      ("In", "Out") -> (runMaude (unifiableLNTerms (head pts) (head cts)))
      -- Everything else must unify based on the whole fact
      (_, _)        -> (runMaude (unifiableLNFacts prem conc))
  where
    -- TODO: check whether this is creating a new instance of Maude for each unification
    runMaude = (`runReader` L.get pcMaudeHandle (getProofContext (getExistsLemma thy) thy))

-- | Find the first exists-trace lemma in the theory
-- TODO: check what happens when there is no exists-trace lemma?
getExistsLemma :: ClosedTheory -> Lemma IncrementalProof
getExistsLemma thy = head $ filter (\l -> (L.get lTraceQuantifier l) == ExistsTrace) $ getLemmas thy

