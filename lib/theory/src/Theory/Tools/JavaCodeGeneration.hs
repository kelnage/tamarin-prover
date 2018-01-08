-- |
-- Copyright   : (c) 2010-2012 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Maintainer  : Simon Meier <iridcode@gmail.com>
-- Portability : GHC only
--
-- Produce Java classes for a Tamarin model
module Theory.Tools.JavaCodeGeneration (
    generateJavaCode
  ) where

import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import           Data.Char
import           Data.Maybe
import qualified Data.Set               as S

import qualified Extension.Data.Label   as L
import qualified Text.PrettyPrint.Class as Pretty

import           Language.Java.Pretty   as JP
import           Language.Java.Syntax   as Java

-- import           Term.Term.FunctionSymbols

import           Theory
import           Term.Maude.Signature

import           Debug.Trace

generateJavaCode :: ClosedTheory -> [(String, String)]
generateJavaCode thy =
  [
    ("src/tamarin/protocols/" ++ lcThyName thy ++  "/Primitive.java", generateJavaPrimitives thy)
  ]

generateJavaPrimitives :: ClosedTheory -> String
generateJavaPrimitives thy = JP.prettyPrint $ generatePrimitiveInterface thy

generatePrimitiveInterface :: ClosedTheory -> Java.CompilationUnit
generatePrimitiveInterface thy = trace (show $ functions thy) baseUnit thy
    [Java.InterfaceTypeDecl $ Java.InterfaceDecl [Java.Public] (Ident "Primitive") [] [] $
        Java.InterfaceBody $ generatePrimitiveMethods thy]

generatePrimitiveMethods :: ClosedTheory -> [Java.MemberDecl]
generatePrimitiveMethods thy = fnToMethod <$> functions thy
  where
    fnToMethod (name, arity) = MethodDecl
        [Java.Public,Java.Abstract] 
        []
        (Just objectType)
        (Java.Ident name)
        (generateParams arity)
        []
        (MethodBody Nothing)
    generateParams arity = generateParam <$> [1..arity]
    generateParam  ind   = FormalParam [] objectType False (varName $ "arg" ++ show ind)

baseUnit :: ClosedTheory -> [Java.TypeDecl] -> Java.CompilationUnit
baseUnit thy defs = Java.CompilationUnit
                        (Just $ Java.PackageDecl $ qualName ["tamarin", "protocols", lcThyName thy])
                        [Java.ImportDecl False (qualName ["javax", "crypto"]) True]
                        defs

functions :: ClosedTheory -> [(String, Int)]
functions thy = getValues <$> (S.toList $ noEqFunSyms $ L.get sigpMaudeSig $ toSignaturePure $ L.get thySignature thy)
  where
    getValues (name, (arity, vis)) = (BC.unpack name, arity)

lcThyName :: ClosedTheory -> String
lcThyName thy = toLower <$> L.get thyName thy

varName :: String -> Java.VarDeclId
varName s = Java.VarId $ Java.Ident s

qualName :: [String] -> Java.Name
qualName ss = Java.Name $ Java.Ident <$> ss

objectType :: Java.Type
objectType = Java.RefType $ Java.ClassRefType $ Java.ClassType $ [(Java.Ident "Object", [])]

