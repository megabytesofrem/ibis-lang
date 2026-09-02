{-# LANGUAGE ImportQualifiedPost #-}

module ElabTests (elabTests) where

import Test.Tasty (TestTree)
import Test.Tasty.Hspec
import Text.Megaparsec (eof, parse)

import Control.Monad (unless)
import Data.Either (isLeft)
import Ibis.Syntax.AST.Core qualified as Core
import Ibis.Syntax.Parser (pDecl, pExpr)
import Ibis.Typecheck.Elab (elabDecl, elabTerm)
import Ibis.Typecheck.Types (emptyElabCtx, runElaboration)

import Test.Hspec

elabTests :: IO TestTree
elabTests = testSpec "ElabTree" spec

-- Port of Rust's unwrap function for testing purposes
unwrap :: (Show a) => Either a b -> b
unwrap (Left x) = error $ "unwrap: Left value: " ++ show x
unwrap (Right x) = x

spec :: Spec
spec =
  describe "ElabTests" $ do
    it "maps named universes to numeric levels" $ do
      let result = parse (pExpr <* eof) "input" "Type u"
      unless (isLeft result) $ do
        let term = unwrap result
        let result' = runElaboration (elabTerm term) emptyElabCtx
        case result' of
          Left err -> expectationFailure $ show err
          Right (Core.Universe 1, _) -> pure ()
          Right (got, _) ->
            expectationFailure ("expected Type u to elaborate to Universe 1, got: " ++ show got)

    it "keeps explicit numeric universe levels unchanged" $ do
      let result = parse (pExpr <* eof) "input" "Type 7"
      unless (isLeft result) $ do
        let term = unwrap result
        let result' = runElaboration (elabTerm term) emptyElabCtx
        case result' of
          Left err -> expectationFailure $ show err
          Right (Core.Universe 7, _) -> pure ()
          Right (got, _) ->
            expectationFailure ("expected Type 7 to elaborate to Universe 7, got: " ++ show got)

    it "elaborates structs into inductive types" $ do
      let result = parse (pDecl <* eof) "input" $ do
            unlines
              [ "struct Point (x : Type u, y : Type u) f : Type u"
              , "  x : Nat"
              , "  y : Nat"
              ]

      -- Check that the elaboration produces the expected constructors
      unless (isLeft result) $ do
        let term = unwrap result
        let result' = runElaboration (elabDecl term) emptyElabCtx
        case result' of
          Left err -> expectationFailure $ show err
          Right (decls, _) -> case decls of
            [Core.CoreInductive _ _ _] -> pure ()
            [got] ->
              expectationFailure ("expected struct to elaborate to InductiveType, got: " ++ show got)
            _ -> expectationFailure "expected a single declaration"

    it "elaborates an inductive type" $ do
      let result =
            parse (pDecl <* eof) "input" $
              unlines
                [ "inductive Vect (A : Type u) : Nat -> Type u where"
                , "  zero : Vect A 0"
                , "  succ : (n : Nat) -> A -> Vect A n -> Vect A (n + 1)"
                ]

      -- Check that the elaboration produces the expected constructors
      unless (isLeft result) $ do
        let term = unwrap result
        let result' = runElaboration (elabDecl term) emptyElabCtx
        case result' of
          Left err -> expectationFailure $ show err
          Right (decls, _) -> case decls of
            [Core.CoreInductive _ _ ctors] -> do
              let ctorNames = map fst ctors
              unless ("zero" `elem` ctorNames && "succ" `elem` ctorNames) $
                expectationFailure ("expected constructors zero and succ, got: " ++ show ctorNames)
            [got] ->
              expectationFailure ("expected inductive type to elaborate to CoreInductive, got: " ++ show got)
            _ -> expectationFailure "expected a single declaration"

    it "generates projection functions for struct fields" $ do
      let result = parse (pDecl <* eof) "input" "struct Point (x : Type u, y : Type u) f : Type u"
      unless (isLeft result) $ do
        let term = unwrap result
        let result' = runElaboration (elabDecl term) emptyElabCtx
        case result' of
          Left err -> expectationFailure $ show err
          Right (decls, _) -> case decls of
            [Core.CoreInductive _ _ ctors] -> do
              let projectionNames = map fst ctors

              unless ("x" `elem` projectionNames && "y" `elem` projectionNames) $
                expectationFailure ("expected projection functions for fields x and y, got: " ++ show projectionNames)
            [got] ->
              expectationFailure ("expected struct to elaborate to InductiveType, got: " ++ show got)
            _ -> expectationFailure "expected a single declaration"