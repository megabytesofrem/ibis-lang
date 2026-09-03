module Ibis.Typecheck.Unify.HasFMV where

import Ibis.Syntax.AST.Core (CoreTerm)
import qualified Ibis.Syntax.AST.Core as Core
import Ibis.Typecheck.Unify.Types (Entry (..), Equation (..), MetaVar (..), Problem (..))

class HasFMV a where
  fmv :: a -> [MetaVar]

instance HasFMV CoreTerm where
  fmv (Core.Universe _) = []
  fmv (Core.Const _) = []
  fmv (Core.MVar m) = [MetaVar m]
  fmv (Core.Var _) = []
  fmv (Core.Lit _) = []
  fmv (Core.Unit) = []
  fmv (Core.Pi _ ty body) = fmv ty ++ fmv body
  fmv (Core.Lam _ body) = fmv body
  fmv (Core.App f x) = fmv f ++ fmv x
  fmv (Core.Sigma _ ty body) = fmv ty ++ fmv body
  fmv (Core.Pair a b) = fmv a ++ fmv b
  fmv (Core.Fst p) = fmv p
  fmv (Core.Snd p) = fmv p
  fmv (Core.Let _ e body) = fmv e ++ fmv body
  fmv (Core.Ann e ty) = fmv e ++ fmv ty
  fmv (Core.Match scrut branches) = fmv scrut ++ concatMap (fmv . snd) branches
  fmv _ = []

instance (HasFMV a) => HasFMV [a] where
  fmv = concatMap fmv

instance HasFMV Entry where
  fmv (BVar _ ty) = fmv ty
  fmv (BDef _ ty val) = fmv ty ++ fmv val
  fmv (Meta m ty mval) = MetaVar (unMetaVar m) : fmv ty ++ maybe [] fmv mval
  fmv (Prob _ p) = fmv p

instance HasFMV Equation where
  fmv (Equation ty lhs rhs) = fmv ty ++ fmv lhs ++ fmv rhs

instance HasFMV Problem where
  fmv (Problem _ _ eq) = fmv eq