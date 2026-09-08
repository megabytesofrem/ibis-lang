-- | Generic Control Flow Graph abstractions.
module Ibis.AST.CFG where

type BBId = Int

data Terminator e
  = Goto BBId -- Unconditional jump: goto label;
  | Branch e BBId BBId -- Conditional branch: if (cond) goto L1; else goto L2;
  | Ret (Maybe e) -- Return statement: return val;
  | Unreachable -- Unreachable instruction
  deriving (Show, Eq)

-- | Generic instruction taxonomy for low-level basic blocks.
--   't' represents target types, 'e' represents target expressions.
data Instruction t e
  = Declare String t -- Variable declaration: T var;
  | Assign String e -- Variable assignment: var = expr;
  | Store e e -- Memory write: *ptr = val;
  | Call (Maybe String) e [e] -- Function call: [res =] fn(args...);
  | Comment String -- Inline target metadata/comment
  deriving (Show, Eq)

data BasicBlock t e = BasicBlock
  { bbId :: BBId
  , instructions :: [Instruction t e]
  , terminator :: Terminator e
  }
  deriving (Show, Eq)

data CFG t e = CFG
  { entryBlock :: BBId
  , blocks :: [BasicBlock t e]
  }
  deriving (Show, Eq)