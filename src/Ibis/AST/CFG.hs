-- Converts the entire AST into a control flow graph using basic blocks and C like control flow.
module Ibis.AST.CFG where

type BBId = Int

data Terminator
  = Goto BBId -- Unconditional jump to a basic block
  | Branch BBId BBId -- Conditional branch to two basic blocks
  | Return -- Return from the function

data BasicBlock a = BasicBlock
  { bbId :: BBId
  , instructions :: [a] -- Instructions in the basic block
  , terminator :: Terminator
  }