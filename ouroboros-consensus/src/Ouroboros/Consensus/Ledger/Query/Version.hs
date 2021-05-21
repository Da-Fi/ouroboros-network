
module Ouroboros.Consensus.Ledger.Query.Version (QueryVersion (..)) where

-- | Version of the `Query blk` type.
data QueryVersion
  = TopLevelQueryDisabled
  | QueryV_1
  deriving (Eq, Ord, Enum, Bounded, Show)
