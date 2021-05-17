
module Ouroboros.Consensus.Ledger.Query.Version (QueryVersion (..)) where


data QueryVersion = TopLevelQueryDisabled
  deriving (Eq, Ord, Enum, Bounded, Show)
