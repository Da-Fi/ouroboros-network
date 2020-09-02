{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Ouroboros.Consensus.Cardano.Condense () where

import           Ouroboros.Consensus.HardFork.Combinator.Condense

import           Ouroboros.Consensus.Byron.Ledger

import           Ouroboros.Consensus.Shelley.Ledger

import           Ouroboros.Consensus.Cardano.Block

import           Cardano.Ledger.Crypto (Crypto)

{-------------------------------------------------------------------------------
  Condense

  TODO where to put this?
-------------------------------------------------------------------------------}

instance CondenseConstraints ByronBlock

instance Era era => CondenseConstraints (ShelleyBlock era)

instance Crypto c => CondenseConstraints (CardanoBlock c)
