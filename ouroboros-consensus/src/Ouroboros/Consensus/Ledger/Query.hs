{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Ouroboros.Consensus.Ledger.Query (
    BlockQuery
  , Query (..)
  , QueryLedger (..)
  , QueryVersion (..)
  , ShowQuery (..)
  , answerQuery
  ) where

import           Codec.CBOR.Decoding
import           Codec.CBOR.Encoding
import           Data.Kind (Type)
import           Data.Maybe (isJust)

import           Ouroboros.Network.Protocol.LocalStateQuery.Type
                     (ShowQuery (..))

import           Ouroboros.Consensus.Ledger.Extended
import           Ouroboros.Consensus.Ledger.Query.Version
import           Ouroboros.Consensus.Node.Serialisation
                     (SerialiseNodeToClient (..), SerialiseResult (..))
import           Ouroboros.Consensus.Util (ShowProxy (..), SomeSecond (..))
import           Ouroboros.Consensus.Util.DepPair

{-------------------------------------------------------------------------------
  Queries
-------------------------------------------------------------------------------}

-- | Different queries supported by the ledger for all block types, indexed
-- by the result type.
data Query blk result = BlockQuery (BlockQuery blk result)

instance (ShowProxy (BlockQuery blk)) => ShowProxy (Query blk) where
  showProxy (Proxy :: Proxy (Query blk)) = "Query (" ++ showProxy (Proxy @(BlockQuery blk)) ++ ")"

instance (ShowQuery (BlockQuery blk)) => ShowQuery (Query blk) where
  showResult (BlockQuery blockQuery) = showResult blockQuery

instance Eq (SomeSecond BlockQuery blk) => Eq (SomeSecond Query blk) where
  SomeSecond (BlockQuery blockQueryA) == SomeSecond (BlockQuery blockQueryB)
    = SomeSecond blockQueryA == SomeSecond blockQueryB

instance Show (SomeSecond BlockQuery blk) => Show (SomeSecond Query blk) where
  show (SomeSecond (BlockQuery blockQueryA)) = "Query " ++ show (SomeSecond blockQueryA)

instance SerialiseNodeToClient blk (SomeSecond BlockQuery blk) => SerialiseNodeToClient blk (SomeSecond Query blk) where
  encodeNodeToClient codecConfig queryVersion blockVersion (SomeSecond query)
    = case queryVersion of
      TopLevelQueryDisabled -> encodeBlockQuery
        (case query of
          BlockQuery blockQuery -> blockQuery
        )
      QueryV_1 -> case query of
        BlockQuery blockQuery ->
          encodeWord8 0
          <> encodeBlockQuery blockQuery
    where
      encodeBlockQuery blockQuery = encodeNodeToClient
        @blk
        @(SomeSecond BlockQuery blk)
        codecConfig
        queryVersion
        blockVersion
        (SomeSecond blockQuery)

  decodeNodeToClient codecConfig queryVersion blockVersion
    = case queryVersion of
        TopLevelQueryDisabled -> decodeBlockQuery
        QueryV_1 -> do
          tag <- decodeWord8
          case tag of
            0 -> decodeBlockQuery
            _ -> error $ "decodeNodeToClient: SomeSecond Query blk: unknown tag " ++ show tag
    where
      decodeBlockQuery = do
        SomeSecond blockQuery <- decodeNodeToClient
          @blk
          @(SomeSecond BlockQuery blk)
          codecConfig
          queryVersion
          blockVersion
        return (SomeSecond (BlockQuery blockQuery))

instance SerialiseResult blk (BlockQuery blk) => SerialiseResult blk (Query blk) where
  encodeResult codecConfig blockVersion (BlockQuery blockQuery) result
    = encodeResult codecConfig blockVersion blockQuery result

  decodeResult codecConfig blockVersion (BlockQuery query)
    = decodeResult codecConfig blockVersion query

instance SameDepIndex (BlockQuery blk) => SameDepIndex (Query blk) where
  sameDepIndex (BlockQuery blockQueryA) (BlockQuery blockQueryB)
    = sameDepIndex blockQueryA blockQueryB

deriving instance Show (BlockQuery blk result) => Show (Query blk result)

-- | Answer the given query about the extended ledger state.
answerQuery :: QueryLedger blk => ExtLedgerCfg blk -> Query blk result -> ExtLedgerState blk -> result
answerQuery cfg query st = case query of
  BlockQuery blockQuery -> answerBlockQuery cfg blockQuery st

-- | Different queries supported by the ledger, indexed by the result type.
data family BlockQuery blk :: Type -> Type

-- | Query the ledger extended state.
--
-- Used by the LocalStateQuery protocol to allow clients to query the extended
-- ledger state.
class (ShowQuery (BlockQuery blk), SameDepIndex (BlockQuery blk)) => QueryLedger blk where

  -- | Answer the given query about the extended ledger state.
  answerBlockQuery :: ExtLedgerCfg blk -> BlockQuery blk result -> ExtLedgerState blk -> result

instance SameDepIndex (BlockQuery blk) => Eq (SomeSecond BlockQuery blk) where
  SomeSecond qry == SomeSecond qry' = isJust (sameDepIndex qry qry')

deriving instance (forall result. Show (BlockQuery blk result)) => Show (SomeSecond BlockQuery blk)
