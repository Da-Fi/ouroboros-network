{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Server
  ( handshakeServerPeer
  ) where

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Client
import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Proofs
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Server following the handshake protocol; it accepts highest version offered
-- by the peer that also belongs to the server @versions@.
--
handshakeServerPeer
  :: ( Functor m
     , Ord vNumber
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsServer StPropose m
          (Either (HandshakeProtocolError vNumber) (r, vNumber, vData))
handshakeServerPeer codec acceptVersion versions =
    proposition_handshake_duality (handshakeClientPeer codec acceptVersion versions)
