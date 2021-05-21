{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Ouroboros.Network.Protocol.Handshake.Client
  ( handshakeClientPeer
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map

import qualified Codec.CBOR.Term     as CBOR

import           Network.TypedProtocol.Core

import           Ouroboros.Network.Protocol.Handshake.Codec
import           Ouroboros.Network.Protocol.Handshake.Type
import           Ouroboros.Network.Protocol.Handshake.Version


-- | Handshake client which offers @'Versions' vNumber vData@ to the
-- remote peer.
--
-- TODO: GADT encoding of the client (@Handshake.Client@ module).
--
handshakeClientPeer
  :: ( Ord vNumber
     )
  => VersionDataCodec CBOR.Term vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Peer (Handshake vNumber CBOR.Term)
          AsClient StPropose m
          (Either
            (HandshakeProtocolError vNumber)
            (r, vNumber, vData))
handshakeClientPeer codec@VersionDataCodec {encodeData, decodeData} acceptVersion versions =
  -- send known versions
  Yield (ClientAgency TokPropose) (MsgProposeVersions $ encodeVersions encodeData versions) $

    Await (ServerAgency TokConfirmServer) $ \msg -> case msg of
      MsgProposeVersions' versionMap -> 
        -- simultanous open; 'accept' will choose version (the greatest common
        -- version), and check if we can accept received version data.
        accept TokAsClient codec acceptVersion versions versionMap

      -- the server refused common highest version
      MsgRefuse vReason ->
        Done TokDone (Left $ HandshakeError vReason)

      -- the server accepted a version, sent back the version number and its
      -- version data blob
      MsgAcceptVersion vNumber vParams ->
        case vNumber `Map.lookup` getVersions versions of
          Nothing -> Done TokDone (Left $ NotRecognisedVersion vNumber)
          Just (Version app vData) ->
            case decodeData vNumber vParams of

              Left err ->
                Done TokDone (Left (HandshakeError $ HandshakeDecodeError vNumber err))

              Right vData' ->
                case acceptVersion vData vData' of
                  Accept agreedData ->
                    Done TokDone $ Right $ ( app agreedData
                                           , vNumber
                                           , agreedData
                                           )
                  Refuse err ->
                    Done TokDone (Left (InvalidServerSelection vNumber err))


-- | Accept a proposed version, or refuse any of them.
--
accept
  :: forall (peerRole :: PeerRole) vParams vNumber vData r m.
     Ord vNumber
  => TokPeerRole peerRole
  -> VersionDataCodec vParams vNumber vData
  -> (vData -> vData -> Accept vData)
  -> Versions vNumber vData r
  -> Map vNumber vParams
  -- ^ proposed versions received either with `MsgProposeVersions` or
  -- `MsgProposeVersions'`
  -> Peer (Handshake vNumber vParams)
          peerRole (StConfirm peerRole) m
          (Either (HandshakeProtocolError vNumber) (r, vNumber, vData))
accept role VersionDataCodec {encodeData, decodeData} acceptVersion
       versions versionMap =
    case lookupGreatestCommonKey versionMap (getVersions versions) of
      Nothing ->
        let vReason = VersionMismatch (Map.keys $ getVersions versions) []
        in Yield tok
                 (MsgRefuse vReason)
                 (Done TokDone (Left $ HandshakeError vReason))

      Just (vNumber, (vParams, Version app vData)) ->
          case decodeData vNumber vParams of
            Left err ->
              let vReason = HandshakeDecodeError vNumber err
              in Yield tok
                       (MsgRefuse vReason)
                       (Done TokDone $ Left $ HandshakeError vReason)

            Right vData' ->
              case acceptVersion vData vData' of

                -- We agree on the version; send back the agreed version
                -- number @vNumber@ and encoded data associated with our
                -- version.
                Accept agreedData ->
                  Yield tok
                        (MsgAcceptVersion vNumber (encodeData vNumber agreedData))
                        (Done TokDone $ Right $
                          ( app agreedData
                          , vNumber
                          , agreedData
                          ))

                -- We disagree on the version.
                Refuse err ->
                  let vReason = Refused vNumber err
                  in Yield tok
                           (MsgRefuse vReason)
                           (Done TokDone $ Left $ HandshakeError vReason)
  where
    tok = case role of
      TokAsClient -> ClientAgency TokConfirmClient
      TokAsServer -> ServerAgency TokConfirmServer


lookupGreatestCommonKey :: Ord k => Map k a -> Map k b -> Maybe (k, (a, b))
lookupGreatestCommonKey l r = Map.lookupMax $ Map.intersectionWith (,) l r


encodeVersions
  :: forall vNumber r vParams vData.
     (vNumber -> vData -> vParams)
  -> Versions vNumber vData r
  -> Map vNumber vParams
encodeVersions encoder (Versions vs) = go `Map.mapWithKey` vs
    where
      go :: vNumber -> Version vData r -> vParams
      go vNumber Version {versionData} = encoder vNumber versionData
