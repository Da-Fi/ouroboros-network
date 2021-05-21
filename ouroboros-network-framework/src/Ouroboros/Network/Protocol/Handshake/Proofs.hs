{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

module Ouroboros.Network.Protocol.Handshake.Proofs
  ( proposition_handshake_duality
  , proposition_clientAsServer
  , connect_clients
  ) where

import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Proofs
import           Ouroboros.Network.Protocol.Handshake.Type


type Shift :: Handshake vNumber vParams -> Handshake vNumber vParams
type family Shift st where
  Shift StPropose            = StPropose
  Shift (StConfirm AsServer) = StPropose
  Shift (StConfirm AsClient) = StConfirm AsServer
  Shift  StDone              = StDone


lemma :: forall vNumber vParams st m a.
         Functor m
      => Peer (Handshake vNumber vParams) AsClient        st  m a
      -> Peer (Handshake vNumber vParams) AsServer (Shift st) m a
lemma (Effect mnext) = Effect (lemma <$> mnext)

lemma (Await (ServerAgency TokConfirmServer) k) =
  Await (ClientAgency TokPropose) $ \(MsgProposeVersions vMap) ->
    lemma (k (MsgProposeVersions' vMap))

lemma (Yield (ClientAgency TokConfirmClient) (MsgAcceptVersion vNumber vParam) k) =
  Yield (ServerAgency TokConfirmServer) (MsgAcceptVersion vNumber vParam) (lemma k)

lemma (Yield (ClientAgency TokConfirmClient) (MsgRefuse reason) k) =
  Yield (ServerAgency TokConfirmServer) (MsgRefuse reason) (lemma k)

-- Ignore sending 'MsgProposeVersions'; in simultanous open the opening
-- message of the client which becomes a server is ignored (but only on its
-- side).
lemma (Yield (ClientAgency TokPropose) (MsgProposeVersions _) k) = lemma k

lemma (Done TokDone a) = Done TokDone a


proposition_handshake_duality
    :: forall vNumber vParams m a.
       Functor m
    => Peer (Handshake vNumber vParams) AsClient StPropose m a
    -> Peer (Handshake vNumber vParams) AsServer StPropose m a
proposition_handshake_duality = lemma


-- | Translate a client into a server.  The difference with
-- 'proposition_handshake_duality' is that it does not need to unwrap the top
-- level @Yield (ClientAgency TokPropose) ...@
--
proposition_clientAsServer
    :: forall vNumber vParams m a.
       Functor m
    => Peer (Handshake vNumber vParams) AsClient (StConfirm AsServer) m a
    -> Peer (Handshake vNumber vParams) AsServer StPropose            m a
proposition_clientAsServer = lemma


-- | A proof that one can connect two clients.
--
-- Note: the server side is rewritten, and it will not send initial
-- `MsgProposeVersions`.  In simultaneuos tcp open the client would send it but
-- at the next step it would need to transform into the server, thus this
-- proves that 'Handshake' can handle simultaneous tcp open.
--
connect_clients :: Monad m
                => Peer (Handshake vNumber vParams) AsClient StPropose m a
                -> Peer (Handshake vNumber vParams) AsClient StPropose m b
                -> m (a, b, TerminalStates (Handshake vNumber vParams))
connect_clients client client' =
    connect client (proposition_handshake_duality client')
