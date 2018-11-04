{-# LANGUAGE OverloadedStrings #-}
module Zbot.Metrics (
    metricErrors
,   metricHandlerSeconds
,   metricReceivedMessages
,   metricSentMessages
)   where

import qualified Prometheus as P


{-# NOINLINE metricErrors #-}
metricErrors :: P.Vector P.Label1 P.Counter
metricErrors =
    P.unsafeRegisterIO $ return $ P.vector "service" $ P.counter $
        P.Info "zbot_errors" "The number of errors caught and suppressed."

{-# NOINLINE metricHandlerSeconds #-}
metricHandlerSeconds :: P.Vector P.Label1 P.Counter
metricHandlerSeconds =
    P.unsafeRegisterIO $ return $ P.vector "service" $ P.counter $
        P.Info
            "zbot_handler_seconds_total"
            "The number of seconds spent executing each service."

{-# NOINLINE metricReceivedMessages #-}
metricReceivedMessages :: P.Counter
metricReceivedMessages = P.unsafeRegisterIO $ return $ P.counter $
    P.Info
        "zbot_irc_received_messages"
        "The number of IRC messages received by the IRC bot."

{-# NOINLINE metricSentMessages #-}
metricSentMessages :: P.Vector P.Label1 P.Counter
metricSentMessages =
    P.unsafeRegisterIO $ return $ P.vector "priority" $ P.counter $
        P.Info
            "zbot_irc_sent_messages"
            "The number of IRC messages sent by the IRC bot."
