{-# LANGUAGE OverloadedStrings #-}
module Zbot.Metrics (
    metricErrors
,   metricHandlerSeconds
,   metricReceivedMessages
,   metricSentMessages
)   where

import Data.List (sort)

import qualified Prometheus as P


{-# NOINLINE metricErrors #-}
metricErrors :: P.Vector P.Label1 P.Counter
metricErrors =
    P.unsafeRegisterIO $ return $ P.vector "service" $ P.counter $
        P.Info "zbot_errors" "The number of errors caught and suppressed."

{-# NOINLINE metricHandlerSeconds #-}
metricHandlerSeconds :: P.Vector P.Label1 P.Histogram
metricHandlerSeconds =
    P.unsafeRegisterIO $ do
      let info = P.Info
            "zbot_handler_duration_seconds"
            "The number of seconds spent executing each service."
      let buckets = sort $ map (\i -> 0.000002 * (2 ** i)) [0..20]
      return $ P.vector "service" $ P.histogram info buckets

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
