{-# LANGUAGE OverloadedStrings #-}
module NegativeCacheSpec where

import DNS.RRCache
import qualified DNS.Types as DNS
import DNS.Types.Time (EpochTime)
import Test.Hspec

spec :: Spec
spec = describe "negative cache ranking" $
    it "degrades rank for NoSOA results" $ do
        let dom = DNS.fromRepresentation "example.com."
            key = DNS.Question dom DNS.A DNS.IN
            now = 0 :: EpochTime
            ttl = 60
            rank = RankAuthAnswer
            Just cache1 = insert now key ttl (negNoSOA DNS.NoErr) rank (empty 10)
        lookupEither now dom DNS.A DNS.IN cache1
            `shouldBe` Just (Left ([], RankAdditional), RankAdditional)
