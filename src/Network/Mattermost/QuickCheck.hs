module Network.Mattermost.QuickCheck where

import qualified Data.Sequence as Seq
import           Network.Mattermost.Types
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary PostId    where arbitrary = PI . Id <$> arbitrary
instance Arbitrary ChannelId where arbitrary = CI . Id <$> arbitrary
instance Arbitrary FileId    where arbitrary = FI . Id <$> arbitrary
instance Arbitrary UserId    where arbitrary = UI . Id <$> arbitrary

instance Arbitrary Type where
    arbitrary = oneof [ return Ordinary
                      , return Direct
                      , return Private
                      , return Group
                      , return SystemHeaderChange
                      , Unknown <$> arbitrary
                      ]

instance Arbitrary PostProps where
    arbitrary = sized $ \s -> PostProps
                             <$> arbitrary <*> arbitrary <*> attached s
                             <*> arbitrary <*> arbitrary


attached :: Int -> Gen (Maybe (Seq.Seq PostPropAttachment))
attached s = oneof [ return Nothing
                   , Just <$> (resize s arbitrary)
                   ]

instance Arbitrary PostPropAttachment where
    arbitrary = PostPropAttachment <$> arbitrary <*> arbitrary

instance Arbitrary Post where
    arbitrary = Post
                <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
