module Network.Mattermost.QuickCheck where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Network.Mattermost.Types
import           Test.QuickCheck


genText :: Gen T.Text
genText = oneof [ return T.empty
                 , return $ T.singleton 'a'
                 , return $ T.singleton '1'
                 , return $ T.pack "b2"
                 , return $ T.singleton ' '
                 , return $ T.singleton '\n'
                 , return $ T.singleton '\r'
                 , return $ T.singleton '\t'
                 , return $ T.pack " \n\r\t"
                 , T.pack <$> arbitrary
                 ]

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = frequency [ (1, return Nothing)
                       , (11, Just <$> g)
                       ]

genSeq :: Gen a -> Gen (Seq.Seq a)
genSeq g = frequency [ (1, return Seq.empty)
                     , (9, Seq.fromList <$> listOf g)
                     ]

genTime :: Gen UTCTime
genTime = UTCTime
           <$> (ModifiedJulianDay <$> (2000 +) <$> arbitrary)
           <*> (secondsToDiffTime <$> choose (0, 86400))

genPostId :: Gen PostId
genPostId = PI . Id <$> genText

genChannelId :: Gen ChannelId
genChannelId = CI . Id <$> genText

genFileId :: Gen FileId
genFileId = FI . Id <$> genText

genUserId :: Gen UserId
genUserId = UI . Id <$> genText

genType :: Gen Type
genType = oneof [ return Ordinary
                , return Direct
                , return Private
                , return Group
                , return SystemHeaderChange
                , Unknown <$> genText
                ]

genPostProps :: Gen PostProps
genPostProps = PostProps
               <$> genMaybe genText
               <*> genMaybe genText
               <*> attached
               <*> genMaybe genText
               <*> genMaybe genText


attached :: Gen (Maybe (Seq.Seq PostPropAttachment))
attached = oneof [ return Nothing
                 , Just <$> (genSeq genPostPropAttachment)
                 ]

genPostPropAttachment :: Gen PostPropAttachment
genPostPropAttachment = PostPropAttachment <$> genText <*> genText

genPost :: Gen Post
genPost = Post
          <$> genMaybe genPostId
          <*> genMaybe genPostId
          <*> genPostProps
          <*> genText
          <*> genSeq genFileId
          <*> genPostId
          <*> genType
          <*> genText
          <*> genMaybe genTime
          <*> genText
          <*> genTime
          <*> genMaybe genUserId
          <*> genTime
          <*> genMaybe genPostId
          <*> genChannelId
          <*> arbitrary
