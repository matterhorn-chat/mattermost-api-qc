module Network.Mattermost.QuickCheck where

import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock (UTCTime(..), secondsToDiffTime)
import           Network.Mattermost.Types
import           Test.QuickCheck


genUserText :: Gen UserText
genUserText = UserText <$> genText

genText :: Gen T.Text
genText = sized $ \s ->
          oneof [ return T.empty
                 , return $ T.singleton 'a'
                 , return $ T.singleton '1'
                 , return $ T.pack "b2"
                 , return $ T.singleton ' '
                 , return $ T.singleton '\n'
                 , return $ T.singleton '\r'
                 , return $ T.singleton '\t'
                 , return $ T.pack " \n\r\t"
                 , T.pack <$> vectorOf s arbitrary
                 ]

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = frequency [ (1, return Nothing)
                       , (11, Just <$> g)
                       ]

genSeq :: Gen a -> Gen (Seq.Seq a)
genSeq g = sized $ \s ->
           frequency [ (1, return Seq.empty)
                     , (9, Seq.fromList <$> vectorOf s g)
                     ]

genTime :: Gen ServerTime
genTime = ServerTime <$>
          (UTCTime
           <$> (ModifiedJulianDay <$> (2000 +) <$> arbitrary)
           <*> (secondsToDiffTime <$> choose (0, 86400)))

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
                , Unknown <$> genText
                ]

genPostProps :: Gen PostProps
genPostProps = PostProps
               <$> genMaybe genText
               <*> genMaybe genText
               <*> genMaybe arbitrary
               <*> attached
               <*> genMaybe genText
               <*> genMaybe genText


attached :: Gen (Maybe (Seq.Seq PostPropAttachment))
attached = oneof [ return Nothing
                 , Just <$> (genSeq genPostPropAttachment)
                 ]

genPostPropAttachmentField :: Gen PostPropAttachmentField
genPostPropAttachmentField =
  PostPropAttachmentField <$> genText
                          <*> genText
                          <*> oneof [ return True, return False ]

genPostPropAttachment :: Gen PostPropAttachment
genPostPropAttachment = PostPropAttachment
                        <$> arbitrary
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genSeq genPostPropAttachmentField
                        <*> genText
                        <*> genText
                        <*> genText
                        <*> genText

genPostType :: Gen PostType
genPostType = oneof [ return PostTypeJoinChannel
                    , return PostTypeLeaveChannel
                    , return PostTypeAddToChannel
                    , return PostTypeRemoveFromChannel
                    , return PostTypeHeaderChange
                    , return PostTypeDisplayNameChange
                    , return PostTypePurposeChange
                    , return PostTypeChannelDeleted
                    , return PostTypeEphemeral
                    , PostTypeUnknown <$> genText
                    ]

genMetadata :: Gen PostMetadata
genMetadata =
    return $ PostMetadata mempty mempty

genPost :: Gen Post
genPost = Post
          <$> genMaybe genPostId
          <*> genMaybe genPostId
          <*> genPostProps
          <*> genMaybe genPostId
          <*> genSeq genFileId
          <*> genPostId
          <*> genPostType
          <*> genUserText
          <*> genMaybe genTime
          <*> genText
          <*> genTime
          <*> genTime
          <*> genMaybe genUserId
          <*> genTime
          <*> genChannelId
          <*> arbitrary
          <*> genMaybe arbitrary
          <*> genMetadata
