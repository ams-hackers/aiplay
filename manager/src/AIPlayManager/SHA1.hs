{-# LANGUAGE FlexibleInstances #-}

module AIPlayManager.SHA1
  ( SHA1
  , sha1LBS
  , sha1BS
  , sha1File
  ) where

import Data.ByteArray (convert)
import Database.PostgreSQL.Simple.FromField
  ( FieldParser
  , FromField
  , ResultError(..)
  , fromField
  , returnError
  )
import Database.PostgreSQL.Simple.ToField (ToField, toField)
import Database.PostgreSQL.Simple.Types (Binary(..))

import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | SHA1 Digest.
newtype SHA1 =
  SHA1 (Hash.Digest Hash.SHA1)
  deriving (Eq, Show)

-- | Compute the hash of a lazy ByteString
sha1BS :: BS.ByteString -> SHA1
sha1BS = SHA1 . Hash.hash

-- | Compute the hash of a lazy ByteString
sha1LBS :: LBS.ByteString -> SHA1
sha1LBS = SHA1 . Hash.hashlazy

-- | Compute the hash of a file lazily.
sha1File :: FilePath -> IO SHA1
sha1File file = sha1LBS <$> LBS.readFile file

-- A Hash is serialized as a bytea when it is about to be written to
-- the database.
instance ToField SHA1 where
  toField (SHA1 hash) = toField $ Binary (convert hash :: BS.ByteString)

instance FromField SHA1 where
  fromField f mdata = do
    bs <- (fromField :: FieldParser BS.ByteString) f mdata
    case Hash.digestFromByteString bs of
      Just hash -> return $ SHA1 hash
      Nothing -> returnError ConversionFailed f "Field doesn't contain a SHA1"
