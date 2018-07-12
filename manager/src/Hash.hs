{-# LANGUAGE FlexibleInstances #-}

module Hash
  ( Hash
  , shaFile
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

-- Hash type
--
-- Represents the hash of a migration.
--
newtype Hash =
  Hash (Hash.Digest Hash.SHA1)
  deriving (Eq)

-- | Compute the hash of a file lazily
shaFile :: FilePath -> IO Hash
shaFile file = Hash <$> Hash.hashlazy <$> LBS.readFile file

-- A Hash is serialized as a bytea when it is about to be written to
-- the database.
instance ToField Hash where
  toField (Hash hash) = toField $ Binary (convert hash :: BS.ByteString)

instance FromField Hash where
  fromField f mdata = do
    bs <- (fromField :: FieldParser BS.ByteString) f mdata
    case Hash.digestFromByteString bs of
      Just hash -> return $ Hash hash
      Nothing -> returnError ConversionFailed f "Field doesn't contain a SHA1"
