-- |
-- This module defines the 'Value' type to represent any valid Bencode value.
--
-- Generally you will want to write decoders and encoders to work with your
-- own types. See "Data.Bencode.Decode" and "Data.Bencode.Encode" to get
-- started.
--
module Data.Bencode.Type
  ( Value(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V

-- | A type that can represent any Bencode value.
data Value
  = String !B.ByteString
  | Integer !Integer
  | List !(V.Vector Value)
  | Dict !(M.Map B.ByteString Value)
  deriving (Eq, Ord, Show)
