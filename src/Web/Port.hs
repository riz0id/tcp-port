{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

-- |
-- Module      :  Web.Port
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Port' type, which represents a TCP port number. 
-- Facilities for reading, writing, and constructing 'Port' values are also 
-- provided here.
--
-- @since 1.0.0
module Web.Port
  ( -- * Port
    Port (Port#, getPort#, ..)
    -- ** Conversion
  , PortOutOfBoundsError (..)
  , integralToPort
  , unsafeIntegralToPort
  , word8ToPort 
  , word16ToPort
    -- ** Read
  , PortParseError (..)
  , PortParseErrorInfo (..)
  , portFromLazyByteString
  , portFromByteString
  , portFromString
    -- ** Write
  , portToBuilder
  , portToLazyByteString
  , portToByteString
    -- ** Query
  , isValidPort
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteString
import Data.ByteString.Lazy qualified as Lazy (ByteString)
import Data.ByteString.Lazy qualified as Lazy.ByteString
import Data.Data (Data)

import GHC.Exts (Word16#)
import GHC.Exts qualified as GHC
import GHC.Generics (Generic)
import GHC.Word (Word16 (W16#), Word8 (W8#))

import Language.Haskell.TH.Syntax (Lift)

-- Port ------------------------------------------------------------------------

-- | The 'Port' type represents a TCP port number. Any unsigned 16-bit integer
-- (i.e. any number from 0 to 65535) is a valid port number.
--
-- @since 1.0.0
newtype Port
  = Port { getPort :: Word16 }
  deriving newtype (Enum, Eq, Num, Ord, Show)
  deriving stock (Data, Generic, Lift)

-- | TODO: docs 
--
-- @since 1.0.0
pattern Port# :: Word16# -> Port
pattern Port# {getPort#} = Port (W16# getPort#)

{-# COMPLETE Port# #-}

-- Port - Conversion -----------------------------------------------------------

-- | 'PortOutOfBoundsError' represents an out-of-bounds errors that can occur
-- when converting an arbitrary 'Integral' value to a 'Port' number.
--
-- @since 1.0.0
data PortOutOfBoundsError
  = PortOverflowError
  -- ^ 'PortOverflowError' indicates that the given 'Integral' value was greater
  -- than the maximum valid TCP port number (65535).
  | PortUnderflowError
  -- ^ 'PortUnderflowError' indicates that the given 'Integral' value was less 
  -- than the minimum valid TCP port number (0).
  deriving (Bounded, Data, Enum, Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Show PortOutOfBoundsError where 
  show PortOverflowError  = "out-of-bounds error (port overflow)"
  show PortUnderflowError = "out-of-bounds error (port underflow)"
  {-# INLINE show #-}

-- | Convert an 'Integral' value to a 'Port' if the given value is a valid port.
-- Otherwise, 'integralToPort' will return a 'PortOutOfBoundsError' describing
-- why the given value could no be converted to a 'Port'.
--
-- @since 1.0.0
integralToPort :: Integral a => a -> Either PortOutOfBoundsError Port
integralToPort num = 
  case isValidPort num of 
    Nothing  -> Right (fromIntegral num)
    Just exn -> Left exn

-- | Unsafe conversion from an arbitrary 'Integral' value to a 'Port'. An 
-- unchecked exception will be raised if the given value is not a valid port.
--
-- @since 1.0.0
unsafeIntegralToPort :: Integral a => a -> Port
unsafeIntegralToPort num = 
  case integralToPort num of 
    Left  exn  -> errorWithoutStackTrace (show exn)
    Right port -> port 
{-# INLINEABLE unsafeIntegralToPort #-}

-- | Cast a 'Word8' directly to a 'Port'. 
--
-- @since 1.0.0
word8ToPort :: Word8 -> Port 
word8ToPort (W8# x#) = Port# (GHC.wordToWord16# (GHC.word8ToWord# x#))
{-# INLINE [0] word8ToPort #-}

-- | Cast a 'Word16' directly to a 'Port'. 
--
-- @since 1.0.0
word16ToPort :: Word16 -> Port 
word16ToPort (W16# x#) = Port# x#
{-# INLINE [0] word16ToPort #-}

-- Port - Read -----------------------------------------------------------------

-- | 'PortParseError' represents parse errors that can occur when parsing a 
-- input string as a 'Port' number.
--
-- @since 1.0.0
data PortParseError = PortParseError
  { port_parse_input :: {-# UNPACK #-} !ByteString
    -- ^ The input string that could not be parsed as a 'Port' number.
  , port_error_info  :: PortParseErrorInfo
    -- ^ Information describing why the parse error occured.
  }
  deriving (Data, Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Show PortParseError where
  show (PortParseError input info) =
    unlines [ "parse error: could not parse port number"
            , "  * reason: " ++ show info
            , "  * input: " ++ show input
            ]
  {-# INLINE show #-}

-- | The 'PortParseErrorInfo' type enumerates the failure modes when a 'Port'
-- can not be parsed from the given input string.
--
-- @since 1.0.0
data PortParseErrorInfo
  = ParseErrorBounds PortOutOfBoundsError
  -- ^ 'ParseErrorBounds' indicates that the parser was able to produce a
  -- numeric value from the input string, but the value was outside of the
  -- range for valid TCP port numbers.
  --
  -- The 'Port' is a 16-bit unsigned word, see 'isValidPort' for more details.
  | ParseErrorSource
  -- ^ 'ParseErrorSource' indicates that the parser encountered a non-numeric
  -- character in the input string.
  deriving (Data, Eq, Generic, Lift, Ord)

-- | @since 1.0.0
instance Bounded PortParseErrorInfo where
  minBound = ParseErrorSource
  {-# INLINE minBound #-}

  maxBound = ParseErrorBounds maxBound
  {-# INLINE maxBound #-}

-- | @since 1.0.0
instance Enum PortParseErrorInfo where
  fromEnum ParseErrorSource       = 0
  fromEnum (ParseErrorBounds exn) = 1 + fromEnum exn
  {-# INLINE fromEnum #-}

  toEnum 0 = ParseErrorSource
  toEnum n = ParseErrorBounds (toEnum (n - 1))
  {-# INLINE toEnum #-}

-- | @since 1.0.0
instance Show PortParseErrorInfo where
  show ParseErrorSource       = "non-numeric characters in input"
  show (ParseErrorBounds exn) = show exn
  {-# INLINE show #-}

-- | Similar to 'portFromByteString', but takes a lazy 'Lazy.ByteString' as the
-- input string.
--
-- @since 1.0.0
portFromLazyByteString :: Lazy.ByteString -> Either PortParseError Port
portFromLazyByteString = portFromByteString . Lazy.ByteString.toStrict
{-# INLINEABLE portFromLazyByteString #-}

-- | Parse a 'Port' number from a 'ByteString' of 8-bit decimal characters. If
-- the given 'ByteString' is a string of decimal character representing a valid
-- TCP port number, then that port will be returned:
--
-- >>> portFromByteString (ByteString.pack "8000")
-- Right 8000
--
-- @since 1.0.0
portFromByteString ::
  -- | The input 'ByteString' to parse.
  ByteString ->
  -- | Either the parsed 'Port' number, or a 'PortParseError' describing why the
  -- parser input failed.
  Either PortParseError Port
portFromByteString input =
  case ByteString.readInteger input of
    Nothing       -> parseError ParseErrorSource
    Just (int, _) ->
      case integralToPort int of
        Left  exn  -> parseError (ParseErrorBounds exn)
        Right port -> Right port
  where
    parseError :: PortParseErrorInfo -> Either PortParseError Port
    parseError = Left . PortParseError input

-- | Similar to 'portFromByteString', but attempts to parse a 'Port' from a
-- 'String' rather than a 'ByteString;.
--
-- @since 1.0.0
portFromString :: String -> Either PortParseError Port
portFromString = portFromByteString . ByteString.pack
{-# INLINEABLE portFromString #-}

-- Port - Write ----------------------------------------------------------------

-- | Convert a 'Port' to a bytestring 'Builder'.
--
-- @since 1.0.0
portToBuilder :: Port -> Builder
portToBuilder = Builder.word16Dec . getPort
{-# INLINABLE portToBuilder #-}

-- | Render a 'Port' number as a lazy 'Lazy.ByteString'.
--
-- >>> portToByteString 80
-- ("80" :: 'Lazy.ByteString')
--
-- @since 1.0.0
portToLazyByteString :: Port -> Lazy.ByteString
portToLazyByteString = Builder.toLazyByteString . portToBuilder
{-# INLINABLE portToLazyByteString #-}

-- | Render a 'Port' number as a strict 'ByteString'.
--
-- >>> portToByteString 80
-- ("80" :: 'ByteString')
--
-- @since 1.0.0
portToByteString :: Port -> ByteString
portToByteString = Lazy.ByteString.toStrict . portToLazyByteString
{-# INLINABLE portToByteString #-}

-- Port - Query ----------------------------------------------------------------

-- | @('isValidPort' x)@ checks if some 'Integral' value @x@ is a valid TCP port
-- number, the resulting value is:
--
-- * @'Nothing'@ if the given value @x@ is a valid port number.
--
-- * @('Just' 'PortOverflowError')@ if the given value @x@ is greater than
--   65535, the maximum value for 16-bit unsigned integers.
--
-- * @('Just' 'PortUnderflowError')@ if the given value @x@ is less than 0.
--
-- @since 1.0.0
isValidPort :: Integral a => a -> Maybe PortOutOfBoundsError
isValidPort num
  | numInteger < minInteger = Just PortUnderflowError
  | numInteger > maxInteger = Just PortOverflowError
  | otherwise               = Nothing
  where
    maxInteger = toInteger (maxBound :: Word16)
    minInteger = toInteger (minBound :: Word16)
    numInteger = toInteger num
