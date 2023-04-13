{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Module      :  Web.Port.TH
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines Template Haskell utilities for working with the 'Port' 
-- type. 
--
-- @since 1.0.0
module Web.Port.TH
  ( -- * Lifting 
    liftPortAsExp,
    liftPortAsPat, 
    liftPortAsType,

    -- * Quasi-Quotation
    port,
    quotePortAsExp,
    quotePortAsPat,
    quotePortAsType,
    parsePortQ
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString

import Language.Haskell.TH (Q, Pat (..), Exp (..), Lit (..), Type (..), TyLit (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Web.Port (Port (..))
import Web.Port qualified as Port

-- Lifting ---------------------------------------------------------------------

-- | Lift a 'Port' as a Haskell expression.
--
-- @since 1.0.0
liftPortAsExp :: Port -> Exp 
liftPortAsExp (Port x) = 
  let integer :: Integer
      integer = fromIntegral x
   in LitE (IntegerL integer)

-- | Lift a 'Port' as a Haskell pattern expression.
--
-- @since 1.0.0
liftPortAsPat :: Port -> Pat 
liftPortAsPat (Port x) = 
  let integer :: Integer
      integer = fromIntegral x
   in ConP 'Port [] [LitP (IntegerL integer)]

-- | Lift a 'Port' as a Haskell type. This will transform the 'Port' into an 
-- equivalent type-level natural number.
--
-- @since 1.0.0
liftPortAsType :: Port -> Type
liftPortAsType (Port x) = 
  let integer :: Integer
      integer = fromIntegral x
   in LitT (NumTyLit integer)

-- Quasiquotation --------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
port :: QuasiQuoter
port = 
  QuasiQuoter 
    { quoteExp  = quotePortAsExp 
    , quotePat  = quotePortAsPat
    , quoteType = quotePortAsType
    , quoteDec  = \_ -> fail "quasiport: not supported in declaration contexts"
    }

-- | 'Port' expression quasi-quoter. Reads a TCP 'Port' from a string literal 
-- and lifts it to a Haskell expression.
--
-- @since 1.0.0
quotePortAsExp :: String -> Q Exp
quotePortAsExp = fmap liftPortAsExp . parsePortQ 
{-# INLINEABLE quotePortAsExp #-}

-- | 'Port' expression quasi-quoter. Reads a TCP 'Port' from a string literal 
-- and lifts it to a Haskell pattern.
--
-- @since 1.0.0
quotePortAsPat :: String -> Q Pat
quotePortAsPat = fmap liftPortAsPat . parsePortQ
{-# INLINEABLE quotePortAsPat #-}

-- | 'Port' expression quasi-quoter. Reads a TCP 'Port' from a string literal 
-- and lifts it to a Haskell type-level literal.
--
-- @since 1.0.0
quotePortAsType :: String -> Q Type
quotePortAsType = fmap liftPortAsType . parsePortQ 
{-# INLINEABLE quotePortAsType #-}

-- | TODO: docs
--
-- @since 1.0.0
parsePortQ :: String -> Q Port
parsePortQ str =  
  let input :: ByteString 
      input = ByteString.strip (ByteString.pack str)
   in case Port.portFromByteString input of 
        Left  exn -> fail (show exn)
        Right px  -> pure px