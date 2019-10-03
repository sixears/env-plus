{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Env.Error
  ( AsEnvError(..), EnvError, envNoParse, envNoParse_, missingEnv, missingEnv_ )
where

import Prelude ( )

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.String        ( String )
import Data.Typeable      ( TypeRep )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Prism    ( Prism' )
import Control.Lens.Review   ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Env.Types  ( EnvKey, EnvVal )

-------------------------------------------------------------------------------

data EnvError = MISSING_ENV_VAR EnvKey
              | ENV_NO_PARSE    EnvKey EnvVal String TypeRep
  deriving (Eq, Show)

class AsEnvError ε where
  _EnvError ∷ Prism' ε EnvError

instance Printable EnvError where
  print (MISSING_ENV_VAR k) =
    P.text $ [fmt|no such environment variable: '%T'|] k
  print (ENV_NO_PARSE k v s t) =
    P.text $
      [fmt|failed to parse environment variable %T (%T) as %w: %s|] k v t s

instance AsEnvError EnvError where
  _EnvError = id

instance Exception EnvError

----------------------------------------

missingEnv ∷ AsEnvError ε ⇒ EnvKey → ε
missingEnv k =  _EnvError # MISSING_ENV_VAR k

missingEnv_ ∷ (AsEnvError ε, MonadError ε η) ⇒ EnvKey → η α
missingEnv_ = throwError ∘ missingEnv

----------------------------------------

envNoParse ∷ AsEnvError ε ⇒ EnvKey → EnvVal → String → TypeRep → ε
envNoParse k v s t =  _EnvError # ENV_NO_PARSE k v s t

envNoParse_ ∷ (AsEnvError ε, MonadError ε η) ⇒
              EnvKey → EnvVal → String → TypeRep → η α
envNoParse_ k v s t = throwError $ envNoParse k v s t

-- that's all, folks! ---------------------------------------------------------
