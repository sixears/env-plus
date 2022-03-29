module Env
  ( adjustEnv, adjustEnvT, alterEnv, alterEnvT, clearEnv, getEnv, getEnvT
  , getEnvironment, setEnv, setEnvT, setEnvironment, unsetEnv, unsetEnvT
  , updateEnv, updateEnvT, withEnv
  )
where

import Base1T

-- base --------------------------------

import Control.Exception.Base  ( bracket )
import Data.String             ( fromString )

-- unix --------------------------------

import qualified System.Posix.Env  as  PosixEnv

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Env.Types  ( Env, EnvKey, EnvMod, EnvVal
                  , fromListT, fromP, strsEnv, runEnvMod )

--------------------------------------------------------------------------------

getEnvironment ∷ MonadIO μ ⇒ μ Env
getEnvironment = fromListT ⊳ liftIO PosixEnv.getEnvironment

setEnvironment ∷ MonadIO μ ⇒ Env → μ ()
setEnvironment = liftIO ∘ PosixEnv.setEnvironment ∘ strsEnv

----------------------------------------

{- | Get a value from the environment. -}
getEnv ∷ MonadIO μ ⇒ EnvKey → μ (Maybe EnvVal)
getEnv = getEnvT

{- | Get a value from the environment, using any printable as a key. -}
getEnvT ∷ (MonadIO μ, Printable τ) ⇒ τ → μ (Maybe EnvVal)
getEnvT k = liftIO $ fmap fromString ⊳ PosixEnv.getEnv (toString k)

{- | Set a value in the environment. -}
setEnv ∷ MonadIO μ ⇒ EnvKey → EnvVal → μ ()
setEnv = setEnvT

{- | Set a value in the environment, from Printables.  The Environment is made
     of `String`s, so using `String`s has the best chance of avoiding conversion
     problems.
 -}
setEnvT ∷ (MonadIO μ, Printable τ, Printable σ) ⇒ τ → σ → μ ()
setEnvT k v =
  liftIO $
    PosixEnv.setEnv (fromString $ toString k) (fromString $ toString v) 𝕿

{- | Clear a value from the environment. -}
unsetEnv ∷ MonadIO μ ⇒ EnvKey → μ ()
unsetEnv = unsetEnv

{- | Clear a value from the environment, using any `Printable` as the key -}
unsetEnvT ∷ (MonadIO μ, Printable τ) ⇒ τ → μ ()
unsetEnvT k = liftIO $ PosixEnv.unsetEnv (toString k)

{- | Alter a value or non-value in the environment. -}
alterEnv ∷ MonadIO μ ⇒ (Maybe EnvVal → Maybe EnvVal) → EnvKey → μ ()
alterEnv f k = f ⊳ getEnv k ≫ \ case
                  Nothing → unsetEnv k
                  Just v  → setEnv k v

{- | Alter a value or non-value in the environment. -}
alterEnvT ∷ (MonadIO μ, Printable σ, Printable τ) ⇒
            (Maybe String → Maybe τ) → σ → μ ()
alterEnvT f k = f ∘ fmap toString ⊳ getEnv (fromP k) ≫ \ case
                  Nothing → unsetEnvT k
                  Just v  → setEnvT k v

{- | Update or delete a value in the environment; no-op for a key that doesn't
     exist in the environment. -}
updateEnv ∷ MonadIO μ ⇒ (EnvVal → Maybe EnvVal) → EnvKey → μ ()
updateEnv f = updateEnvT (f ∘ fromString)

{- | Update or delete a value in the environment; no-op for a key that doesn't
     exist in the environment. -}
updateEnvT ∷ (MonadIO μ, Printable τ, Printable σ) ⇒
             (String → Maybe τ) → σ → μ ()
updateEnvT f k = getEnv (fromP k) ≫ \ case
                   Nothing → return ()
                   Just v → case f (toString v) of
                              Nothing → unsetEnvT k
                              Just v' → setEnvT k v'

{- | Adjust a value in the environment; no-op for a key that doesn't exist in
     the environment. -}
adjustEnv ∷ MonadIO μ ⇒ (EnvVal → EnvVal) → EnvKey → μ ()
adjustEnv f = adjustEnvT (f ∘ fromString)

{- | Adjust a value in the environment; no-op for a key that doesn't exist in
     the environment. -}
adjustEnvT ∷ (MonadIO μ, Printable τ, Printable σ) ⇒ (String → τ) → σ → μ ()
adjustEnvT f k = getEnv (fromP k) ≫ \ case
                   Nothing → return ()
                   Just v  → setEnvT k (f (toString v))

{- | Clear the environment; remove all key-value pairs. -}
clearEnv ∷ MonadIO μ ⇒ μ ()
clearEnv = liftIO PosixEnv.clearEnv

withEnv ∷ MonadIO μ ⇒ EnvMod → IO α → μ α
withEnv m io = liftIO $ do
  let editEnv = do env ← getEnvironment
                   setEnvironment (runEnvMod m env)
                   return env
  bracket editEnv setEnvironment (const io)

-- that's all, folks! ----------------------------------------------------------
