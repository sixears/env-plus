module Env
  ( adjustEnv, adjustEnvT, alterEnv, alterEnvT, clearEnv, getEnv, getEnvT
  , getEnvironment, setEnv, setEnvT, setEnvironment, unsetEnv, unsetEnvT
  , updateEnv, updateEnvT, withEnv

  , tests
  )
where

import Base1T

-- base --------------------------------

import Control.Exception.Base  ( bracket )
import Data.String             ( fromString )

-- containers --------------------------

import qualified  Data.Set

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Tuple   ( _1 )

-- tasty-plus --------------------------

import TastyPlus  ( ioTests )

-- unix --------------------------------

import qualified System.Posix.Env  as  PosixEnv

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Env.Types  ( Env, EnvKey, EnvMod, EnvVal, ә, clearEnvMod, fromListT
                  , fromP, setEnvMod, strsEnv, runEnvMod' )

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

{- | Perform IO in an environment subject to a set of modifications.
     Return the IO result, along with a set of log messages describing the
     environment modification (note: these are purely informational, no specific
     semantic must be inferred) and the actual environment used for the IO.
 -}
withEnvMod ∷ MonadIO μ ⇒ EnvMod → IO α → μ (α,[𝕋],Env)
withEnvMod m io = liftIO $ do
  let editEnv = do env ← getEnvironment
                   let (ioEnv,msgs) = runEnvMod' m env
                   setEnvironment ioEnv
                   return (env,msgs,ioEnv)
  bracket editEnv (setEnvironment ∘ view _1)
                  (\ (_,msgs,e) → io ≫ return ∘ (,msgs,e))

withEnvModTests ∷ TestTree
withEnvModTests =
  let home     = "HOME"
      nonesuch = "/home/nonesuch"
      modEnv   = setEnvMod (ә "HOME") nonesuch
               ⊕ clearEnvMod (Data.Set.fromList [ home ])
      msgs     = [ "env set [HOME] to '/home/nonesuch'"
                 , "env clear except [HOME]" ]
   in testGroup "withEnv…" $
      [ ioTests "withEnv"
                [ ("get HOME (pre)", \ h → getEnv home ≫ (@=? h))
                , ("set HOME", const $
                      withEnv modEnv getEnvironment ≫ (@=? [(home,nonesuch)]))
                , ("get HOME (post)", \ h → getEnv home ≫ (@=? h))
                ]
                (getEnv home)
      , ioTests "withEnvMod"
                [ ("get HOME (pre)", \ h → getEnv home ≫ (@=? h))
                , ("set HOME", const $
                      withEnvMod modEnv getEnvironment ≫ \ (env,msgs',e) → do
                        (([(home,nonesuch)]) @=? env)
                        msgs @=? msgs'
                        e @=? [(home,nonesuch)]
                  )
                , ("get HOME (post)", \ h → getEnv home ≫ (@=? h))
                ]
                (getEnv home)
      ]

{-# DEPRECATED withEnv "use `withEnvMod` instead" #-}
withEnv ∷ MonadIO μ ⇒ EnvMod → IO α → μ α
withEnv m io = {- liftIO $ do
  let editEnv = do env ← getEnvironment
                   setEnvironment (runEnvMod m env)
                   return env
  bracket editEnv setEnvironment (const io) -}
  view _1 ⊳ withEnvMod m io

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Env" [ withEnvModTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
