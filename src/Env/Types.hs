{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Env.Types
  ( Env( Env, unEnv ), EnvKey( EnvKey ), EnvMod, EnvVal, FromP( fromP )
  , adjustEnv, adjustEnvT, alterEnv, alterEnvT, clearEnv
  , fromList, fromListT, fromMap, fromMapT, mapf, setEnv, setEnvT, smap, strsEnv
  , runEnvMod, unKey, unsetEnv, unsetEnvT, updateEnv, updateEnvT

  , tests
  )
where

import Control.Monad  ( mapM_ )
import Control.Monad.State  ( execState, modify )

-- base --------------------------------

import Control.Applicative     ( many )
import Data.Bifunctor          ( bimap )
import Data.Char               ( isAlphaNum )
import Data.Eq                 ( Eq )
import Data.Foldable           ( all, concatMap )
import Data.Function           ( ($), const, id )
import Data.Functor            ( fmap )
import Data.List               ( replicate, reverse )
import Data.Maybe              ( catMaybes )
import Data.Monoid             ( Monoid( mappend, mconcat, mempty ) )
import Data.Ord                ( Ord )
import Data.Semigroup          ( Semigroup( (<>) ) )
import Data.String             ( IsString( fromString ) )
import System.Exit             ( ExitCode )
import System.IO               ( IO )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∨) )
import Data.Eq.Unicode        ( (≡) )
import Data.Foldable.Unicode  ( (∈) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified Data.Map  as  Map

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual ), toString )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Char     ( ℂ )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Maybe    ( 𝕄 )
import Data.MoreUnicode.Natural  ( ℕ )
import Data.MoreUnicode.String   ( 𝕊 )

-- parsers -----------------------------

import Text.Parser.Char  ( anyChar )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( runTestsP, runTestsReplay, runTestTree )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtS )

--------------------------------------------------------------------------------

class FromP α where
  fromP ∷ Printable τ ⇒ τ → α

------------------------------------------------------------

newtype EnvKey  = EnvKey { unKey ∷ 𝕊 }
  deriving (Eq, Ord, Show)

instance Printable EnvKey where
  print (EnvKey t) = P.string t

instance IsString EnvKey where
  fromString = EnvKey

instance Textual EnvKey where
  textual = EnvKey ⊳ many anyChar

instance FromP EnvKey where
  fromP = fromString ∘ toString

------------------------------------------------------------

newtype EnvVal  = EnvVal { unVal ∷ 𝕊 }
  deriving (Eq, Show)

instance Printable EnvVal where
  print (EnvVal t) = P.string t

instance IsString EnvVal where
  fromString = EnvVal

instance Textual EnvVal where
  textual = EnvVal ⊳ many anyChar

instance FromP EnvVal where
  fromP = fromString ∘ toString

------------------------------------------------------------

-- I dislike using a List of Pairs here, as then `getEnv ∘ setEnv` is not `id`
-- in general (i.e., in the presence of duplicate keys, or differing sorts)
newtype Env = Env { unEnv ∷ Map.Map EnvKey EnvVal }
  deriving (Eq, Show)

shell_quote ∷ 𝕊 → 𝕊
shell_quote s =
  let is_safe ∷ ℂ → 𝔹
      is_safe c = '_' ≡ c ∨ isAlphaNum c
      quote_char ∷ ℂ → 𝕊
      quote_char c = if isAlphaNum c
                     then [c]
                     else if '_' ≡ c
                          then [c]
                          else "\\" ⊕ [c]
  in if all is_safe s
     then s
     else if '\'' ∈ s
          then concatMap quote_char s
          else "'" ⊕ s ⊕ "'"

instance Printable Env where
  print e =
    P.text $ let qu = shell_quote
              in [fmt|[%L]|] [ [fmtS|%s=%s|] (qu k) (qu v) | (k,v) ← strsEnv e ]

{- | Construct an Env from a Map from EnvKeys to EnvVals. -}
fromMap ∷ Map.Map EnvKey EnvVal → Env
fromMap = Env

{- | Construct an Env from a Map from Printables to Printables. -}
fromMapT ∷ (Printable τ, Printable σ) ⇒ Map.Map τ σ → Env
fromMapT =
  Env ∘ Map.mapKeys (fromString ∘ toString) ∘ Map.map (fromString ∘ toString)

{- | Construct an Env from a list of pairs of (EnvKey,EnvVal). -}
fromList ∷ [(EnvKey,EnvVal)] → Env
fromList = fromMap ∘ Map.fromList

{- | Construct an Env from a list of pairs of (EnvKey,EnvVal). -}
fromListT ∷ (Ord τ, Printable τ, Printable σ) ⇒ [(τ,σ)] → Env
fromListT = fromMapT ∘ Map.fromList

----------------------------------------

{- | Convert back to `base`-format environment (list of pairs of `String`s). -}
strsEnv ∷ Env → [(𝕊, 𝕊)]
strsEnv = bimap toString toString ⩺ Map.toList ∘ unEnv

----------------------------------------

type instance Element Env = EnvVal

instance MonoFunctor Env where
  omap ∷ (EnvVal → EnvVal) → Env → Env
  omap f = Env ∘ Map.map f ∘ unEnv

omapTests ∷ TestTree
omapTests =
  let f ∷ EnvVal → EnvVal
      f = fromString ∘ (\ t → t ⊕ reverse t) ∘ toString
   in testGroup "omap"
                [ testCase "t ++ reverse t" $
                        Env (Map.fromList [("a", "cattac"), ("c", "doggod")])
                    @=? omap f e1
                ]

innerMap ∷ (Map.Map EnvKey EnvVal → Map.Map EnvKey EnvVal) → Env → Env
innerMap f = Env ∘ f ∘ unEnv

{- | Update the map internal to `Env`, with keys and values pre-converted to
     strings.
 -}
smap ∷ (Ord τ, Printable τ, Printable σ) ⇒
       ([(𝕊,𝕊)] → [(τ,σ)]) → Env → Env
smap f = fromListT ∘ f ∘ strsEnv

{- | "Map" a function over pairs of keys, expressed as `Strings`.  This is not a
     proper functor, as the mapped function may collapse keys together, leaving
     fewer keys (or indeed delete keys altogether).  Internally the environment
     is stored as Strings, so best to stick with those to avoid possible
     conversion issues.
 -}
mapf ∷ (Ord τ, Printable τ, Printable σ) ⇒
       ((𝕊,𝕊) → 𝕄 (τ,σ)) → Env → Env
mapf f = fromListT ∘ catMaybes ∘ fmap f ∘ strsEnv

------------------------------------------------------------

data EnvMod = EnvMod [(Env → Env)]

instance Semigroup EnvMod where
  (EnvMod xs) <> (EnvMod ys) = EnvMod (xs ⊕ ys)

instance Monoid EnvMod where
  mempty  = EnvMod []
  mappend (EnvMod xs) (EnvMod ys) = EnvMod (xs ⊕ ys)

{- | Clear a key/value from the environment. -}
unsetEnv ∷ EnvKey → EnvMod
unsetEnv = unsetEnvT

{- | Clear a key/value from the environment. -}
unsetEnvT ∷ Printable τ ⇒ τ → EnvMod
unsetEnvT k = EnvMod [ innerMap $ Map.delete (fromP k) ]

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnv ∷ EnvKey → EnvVal → EnvMod
setEnv = setEnv

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnvT ∷ (Printable τ, Printable σ) ⇒ τ → σ → EnvMod
setEnvT k v =
  EnvMod [ innerMap $ Map.insert (fromP k) (fromP v) ]

{- | Clear the environment; remove all keys. -}
clearEnv ∷ EnvMod
clearEnv = EnvMod [ innerMap ∘ const $ Map.empty ]

{- | Update or delete the value attached to a key in the environment; no-op for
     a key that doesn't exist in the environment.. -}
updateEnv ∷ (EnvVal → 𝕄 EnvVal) → EnvKey → EnvMod
updateEnv f = updateEnvT (f ∘ fromString)

{- | Update or delete the value attached to a key in the environment; no-op for
     a key that doesn't exist in the environment.. -}
updateEnvT ∷ (Printable τ, Printable σ) ⇒ (𝕊 → 𝕄 σ) → τ → EnvMod
updateEnvT f k =
  EnvMod [ innerMap $ Map.update (fromP ⩺ f ∘ toString) (fromP k) ]

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnv ∷ (EnvVal → EnvVal) → EnvKey → EnvMod
adjustEnv f = adjustEnvT (f ∘ fromString)

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnvT ∷ (Printable τ, Printable σ) ⇒ (𝕊 → σ) → τ → EnvMod
adjustEnvT f k =
  EnvMod [ innerMap $ Map.adjust (fromP ∘ f ∘ toString) (fromP k) ]

----------------------------------------

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnv ∷ (𝕄 EnvVal → 𝕄 EnvVal) → EnvKey → EnvMod
alterEnv f = alterEnvT (f ∘ fmap fromString)

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnvT ∷ (Printable τ, Printable σ) ⇒ (𝕄 𝕊 → 𝕄 σ) → τ → EnvMod
alterEnvT f k =
  EnvMod [ innerMap $ Map.alter (fromP ⩺ f ∘ fmap toString) (fromP k) ]

--------------------

alterEnvTests ∷ TestTree
alterEnvTests =
  let a = "a" ∷ 𝕊
   in testGroup "alterEnv"
                [ testCase "id (a)" $ e1 @=? runEnvMod (alterEnv id "a") e1
                , testCase "id (e)" $ e1 @=? runEnvMod (alterEnv id "e") e1
                , testCase "reverse (a)" $
                        Env (Map.fromList [("a", "tac"), ("c", "dog")])
                    @=? runEnvMod (alterEnvT (fmap reverse) a) e1
                , testCase "reverse (e)" $
                      e1 @=? runEnvMod (alterEnvT (fmap reverse) ("e" ∷ 𝕊)) e1
                , testCase "reverse - replicate (a)" $
                      Env (Map.fromList [("a", "tactac"), ("c", "dog")])
                    @=? runEnvMod (  alterEnvT (fmap reverse) a
                                   ⊕ alterEnvT (fmap (mconcat ∘ replicate 2)) a) e1
                ]

----------------------------------------

{- | Apply a set of modifications to a Environment -}
runEnvMod ∷ EnvMod → Env → Env
-- execState ∷ State s a {- state-passing computation to execute -}
--                 (EnvMod es) ⤳ (mapM_ modify es)
--           → s {- initial value -}                                -- Env
--           → s {- final state -}                                  -- Env
--
-- Thus `modify` is the key here, that's the thing that 'introduces' the
-- MonadState, using an s → s transformation that is encapsulated within
-- EnvMod; `mapM_` covers the fact that there are a sequence of them.
-- There is no extra result to each modification, hence `mapM_` rather than
-- `mapM` and `execState` rather than `runState`.
runEnvMod (EnvMod es) s0 = execState (mapM_ modify es) s0

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

e1 ∷ Env
e1 = Env $ Map.fromList [("a", "cat"), ("c", "dog")]

tests ∷ TestTree
tests = testGroup "Env.Types" [ omapTests, alterEnvTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
