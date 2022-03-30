module Env.Types
  ( Env( Env, unEnv ), EnvKey( EnvKey ), EnvMod, EnvVal, FromP( fromP )
  , adjustEnvMod, adjustEnvModT
  , alterEnvMod, alterEnvModT
  , clearEnvMod, clearEnvModT, clearEnvMod',
    fromList, fromListT, fromMap, fromMapT, mapf
  , runEnvMod, runEnvMod'
  , setEnvMod, setEnvModT
  , smap, strsEnv
  , unKey
  , unsetEnvMod, unsetEnvModT
  , updateEnvMod, updateEnvModT

  , tests
  )
where

import Base1T

-- base --------------------------------

import qualified  Data.List

import Data.Bifunctor          ( bimap )
import Data.Char               ( isAlphaNum )
import Data.Foldable           ( all, concatMap )
import Data.Function           ( flip )
import Data.Maybe              ( catMaybes )
import Data.Monoid             ( Monoid( mappend, mempty ) )
import Data.String             ( IsString( fromString ) )
import GHC.Exts                ( IsList( toList ) )
import GHC.Generics            ( Generic )

-- containers --------------------------

import qualified Data.Map  as  Map
import qualified Data.Set  as Set

-- data-textual ------------------------

import Data.Textual  ( Textual( textual ) )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

-- mtl ---------------------------------

import Control.Monad.State  ( execState, modify )

-- parsers -----------------------------

import Text.Parser.Char  ( anyChar )

-- text --------------------------------

import Data.Text  ( pack, replicate, reverse )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

class FromP α where
  fromP ∷ Printable τ ⇒ τ → α

------------------------------------------------------------

-- things that can usefully have a mapped text function
class TMap γ where
  tmap  ∷ (𝕋 → 𝕋) → γ → γ
  mtmap ∷ (𝕋 → 𝕋) → 𝕄 γ → 𝕄 γ
  mtmap f = fmap $ tmap f

------------------------------------------------------------

newtype EnvKey = EnvKey { unKey ∷ 𝕋 }
  deriving (Eq,Generic,NFData,Ord,Show)

instance Printable EnvKey where
  print (EnvKey t) = P.text t

instance IsString EnvKey where
  fromString = EnvKey ∘ pack

instance Textual EnvKey where
  textual = EnvKey ∘ pack ⊳ many anyChar

instance FromP EnvKey where
  fromP = fromString ∘ toString

instance TMap EnvKey where
  tmap f = EnvKey ∘ f ∘ unKey

------------------------------------------------------------

newtype EnvVal  = EnvVal { unVal ∷ 𝕋 }
  deriving (Eq,Generic,NFData,Show)

instance Printable EnvVal where
  print (EnvVal t) = P.text t

instance IsString EnvVal where
  fromString = EnvVal ∘ pack

instance Textual EnvVal where
  textual = EnvVal ∘ pack ⊳ many anyChar

instance FromP EnvVal where
  fromP = fromString ∘ toString

instance TMap EnvVal where
  tmap f = EnvVal ∘ f ∘ unVal

------------------------------------------------------------

-- I dislike using a List of Pairs here, as then `getEnv ∘ setEnv` is not `id`
-- in general (i.e., in the presence of duplicate keys, or differing sorts)
newtype Env = Env { unEnv ∷ Map.Map EnvKey EnvVal }
  deriving (Eq,Generic,NFData,Show)

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

instance IsList Env where
  type instance Item Env = (EnvKey,EnvVal)

  fromList ∷ [(EnvKey,EnvVal)] → Env
  fromList = fromMap ∘ Map.fromList

  toList ∷ Env → [(EnvKey,EnvVal)]
  toList = Map.toList ∘ unEnv

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
      f = tmap (\ t → t ⊕ reverse t)
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

-- Each element is an environment transformation function, with a description.
-- The description has no semantic value; it is used purely for logging &
-- debugging.
data EnvMod = EnvMod [(𝕋,Env → Env)]

instance Semigroup EnvMod where
  (EnvMod xs) <> (EnvMod ys) = EnvMod (xs ⊕ ys)

instance Monoid EnvMod where
  mempty = EnvMod []
  mappend (EnvMod xs) (EnvMod ys) = EnvMod (xs ⊕ ys)

{- | Clear a key/value from the environment. -}
unsetEnvMod ∷ EnvKey → EnvMod
unsetEnvMod = unsetEnvModT

{- | Clear a key/value from the environment. -}
unsetEnvModT ∷ Printable τ ⇒ τ → EnvMod
unsetEnvModT k = let msg = [fmt|env unset '%T'|] k
                  in EnvMod [ (msg, innerMap $ Map.delete (fromP k)) ]

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnvModT ∷ ∀ τ σ . (Printable τ, Printable σ) ⇒ τ → σ → EnvMod
setEnvModT k v = let msg = [fmt|env set '%T' to '%T'|] k v
                  in EnvMod [ (msg, innerMap $ Map.insert (fromP k) (fromP v)) ]

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnvMod ∷ EnvKey → EnvVal → EnvMod
setEnvMod = setEnvModT

----------------------------------------

{- | Clear the environment; remove all keys, except a given set. -}
clearEnvModT ∷ Printable τ ⇒ Set.Set τ → EnvMod
clearEnvModT keeps
  | Set.null keeps = EnvMod [ ("env clear",innerMap ∘ const $ Map.empty) ]
  | otherwise      =
      let msg = [fmt|env clear except [%L]|] keeps
       in EnvMod [(msg,innerMap $ flip Map.restrictKeys (Set.map fromP keeps))]

--------------------

{- | Clear the environment; remove all keys, except a given set. -}
clearEnvMod ∷ Set.Set EnvKey → EnvMod
clearEnvMod keeps
  | Set.null keeps = EnvMod [ ("env clear",innerMap ∘ const $ Map.empty) ]
  | otherwise      = let msg = [fmt|env clear except [%L]|] keeps
                      in EnvMod [ (msg,innerMap $ flip Map.restrictKeys keeps) ]

----------------------------------------

{- | Clear the environment; remove all keys. -}
clearEnvMod' ∷ EnvMod
clearEnvMod' = clearEnvMod Set.empty

--------------------

clearEnvModTests ∷ TestTree
clearEnvModTests =
  let a = "a"; c = "c"
      env = Env ∘ Map.fromList
      check ∷ TestName → Env → [EnvKey] → TestTree
      check nm exp ks =
        testCase nm $ exp @=? runEnvMod (clearEnvMod $ Set.fromList ks) e1
   in testGroup "clearEnvMod"
                [ check "clear; keep only c" (env [(c,"dog")]) [c]
                , check "clear; keep a & c" e1 [a,c]
                , check "clear; keep none" (env []) []
                ]

----------------------------------------

{- | Update or delete the value attached to a key in the environment; no-op for
     a key that doesn't exist in the environment.. -}
updateEnvModT ∷ ∀ τ σ . (Printable τ, Printable σ) ⇒ 𝕋 → (𝕊 → 𝕄 σ) → τ → EnvMod
updateEnvModT msg f k =
  EnvMod [ (msg, innerMap $ Map.update (fromP ⩺ f ∘ toString) (fromP k)) ]

--------------------

{- | Update or delete the value attached to a key in the environment; no-op for
     a key that doesn't exist in the environment.. -}
updateEnvMod ∷ 𝕋 → (EnvVal → 𝕄 EnvVal) → EnvKey → EnvMod
updateEnvMod msg f = updateEnvModT msg (f ∘ fromString)

----------------------------------------

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnvModT ∷ ∀ τ σ . (Printable τ, Printable σ) ⇒ 𝕋 → (𝕊 → σ) → τ → EnvMod
adjustEnvModT msg f k =
  EnvMod [ (msg,innerMap $ Map.adjust (fromP ∘ f ∘ toString) (fromP k)) ]

--------------------

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnvMod ∷ 𝕋 → (EnvVal → EnvVal) → EnvKey → EnvMod
adjustEnvMod msg f = adjustEnvModT msg (f ∘ fromString)

----------------------------------------

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnvModT ∷ ∀ τ σ .
               (Printable τ, Printable σ) ⇒ 𝕋 → (𝕄 𝕊 → 𝕄 σ) → τ → EnvMod
alterEnvModT msg f k =
  EnvMod [ (msg,innerMap $ Map.alter (fromP ⩺ f ∘ fmap toString) (fromP k)) ]

--------------------

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnvMod ∷ 𝕋 → (𝕄 EnvVal → 𝕄 EnvVal) → EnvKey → EnvMod
alterEnvMod msg f = alterEnvModT msg (f ∘ fmap fromString)

----------

alterEnvModTests ∷ TestTree
alterEnvModTests =
  let a = "a"; c = "c"
      env = Env ∘ Map.fromList
      check ∷ TestName → Env → (𝕋 → 𝕋) → EnvKey → TestTree
      check nm exp f k =
        testCase nm $ exp @=? runEnvMod (alterEnvMod (toText nm) (mtmap f) k) e1
   in testGroup "alterEnvMod"
                [ check "id (a)" e1 id a
                , check "id (e)" e1 id "e"
                , check "reverse (a)" (env [(a, "tac"), (c, "dog")]) reverse a
                , check "reverse (e)" e1 reverse "e"
                , let g = mtmap $ replicate 2
                      f = alterEnvMod "reverse" (mtmap reverse) a
                        ⊕ alterEnvMod "replicate" g a
                   in checkRun "reverse - replicate (a)" f
                               (env [ (a, "tactac"∷EnvVal)
                                    , (c, "dog")])
                ]

----------------------------------------

{- | Apply a set of modifications to a Environment; also return the list of
     modifications applied as Text descriptions.  Note that those texts are
     purely informational, must not be programmatically interrogated. -}
runEnvMod' ∷ EnvMod → Env → (Env,[𝕋])
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
-- runEnvMod (EnvMod es) s0 = execState (mapM_ modify (snd ⊳ es)) s0
runEnvMod' (EnvMod es) s0 =
  let (ts',s') = execState (mapM_ modify [\ (ts,x) → (t:ts,e x) | (t,e) ← es])
                           ([],s0)
   in (s',Data.List.reverse ts')

{- | Apply a set of modifications to a Environment -}
runEnvMod ∷ EnvMod → Env → Env
runEnvMod e = fst ∘ runEnvMod' e

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

e1 ∷ Env
e1 = Env $ Map.fromList [("a", "cat"), ("c", "dog")]

checkRun ∷ TestName → EnvMod → Env → TestTree
checkRun nm mod exp = testCase nm $ exp @=? runEnvMod mod e1

tests ∷ TestTree
tests = testGroup "Env.Types" [ omapTests, alterEnvModTests, clearEnvModTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
