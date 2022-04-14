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

  , EnvModFrag, MkEnvModFrag(..)
  , ә, ӛ, ֆ
  , preclearEnvMod, ҙ
  , retainKey, retainKeys, ӭ
  , mkEnvModFrag, э

  , tests
  )
where

import Base1T

-- base --------------------------------

import qualified  Data.List

import Data.Bifunctor  ( bimap )
import Data.Function   ( flip )
import Data.Maybe      ( catMaybes )
import Data.Monoid     ( Monoid( mappend, mempty ) )
import Data.String     ( IsString( fromString ) )
import GHC.Exts        ( IsList( toList ) )
import GHC.Generics    ( Generic )

-- containers --------------------------

import qualified Data.Map  as  Map
import qualified Data.Set  as  Set

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
  deriving (Eq,Generic,Monoid,NFData,Semigroup,Show)

instance Printable Env where
  print e =
    P.text $ [fmt|[%L]|] [ [fmtS|%q=%q|] k v | (k,v) ← strsEnv e ]

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

instance Show EnvMod where
  show (EnvMod m) = [fmt|%L|] (fst ⊳ m)

instance Semigroup EnvMod where
  (EnvMod xs) <> (EnvMod ys) = EnvMod (xs ⊕ ys)

instance Monoid EnvMod where
  mempty = EnvMod []
  mappend (EnvMod xs) (EnvMod ys) = EnvMod (xs ⊕ ys)

------------------------------------------------------------

{- | Clear some keys from the environment. -}
unsetEnvMod ∷ EnvKeySet α ⇒ α → EnvMod
unsetEnvMod = unsetEnvModT ∘ envKeySet

{- | Clear a key/value from the environment. -}
unsetEnvModT ∷ (Printable τ, Ord τ) ⇒ Set.Set τ → EnvMod
unsetEnvModT (Set.map fromP → ks) =
  let msg = [fmt|env unset [%L]|] [[fmtT|%q|] x | x ← Set.toList ks]
   in EnvMod [ (msg, innerMap $ flip Map.withoutKeys ks) ]

----------------------------------------

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnvModT ∷ ∀ τ σ . (Printable τ, Printable σ) ⇒ Set.Set τ → σ → EnvMod
setEnvModT (Set.map fromP → ks) v =
  let msg = [fmt|env set [%L] to '%T'|] ([[fmtT|%q|] x | x ← Set.toList ks]) v
   in EnvMod [ (msg, innerMap $ Map.union (Map.fromSet (const (fromP v)) ks)) ]

--------------------

{- | Set a key to a constant value pair in the environment irrespective of any
     prior value or lack for that key. -}
setEnvMod ∷ EnvKeySet α ⇒ α → EnvVal → EnvMod
setEnvMod = setEnvModT ∘ envKeySet

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
updateEnvModT ∷ ∀ τ σ .
                (Printable τ, Printable σ) ⇒ 𝕋 → (𝕊 → 𝕄 σ) → Set.Set τ → EnvMod
updateEnvModT msg f (Set.toList → ks) =
  EnvMod [ (msg, innerMap $ Map.update (fromP ⩺ f ∘ toString) (fromP k)) | k←ks]

--------------------

{- | Update or delete the value attached to a key in the environment; no-op for
     a key that doesn't exist in the environment.. -}
updateEnvMod ∷ EnvKeySet α ⇒ 𝕋 → (EnvVal → 𝕄 EnvVal) → α → EnvMod
updateEnvMod msg f = updateEnvModT msg (f ∘ fromString) ∘ envKeySet

----------------------------------------

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnvModT ∷ ∀ τ σ .
                (Printable τ, Printable σ) ⇒ 𝕋 → (𝕊 → σ) → Set.Set τ → EnvMod
adjustEnvModT msg f (Set.toList → ks) =
  EnvMod [ (msg,innerMap $ Map.adjust (fromP ∘ f ∘ toString) (fromP k)) | k←ks ]

--------------------

{- | Update the value attached to a key in the environment; no-op if the key is
     not in the environment. -}
adjustEnvMod ∷ EnvKeySet α ⇒ 𝕋 → (EnvVal → EnvVal) → α → EnvMod
adjustEnvMod msg f = adjustEnvModT msg (f ∘ fromString) ∘ envKeySet

----------------------------------------

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnvModT ∷ ∀ τ σ . (Printable τ, Printable σ) ⇒
               𝕋 → (𝕄 𝕊 → 𝕄 σ) → Set.Set τ → EnvMod
alterEnvModT msg f (Set.toList → ks) =
  EnvMod [ (msg,innerMap $ Map.alter (fmap fromP ∘ f ∘ fmap toString) (fromP k))
         | k ← ks ]

--------------------

{- | Update or delete the value or non-value attached to a key in the
     environment. -}
alterEnvMod ∷ EnvKeySet α ⇒ 𝕋 → (𝕄 EnvVal → 𝕄 EnvVal) → α → EnvMod
alterEnvMod msg f = alterEnvModT msg (f ∘ fmap fromString) ∘ envKeySet

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

----------------------------------------

{- | Single instruction to modify the environment in a simple way, such that
     these instructions may be concatenated and applied to a pre-existing env.
 -}
data DiscardOnPreclear = DiscardOnPreclear | NoDiscardOnPreclear
  deriving (Eq,Show)

data EnvModFrag = EnvModFrag { _envMod  ∷ EnvMod
                             , _envKeys ∷ Set.Set EnvKey
                             , _discard ∷ DiscardOnPreclear
                             }

------------------------------------------------------------

{-| Things that may be converted to a set of `EnvKey`s -}

class EnvKeySet α where
  envKeySet ∷ α → Set.Set EnvKey
  ekList ∷ α → [EnvKey]
  ekList = Set.toList ∘ envKeySet

instance EnvKeySet (Set.Set EnvKey) where
  envKeySet = id

instance EnvKeySet [EnvKey] where
  envKeySet = Set.fromList

instance EnvKeySet EnvKey where
  envKeySet = Set.singleton

------------------------------------------------------------

retainKey ∷ EnvKey → EnvModFrag
retainKey k =
  let ks = Set.singleton k
      msg = [fmt|retain key '%T'|] k
   in EnvModFrag (adjustEnvMod msg id ks) ks DiscardOnPreclear

retainKeys ∷ EnvKeySet α ⇒ α → EnvModFrag
retainKeys ks = EnvModFrag (adjustEnvMod ([fmt|retain key '%L'|] $ ekList ks)
                           id (ekList ks)) (envKeySet ks) DiscardOnPreclear

ӭ  ∷ EnvKeySet α ⇒ α → EnvModFrag
ӭ = retainKeys

----------------------------------------

{- | Create an `EnvKey` from a `𝕋`. -}
ә ∷ 𝕋 → EnvKey
ә = EnvKey

----------------------------------------

{- | Create an `EnvVal` from a `𝕋`. -}
ӛ ∷ 𝕋 → EnvVal
ӛ = EnvVal

-- t = э (ә "HOME",ӛ "/home") ⊕ э ("msg"∷𝕋, ә "FOO", \ (e ∷ 𝕄 𝕊) → e ⊕ e)

----------------------------------------

{- | Construct an EnvMod that is the `mconcat` of a set of envmods; with the
     environment otherwise cleared of all keys.

     To retain a key without amending it; use @ӭ "KEY"@.
-}
preclearEnvMod ∷ [EnvModFrag] → EnvMod
preclearEnvMod fs = ю ([_envMod f | f ← fs, NoDiscardOnPreclear ≡ (_discard f)])
                    -- we keep even 'unset's in the list of keys to ignore;
                    -- because we would otherwise have to check for 'set's &
                    -- 'adjust's later in the list : clearly possible, but
                    -- probably not worth the bother at this time
                  ⊕ clearEnvMod (Set.unions $ _envKeys ⊳ fs)


----------

{-| Alias for `preclearEnvMod` -}
ҙ ∷ [EnvModFrag] → EnvMod
ҙ = preclearEnvMod

------------------------------------------------------------

{- | Easy creation of simple env mods by type. -}
class MkEnvModFrag α where
  mkEnvMod   ∷ α → EnvMod
  envModKeys ∷ α → Set.Set EnvKey

mkEnvModFrag ∷ MkEnvModFrag α ⇒ α → EnvModFrag
mkEnvModFrag a = EnvModFrag (mkEnvMod a) (envModKeys a) NoDiscardOnPreclear

э ∷ MkEnvModFrag α ⇒ α → EnvModFrag
э = mkEnvModFrag

ֆ ∷ Ord α ⇒ [α] → Set.Set α
ֆ = Set.fromList

--------------------

{-| @э (EnvKeySet,EnvVal) @ - set -}

instance MkEnvModFrag (Set.Set EnvKey, EnvVal) where
  mkEnvMod (ks,v)   = setEnvMod ks v
  envModKeys (ks,_) = ks

--------------------

{-| @э EnvKeySet@ - unset -}

-- can't use EnvKeySet α ⇒ MkEnvModFrag α here; as that would be Undecidable
instance MkEnvModFrag (Set.Set EnvKey) where
  mkEnvMod ks = unsetEnvMod ks
  envModKeys  = id

--------------------

-- can't use Printable τ ⇒ MkEnvModFrag τ here; as that would be Undecidable
instance MkEnvModFrag EnvKey where
  mkEnvMod   k = unsetEnvMod k
  envModKeys k = Set.singleton k

--------------------

{-| @э (𝕋,EnvKeySet,𝕊→Printable)@ - msg,key,function - update  -}

instance (EnvKeySet α, Printable τ) ⇒ MkEnvModFrag (𝕋,α,𝕊 → 𝕄 τ) where
  mkEnvMod (msg,ks,f) = updateEnvModT msg f (envKeySet ks)
  envModKeys (_,ks,_) = envKeySet ks

--------------------

{-| @э (𝕋,EnvKeySet,𝕊→Printable)@ - msg,key,function - adjust  -}

instance (EnvKeySet α, Printable τ) ⇒ MkEnvModFrag (𝕋,α,𝕊 → τ) where
  mkEnvMod (msg,ks,f) = adjustEnvModT msg f (envKeySet ks)
  envModKeys (_,ks,_) = envKeySet ks

--------------------


{-| @э (𝕋,EnvKeySet,𝕄 𝕊→𝕄 Printable)@ - msg,key,function - alter  -}

instance (EnvKeySet α, Printable τ) ⇒ MkEnvModFrag (𝕋,α,𝕄 𝕊 → 𝕄 τ) where
  mkEnvMod (msg,ks,f) = alterEnvModT msg f (envKeySet ks)
  envModKeys (_,ks,_) = envKeySet ks

ѯ ∷ [EnvModFrag] → Env → Env
ѯ frags = runEnvMod (ҙ frags)

mkEnvModFragTests ∷ TestTree
mkEnvModFragTests =
  testGroup "MkEnvModFrag" $
    let set   = э (ֆ [ә"A","b"], ӛ "v1")
        unset = э (ֆ [ә"A","b"])
        env0  = Env [("A","v1") ]
        env0' = Env [("b","v1") ]
        env1  = Env [("A","v1"),("b","v1")]
        env2  = Env [("A","v1"),("c","v2")]
        env3  = Env [("A","v1"),("b","v1"),("c","v2")]
     in [ testCase "base case (empty)" $ ф @=? (ѯ []) ф
          -- remember!  preclearEnvMod (ҙ) empties the environment of any keys
          -- that are not explicitly retained
        , testCase "base case (env1)" $ ф @=? (ѯ []) env1
        , testCase "retain A" $ env0 @=? (ѯ $ [ӭ $ ֆ [ә "A"]]) env1
        , testCase "retain A,c" $ env2 @=? (ѯ $ [ӭ $ ֆ [ә "A",ә "c"]]) env3
        , testCase "retain A,b,c" $ env3 @=? (ѯ $ [ӭ $ ֆ [ә"A",ә"b",ә"c"]]) env3
        , testCase "set keys" $ Set.fromList [ "A", "b" ] @=? _envKeys set
        , testCase "set" $ env1 @=? (ѯ [set]) ф
        , testCase "unset keys" $ Set.fromList [ "A", "b" ] @=? _envKeys unset
        , testCase "retain all;unset b" $
            env2 @=? (ѯ [ӭ $ ֆ [ә"A",ә"b",ә"c"],э $ ֆ [ә "b"]]) env3
        , testCase "retain all;unset A,c" $
            env0' @=? (ѯ [ӭ $ ֆ [ә"A",ә"b",ә"c"],э $ ֆ [ә"A",ә"c"]]) env3
        , testCase "retain A,b;unset b" $
            env0 @=? (ѯ [ӭ $ ֆ [ә"A",ә"b"],э $ ֆ [ә "b"]]) env3
        , testCase "unset b;retain all" $
            env2 @=? (ѯ [ӭ $ ֆ [ә"A",ә"b",ә"c"],э $ ֆ [ә "b"]]) env3
        , testCase "unset b;retain A,b" $
            env0 @=? (ѯ [ӭ $ ֆ [ә"A",ә"b"],э $ ֆ [ә "b"]]) env3
        , testCase "set A,b; adjust A" $
            Env [("A","v3v3"), ("b","v3")] @=? (ѯ [ э (ֆ [ә"A",ә"b"],ӛ"v3")
                                                  , э (("x"∷𝕋,ֆ [ә "A", ә "a"],
                                                        \ (v∷𝕊) → v⊕v))
                                                  ]) env3
        , testCase "adjust A; set A,b" $
            Env [("A","v3"), ("b","v3")] @=? (ѯ [ э (("x"∷𝕋,ֆ [ә "A", ә "a"],
                                                      \ (v∷𝕊) → v⊕v))
                                                , э (ֆ [ә"A",ә"b"],ӛ"v3")
                                                ]) env3
        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

e1 ∷ Env
e1 = Env $ Map.fromList [("a", "cat"), ("c", "dog")]

checkRun ∷ TestName → EnvMod → Env → TestTree
checkRun nm mod exp = testCase nm $ exp @=? runEnvMod mod e1

tests ∷ TestTree
tests = testGroup "Env.Types" [ omapTests, alterEnvModTests, clearEnvModTests
                              , mkEnvModFragTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ 𝕊 → IO ExitCode
_tests = runTestsP tests

_testr ∷ 𝕊 → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

