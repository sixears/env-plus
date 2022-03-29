module Env.Reader
  ( EnvReader
  , envAsk, envGet, envGetT, envLookup, envLookupM
  , envParse, envParseE, envParseY, envParseYE
  , runEnv

  , tests
  )
where

import Base1T

-- base --------------------------------

import Data.Typeable  ( TypeRep )
import Text.Read      ( Read, readMaybe )

-- containers --------------------------

import qualified Data.Map  as  Map

-- data-textual ------------------------

import Data.Textual  ( Parsed( Malformed, Parsed ), Textual, parseString )

-- lens --------------------------------

import Control.Lens.Prism  ( prism )

-- mtl ---------------------------------

import Control.Monad.Except  ( runExceptT )
import Control.Monad.Reader  ( MonadReader, ReaderT, ask, runReaderT )

-- unix --------------------------------

import System.Posix.Env  ( setEnv )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Env        ( getEnvironment )
import Env.Types  ( Env( unEnv ), EnvKey, EnvVal )
import Env.Error  ( AsEnvError( _EnvError ), EnvError
                  , envNoParse_, missingEnv, missingEnv_ )

-------------------------------------------------------------------------------

type EnvReader = MonadReader Env

runEnv ∷ MonadIO m ⇒ ReaderT Env m a → m a
runEnv f = getEnvironment ≫ runReaderT f

----------------------------------------

{- | Retrieve an EnvVal from the environment, if present. -}

envAsk ∷ EnvReader μ ⇒ EnvKey → μ (Maybe EnvVal)
envAsk k = Map.lookup k ∘ unEnv ⊳ ask

{- | Retrieve an `EnvVal` from the environment, throwing an `EnvErr` if it's not
     present. -}

envGet ∷ (EnvReader η, AsEnvError ε, MonadError ε η)
        ⇒ EnvKey → η EnvVal
envGet k = ask ≫ maybe (missingEnv_ k) return ∘ Map.lookup k ∘ unEnv

{- | Retrieve an `EnvVal` from the environment, parsing it as a `Textual` value;      throw an `EnvErr` if the key is not present or if the value does not parse.
 -}

envGetT ∷ (EnvReader η, AsEnvError ε, MonadError ε η, Textual α) ⇒
          EnvKey → TypeRep → η α
envGetT k t = do
  v ← envGet k
  case parseString $ toString v of
    Parsed a      → return a
    Malformed _ x → envNoParse_ k v x t

----------------------------------------

{- | Parse a value from the environment, with a custom parser.  Will throw if
     the key is not present in the environment. -}

envParse ∷ (EnvReader η, MonadError ε η, AsEnvError ε) ⇒
           (String → ExceptT ε η α) → EnvKey → η α
envParse f k = do
  v ← envGet k
  x ← runExceptT $ f (toString v)
  case x of
    Right a → return a
    Left  e → throwError e

--------------------

envParseTests ∷ TestTree
envParseTests =
  let parse ∷ (MonadIO μ, AsEnvError ε, AsTestError ε String, MonadError ε η,
               Read α) ⇒
              EnvKey → μ (η α)
      parse b = ѥ (runEnv $ envParse readErr b)
      parseNat ∷ EnvKey → IO (Either (EnvTestError String) ℕ)
      parseNat = parse
      x  = "x" ∷ String
      kx = "TEST_ENV_ERRX" ∷ EnvKey
   in testGroup "envParse"
                [ testCase "TEST_ENV_VAL" $ setEnv "TEST_ENV_VAL" "7" 𝕿 ⪼
                      parseNat "TEST_ENV_VAL" ≫ (Right 7 @=?)
                , testCase "TEST_ENV_ERR" $ setEnv "TEST_ENV_ERR" x 𝕿 ⪼
                      parseNat "TEST_ENV_ERR" ≫ (Left (asTestError x) @=?)
                , testCase "TEST_ENV_ERRX" $
                      (parseNat kx) ≫ (Left (missingEnv kx) @=?)
                ]

----------------------------------------

{- | Like `envParse`, but with a parser that produces an Either value -}

envParseE ∷ (EnvReader η, MonadError ε η, AsEnvError ε) ⇒
            (String → Either ε α) → EnvKey → η α
envParseE f k = do
  v ← envGet k
  case f (toString v) of
    Right a → return a
    Left  e → throwError e

--------------------

envParseETests ∷ TestTree
envParseETests =
  let parse2 ∷ (MonadIO μ, AsEnvError ε, AsTestError ε String, MonadError ε η,
                Read α) ⇒
               EnvKey → μ (η α)
      parse2 = splitMError ∘ runEnv ∘ envParseE readErr
      parseNat2 ∷ EnvKey → IO (Either (EnvTestError String) ℕ)
      parseNat2 = parse2
      y = "y" ∷ String
      kx = "TEST_ENV_ERRX" ∷ EnvKey
   in testGroup "envParseE"
                [ testCase "TEST_ENV_VAR2" $ setEnv "TEST_ENV_VAR" "8" 𝕿 ⪼
                      (parseNat2 "TEST_ENV_VAR") ≫ (Right 8 @=?)
                , testCase "TEST_ENV_ERR2" $ setEnv "TEST_ENV_ERR" y 𝕿 ⪼
                      (parseNat2 "TEST_ENV_ERR") ≫ (Left (asTestError y) @=?)
                , testCase "TEST_ENV_ERRX" $
                      (parseNat2 kx) ≫ (Left (missingEnv kx) @=?)
                ]

----------------------------------------

{- | Like `envParse`, but gives a `Nothing` when the envkey is missing, rather
     than throwing an `EnvErr` -}

envParseY ∷ (EnvReader η, MonadError ε η) ⇒
            (String → ExceptT ε η α) → EnvKey → η (Maybe α)
envParseY f k = do
  envAsk k ≫ \ case
    Nothing → return Nothing
    Just v  → runExceptT (f (toString v)) ≫ \ case
      Right a → return (Just a)
      Left  e → throwError e

envParseYTests ∷ TestTree
envParseYTests =
  let parseY ∷ (MonadIO μ, AsTestError ε String, MonadError ε η, Read α) ⇒
               EnvKey → μ (η (Maybe α))
      parseY = splitMError ∘ runEnv ∘ envParseY readErr
      parseNatY ∷ EnvKey → IO (Either (TestError String) (Maybe ℕ))
      parseNatY = parseY
      z = "z" ∷ String
   in testGroup "envParseY"
                [ testCase "TEST_ENV_VAR2" $ setEnv "TEST_ENV_VAR" "13" 𝕿 ⪼
                      (parseNatY "TEST_ENV_VAR") ≫ (Right (Just 13) @=?)
                , testCase "TEST_ENV_ERR" $ setEnv "TEST_ENV_ERR" z 𝕿 ⪼
                      parseNatY "TEST_ENV_ERR" ≫ (Left (asTestError z) @=?)
                , testCase "TEST_ENV_ERRX" $
                      (parseNatY "TEST_ENV_ERRX") ≫ (Right Nothing @=?)
                ]

----------------------------------------

{- | Like `envParseY`, but takes an `Either`-producing parser, and gives an
     `Either`-based result. -}

envParseYE ∷ EnvReader η ⇒
             (String → Either ε α) → EnvKey → η (Either ε (Maybe α))
envParseYE f k = do
  envAsk k ≫ \ case
    Nothing → return (Right Nothing)
    Just v  → case f (toString v) of
      Right a → return (Right $ Just a)
      Left  e → return (Left e)

envParseYETests ∷ TestTree
envParseYETests =
  let parseY' ∷ (MonadIO μ, AsTestError ε String, Read α) ⇒
                EnvKey → μ (Either ε (Maybe α))
      parseY' = runEnv ∘ envParseYE readErr
      parseNatY' ∷ EnvKey → IO (Either (TestError String) (Maybe ℕ))
      parseNatY' = parseY'
      z = "z" ∷ String
   in testGroup "envParseYE"
                [ testCase "TEST_ENV_VAR2" $ setEnv "TEST_ENV_VAR" "7" 𝕿 ⪼
                      (parseNatY' "TEST_ENV_VAR") ≫ (Right (Just 7) @=?)
                , testCase "TEST_ENV_ERR" $ setEnv "TEST_ENV_ERR" z 𝕿 ⪼
                      parseNatY' "TEST_ENV_ERR" ≫ (Left (asTestError z) @=?)
                , testCase "TEST_ENV_ERRX" $
                      (parseNatY' "TEST_ENV_ERRX") ≫ (Right Nothing @=?)
                ]

----------------------------------------

{- | Apply a monadic fn to a possible env value. -}

envLookupM ∷ EnvReader μ ⇒ (EnvVal → μ α) → EnvKey → μ (Maybe α)
envLookupM f k = do
  v  ← envAsk k
  sequence $ f <$> v

----------------------------------------

{- | Apply a fn to a possible env value. -}

envLookup ∷ EnvReader μ ⇒ (EnvVal → α) → EnvKey → μ (Maybe α)
envLookup f = fmap (f <$>) ∘ envAsk

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

data Show α ⇒ TestError α = TestError α
  deriving (Eq, Show)

class Show α ⇒ AsTestError ε α where
  _TestError ∷ Prism' ε (TestError α)

instance Show α ⇒ AsTestError (TestError α) α where
  _TestError = id

asTestError ∷ AsTestError ε α ⇒ α → ε
asTestError x = _TestError # TestError x

asTestError_ ∷ (AsTestError ε α, MonadError ε η) ⇒ α → η β
asTestError_ = throwError ∘ asTestError

data Show α ⇒ EnvTestError α = ETE_ENV_ERROR  EnvError
                             | ETE_TEST_ERROR (TestError α)
  deriving (Eq, Show)

instance AsEnvError (EnvTestError α) where
  _EnvError = prism ETE_ENV_ERROR
                    (\ case (ETE_ENV_ERROR e) → Right e; x → Left x)

instance Show α ⇒ AsTestError (EnvTestError α) α where
  _TestError = prism ETE_TEST_ERROR
                     (\ case (ETE_TEST_ERROR e) → Right e; x → Left x)

readErr ∷ (AsTestError ε String, MonadError ε η, Read α) ⇒ String → η α
readErr s = maybe (asTestError_ s) return $ readMaybe s

------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Env.Reader" [ envParseTests, envParseETests
                               , envParseYTests, envParseYETests
                               ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ---------------------------------------------------------
