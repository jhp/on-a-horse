{-#LANGUAGE Arrows, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
  UndecidableInstances, FunctionalDependencies, NoMonomorphismRestriction #-}

module Control.Arrow.Transformer.LabeledArrow where

import qualified Control.Category as C
import Control.Monad
import Control.Monad.Cont

import Control.Arrow
import Control.Arrow.Operations
import qualified Control.Arrow.Transformer as AT
import Control.Arrow.Transformer.All
import Control.Arrow.Transformer.Automaton.Monad

import Data.Maybe
import Data.List
import Data.List.Split (splitOn)
import qualified Data.Map as M

newtype Label = Label Integer

base64 = "abcdefghijklmnopqrstuvwxyz"++
         "ABCDEFGHIJKLMNOPQRSTUVWXYZ"++
         "1234567890_:"

instance Show Label where
    show (Label 0) = ""
    show (Label n) = [(base64 !! fromIntegral mod)] ++ show (Label div)
        where
          (div,mod) = divMod n 64
    
instance Read Label where
    readsPrec _ s = case read64 s of
                      Just x -> [(Label x, "")]
                      Nothing -> []

read64 [] = Just 0
read64 (c:cs) = case lookup c (zip base64 [0..]) of
                  Just n -> fmap (\k -> n + 64*k) (read64 cs)
                  Nothing -> Nothing

alterReader q f = proc i -> do
  s <- readState -< ()
  newReader f -< (i,q s)

pushId x = alterReader (\(Label z) -> Label (2*z + x))

class (Arrow a, Arrow a') => ArrowAddLabel a a' | a -> a' where
    runLabel :: a' (e,Label) b -> a e b

instance Arrow a => ArrowAddLabel (LabeledArrow a) a where
    runLabel = runArrowLabel

instance (ArrowAddLabel a a') => 
    ArrowAddLabel (ReaderArrow r a) (ReaderArrow r a') where
    runLabel f = readerArrow $ runLabel (arr swapsnd >>> runReader f)

instance (ArrowAddLabel a a',ArrowChoice a, ArrowChoice a') => 
    ArrowAddLabel (ErrorArrow ex a) (ErrorArrow ex a') where
    runLabel f = pushError $ runLabel $ popError f

instance ArrowReader r a => ArrowReader r (LabeledArrow a) where
    readState = LabeledArrow (AT.lift readState)
    newReader (LabeledArrow f) = LabeledArrow $ readerArrow $ 
                                 arr (swapsnd) >>> newReader (runReader f) 

runArrowLabel f = LabeledArrow (proc e -> do
                                  lab <- readState -< ()
                                  AT.lift f -< (e,lab))

newtype LabeledArrow a i o = LabeledArrow (ReaderArrow Label a i o)

unLA (LabeledArrow f) = f

runLabeledArrow (LabeledArrow f) = proc i -> do
                           runReader f -< (i,Label 1)

instance (C.Category a, Arrow a) => C.Category (LabeledArrow a) where
    (.) (LabeledArrow f) (LabeledArrow g) = 
        LabeledArrow $ (C..) (pushId 1 f)  (pushId 0 g)
    id = LabeledArrow (C.id)

instance Arrow a => Arrow (LabeledArrow a) where
    arr = LabeledArrow . arr
    (***) (LabeledArrow f) (LabeledArrow g) = 
        LabeledArrow $ (pushId 0 f *** pushId 1 g)
    first f = f *** (arr id)

instance (Arrow a, Arrow a', ArrowAddAutomaton a may a') =>
    ArrowAddAutomaton (LabeledArrow a) (LabeledArrow may) (LabeledArrow a') where
        liftAutomaton (LabeledArrow f) = 
            LabeledArrow $ liftAutomaton (f >>> second (arr unLA))

        liftMaybeAutomaton (LabeledArrow f) =
            LabeledArrow $ liftMaybeAutomaton (f >>> second (arr (fmap unLA)))

        elimAutomaton (LabeledArrow f) = 
            LabeledArrow $ elimAutomaton f >>> second (arr LabeledArrow)

        elimMaybeAutomaton (LabeledArrow f) =
            LabeledArrow $ elimMaybeAutomaton f 
                             >>> second (arr (fmap LabeledArrow))

        constantAutomaton = LabeledArrow . constantAutomaton . unLA
        toMaybeAutomaton = LabeledArrow . toMaybeAutomaton . unLA
        fromMaybeAutomaton = LabeledArrow . fromMaybeAutomaton . unLA

instance (ArrowError ex a) => (ArrowError ex (LabeledArrow a)) where
    raise = LabeledArrow raise
    tryInUnless f g h = LabeledArrow $ tryInUnless (unLA f) (unLA g) (unLA h)

instance (ArrowAddError ex a a') 
    => ArrowAddError ex (LabeledArrow a) (LabeledArrow a')
    where
      liftError = LabeledArrow . liftError . unLA
      elimError (LabeledArrow f) (LabeledArrow h) = 
          LabeledArrow $ elimError f h

instance (ArrowApply a) => ArrowApply (LabeledArrow a) where
    app = LabeledArrow (first (arr unLA) >>> app)

instance ArrowChoice a => ArrowChoice (LabeledArrow a) where
    (+++) (LabeledArrow f) (LabeledArrow g) = 
        LabeledArrow $ pushId 0 f +++ pushId 1 g

    left f = f +++ arr id


writeState l = LabeledArrow (proc i -> do
               s <- readState -< ()
               AT.lift (Kleisli (print)) -< (l,s))

foo = (`runKleisli` ()) $ runLabeledArrow $ 
      ( writeState "a" >>> writeState "b" >>> writeState "c")
