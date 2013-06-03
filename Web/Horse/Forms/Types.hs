{-#LANGUAGE Arrows, TypeSynonymInstances, FlexibleInstances,
  FlexibleContexts, NoMonomorphismRestriction #-}

module Web.Horse.Forms.Types where

import Control.Arrow.Transformer.Automaton.Monad
import Control.Arrow.Transformer.Automaton.Maybe
import Control.Arrow.Transformer.LabeledArrow
import Debug.Trace
import Data.List
import Control.Arrow
import Control.Arrow.Transformer.All
import Control.Arrow.Operations

type FormOut = String
newtype FormIn = FormIn [(String,String)] deriving (Show)

type HoH i o = LabeledArrow (ReaderArrow FormIn (Automaton (Kleisli IO))) i o
type HoHMay i o = LabeledArrow (ReaderArrow FormIn (MaybeAutomaton (Kleisli IO))) i o

type HoHErr ex i o = LabeledArrow (ErrorArrow ex (ReaderArrow FormIn (Automaton (Kleisli IO)))) i o
type HoHErrMay ex i o = LabeledArrow (ErrorArrow ex (ReaderArrow FormIn (MaybeAutomaton (Kleisli IO)))) i o


noInput :: FormIn
noInput = FormIn [] 

filterPrefix :: String -> FormIn -> FormIn
filterPrefix s (FormIn xss) = FormIn $ filter ((== s) . fst) xss

class HasFormOut o where
    getFormOut :: o -> FormOut
    setFormOut :: FormOut -> o -> o

instance HasFormOut FormOut where
    getFormOut = id
    setFormOut = const

instance HasFormOut (FormOut, i) where
    getFormOut (fo,_) = fo
    setFormOut fo (_,o) = (fo,o)

instance HasFormOut (FormOut, o1, o2) where
    getFormOut (fo,_,_) = fo
    setFormOut fo (_,o1,o2) = (fo,o1,o2)


getSingle :: FormIn -> Maybe String
getSingle (FormIn [(_,x)]) = Just x
getSingle _ = Nothing


withInput
  :: (ArrowReader FormIn a',
      ArrowAddLabel a a',
      ArrowAddAutomaton a1 a' a'1) =>
     a1 (e, String, Maybe String) b -> a e b
withInput = withInput0 . restify

withInput0 :: (ArrowReader FormIn a', 
              ArrowAddLabel a a')
             => a' (e,String,Maybe String) b -> a e b
withInput0 f = runLabel $ proc (e,lab) -> do
              fi <- readState -< ()
              f -< (e,show lab,getSingle $ filterPrefix (show lab) fi)

restify g = liftMaybeAutomaton $ proc (e,lab,inp) -> do
              (o,g') <- elimAutomaton g -< (e,lab,inp)
              case inp of
                Nothing -> returnA -< (o,Nothing)
                _ -> returnA -< (o, Just g')

catchAuto
  :: (ArrowAddAutomaton a may a') =>
     (LabeledArrow (ErrorArrow (LabeledArrow a i o) a)) i o 
         -> LabeledArrow a i o
catchAuto f = liftAutomaton $
              (LabeledArrow $
               unLA (elimAutomaton f) 
                >>> second (arr catchAuto)) `elimError` 
              (LabeledArrow $ proc (i,f') -> 
                   app -< (unLA (elimAutomaton f'), i))

catchMayAuto
  :: (ArrowAddAutomaton a may a') =>
     LabeledArrow (ErrorArrow (LabeledArrow a t1 o) may) t1 o
     -> LabeledArrow may t1 o
catchMayAuto f = liftMaybeAutomaton $
                 (LabeledArrow $
                  unLA (elimMaybeAutomaton f)
                  >>> second (arr (fmap (fromMaybeAutomaton . catchMayAuto . toMaybeAutomaton) ))) `elimError`
                 (LabeledArrow $ proc (i,f') -> do
                      (o,g) <- app -< (elimAutomaton $ unLA f', i)
                      returnA -< (o, Just $ LabeledArrow g))
