{-#LANGUAGE Arrows, RankNTypes, ScopedTypeVariables, FlexibleContexts,
  TypeSynonymInstances, NoMonomorphismRestriction, FlexibleInstances #-}

module Web.Horse.Forms where

import Web.Horse.Forms.Types
import Web.Horse.Forms.Basic
import Data.ByteString.Lazy (ByteString)
import Data.Monoid
import Data.List.Split (splitOn)
import Safe (readMay)
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Char
import Control.Arrow.Transformer.All
import Control.Arrow.Transformer.Automaton.Monad
import Control.Arrow.Transformer.LabeledArrow
import Control.Arrow.Operations hiding (write)

import Debug.Trace

valForm initVal vtor label = withInput $
    proc ((),nm,fi) -> do
      s_curr <- keepState initVal -< fi
      valid <- vtor -< s_curr
      case valid of
         Left err -> returnA -< (textField label (Just err) s_curr nm, 
                                                   Nothing)
         Right x -> returnA -< (textField label Nothing s_curr nm,
                                Just x)

stringForm = valForm "" (arr Right)

readForm = valForm "" (arr (\x -> maybe (Left ("No read: " ++ x)) Right (readMay x)))

enumForm
  :: (ArrowAddAutomaton a1 may a',
      ArrowAddLabel a may,
      ArrowReader FormIn may) =>
     String -> [(String, b)] -> a () (String, Maybe b)
enumForm label vs = withInput $
    (proc ((),nm,fi) -> do
       n_curr <- keepState (-1) -< extractNumber fi
       let n_val = max n_curr 0
           res = if n_curr < 0 then Nothing else (Just $ snd $ vs !! n_curr)
       returnA -< (select label (map fst vs) n_val nm, res))
           where
             extractNumber i = checkBounds $ readMay =<< i

             checkBounds Nothing = Nothing
             checkBounds (Just k) = if k >= 0 && k < length vs 
                                    then Just k else Nothing

runSubStream :: (ArrowChoice a) => a i o -> a (Maybe i) (Maybe o)
runSubStream f = proc i -> 
                   case i of
                          Just i' -> f >>> (arr Just) -< i'
                          Nothing -> returnA -< Nothing

filterDiffs
  :: (ArrowAddAutomaton a may a',
      Eq i,
      ArrowApply a') =>
     a i (Maybe i)
filterDiffs = monadToAuto $ \i1 -> do
                i2 <- co (Just i1)
                runFilter i1 i2
              where
                runFilter i1 = \i2 -> do
                               case i1 == i2 of
                                     True -> runFilter i1 =<< co Nothing
                                     False -> runFilter i2 =<< co (Just i2)

keepState s0 = monadToAuto (f s0)
               where
                 f s0' ms1 = f (fromMaybe s0' ms1) =<< co (fromMaybe s0' ms1)


{-
replaceSecond
  :: (ArrowAddAutomaton a a', ArrowApply a') =>
     a i o -> a (i, Maybe (a i o)) o
-}
replaceSecond g = liftAutomaton $ 
                  (proc (i,g_new) -> do
                     let g_curr = elimAutomaton (fromMaybe g g_new)
                     (o,g') <- g_curr -<< i
                     returnA -< (o, replaceSecond g'))
                                     
{-
once :: (ArrowAddAutomaton a a', ArrowApply a') => a1 -> a i (Maybe a1)
-}
once x = monadToAuto $ \_ -> co (Just x) >> forever (co (Nothing))

{-
auto :: Automaton a i o -> a i (o, Automaton a i o)
-}
auto (Automaton f) = f




formSum _ [] _ = error "formSum requires at least one argument"

formSum label fs def = catchMayAuto $ proc _ -> do
  (fo,f) <- enumForm label fs -< ()
  case f of
    Just f' -> throwMayAuto -< f'
    Nothing -> returnA -< setFormOut fo def


{-
throwMayAuto = proc f -> do
                 raise -< liftMaybeAutomaton $ LabeledArrow $
                       (arr (flip (,) noInput)
                            >>> (unLA (newReader (elimMaybeAutomaton f))))
-}

throwMayAuto = arr (fromMaybeAutomaton) >>> throwAuto 

throwAuto = proc f -> do
              raise -< liftAutomaton $ LabeledArrow $
                  (arr (flip (,) noInput) 
                         >>> (unLA (newReader (elimAutomaton f))))

linkForm linkName f = withInput $ proc ((),nm,iname) -> do
              case iname of
                Just _ -> throwMayAuto -< f
                Nothing -> returnA -< (link linkName nm)



staticUrls :: a -> [(String, a)] -> ([String] -> a)
staticUrls def fs s = 
    case lookup (filter (/= "") $ map (map toLower) s) 
             (map ((filter (/= "") . splitOn "/") *** id) fs) of
                    Nothing -> def
                    Just f -> f
