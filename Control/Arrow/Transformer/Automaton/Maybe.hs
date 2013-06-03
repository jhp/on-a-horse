module Control.Arrow.Transformer.Automaton.Maybe where

import Control.Arrow
import qualified Control.Category as C
import Control.Arrow.Operations
import qualified Control.Arrow.Transformer as AT
import Control.Arrow.Transformer.All

import Data.Maybe
import qualified Data.Map as M

--A MaybeAutomaton returns either Just f to indicate a new Automaton,
--or Nothing to indicate 'no change'/'do the same thing'.
newtype MaybeAutomaton a i o = 
    MaybeAutomaton (a i (o, Maybe (Automaton a i o)))

mAut (MaybeAutomaton f) = f

automatonFromMaybe f = 
    Automaton (mAut f >>> second (arr (fromMaybe (automatonFromMaybe f))))

maybeFromAutomaton (Automaton f) = 
    MaybeAutomaton (f >>> second (arr Just))


--Why use MaybeAutomata?  The arrow instances coalesce Nothings,
--reducing memory use.

instance (Arrow a) => C.Category (MaybeAutomaton a) where
    (.) f g = 
        MaybeAutomaton $ (mAut g) >>> (mAut f *** arr id) >>> 
               arr (\((o,f'),g') -> (o, 
                        case (f',g') of
                          (Nothing,Nothing) -> Nothing
                          (_,_) -> Just $ 
                                   fromMaybe (automatonFromMaybe f) f'
                                   C..
                                   fromMaybe (automatonFromMaybe g) g'))

    id = MaybeAutomaton (C.id >>> arr (flip (,) Nothing))

instance (Arrow a) => Arrow (MaybeAutomaton a) where
    arr f = MaybeAutomaton (arr f >>> arr (flip (,) Nothing))
    

    (***) f g = MaybeAutomaton $ (mAut f *** mAut g) >>> 
                arr (\((o1,f'),(o2,g')) -> 
                         ((o1,o2),
                          case (f',g') of
                            (Nothing,Nothing) -> Nothing
                            (_,_) -> Just $ 
                                     fromMaybe (automatonFromMaybe f) f'
                                     ***
                                     fromMaybe (automatonFromMaybe g) g'))

    first f = f *** arr id
    second f = arr id *** f
                                                   

instance (ArrowChoice a) => ArrowChoice (MaybeAutomaton a) where
    (+++) f g = MaybeAutomaton $ mAut f +++ mAut g >>>
                arr (\x -> case x of
                             Left (o,f') -> (Left o, 
                                fmap (+++ automatonFromMaybe g) f')
                             Right (o,g') -> (Right o, 
                                fmap (automatonFromMaybe f +++) g'))

    left f = f +++ arr id
    right f = arr id +++ f
