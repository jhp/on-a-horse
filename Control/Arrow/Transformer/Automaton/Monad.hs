{-#LANGUAGE Arrows, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
  UndecidableInstances, FunctionalDependencies, NoMonomorphismRestriction #-}

module Control.Arrow.Transformer.Automaton.Monad
    (readerArrow, swapsnd, autoToMonad, co, monadToAuto,
     pushError,popError,rstrength,
     ArrowAddAutomaton (..), dispatch) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State (MonadState (..))

import Control.Arrow
import Control.Arrow.Operations
import qualified Control.Arrow.Transformer as AT
import Control.Arrow.Transformer.All
import Control.Arrow.Transformer.Automaton.Maybe

import Data.Maybe
import qualified Data.Map as M

unAM (ArrowMonad f) = f

monadToAuto
  :: (ArrowAddAutomaton a may a', ArrowApply a') =>
     (i -> ContT (o, a i o) (ArrowMonad a') z) -> a i o
monadToAuto f = liftAutomaton (proc i -> 
     unAM ((f i) `runContT` (error "automaton ended")) -<< ())


co
  :: (ArrowApply a', ArrowAddAutomaton a may a') =>
     o -> ContT (o, a i o) (ArrowMonad a') i
co o = ContT (\fi -> 
                  return (o, liftAutomaton (proc i -> unAM (fi i) -<< ())))

autoToMonad
  :: (ArrowApply a', ArrowAddAutomaton a may a') =>
     a i (Either o z)
     -> i
     -> ContT (o, a i o) (ArrowMonad a') z
autoToMonad f i = do
  x <- lift $ ArrowMonad $ (proc () -> elimAutomaton f -< i)
  case x of
    (Right z,_) -> return z
    (Left o,f') -> autoToMonad f' =<< co o



class (ArrowChoice a, ArrowChoice may, ArrowChoice a', ArrowApply a') 
    => ArrowAddAutomaton a may a' | a -> a', a -> may, may -> a where
    elimAutomaton :: a i o -> a' i (o, a i o)
    liftAutomaton :: a' i (o, a i o) -> a i o
    constantAutomaton :: a' i o -> a i o

    toMaybeAutomaton :: a i o -> may i o
    fromMaybeAutomaton :: may i o -> a i o
    liftMaybeAutomaton :: a' i (o, Maybe (a i o)) -> may i o
    elimMaybeAutomaton :: may i o -> a' i (o, Maybe (a i o))

instance (ArrowChoice a, ArrowApply a) => 
    ArrowAddAutomaton (Automaton a) (MaybeAutomaton a) a where

    elimAutomaton (Automaton f) = f
    liftAutomaton = Automaton
    constantAutomaton f = Automaton (f >>> 
                                     arr (flip (,) (constantAutomaton f)))

    toMaybeAutomaton = maybeFromAutomaton
    fromMaybeAutomaton = automatonFromMaybe
    liftMaybeAutomaton = MaybeAutomaton
    elimMaybeAutomaton = mAut

instance (Arrow a, Arrow may, Arrow a', ArrowAddAutomaton a may a') 
    => ArrowAddAutomaton (StateArrow s a) (StateArrow s may) (StateArrow s a') where
   elimAutomaton = autoState . elimAutomaton . runState 
   liftAutomaton = stateArrow . liftAutomaton . stateAuto
   constantAutomaton = stateArrow . constantAutomaton . runState

   toMaybeAutomaton = stateArrow . toMaybeAutomaton . runState
   fromMaybeAutomaton = stateArrow . fromMaybeAutomaton . runState
   liftMaybeAutomaton = error "not implemented yet"
   elimMaybeAutomaton = error "not implemented yet"
    

instance (ArrowState s a, ArrowApply a) => (MonadState s (ArrowMonad a)) where
    put s = ArrowMonad (proc () -> store -< s)
    get = ArrowMonad fetch

instance (Arrow a, Arrow a', ArrowAddAutomaton a may a') 
    => ArrowAddAutomaton (ReaderArrow r a) (ReaderArrow r may) (ReaderArrow r a') where
   elimAutomaton = (>>> (second (arr readerArrow))) . 
                   readerArrow . elimAutomaton . runReader

   elimMaybeAutomaton = (>>> (second (arr (fmap readerArrow)))) .
                        readerArrow . elimMaybeAutomaton . runReader

   liftAutomaton = readerArrow . liftAutomaton . 
                   (>>> (second (arr runReader))) . runReader

   liftMaybeAutomaton = readerArrow . liftMaybeAutomaton .
                        (>>> (second (arr (fmap runReader)))) . runReader
    

   constantAutomaton = readerArrow . constantAutomaton . runReader
   toMaybeAutomaton = readerArrow . toMaybeAutomaton . runReader
   fromMaybeAutomaton = readerArrow . fromMaybeAutomaton . runReader

instance (ArrowChoice a, ArrowChoice may, ArrowChoice a', ArrowAddAutomaton a may a')
    => ArrowAddAutomaton (ErrorArrow ex a) (ErrorArrow ex may) (ErrorArrow ex a') where
        elimAutomaton = pushError . 
                (>>> second (arr pushError) >>> arr rstrength) 
                . elimAutomaton . popError

        elimMaybeAutomaton = pushError .
                (>>> second (arr (fmap pushError)) >>> arr rstrength)
                . elimMaybeAutomaton . popError

        liftAutomaton f = 
            pushError $ liftAutomaton $
            (>>> arr (revrstrength (liftAutomaton f)) 
             >>> second (arr popError)) 
            $ popError f

        liftMaybeAutomaton f =
            pushError $ liftMaybeAutomaton $
            (>>> arr (revrstrength (Just $ fromMaybeAutomaton $ liftMaybeAutomaton f))
             >>> second (arr (fmap popError)))
            $ popError f

        constantAutomaton = pushError . constantAutomaton . popError
        toMaybeAutomaton = pushError . toMaybeAutomaton . popError
        fromMaybeAutomaton = pushError . fromMaybeAutomaton . popError


dispatch = dispatch0 M.empty

dispatch0
  :: (ArrowAddAutomaton a may a',
      Ord k) =>
     M.Map k (may i o) -> (k -> may i o) -> may (i, k) o
dispatch0 mp def = liftMaybeAutomaton $ proc (i,k) -> do
                    let f = fromMaybe (def k) (M.lookup k mp)
                    (o,f') <- app -< (elimMaybeAutomaton f,i)
                    case f' of
                      Nothing -> returnA -< (o, Nothing)
                      Just f' -> returnA -< (o, Just $ fromMaybeAutomaton $ dispatch0 (M.insert k (toMaybeAutomaton f') mp) def)
                    

--Utility functions

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), z) = ((x, z), y)

rstrength :: (Either ex a, b) -> Either ex (a, b)
rstrength (Left ex, _) = Left ex
rstrength (Right a, b) = Right (a, b)

revrstrength :: b -> Either ex (a,b) -> (Either ex a, b)
revrstrength def (Left ex) = (Left ex, def)
revrstrength _ (Right (a,b)) = (Right a, b)

autoState :: (Arrow a, Arrow a') => a' (i,s) ((o,s), a (i,s) (o,s)) -> 
             StateArrow s a' i (o,StateArrow s a i o)
autoState f = stateArrow $ f >>> second (arr stateArrow) >>> arr swapsnd

stateAuto :: (Arrow a, Arrow a') => StateArrow s a' i (o,StateArrow s a i o) ->
             a' (i,s) ((o,s), a (i,s) (o,s))
stateAuto f = runState (f >>> second (arr runState)) >>> arr swapsnd


--simulating the unexported data constructors for StateArrow,
--ReaderArrow, ErrorArrow

stateArrow :: (Arrow a) => a (t, s) (b, s) -> StateArrow s a t b
stateArrow f = proc i -> do
                 s <- fetch -< ()
                 (o,s') <- AT.lift f -< (i,s)
                 store -< s'
                 returnA -< o

readerArrow :: (Arrow a) => a (e,r) b -> ReaderArrow r a e b
readerArrow f = proc i -> do
                  r <- readState -< ()
                  AT.lift f -< (i,r)

popError :: (ArrowChoice a) => ErrorArrow ex a e b -> a e (Either ex b)
popError f = runError (f >>> arr Right) (arr snd >>> arr Left)

pushError :: (ArrowChoice a) => a e (Either ex b) -> ErrorArrow ex a e b
pushError f = (AT.lift f) >>> (raise ||| arr id)
