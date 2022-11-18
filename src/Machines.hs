{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}

module Machines where

import qualified Control.Arrow    as Arrow ((***))
import qualified Control.Category as Cat (Category (id, (.)))
import           Data.Bifunctor   (first)
import           Data.Foldable    (foldlM)
import           Data.Profunctor  (Profunctor (..), Strong (..))


-- define a Monad Transformer in order to make Mealy Monad
-- combinable with other Monads like IO
newtype MealyT m a b = MealyT
  -- when executed produces an effect with
  -- a value b (function result) and
  -- an updated machine
  { runMealyT :: a -> m (b, MealyT m a b) }

instance Monad m => Cat.Category (MealyT m) where
  id :: MealyT m a a
  id = stateless id

  (.) :: MealyT m b c -> MealyT m a b -> MealyT m a c
  (.) (MealyT m1) (MealyT m2) = MealyT $ \a -> do
    -- (resultFirstMachine, newFirstMachine)
    (b, m2') <- m2 a
    -- (resultSecondMachine, newSecondMachine)
    (c, m1') <- m1 b
    -- (resultSecondMachine, the composition of newSecondMachine and newFirstMachine)
    pure (c, m1' Cat.. m2')

instance Functor m => Profunctor (MealyT m) where
  lmap :: (a -> b) -> MealyT m b c -> MealyT m a c
  lmap f (MealyT m) = MealyT $ fmap (fmap $ lmap f) <$> m . f

  rmap :: (b -> c) -> MealyT m a b -> MealyT m a c
  rmap f (MealyT m) = MealyT $ fmap (f Arrow.*** rmap f) <$> m

instance Functor m => Strong (MealyT m) where
  first' :: MealyT m a b -> MealyT m (a, c) (b, c)
  first' (MealyT m) = MealyT $ \(a, c) -> ((, c) Arrow.*** first') <$> m a

(***) :: (Cat.Category p, Strong p) => p a b -> p c d -> p (a, c) (b, d)
(***) pab pcd = second' pcd Cat.. first' pab

(&&&) :: (Cat.Category p, Strong p) => p a b -> p a c -> p a (b, c)
(&&&) pab pac = (pab *** pac) Cat.. dimap id (\a -> (a, a)) Cat.id

-- define the Monad as type alias of the Monad Transformer
-- in other words define a more closed type from a more open one
-- typical Haskell pattern
type Mealy input output = forall m . Monad m => MealyT m input output


-- build a machine transformer from raw types
mealyT
  :: Functor m
  -- state, action: effect (result, newState)
  => (s -> a -> m (b, s))
  -- currentState
  -> s
  -- newMachineT with result as output
  -> MealyT m a b
mealyT f = MealyT . (fmap . fmap . fmap $ mealyT f) . f

-- build a machine from raw types
mealy
  -- state, action: result, newState
  :: (s -> a -> (b, s))
  -- currentState
  -> s
  -- newMachine with result as output
  -> Mealy a b
mealy f s = (mealyT . (fmap . fmap $ pure)) f s

statefulT :: Functor m => (s -> a -> m s) -> s -> MealyT m a s
statefulT = mealyT . ((fmap (\a -> (a, a)) .) .)

-- build a machine from raw types
stateful
  -- state, action: newState
  :: (s -> a -> s)
  -- currentState
  -> s
  -- newMachine with state as output
  -> Mealy a s
stateful f s = (statefulT . (fmap . fmap $ pure)) f s

mooreT :: Functor m => (s -> m (b, a -> s)) -> s -> MealyT m a b
mooreT f = mealyT (\s a -> fmap ($ a) <$> f s)

-- build a machine from raw types
moore
  -- state: result, function action to newState
  :: (s -> (b, a -> s))
  -- currentState
  -> s
  -- newMachine with result as output
  -> Mealy a b
moore f s = (mooreT . (\f' s' -> pure (($) <$> f' s'))) f s

statelessT :: Functor m => (a -> m b) -> MealyT m a b
statelessT f = mealyT (\() a -> (, ()) <$> f a) ()

-- build a machine from raw types
stateless
  -- function a to b
  :: (a -> b)
  -- newMachine with result as output
  -> Mealy a b
stateless f = (statelessT . (pure .)) f

{- | Iteratively passes a sequence of arguments to a machine accumulating the results in a Semigroup.
It returns also a new version of the machine with the status updated after all the applications.
-}
run
  :: (Monad m, Semigroup b, Foldable f)
  -- a machine
  -- es Application: projectionMachine . aggregateMachine
  -- es Application w/ policy: projectionMachine . feedback (policyMachine . aggregateMachine)
  => MealyT m a b
  -- combinable output
  -- es Application final output: readModel of a projection
  -> b
  -- a list of inputs
  -- es Application: the input commands
  -> f a
  -- effectful machine's final result (like runMealyT
  -- es Application: (readModel, updatedApplication)
  -> m (b, MealyT m a b)
run mealy' initial =
  -- iterate over f a (not visibile here due to eta-reduction)
  -- es Application: commands
  foldlM
  -- (lastUpdatedOutput, lastUpdatedMachine) current a
  -- es Application: (lastReadModel, lastApplication) currentCommand
  (\(b, mealy'') a ->
    -- combine the left/first part of the tuple with the accumulated result
    -- es Application: "merge" lastReadModel with the currentReadModel
    first (b <>)
    -- map over the effect and get the tuple
    <$>
    -- run the machine and get a tuple in an effect
    runMealyT mealy'' a)
  (initial, mealy')

-- combine machines
-- es: projectionMachine after aggregateMachine
compose
  :: (Monad m, Semigroup c, Foldable f)
  -- combinable output
  => c
  -- effectful machine from b to c
  -> MealyT m b c
  -- effectful machine from a to list of b
  -> MealyT m a (f b)
  -- effectful machine from a to c
  -> MealyT m a c
compose c p q = MealyT $ \a -> do
  -- run second machine and get a list of b
  (fb, q') <- runMealyT q a
  -- run first machine that require one b from a list of b
  (c', p') <- run p c fb
  -- (result, the composition of the updated machine)
  pure (c', compose c' p' q')

-- combine machines
-- es: one the write side allows us to combine Aggregate and Policy
feedback
  -- m is the effect
  -- f is the "list" of something
  :: (Monad m, Foldable f, Monoid (f a), Monoid (f b))
  -- effectful machine from a to list of b
  -- es Aggregate: Mealy command [event]
  => MealyT m a (f b)
  -- effectful machine from b to list of a
  -- es Policy: Mealy event [command]
  -> MealyT m b (f a)
  -- effectful machine from a to list of b
  -- es richer Aggregate: Mealy command [event]
  -> MealyT m a (f b)
feedback m1 m2 = MealyT $ \a -> do
  -- run first machine
  -- es run Aggregate: ([event], updatedAggregate)
  (bs, m1') <- runMealyT m1 a

  -- run second machine
  -- es run Policy: ([command], executedPolicy)
  (as, m2') <- run m2 mempty bs

  -- recursively run the whole machine with the states updated
  -- NOTE: run just one more time the aggregate logic
  -- es run updatedAggregate with commands produced by executedPolicy
  (bs', m12) <- run (feedback m1' m2') mempty as

  -- merge the two list of outputs of the first machine
  -- es Aggregate: merge list of events or commands
  pure (bs <> bs', m12)
