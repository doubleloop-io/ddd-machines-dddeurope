{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TupleSections #-}

module DDD where

import           Machines         (Mealy, MealyT, compose, feedback, run)

-- base
import qualified Control.Category as Cat (Category (id, (.)))

-- profunctors
import           Data.Profunctor

-- AGGREGATE

-- a machine from command to events
newtype Aggregate command event = Aggregate
  { aggregateMachine :: Mealy command [event] }

instance Profunctor Aggregate where
  lmap :: (a -> b) -> Aggregate b c -> Aggregate a c
  lmap f (Aggregate mealy) = Aggregate (lmap f mealy)

  rmap :: (b -> c) -> Aggregate a b -> Aggregate a c
  rmap f (Aggregate mealy) = Aggregate (rmap (fmap f) mealy)

instance Strong Aggregate where
  first' :: Aggregate a b -> Aggregate (a, c) (b, c)
  first' (Aggregate mealy) = Aggregate (rmap (\(bs, c) -> (, c) <$> bs) $ first' mealy)


-- POLICY

-- an effectful machine from event to commands
newtype Policy m event command = Policy
  { policyMachine :: MealyT m event [command] }

instance Monad m => Cat.Category (Policy m) where
  id :: Policy m a a
  id = Policy (rmap pure Cat.id)

  (.) :: Policy m b c -> Policy m a b -> Policy m a c
  (.) (Policy mealy1) (Policy mealy2) = Policy (compose [] mealy1 mealy2)

instance Functor m => Profunctor (Policy m) where
  lmap :: (a -> b) -> Policy m b c -> Policy m a c
  lmap f (Policy mealy) = Policy (lmap f mealy)

  rmap :: (b -> c) -> Policy m a b -> Policy m a c
  rmap f (Policy mealy) = Policy (rmap (fmap f) mealy)

instance Functor m => Strong (Policy m) where
  first' :: Policy m a b -> Policy m (a, c) (b, c)
  first' (Policy mealy) = Policy (rmap (\(bs, c) -> (, c) <$> bs) $ first' mealy)

-- PROJECTION

-- a machine from event to readModel
newtype Projection event readModel = Projection
  { projectionMachine :: Mealy event readModel }

instance Cat.Category Projection where
  id :: Projection a a
  id = Projection Cat.id

  (.) :: Projection b c -> Projection a b -> Projection a c
  (.) (Projection mealy1) (Projection mealy2) = Projection (mealy1 Cat.. mealy2)

instance Profunctor Projection where
  lmap :: (a -> b) -> Projection b c -> Projection a c
  lmap f (Projection mealy) = Projection (lmap f mealy)

  rmap :: (b -> c) -> Projection a b -> Projection a c
  rmap f (Projection mealy) = Projection (rmap f mealy)

instance Strong Projection where
  first' :: Projection a b -> Projection (a, c) (b, c)
  first' (Projection mealy) = Projection (first' mealy)

-- APPLICATION

-- a record that wire together the DDD building blocks
data Application m command event readModel = Application
  { aggregate  :: Aggregate command event
  -- NOTE: policy in optional
  , policy     :: Maybe (Policy m event command)
  , projection :: Projection event readModel
  }

-- typical "run something" function to execute a specific behavior
-- common Haskell pattern that similar to Method Object
runApplication
  :: (Monad m, Foldable t, Monoid readModel)
  -- wired application
  => Application m command event readModel
  -- a "list" of commands
  -> t command
  -- a effectful readModel
  -> m readModel
runApplication application commands =
  -- get result from the tuple
  fst
  -- map over the effect and get a tuple (result, newMachine)
  <$>
  -- run the composed machine, with empty initial state and list of commands
  run machine mempty commands
  where
    machine = compose
      -- empty final output, the readModel
      mempty
      -- after machine in the composition sense
      -- projectionMachine that update the readModel w/ a projection
      ((\p -> projectionMachine p) . projection $ application)
      -- before machine for two cases:
      --  no policy, so it's only the aggregateMachine
      --  has a policy, so it's a feedback composition of aggregateMachine and then policyMachine
      (case policy application of
        Nothing      -> (\a -> aggregateMachine a) . aggregate $ application
        Just policy' -> feedback ((\a -> aggregateMachine a) . aggregate $ application) (policyMachine policy'))
