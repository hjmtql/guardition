module Guardition where

import Prelude
import Data.Array (partition)
import Data.Symbol (class IsSymbol, SProxy(..))
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Type.Data.RowList (RLProxy(..))

guardition ::
  forall row row' rl a.
  RL.RowToList row rl =>
  Guardition row rl row' a =>
  Array a -> Record row -> Record row'
guardition = buildGuardition (RLProxy :: RLProxy rl)

class Guardition (row :: # Type) (xs :: RL.RowList) (row' :: # Type) a | row xs -> row' a where
  buildGuardition :: RLProxy xs -> Array a -> Record row -> Record row'

instance guarditonCons ::
  ( R.Cons name (a -> Boolean) trash row
  , R.Cons name (Array a) from to
  , IsSymbol name
  , R.Lacks name from
  , R.Lacks "otherwise" row
  , Guardition row tail from a
  ) =>
  Guardition row (RL.Cons name f tail) to a where
  buildGuardition _ xs r = Record.insert nameP res.yes $ buildGuardition rlP res.no r
    where
    rlP = RLProxy :: RLProxy tail

    nameP = SProxy :: SProxy name

    f = Record.get nameP r

    res = partition f xs

instance guarditonNil :: Guardition row RL.Nil ( otherwise :: Array a ) a where
  buildGuardition _ xs _ = { otherwise: xs }
