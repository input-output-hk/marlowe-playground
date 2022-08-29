module Language.Marlowe.ToTerm where

import Prelude

import Language.Marlowe.Core.V1.Semantics.Types as S
import Language.Marlowe.Extended.V1 as E
import Marlowe.Holes (Location(..), Term(..), TermWrapper(..))
import Marlowe.Holes as T

-- This class (and Module) helps to translate from Extended Marlowe (and eventually Semantic)
-- to the Term level, which can be useful for pretty printing for example.
class ToTerm :: forall k. (k -> Type) -> Type -> k -> Constraint
class ToTerm wrap from to where
  toTerm :: from -> wrap to

instance ToTerm Term S.Bound T.Bound where
  toTerm (S.Bound a b) = Term (T.Bound a b) NoLocation

instance ToTerm Term E.Timeout T.Timeout where
  toTerm (E.TimeParam s) = Term (T.TimeParam s) NoLocation
  toTerm (E.TimeValue v) = Term (T.TimeValue v) NoLocation

instance ToTerm TermWrapper S.ValueId T.ValueId where
  toTerm (S.ValueId s) = TermWrapper (T.ValueId s) NoLocation

instance ToTerm Term S.Party T.Party where
  toTerm (S.PK pubKey) = Term (T.PK pubKey) NoLocation
  toTerm (S.Role tokenName) = Term (T.Role tokenName) NoLocation

instance ToTerm Term E.Payee T.Payee where
  toTerm (E.Account a) = Term (T.Account (toTerm a)) NoLocation
  toTerm (E.Party p) = Term (T.Party (toTerm p)) NoLocation

instance ToTerm Term S.Token T.Token where
  toTerm (S.Token c t) = Term (T.Token c t) NoLocation

instance ToTerm Term E.Value T.Value where
  toTerm (E.AvailableMoney accountId token) = Term
    (T.AvailableMoney (toTerm accountId) (toTerm token))
    NoLocation
  toTerm (E.Constant number) = Term (T.Constant number) NoLocation
  toTerm (E.ConstantParam name) = Term (T.ConstantParam name) NoLocation
  toTerm (E.NegValue v) = Term (T.NegValue (toTerm v)) NoLocation
  toTerm (E.AddValue l r) = Term (T.AddValue (toTerm l) (toTerm r)) NoLocation
  toTerm (E.SubValue l r) = Term (T.SubValue (toTerm l) (toTerm r)) NoLocation
  toTerm (E.MulValue l r) = Term (T.MulValue (toTerm l) (toTerm r)) NoLocation
  toTerm (E.DivValue l r) = Term (T.DivValue (toTerm l) (toTerm r)) NoLocation
  toTerm (E.ChoiceValue (S.ChoiceId s p)) = Term
    (T.ChoiceValue (T.ChoiceId s (toTerm p)))
    NoLocation
  toTerm (E.TimeIntervalStart) = Term (T.TimeIntervalStart) NoLocation
  toTerm (E.TimeIntervalEnd) = Term (T.TimeIntervalEnd) NoLocation
  toTerm (E.UseValue vid) = Term (T.UseValue (toTerm vid)) NoLocation
  toTerm (E.Cond obs t f) = Term (T.Cond (toTerm obs) (toTerm t) (toTerm f))
    NoLocation

instance ToTerm Term E.Observation T.Observation where
  toTerm (E.AndObs l r) = Term (T.AndObs (toTerm l) (toTerm r)) NoLocation
  toTerm (E.OrObs l r) = Term (T.OrObs (toTerm l) (toTerm r)) NoLocation
  toTerm (E.NotObs o) = Term (T.NotObs (toTerm o)) NoLocation
  toTerm (E.ChoseSomething (S.ChoiceId s p)) = Term
    (T.ChoseSomething (T.ChoiceId s (toTerm p)))
    NoLocation
  toTerm (E.ValueGE l r) = Term (T.ValueGE (toTerm l) (toTerm r)) NoLocation
  toTerm (E.ValueGT l r) = Term (T.ValueGT (toTerm l) (toTerm r)) NoLocation
  toTerm (E.ValueLT l r) = Term (T.ValueLT (toTerm l) (toTerm r)) NoLocation
  toTerm (E.ValueLE l r) = Term (T.ValueLE (toTerm l) (toTerm r)) NoLocation
  toTerm (E.ValueEQ l r) = Term (T.ValueEQ (toTerm l) (toTerm r)) NoLocation
  toTerm (E.TrueObs) = Term (T.TrueObs) NoLocation
  toTerm (E.FalseObs) = Term (T.FalseObs) NoLocation

instance ToTerm Term E.Action T.Action where
  toTerm (E.Deposit a p t v) = Term
    (T.Deposit (toTerm a) (toTerm p) (toTerm t) (toTerm v))
    NoLocation
  toTerm (E.Choice (S.ChoiceId s p) bounds) = Term
    (T.Choice (T.ChoiceId s (toTerm p)) (toTerm <$> bounds))
    NoLocation
  toTerm (E.Notify o) = Term (T.Notify (toTerm o)) NoLocation

instance ToTerm Term E.Case T.Case where
  toTerm (E.Case a c) = Term (T.Case (toTerm a) (toTerm c)) NoLocation

instance ToTerm Term E.Contract T.Contract where
  toTerm E.Close = Term (T.Close) NoLocation
  toTerm (E.Pay a p t v c) = Term
    (T.Pay (toTerm a) (toTerm p) (toTerm t) (toTerm v) (toTerm c))
    NoLocation
  toTerm (E.If o t e) = Term (T.If (toTerm o) (toTerm t) (toTerm e)) NoLocation
  toTerm (E.When cases t c) = Term
    (T.When (toTerm <$> cases) (toTerm t) (toTerm c))
    NoLocation
  toTerm (E.Let vid v c) = Term (T.Let (toTerm vid) (toTerm v) (toTerm c))
    NoLocation
  toTerm (E.Assert o c) = Term (T.Assert (toTerm o) (toTerm c)) NoLocation
