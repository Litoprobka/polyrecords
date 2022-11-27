{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module Polyrecords (
    Record(..),
    getter, setter', plens', plens
) where

import           Control.Lens      (Lens, Lens', set, (^.))
import qualified Control.Lens.Lens as Lens (lens)
import           Data.Kind
import           Data.Type.Bool
import           Data.Type.Ord
import           GHC.TypeLits

infixl 6 := -- so that it works with []
data (:=) (name :: Symbol) a

infixr 5 :*:

infixr 0 $
type (f :: k -> k2) $ (a :: k) = f a

infixr 4 ==?
-- why do I have to write this myself?
type family a ==? b where
    a ==? a = True
    _ ==? _ = False

data Peano = Z | S Peano

data Record r where
    RNil :: Record '[]
    (:*:) :: forall (name :: Symbol) a r.
        NoField name (Record r)
        => a -> Record r -> Record (name := a : r)

-- * Record comparison stuff * --

-- assumes that a record never contains multiple fields with the same name
type instance Compare (name := a) (name' := b) = CmpSymbol name name'

infixr 5 :<:
-- | sorted insertion; should replace : in :*:, but the type checker has trouble with it
type family a :<: r where
    a :<: '[] = '[a]
    a :<: (b : r') = If (a >? b) (b : a :<: r') (a : b : r')


type family RecEq r1 r2 where
    RecEq (Record r1) (Record r2) = SRec r1 ==? SRec r2

type family Sort list where
    Sort '[] = '[]
    Sort (x : xs) = x :<: Sort xs

-- Record with sorted fields
type SRec r = Record $ Sort r

type FieldI :: Type -> [Type] -> Peano
type family FieldI field fs where
    FieldI f (f : rest) = Z
    FieldI f (f' : rest) = S $ FieldI f rest

type NoField :: Symbol -> Type -> Constraint
type family NoField name r where
    NoField name (Record '[]) = ()
    NoField name (Record $ name := a : ns) = Int ~ Bool
    NoField name (Record $ _ : ns) = NoField name (Record ns)

class FieldI (name := a) ns ~ i => HasLens' (name :: Symbol) (i :: Peano) a ns | ns -> a where
    getter :: Record ns -> a
    setter' :: Record ns -> a -> Record ns

    plens' :: Lens' (Record ns) a
    plens' = Lens.lens (getter @name) (setter' @name) -- todo: fundep for i

instance (r ~ (name := a : ns), FieldI (name := a) r ~ Z) => HasLens' name Z a r where
    getter (x :*: _) = x
    setter' (_ :*: r') y = y :*: r'

instance (r ~ (name' := b : ns), FieldI (name := a) r ~ S i, HasLens' name i a ns) => HasLens' name (S i) a r where
    getter (_ :*: r') = getter @name @i r'
    setter' (x :*: r') y = (:*:) @name' x $ setter' @name @i r' y

type family ReplaceField (name :: Symbol) b r where
    ReplaceField name b '[] = '[]
    ReplaceField name b (name := a : ns) = name := b : ns
    ReplaceField name b (name':= c : ns) = name' := c : ReplaceField name b ns

-- * Stuff that does not work yet * --


-- | This class would provide a full-featured lens, but the type checker has trouble with ReplaceField
class HasLens' name n a ns => HasLens name n a ns where
    setter :: Record ns -> b -> Record (ReplaceField name b ns)

    plens :: Lens (Record ns) (Record (ReplaceField name b ns)) a b
    plens = Lens.lens (getter @name) (setter @name)

instance HasLens name Z a (name := a : ns) where
    setter :: Record (name := a : ns) -> b -> Record (name := b : ns)
    setter (_ :*: r') y = y :*: r'

{-
instance HasLens' name (S i) a (name' := c : ns) => HasLens name (S i) a (name' := c : ns) where

    setter :: Record (name' := c : ns) -> b -> Record (name' := c : ReplaceField name b ns)
    setter (x :*: r') y = (:*:) @name' x $ setter @name @i r' y where
-}

{-
-- I've tried to make FieldI injective, but Haskell doesn't allow nested injective type families
type S2 :: Type -> (Peano, [Type]) -> (Peano, [Type])
type family S2 tuple f = res | res -> tuple f where
    S2 f '(n, list) = '(S n, f : list)

type FieldInj :: Type -> [Type] -> (Peano, [Type])
type family FieldInj f fs = res | res -> f where
    FieldInj f (f : rest) = '(Z, f : rest)
    FieldInj f (f' : rest) = S2 f' (FieldInj f rest)
-}
