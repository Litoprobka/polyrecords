{-# LANGUAGE DataKinds, TypeFamilies, FunctionalDependencies, AllowAmbiguousTypes, UndecidableInstances #-}

module Polyrecords (
    Record(..), 
    getter, setter', plens', plens
) where

import GHC.TypeLits
import Data.Type.Bool (If)
import Data.Type.Ord (type (>?), Compare)
import Control.Lens (Lens, Lens', (^.), set)
import qualified Control.Lens.Lens as Lens (lens)

data MissingField

infixl 6 := -- so that it works with []
data (:=) (name :: Symbol) a = Placeholder
    

data Record r where
    RNil :: Record '[]
    RCons :: forall (name :: Symbol) a r. 
        FieldType name (Record r) ~ MissingField
         a -> Record r -> Record (name := a : r)


type family FieldType (name :: Symbol) r where
    FieldType name (Record '[]) = MissingField
    FieldType name (Record (name := a : ns)) = a
    FieldType name (Record (_ : ns)) = FieldType name (Record ns)

class HasLens' name a ns | name ns -> a where
    getter :: Record ns -> a
    setter' :: Record ns -> a -> Record ns

    plens' :: Lens' (Record ns) a
    plens' = Lens.lens (getter @name) (setter' @name) 

instance {-# OVERLAPPING #-} HasLens' name a (name := a : ns) where
    getter (RCons x _) = x
    setter' (RCons _ r') y = RCons y r'

instance {-# OVERLAPPABLE #-} HasLens' name a ns => HasLens' name a (name' := b : ns) where
    getter (RCons _ r') = getter @name r'
    setter' (RCons x r') y = RCons @name' x $ setter' @name r' y

type family ReplaceField (name :: Symbol) b r where
    ReplaceField name b '[] = '[]
    ReplaceField name b (name := a : ns) = name := b : ns
    ReplaceField name b (name':= c : ns) = name' := c : ReplaceField name b ns

-- * Stuff that does not work yet * --

type instance Compare (name := a) (name' := b) = CmpSymbol name name'

infixr 5 :<: 
-- | sorted insertion; should replace : in RCons, but the type checker has trouble with it
type family a :<: (r :: [*]) where
    a :<: '[] = '[a]
    a :<: (b : r') = If (a >? b) (b : a :<: r') (a : b : r')

-- | This class would provide a full-featured lens, but the type checker has trouble with ReplaceField
class HasLens' name a ns => HasLens name a ns | name ns -> a where
    setter :: Record ns -> b -> Record (ReplaceField name b ns)
    
    plens :: Lens (Record ns) (Record (ReplaceField name b ns)) a b
    plens = Lens.lens (getter @name) (setter @name)

instance {-# OVERLAPPING #-} HasLens name a (name := a : ns) where
    setter :: Record (name := a : ns) -> b -> Record (name := b : ns)
    setter (RCons _ r') y = RCons y r'

{-
instance {-# OVERLAPPABLE #-} HasLens' name a (name' := c : ns) => HasLens name a (name' := c : ns) where

    setter :: Record (name' := c : ns) -> b -> Record (name' := c : ReplaceField name b ns)
    setter (RCons x r') y = RCons @name' x $ setter @name r' y where
-}
