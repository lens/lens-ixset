{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
module Data.IxSet.Lens (ixTraversal, ixSetOf) where

import Control.Applicative (pure)
import Control.Lens hiding (Indexable)

import Data.IxSet
import Data.Typeable

-- | The next-best thing to an 'Control.Lens.At.Ix' instance, which can't be
-- done due to the flexibility 'IxSet' allows for its keys.
ixTraversal :: (Indexable a, Typeable a, Typeable k, Ord a) 
               => k -> IndexedTraversal' k (IxSet a) a
ixTraversal k f i = case getOne (i @= k) of
  Nothing -> pure i
  Just a -> indexed f k a <&> \a' -> updateIx k a' i
  
-- | Construct an ixset from a 'Getter', 'Fold', 'Traversal', 'Lens', or 'Iso'
ixSetOf :: (Indexable a, Ord a, Typeable a) 
           => Getting (IxSet a) s a -> s -> IxSet a
ixSetOf l = views l (`insert` empty)