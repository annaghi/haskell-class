module OBE.P025
    ( second
    , inMaybe
    , conditional
    ) where

import Control.Lens (Lens', lens)
import Data.Maybe (fromJust)


second :: Lens' (a, b, c) b
second =
    lens
        (\(_, b, _) -> b)
        (\(a, _, c) newB -> (a, newB, c))


inMaybe :: Lens' (Maybe a) a
inMaybe =
    lens
        fromJust -- Will thore an error if the focus is on Nothing.
        (\maybeStructure newValue -> fmap (const newValue) maybeStructure)


-- Cannot use lenses here, that is what prisms for
-- left :: Lens' (Either a b) a


conditional :: Lens' (Bool, a, a) a
conditional =
    lens
        (\(bool, a1, a2) -> if bool then a1 else a2)
        (\(bool, a1, a2) newValue -> if bool then (bool, newValue, a2) else (bool, a1, newValue))
