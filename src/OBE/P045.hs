module OBE.P045
    ( ProducePrices(..)
    , limePrice
    , lemonPrice
    , max50Diff
    ) where

import Optics.Lens (Lens', lens)


data ProducePrices = ProducePrices
    { _limePrice :: Float
    , _lemonPrice :: Float
    }
    deriving (Show, Eq)


limePrice :: Lens' ProducePrices Float
limePrice =
    lens
        (\(ProducePrices lime _) ->
            lime
        )
        (\(ProducePrices _ lemon) newLime ->
            let
                maxLime :: Float
                maxLime = max 0 newLime
            in
            ProducePrices maxLime (max50Diff maxLime lemon)
        )


lemonPrice :: Lens' ProducePrices Float
lemonPrice =
    lens
        (\(ProducePrices _ lemon) ->
            lemon
        )
        (\(ProducePrices lime _) newLemon ->
            let
                maxLemon :: Float
                maxLemon = max 0 newLemon
            in
            ProducePrices (max50Diff maxLemon lime) maxLemon
        )


max50Diff :: Float -> Float -> Float
max50Diff base aligned =
    if base < aligned then
        base + min 0.5 diff
    else
        base - min 0.5 diff
    where
        diff :: Float
        diff = abs (base - aligned)
