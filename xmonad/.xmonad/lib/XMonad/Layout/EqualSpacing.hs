{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module XMonad.Layout.EqualSpacing ( equalSpacing
                                  , EqualSpacing
                                  , EqualSpacingMsg (..) ) where

import Control.Arrow                (second)
import Graphics.X11                 (Rectangle(..))
import XMonad.Core
import XMonad.StackSet              (integrate', stack, Stack(..) )
import XMonad.Util.Font             (fi)
import XMonad.Layout.LayoutModifier


equalSpacing :: Int -> Int -> Rational -> Int -> l a -> ModifiedLayout EqualSpacing l a
equalSpacing gap add mult min = ModifiedLayout (EqualSpacing gap add mult min)


data EqualSpacingMsg = MoreSpacing | LessSpacing deriving (Typeable)


instance Message EqualSpacingMsg


data EqualSpacing a = EqualSpacing
    { gap  :: Int
    , add  :: Int
    , mult :: Rational
    , min  :: Int
    } deriving (Show, Read)


instance LayoutModifier EqualSpacing a where

    modifierDescription eqsp = "EqualSpacing " ++ show eqsp

    modifyLayout eqsp workspace screen =
        runLayout workspace $ shrinkScreen eqsp ((length $ integrate' $ stack workspace) - 1) screen

    pureModifier eqsp _ stck windows =
        (map (second $ shrinkWindow eqsp ((length $ integrate' stck) - 1)) windows, Nothing)

    pureMess eqsp msg

        | Just MoreSpacing <- fromMessage msg = Just $
            eqsp { gap = (1+(fi $ gap eqsp)) }

        | Just LessSpacing <- fromMessage msg = Just $
            eqsp { gap = max 0 (-1+(fi $ gap eqsp)) }

        | otherwise = Nothing


shrinkScreen :: EqualSpacing a -> Int -> Rectangle -> Rectangle
shrinkScreen (EqualSpacing gap add mult m) num (Rectangle x y w h) =
    Rectangle x y (w-fi sp) (h-fi sp)
    where sp = max m $ gap - (num * add)


shrinkWindow :: EqualSpacing a -> Int -> Rectangle -> Rectangle
shrinkWindow (EqualSpacing gap add mult m) num (Rectangle x y w h) =
    Rectangle (x+fi sp) (y+fi sp) (w-fi sp) (h-fi sp)
    where sp = max m $ gap - (num * add)
