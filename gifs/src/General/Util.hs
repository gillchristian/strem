module General.Util
  ( (<&>),
    (<|>),
    dropLabelPrefix,
    camelTags,
    stripPrefix,
    whenJust,
    filter,
  )
where

import Control.Applicative (liftA2)
import Control.Monad (MonadPlus, mzero, void)
import qualified Data.Aeson as Json
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Text.Casing (camel)
import Prelude hiding (filter)

(<&>) :: Applicative f => f Bool -> f Bool -> f Bool
(<&>) = liftA2 (&&)

(<|>) :: Applicative f => f Bool -> f Bool -> f Bool
(<|>) = liftA2 (||)

dropLabelPrefix :: String -> Json.Options
dropLabelPrefix prefix =
  Json.defaultOptions {Json.fieldLabelModifier = camel . stripPrefix prefix}

camelTags :: Json.Options
camelTags =
  Json.defaultOptions {Json.constructorTagModifier = camel}

stripPrefix :: Eq a => [a] -> [a] -> [a]
stripPrefix prefix str = Maybe.fromMaybe str $ List.stripPrefix prefix str

whenJust :: Applicative f => Maybe a -> (a -> f b) -> f (Maybe b)
whenJust Nothing _ = pure Nothing
whenJust (Just a) f = Just <$> f a

filter :: MonadPlus m => (a -> Bool) -> m a -> m a
filter cond =
  (>>= f)
  where
    f x
      | cond x = pure x
      | otherwise = mzero
