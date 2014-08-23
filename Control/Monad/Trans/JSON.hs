{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.Trans.JSON
where

import Data.Text hiding (empty)
import Data.Aeson
import Data.Aeson.Types
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Applicative

----------
-- Classes
----------

-- | A monad which is capable of querying JSON-input and
--   writing JSON ouput
class MonadJSONError m => MonadJSON m where
    -- | try to read a property from the input
    maybeValue :: Text -> m (Maybe Value)
    -- | write a property to the output
    writeProp :: ToJSON a => Text -> a -> m a 
    -- | run a computation and get the result and the JSON-output
    --   without writing the output to the monad.
    extract :: m a -> m (a, Value)
    -- | run a computation with another object as input.
    useObject :: Object -> m a -> m a
    

-- | An error occuring during the processing of the monad
data JSONError
    = MissingProperty Text
    | CantDecodeProperty Text String -- ^ name of property and aeson error message
    deriving (Show)

-- | A monad which is capable of processing json errors
class Monad m => MonadJSONError m where
    throwJSONError :: JSONError -> m a

------------
-- Operators
------------

-- | Write an ordinary value to the JSON-output
(<:) :: (ToJSON a, MonadJSON m)
     => Text -> a -> m a
(<:) = writeProp

infixl 0 <:

-- | Write monadic value to the JSON-output
(<$) :: (ToJSON a, MonadJSON m)
      => Text -> m a -> m a
p <$ m = m >>= writeProp p

infixl 0 <$

-- | Write the outputs of some monads to a list in the output
(<$:) :: (ToJSON a, MonadJSON m)
      => Text -> [m a] -> m [a] 
p <$: ms = do
    xs <- sequence . fmap extract $ ms
    let (res, json) = unzip xs
    p <: toJSON json
    return res

infixl 0 <$:

-- | Write the json output of the monad to the property
(<$.) :: MonadJSON m
      => Text -> m a -> m a
p <$. m = do
    (res, val) <- extract m
    p <: val
    return res

infixl 0 <$.

-- | Get the property if it is there.
maybeProp :: (FromJSON a, MonadJSON m) => Text -> m (Maybe a)
maybeProp p = do
    val <- maybeValue p
    case val of
        Nothing -> return Nothing
        Just v -> case parseEither parseJSON v of
            Left err -> throwJSONError $ CantDecodeProperty p err
            Right res -> return . Just $ res

-- | Get the property or throw error if that is not possible
prop :: (FromJSON a, MonadJSON m) => Text -> m a
prop p = do
    val <- maybeProp p
    case val of
        Nothing -> throwJSONError $ MissingProperty p
        Just val -> return val

-- | Use the value of a property to get a new monad. 
($>) :: (FromJSON a, MonadJSON m) => Text -> (a -> m b) -> m b
p $> m = prop p >>= m

infixl 0 $>

-- | Use the value of a property to get a new monad if it is there.
(?>) :: (FromJSON a, MonadJSON m) => Text -> (a -> m b) -> m (Maybe b)
p ?> m = do
    val <- maybeProp p 
    case val of
        Nothing -> return Nothing
        Just v -> do
            res <- m v
            return $ Just res

infixl 0 ?>

-- | Use the value of a property as input for the monad.
(.$>) :: MonadJSON m => Text -> m a -> m a
p .$> m = do
    obj <- prop p
    useObject obj m 

infixl 0 .$>

-- | Use the value of a property as input for the monad if it is there.
(.?>) :: MonadJSON m => Text -> m a -> m (Maybe a)
p .?> m = do
    obj' <- prop p
    case obj' of
        Nothing -> return Nothing
        Just obj -> do
            res <- useObject obj m
            return . Just $ res

infixl 0 .?>

-- | Helper for composition, has higher fixity than read
--   and write operators.
(.$) = ($)
infixr 1 .$


--------------
-- Transformer
--------------

newtype JSONMonadT m a 
    = JSONMonadT { runJSONMonadT' :: ReaderT Object (WriterT [Pair] m) a }
    deriving (Monad)

instance MonadTrans JSONMonadT where
    lift = JSONMonadT . lift . lift

instance Functor m => Functor (JSONMonadT m) where
    fmap f = JSONMonadT . fmap f . runJSONMonadT'

instance Applicative m => Applicative (JSONMonadT m) where
    pure = JSONMonadT . pure
    l <*> r = JSONMonadT $ runJSONMonadT' l <*> runJSONMonadT' r

instance Alternative m => Alternative (JSONMonadT m) where
    empty = JSONMonadT empty
    l <|> r = JSONMonadT $ runJSONMonadT' l <|> runJSONMonadT' r
    some = JSONMonadT . some . runJSONMonadT'
    many = JSONMonadT . many . runJSONMonadT'

instance MonadFix m => MonadFix (JSONMonadT m) where
    mfix f = JSONMonadT . mfix $ runJSONMonadT' . f

instance MonadPlus m => MonadPlus (JSONMonadT m) where
    mzero = JSONMonadT mzero
    l `mplus` r = JSONMonadT $ runJSONMonadT' l `mplus` runJSONMonadT' r

instance MonadIO m => MonadIO (JSONMonadT m) where
    liftIO = JSONMonadT . liftIO
