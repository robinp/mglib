{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE CPP #-}

-- TODO separate rebindable stuff?

module MLib where

#ifndef FAY
import Prelude hiding (fromInteger, fromRational, ifThenElse, fail)
#else
import Prelude
#endif

-- Fake stuff for RebindableSyntax
fromInteger :: Integer -> Int
fromInteger = undefined

fromRational :: a -> Double
fromRational = undefined

ifThenElse True  a _ = a
ifThenElse False _ b = b
fail = undefined

-- now the fun
type FmapType f = forall a b . (a -> b) -> f a -> f b

data Functor' f = Functor' {
  fmap'   :: FmapType f }

void :: Functor' f -> f a -> f ()
void f = (fmap' f) (const ())

type ReturnType m = forall a   . a -> m a
type JoinType m   = forall a   . m (m a) -> m a
type BindType m   = forall a b . m a -> (a -> m b) -> m b
type ShiftType m  = forall a b . m a -> m b -> m b

data Monad' m = Monad' {
  functorOfM :: Functor' m,
  return' :: ReturnType m,
  bind'   :: BindType m,
  join'   :: JoinType m }

shift' :: Monad' m -> m a -> m b -> m b
shift' m ma mb = bind' m ma (const mb)

-- RankNTypes needs explicit type annotations
withM :: Monad' m -> (ReturnType m -> BindType m -> ShiftType m -> r) -> r
withM (Monad' _ ret' bind' _) f = f ret' bind' (>>)
  where ma >> mb = ma `bind'` const mb

withMF :: Monad' m -> (ReturnType m -> BindType m -> ShiftType m -> FmapType m -> r) -> r
withMF m f =
  case m of
    Monad' functor ret' bind' _ -> withM m $ \r b s -> f r b s (fmap' functor)

liftM' :: Monad' m -> (a -> b) -> m a -> m b
liftM' m f = fmap' (functorOfM m) f

liftM2' :: Monad' m -> (a -> b -> r) -> m a -> m b -> m r
liftM2' m f ma mb = withMF m (\return (>>=) _ fmap ->
  ma >>= \a ->
  mb >>= \b ->
  return (f a b))

sequence' :: Monad' m -> [m a] -> m [a]
sequence' m mas =
  rev $ foldl f initAcc mas
  where f acc ma  = liftM2' m (flip (:)) acc ma
        rev       = liftM' m reverse
        initAcc   = return' m []

mutate' :: Monad' m -> (a -> m ()) -> a -> m a
mutate' m f a = shift' m (f a) (return' m a)

mutateM' :: Monad' m -> m (a -> m ()) -> a -> m a
mutateM' m mf a = bind' m mf (\f -> mutate' m f a)

-- List

listFunctor = Functor' map

listMonad = Monad' {
  functorOfM = listFunctor,
  return' = \a -> [a],
  bind'   = \xs -> concat . flip map xs,
  join'   = concat }

-- Either

mapEither :: (a -> b) -> Either e a -> Either e b
mapEither f e = case e of
  Left e  -> Left e
  Right a -> Right (f a)

eitherFunctor = Functor' mapEither

joinEither :: Either e (Either e a) -> Either e a
joinEither (Left e) = Left e
joinEither (Right r) = r

eitherMonad :: Monad' (Either e)
eitherMonad = Monad' {
  functorOfM = eitherFunctor,
  return' = Right,
  bind'   = \ma f -> joinEither (mapEither f ma),
  join'   = joinEither }

-- Tuple2

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

-- State

-- can't newtype - Fay bug?
data State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

mapState :: (a -> b) -> State s a -> State s b
mapState f (State fs) = state $ \r -> mapFst f (fs r)

stateFunctor = Functor' mapState

bindState :: State s a -> (a -> State s b) -> State s b
bindState (State fs) f = state $ (\r -> 
  let (a, s1) = fs r
  in runState (f a) s1)

stateMonad :: Monad' (State s)
stateMonad = Monad' {
  functorOfM = stateFunctor,
  return' = \a -> state $ \s -> (a, s),
  bind' = bindState,
  join' = \(State fss) -> state (\s -> 
    let (State fs, r) = fss s
    in fs r)
}

-- Fay

mapFay :: (a -> b) -> Fay a -> Fay b
mapFay f m = m >>= (return . f)

fayFunctor = Functor' mapFay

fayMonad :: Monad' Fay
fayMonad = Monad' {
  functorOfM = fayFunctor,
  return' = return,
  bind' = (>>=),
  join' = \mma -> mma >>= id
}

mutate = mutate' fayMonad
mutateM = mutate' fayMonad
-- transformers

{--
type EitherW m e a = m (Either e a)
M

returnEitherW :: Monad' m -> ReturnType (EitherW m e)
returnEitherW m = return' m . Right

bindEitherW :: Monad' m -> BindType (EitherW m e)
bindEitherW m = \mea f ->
    bind' m mea (either (const mea) f)

eitherW :: Monad' m -> Monad' (EitherW m e) -- type lambdas missing? nooo.. will need newtyping
eitherW m = Monad' {
  functorOfM = undefined,
  return' = returnEitherW m,
  bind'   = bindEitherW m,
  join'   = joinDefault bindEitherW
}
--}

-- Example

monadic :: Monad' [] -> [Int] -> [Int]
monadic m ma = withMF m (\return (>>=) (>>) fmap ->
  fmap (*2) ma >>= dups >>= dups)
  where dups x = [x, x]

monadic2 :: Monad' m -> m Int -> m Int
monadic2 m ma = withM m (\return (>>=) (>>) -> do
  a <- ma
  b <- ma
  return $ a * b)

inc :: State Int Int
inc = state $ \s -> (s+1, s+1)

incBy :: Int -> State Int Int
incBy x = state $ \s -> (s+x, s+x) 

mlibMain = do
  let xs = monadic listMonad [1,2,3]
  let sm = sum xs
  putStrLn "Hello"
  putStrLn $ show (sm == 48)
  putStrLn $ show (if 1 < 2 then 3.4 else 4.2) -- to test RebindableSyntax
  putStrLn $ show (sum (monadic2 listMonad [1,2,3]) == 36)
  putStrLn $ show $ (monadic2 eitherMonad (Right (2)) :: Either String Int)
  putStrLn $ show $ mapEither (sum) $ sequence' eitherMonad [Right 2, Right 3, Left "ouch"]
  putStrLn $ show $ snd $ runState (inc `bindState` incBy) 1
  let xs = fst $ runState (sequence' stateMonad [inc, incBy 3, inc, inc]) 0
  putStrLn $ show $ sum xs
  putStrLn $ show $ xs
  (mapFay (2*) $ return 5) >>= (putStrLn . show)
