#

## The Tardis Monad

![Tardis](http://hello.eboy.com/eboy/wp-content/uploads/2013/10/PT-Tardis-01t-6x.png)

## What is it?

Tardis (or TardisT) provides the capabilities of both the State, and the Reverse-State monads in one handy package.

	 cabal install tardis

## What's in the Box?
	 
With this package you have several monadic functions available.

	 class ... MonadTardis bw fw m ... where
	   getPast :: m fw
	   getFuture :: m bw
	   sendPast :: bw -> m ()
	   sendFuture :: fw -> m ()
	   tardis :: ((bw, fw) -> (a, (bw, fw))) -> m a

	 evalTardis      :: Tardis bw fw a -> (bw, fw) -> a
	 evalTardisT     :: Monad m => TardisT bw fw m a -> (bw, fw) -> m a
	 execTardis      :: Tardis bw fw a -> (bw, fw) -> (bw, fw)
	 execTardisT     :: Monad m => TardisT bw fw m a -> (bw, fw) -> m (bw, fw)
	 getsFuture      :: MonadTardis bw fw m => (bw -> a) -> m a
	 getsPast        :: MonadTardis bw fw m => (fw -> a) -> m a
	 modifyBackwards :: MonadTardis bw fw m => (bw -> bw) -> m ()
	 modifyForwards  :: MonadTardis bw fw m => (fw -> fw) -> m ()
	 noState         :: (a, b)
	 runTardis       :: Tardis bw fw a -> (bw, fw) -> (a, (bw, fw))

## What's important?

The most important being...

* getPast
* getFuture
* sendPast
* sendFuture
* runTardisT

#

## Work with the state monad

* getPast
* sendFuture

#

## And the reverse-state monad

* getFuture
* sendPast

# ... At the same time

## State Example

	 state_example = do
	 	sendFuture 123
	 	x <- getPast
	 	return (x + 1)
	
	 runTardis state_example ((),5) -- => (124,((),123))

## Reverse State Example

	 state_example = do
	 	x <- getFuture
	 	sendPast 123
	 	return (x + 1)
	
	 runTardis state_example (5,()) -- => (124,(123,()))

## Tardis Example

	 tardis_example = do
	 	x <- getFuture
		sendFuture 234
	 	sendPast   123
		y <- getPast
	 	return (x + y)
	
	 runTardis state_example (0,0) -- => (357,(123,234))


## Other Links

* [Temporally Quaquaversal Virtual Nanomachine Programming In Multiple Topologically Connected Quantum-Relativistic Parallel Timespaces...Made Easy!](http://blip.tv/oreilly-open-source-convention/oscon-2008-damian-conway-thoughtstream-temporally-quaquaversal-virtual-nanomachine-programming-in-multiple-t-1151669)
* [Bowling on a Tardis](http://unknownparallel.wordpress.com/2012/11/05/bowling-on-a-tardis/)
* [Tardis Hackage Docs](https://hackage.haskell.org/package/tardis-0.3.0.0/docs/Control-Monad-Trans-Tardis.html)
* [Two implementations of seers](http://unknownparallel.wordpress.com/2013/05/07/two-implementations-of-seers/)
