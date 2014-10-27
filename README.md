# The Tardis Monad

![Tardis](http://hello.eboy.com/eboy/wp-content/uploads/2013/10/PT-Tardis-01t-6x.png)

Tardis (or TardisT) provides the capabilities of both the State, and the Reverse-State monads in one handy package.

	 cabal install tardis
	 
With this package you have several monadic functions available.

	 class (Control.Applicative.Applicative m, Control.Monad.Fix.MonadFix m) =>
	       MonadTardis bw fw (m :: * -> *) | m -> bw, m -> fw where
	   getPast :: m fw
	   getFuture :: m bw
	   sendPast :: bw -> m ()
	   sendFuture :: fw -> m ()
	   tardis :: ((bw, fw) -> (a, (bw, fw))) -> m a

	 type Tardis bw fw = TardisT bw fw Data.Functor.Identity.Identity
	 type role TardisT nominal nominal representational nominal
	 newtype TardisT bw fw (m :: * -> *) a
	       = Control.Monad.Trans.Tardis.TardisT {runTardisT :: (bw, fw) -> m (a, (bw, fw))}

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

The most important being...

* getPast
* getFuture
* sendPast
* sendFuture

These allow you to work with the state monad

* getPast
* sendFuture

and the reverse-state monad

* getFuture
* sendPast

at the same time.

# State Example

	state_example = do
		sendFuture 123
		x <- getPast
		return (x + 1)
	
	runTardis state_example ((),5) -- => (124,((),123))

# Reverse State Example



# Other Links

* [Temporally Quaquaversal Virtual Nanomachine Programming In Multiple Topologically Connected Quantum-Relativistic Parallel Timespaces...Made Easy!](http://blip.tv/oreilly-open-source-convention/oscon-2008-damian-conway-thoughtstream-temporally-quaquaversal-virtual-nanomachine-programming-in-multiple-t-1151669)
