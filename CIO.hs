module CIO
(CIO (C)
,run
,vc
,qc
)where

import System.IO

data CIO a = C (IO (a , [IO ()]))

instance Functor CIO where
	fmap f (C a) = C ( do
				(x,cs) <- a
				return (f x, cs) )

instance Applicative CIO where
	pure x = C (return (x,[]))
	(C a1) <*> (C a2) = C ( do
				(f,cs1) <- a1
				(x,cs2) <- a2
				return (f x, cs1++cs2) )

instance Monad CIO where
	return = pure
	(>>=) (C ax) f = C (do
				(x,cs1) <- ax
				(r,cs2) <- unC (f x)
				return (r,cs1++cs2) )

unC::CIO a -> IO (a, [ IO () ] )
unC (C x) = x

moreClean::CIO a ->[IO()]->CIO a
moreClean (C f) cs2 = C (do
			(x,cs1) <- f
			return (x,cs1 ++ cs2) )

cleanInt:: Int -> CIO Int
cleanInt n = C ( do
	return (n, [putStrLn "cleaned your int"]) )

someCleanInts::CIO ()
someCleanInts	= do
		x <- cleanInt 1
		y <- cleanInt 2
		vc (putStrLn (show (x+y) ) )


vc::IO a-> CIO a
vc a = C ( do
	r <- a
	return (r,[])
	)

doAll::[IO()] -> IO ()
doAll = foldl (>>) (return())

run::CIO () -> IO ()
run (C a) = do
	(x,cs) <- a
	doAll cs

qc::IO () -> CIO ()
qc clean = C (do return ((), [clean] )  )

main = run someCleanInts
