all:
	runhaskell Setup.lhs configure --prefix ~
	runhaskell Setup.lhs build
	runhaskell Setup.lhs install --user

clean:
	runhaskell Setup.lhs clean
