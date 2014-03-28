Programming framework for reproducible research with PDEs

Compile
---------------------------------------------

Requires 'array-memoize' library: 

  	 git clone http://github.com/dorchard/array-memoize

	 cd array-memoize

	 cabal configure

  	 cabal build

  	 sudo cabal install

Then build via

     	 ghc TestCase.hs -o testcase

Usage
---------------------------------------------

From the command line, set the parameters for the model

  ./testcase [dx] [dt] [nx] [nt] [alpha]

e.g.

  ./testcase 0.05 0.05 40 200 0.006