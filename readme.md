About
---------------------------------------------

A novel programming framework for reproducible research with PDEs.

This a prototype, exploring a research hypotheses about the future of 
programming language features for reproducible computational science. 

It was built during the Hackday at the Software Sustainability Institute 
(http://software.ac.uk) Collaboration Workshop 2014.

By Dominic Orchard and Michael Fischer.

Compile
---------------------------------------------

Requires 'array-memoize' library: 

  	 git clone http://github.com/dorchard/array-memoize
	 cd array-memoize
	 cabal configure
  	 cabal build
  	 sudo cabal install

Then build via

     	 ghc TestCaseCmd.hs -o testcase

(this provides a command line interface to a test case -- see usage below)

OR, you can using interactive mode with

         ghci TestCase.hs

Usage
---------------------------------------------

You can query the test case model from the command line
(this is one-dimensional heat flow model). Parameters must be set 
as follows:

	./testcase [dx] [dt] [nx] [nt] [alpha]

All components are necessary, e.g. 

	./testcase 0.05 0.05 40 50 0.006