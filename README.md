# test-cloud-haskell
Testing cloud haskell programs

## Build
* cabal configure
* cabal build
* cabal install

### On Worker
* cabal run worker 8080

### On Manager
* cabal run manager 8081 8080
