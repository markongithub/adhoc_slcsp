# adhoc_slcsp

This package provides a solution in Haskell to [Ad Hoc's Second-Lowest Cost Silver Plan](https://homework.adhoc.team/slcsp/) problem.

After downloading it, you can run
- cabal install --enable-tests
- cabal test (optional)
- cabal run

By default we assume the three input files specified in the assignment are in
the `./input` subdirectory of the working directory. However, the user can
override this default with the command-line parameters `--planfile`,
`--zipfile`, and `--requestfile`. For example one could run
> `cabal run -- --zipfile /tmp/lots_of_zip_codes.csv`
