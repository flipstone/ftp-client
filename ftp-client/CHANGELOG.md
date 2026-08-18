# Changelog for ftp-client

## 0.5.1.7

* Correct the `base` bound. The package claimed `>= 4.8`, i.e. support back to
  GHC 7.10, but no compiler that old can build it because `crypton-connection`
  does not exist there. The bound is now `>= 4.16`, the oldest GHC that is
  actually tested, and `tested-with` records the full set.

## Earlier releases

Prior to 0.5.1.7 this package had no changelog. See the git history at
<https://github.com/flipstone/ftp-client> for changes in earlier versions.
