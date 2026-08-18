# Changelog for ftp-client-conduit

## 0.5.0.6

* Correct the `resourcet` bound. `>= 1.2 && < 1.3` excluded `resourcet-1.3.0`,
  which is what current Stackage snapshots carry, so the package could not build
  against them at all. Only the `MonadResource` class is used from resourcet and
  it is unchanged across that boundary, so the bound is now `< 1.4`.

* Relax the `crypton-connection` bound from `>= 0.4` to `>= 0.3`, matching
  `ftp-client`. Only `connectionClose` is used from that package, and 0.3 has it.

* Correct the `base` bound. The package claimed `>= 4.7`, i.e. support back to
  GHC 7.8, which has never been buildable. It is now `>= 4.16`, the oldest GHC
  that is actually tested, and `tested-with` records the full set.

## Earlier releases

Prior to 0.5.0.6 this package had no changelog. See the git history at
<https://github.com/flipstone/ftp-client> for changes in earlier versions.
