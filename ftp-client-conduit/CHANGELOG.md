# Changelog for ftp-client-conduit

## 0.6.0.0

Requires `ftp-client` 0.6, whose `withFTPS` now validates certificates. See its
changelog for the security implications of that change.

* IO failures during a transfer are no longer reported as a completed one.
  `retr` and the listing sources turned any `IOError` into a clean end of
  stream, so a reset connection wrote a truncated file and reported success.

* A blank line no longer truncates a listing. `nlst`, `list` and `mlsd` stopped
  at the first empty line and the caller still saw the normal completion reply,
  so a short listing looked complete. This also brings the conduit `mlsd` into
  agreement with `Network.FTP.Client.mlsd`, which skipped blank lines.

* The server's completion reply is now consumed even when the downstream
  consumer terminates early. With `takeC`, `headC` or any short circuit it was
  skipped, and became the answer to the next command on the control connection.

  It is also consumed when setting up the data connection fails after the
  server has already accepted the transfer -- a rejected certificate on the
  data channel handshake, for instance. `bracketP` runs no release action when
  acquisition fails, so that reply was previously left queued.

* `stor` in `TYPE A` mode now frames by line and sends CRLF. It appended a
  terminator to every awaited chunk, so uploading from `sourceFile` injected one
  at every chunk boundary, and it used a bare LF where RFC 959 requires CRLF.

* Dropped the `exceptions` dependency, which is no longer used.

## 0.5.0.8

* Enable the `henforcer` plugin and `fourmolu` under the `ci` flag. Imports are
  now qualified per the house style and the source is fourmolu formatted;
  neither changes the API.

* Correct the Haddock module header, which named `Network.FTP.Client` rather
  than `Network.FTP.Client.Conduit`.

## 0.5.0.7

* Add the missing upper bounds on `bytestring`, `conduit` and `exceptions`.

* Replace conduit's deprecated `Producer` and `Consumer` synonyms with
  `ConduitT` in the exported signatures of `nlst`, `retr`, `list`, `stor` and
  `mlsd`. The type variables are left free, so the exported types are unchanged
  -- `Producer m o` is `forall i. ConduitT i o m ()`, not `ConduitT () o m ()`.

* Raise the `conduit` lower bound to `>= 1.3`. `ConduitT` arrived in conduit
  1.3, so the previous `>= 1.1` allowed dependency selections that cannot
  compile the signatures above. The bounds audit only added the missing upper
  halves; this is the lower half that replacing `Producer` and `Consumer`
  invalidated.

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
