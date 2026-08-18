# Changelog for ftp-client

## 0.5.3.0

* Export `acct`, `pbsz`, `prot`, `ccc` and `auth`. These command wrappers were
  defined but never exported, unlike every other command wrapper in the module.

* Drop the `transformers` dependency. The only module it supplied,
  `Control.Monad.IO.Class`, has been in `base` since 4.9.

* Stop deriving `Typeable` for `FTPException`. It has been a no-op since GHC
  7.10 and GHC 9.12 warns about it.

## 0.5.2.0

* Expose `createSIOHandle`, `createTLSConnection` and `connectTLS` so callers can
  manage the handle lifecycle themselves rather than going through `withFTP` and
  `withFTPS`. Thanks to @pucsdian.

* Fix multiline response parsing. A response was terminated at the first
  continuation line whose first three bytes matched the response code, so a reply
  such as `220-First` / `220-Second` / `220 Third` was truncated to two lines. Per
  [RFC 959](https://datatracker.ietf.org/doc/html/rfc959#page-36) only the code
  followed by a space ends a multiline reply; the code followed by a hyphen
  continues it. A final line consisting of the bare code is also accepted, for
  servers that omit the trailing space. Thanks to @pucsdian.

## 0.5.1.8

* Fix a crash on short response lines. `getResponse` called `head` on the bytes
  following the response code, so a line shorter than four bytes failed with
  `Prelude.head: empty list` instead of an `FTPException`. Response lines that do
  not begin with a three digit code now raise `BadProtocolResponseException`.

* Fix a hang when the server closes the connection partway through a multiline
  response. The read loop had no terminating condition other than the closing
  code, so it never returned. An exhausted stream now raises
  `BadProtocolResponseException`. The loop terminates, and a reply the server
  never finished is reported as bad rather than handed back as though it were
  complete -- which would have let a truncated `220-` greeting read as a
  successful 220 and let `withFTP` proceed against a dead control connection.

## 0.5.1.7

* Correct the `base` bound. The package claimed `>= 4.8`, i.e. support back to
  GHC 7.10, but no compiler that old can build it because `crypton-connection`
  does not exist there. The bound is now `>= 4.16`, the oldest GHC that is
  actually tested, and `tested-with` records the full set.

## Earlier releases

Prior to 0.5.1.7 this package had no changelog. See the git history at
<https://github.com/flipstone/ftp-client> for changes in earlier versions.
