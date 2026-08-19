# Changelog for ftp-client

## 0.6.0.0

**Breaking change.** `withFTPS` now verifies the server's certificate chain and
host name. Validation was previously disabled, and a caller had no way to enable
it. Reported by @ysangkok in
<https://github.com/flipstone/ftp-client/pull/1>, which proposed the same fix and
was approved but closed unmerged; this completes that change.

If you connect to a server whose certificate cannot be validated, that
connection will now fail. Use the new `withFTPSSettings` with
`settingDisableCertificateValidation` set to keep the previous behaviour
deliberately.

* `withFTPSSettings` takes `Connection.TLSSettings`, for callers who need to
  choose their own.

* Data connections now authenticate against the host the control connection was
  opened to. They previously used the *local* end of the data socket, which no
  server certificate can match. This is why enabling validation on the control
  connection alone was not sufficient: PR #1 changed only that, and on its own
  would have left every FTPS data transfer unable to validate.

* `Security` now carries a `TLSContext` (settings, host, port) so a data
  connection can reproduce the control connection's protection. `connectTLS`,
  `createTLSConnection`, `withTLSHandle` and `tlsHandleImpl` take the settings
  or context they need.

* Reply lines are now length limited. `connectionGetLine` was called with
  `maxBound`, so a server that never sent a newline could exhaust memory before
  authentication.

* IO failures during a transfer are no longer reported as a completed one.
  `recvAll`, `getAllLineResp` and `getMlsxResponse` turned any `IOError` into a
  clean end of data, so a reset or timed-out connection produced a truncated
  result that a caller could not distinguish from a whole one. End of input is
  now distinguished from failure, and only end of input terminates a read.

* Fixed three descriptor leaks: `createTLSConnection` on a refused greeting,
  rejected `AUTH TLS` or failed handshake; the data handshake, where
  `socketToHandle` had already invalidated the socket the release closed; and
  the active-mode listening socket, which was never closed on success.

* A data transfer that does not complete normally now still consumes the
  server's completion reply. Left unread it became the answer to the next
  command, and every reply after that belonged to the previous command.

* `TYPE A` transfers now send CRLF as RFC 959 requires. `sendType TA` doubled a
  CR that was already there and appended a record the input did not have, and
  `sendLine` sent a bare LF.

* `ccc` and `auth` are removed. CCC cannot work here -- there is no way to
  downgrade our side of the connection, so the control connection would
  desynchronise -- and `auth` on its own tells the server to expect a handshake
  that never happens. Both remain reachable as `FTPCommand` constructors.

* `getLineRespMaybe`, `getAllLineResp` and `toNetworkAscii` are now exported.

## 0.5.3.1

* Enable the `henforcer` plugin and `fourmolu` under the `ci` flag. Imports are
  now qualified per the house style and the source is fourmolu formatted;
  neither changes the API.

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
