# Changelog for ftp-client

## 0.5.1.8

* Fix a crash on short response lines. `getResponse` called `head` on the bytes
  following the response code, so a line shorter than four bytes failed with
  `Prelude.head: empty list` instead of an `FTPException`. Response lines that do
  not begin with a three digit code now raise `BadProtocolResponseException`.

* Fix a hang when the server closes the connection partway through a multiline
  response. The read loop had no terminating condition other than the closing
  code, so it never returned. It now stops on an exhausted stream and returns the
  lines received, matching the existing behaviour of `recvAll`.

## 0.5.1.7

* Correct the `base` bound. The package claimed `>= 4.8`, i.e. support back to
  GHC 7.10, but no compiler that old can build it because `crypton-connection`
  does not exist there. The bound is now `>= 4.16`, the oldest GHC that is
  actually tested, and `tested-with` records the full set.

## Earlier releases

Prior to 0.5.1.7 this package had no changelog. See the git history at
<https://github.com/flipstone/ftp-client> for changes in earlier versions.
