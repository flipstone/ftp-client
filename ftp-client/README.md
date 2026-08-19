# FTP Client

ftp-client is a client library for the FTP protocol in Haskell.

# Examples

## Insecure
```haskell
withFTP "ftp.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
```

## Secured with TLS

`withFTPS` verifies the server's certificate chain and host name, so a server
that cannot be validated is refused.

```haskell
withFTPS "ftps.server.com" 21 $ \h welcome -> do
    print welcome
    login h "username" "password"
    print =<< nlst h []
```

To talk to a server whose certificate cannot be validated, pass your own
settings. Disabling validation leaves the connection encrypted but not
authenticated, so anyone on the network path can read the credentials and alter
transferred data:

```haskell
let insecure = def { settingDisableCertificateValidation = True }
withFTPSSettings insecure "ftps.server.com" 21 $ \h welcome ->
    print welcome
```
