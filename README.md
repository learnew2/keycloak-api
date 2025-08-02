# keycloak-api

Haskell library for working with Keycloak API with partial bindings to
admin API and OAuth2 API using servant.

## Preparing test client in GHCi

```haskell
import Servant.Client
import Network.HTTP.Conduit
import Network.HTTP.Client.Conduit
baseUrl <- parseBaseUrl "<KEYCLOAK-URL>"
manager <- (Network.HTTP.Conduit.newManager defaultManagerSettings)
let env = mkClientEnv manager baseUrl
:set -XOverloadedStrings
flip runClientM env $ do validateToken "<REALM>" (IntrospectRequest "<TOKEN>" "<CLIENT_ID>" "<CLIENT_SECRET>")
```
