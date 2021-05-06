module Config exposing (..)


maxCartSize : Int
maxCartSize =
    1000


serverAddress : String
serverAddress =
    -- "http://127.0.0.1:8001"
    "http://ct.c-path.org/"


apiServer : String
apiServer =
    -- "http://127.0.0.1:8080"
    "http://ct.c-path.org/api/v1"


signInRedirectHost : String
signInRedirectHost =
    -- "http://localhost:8080"
    "http://ct.c-path.org"


signInRedirectFragment : String
signInRedirectFragment =
    "/#/signin"
