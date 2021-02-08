module Config exposing (apiServer, maxCartSize, serverAddress)


maxCartSize : Int
maxCartSize =
    250


serverAddress : String
serverAddress =
    -- "http://127.0.0.1:8001"
    "http://ct.c-path.org/"


apiServer : String
apiServer =
    -- "http://127.0.0.1:8080"
    "http://ct.c-path.org/api/v1"
