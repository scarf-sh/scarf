    let platforms =
          https://gist.githubusercontent.com/aviaviavi/75b14f444ee34eb60dbc0c0cf91f4e26/raw/8665f639d0fd0459caee760209534da747ec316c/platforms.dhall 

in  { distributions =
        [ { platform                =
              platforms.mac
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "curl-runnings" ] : Optional Text
          , url                     =
              "https://github.com/aviaviavi/curl-runnings/releases/download/0.10.0/curl-runnings-0.10.0-mac.tar.gz"
          }
        , { platform                =
              platforms.linux64
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "curl-runnings" ] : Optional Text
          , url                     =
              "https://github.com/aviaviavi/curl-runnings/releases/download/0.10.0/curl-runnings-0.10.0.tar.gz"
          }
        ]
    , name          = "curl-runnings"
    , version       = "0.10.0"
    }
