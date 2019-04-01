    let platforms =
          https://gist.githubusercontent.com/aviaviavi/75b14f444ee34eb60dbc0c0cf91f4e26/raw/c3613d8cee88061dee093ece3384923973bae600/platforms.dhall 

in  { distributions =
        [ { platform                =
              platforms.mac
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "curl-runnings" ] : Optional Text
          , uri                     = "./curl-runnings-0.11.0-mac.tar.gz"
          }
        , { platform                =
              platforms.linux_x86_64
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "curl-runnings" ] : Optional Text
          , uri                     =
              "https://github.com/aviaviavi/curl-runnings/releases/download/0.11.0/curl-runnings-0.11.0.tar.gz"
          }
        ]
    , name          = "curl-runnings"
    , version       = "0.11.0"
    }
