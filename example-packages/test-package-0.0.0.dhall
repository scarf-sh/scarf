    let platforms =
          https://gist.githubusercontent.com/aviaviavi/75b14f444ee34eb60dbc0c0cf91f4e26/raw/8665f639d0fd0459caee760209534da747ec316c/platforms.dhall 

in  { distributions =
        [ { platform                = platforms.mac
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "p" ] : Optional Text
          , url                     = "file:///Users/avipress/side/u-test.sh"
          }
        , { platform                = platforms.linux64
          , signature               = [] : Optional Text
          , simpleExecutableInstall = [ "p" ] : Optional Text
          , url                     = "file:///Users/avipress/side/u-test.sh"
          }
        ]
    , name          = "p"
    , version       = "0.0.1"
    }
