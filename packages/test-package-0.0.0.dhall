    let platforms = ./platforms.dhall 

in  { distributions    =
        [ { platform  = platforms.mac
          , signature = [] : Optional Text
          , url       = "file:///Users/avipress/side/u-test.sh"
          }
        , { platform  = platforms.linux64
          , signature = [] : Optional Text
          , url       = "file:///Users/avipress/side/u-test.sh"
          }
        ]
    , name             = "u-test"
    , uploaderUsername = "aviaviavi"
    , version          = "0.0.1"
    }
