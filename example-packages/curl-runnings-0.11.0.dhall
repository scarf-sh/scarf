-- import scarf dhall definitions at the top
-- then, you'll specify a list of distributions (one per platform)
-- `URI` can point to a local tar archive or a remote one
let platforms =
	  https://gist.githubusercontent.com/aviaviavi/16caf330e97df23c892cab1c97316ba9/raw

in  { name =
		"curl-runnings"
	, author =
		"Avi Press"
	, copyright =
		"2019 Avi Press"
	, license =
		"MIT"
	, version =
		"0.11.0"
	, distributions =
		[ { platform =
			  platforms.mac
		  , signature =
			  [] : Optional Text
		  , simpleExecutableInstall =
			  [ "curl-runnings" ] : Optional Text
		  , uri =
			  "./curl-runnings-0.11.0-mac.tar.gz"
		  }
		, { platform =
			  platforms.linux_x86_64
		  , signature =
			  [] : Optional Text
		  , simpleExecutableInstall =
			  [ "curl-runnings" ] : Optional Text
		  , uri =
			  "https://github.com/aviaviavi/curl-runnings/releases/download/0.11.0/curl-runnings-0.11.0.tar.gz"
		  }
		]
	}
