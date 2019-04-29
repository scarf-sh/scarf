# Using Scarf as software distribution tool

Scarf's mission is to help open source developers distribute and monetize their work. 

## What is Scarf? Why should I use it?

Scarf is a developer-centric, cross-platform system package manager
that enables developers to see how their software is actually used and to get
paid for their work. If you distribute your software with Scarf, your users will
be able to easily install it with the `scarf` CLI, and you'll gain insights into
how your software is used such as:

- Install counts
- Exit codes when your program is invoked
- Execution times
- Sub-commands and flags that are passed on the command line

The best part is that you can get all of these features without writing any
code! You simply upload your package to Scarf. The end-user will install your
package via `scarf` which installs your program inside a wrapper so that Scarf
can capture your package's usage statistics.

Future work will be to allow you to charge installers of your packages for
installation, usage, and/or support.

## Install Scarf

You'll need to install the `scarf` CLI with:

```bash
$ curl https://gist.githubusercontent.com/aviaviavi/860638ec8d3728ea74ed36176cd26173/raw/ | bash
```

## Create your Scarf account

Head over to [https://scarf.sh](https://scarf.sh) to register your developer
account.

## Create your first package

Creating a Scarf package primarily involves writing a small package
specification. Scarf specs are written in [Dhall](https://dhall-lang.org/), a
nifty alternative to yaml. An example package file would look like:

```dhall
-- import scarf dhall definitions at the top
-- then, you'll specify a list of distributions (one per platform)
-- `URI` can point to a local tar archive or a remote one
let platforms =
	  https://gist.githubusercontent.com/aviaviavi/16caf330e97df23c892cab1c97316ba9/raw

in  { distributions =
		[ { platform =
			  platforms.mac
		  , simpleExecutableInstall =
			  [ "curl-runnings" ] : Optional Text
		  , uri =
			  "./curl-runnings-0.11.0-mac.tar.gz"
		  }
		, { platform =
			  platforms.linux_x86_64
		  , simpleExecutableInstall =
			  [ "curl-runnings" ] : Optional Text
		  , uri =
			  "https://github.com/aviaviavi/curl-runnings/releases/download/0.11.0/curl-runnings-0.11.0.tar.gz"
		  }
		]
	, name =
		"curl-runnings"
	, version =
		"0.11.0"
	, author =
		"Avi Press"
	, copyright =
		"2019 Avi Press"
	, license =
		"MIT"
	}
```

Some notes: 

- `simpleExecutableInstall` is currently the only install type supported, but more will be coming soon.

