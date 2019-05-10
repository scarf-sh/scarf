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
$ curl -L https://scarf.sh/install | bash
```

## Create your Scarf account

Head over to [https://scarf.sh](https://scarf.sh) to register your developer
account.

## Create your first package

Once you're registered, you'll want to create your package on the "New Package"
page of the [Scarf website](https://scarf.sh/#/create-package). You'll need a
unique package name, and some description of what your package does.

### Define your package specification

You can now add releases to your package that your users can install! A Scarf
package release primarily involves writing a small package specification or
editing your existing one. Scarf specs are written in
[Dhall](https://dhall-lang.org/), a nifty alternative to yaml. An example
package file would look something like:

```dhall
-- import scarf dhall definitions at the top, scarf installs a dhall library in ~/.scarf/include/scarf.dhall
let platforms =
	  ~/.scarf/include/scarf.dhall

-- then, you'll specify a list of distributions (one per platform)
in  { distributions =
		[ { platform =
			  platforms.mac
		  , simpleExecutableInstall =
			  "curl-runnings"
          -- `URI` can point to a local tar archive or a remote one, here, we use a local one
		  , uri =
			  "./curl-runnings-0.11.0-mac.tar.gz"
		  }
		, { platform =
			  platforms.linux_x86_64
		  , simpleExecutableInstall =
			  "curl-runnings"
          -- `URI` can point to a remote archive, like a Github release
		  , uri =
			  "https://github.com/aviaviavi/curl-runnings/releases/download/0.11.0/curl-runnings-0.11.0.tar.gz"
          , includes = [ "includes", "files", "in your archive" ] : List Text 
		  }
		]
	, name =
		"curl-runnings"
    -- Use standard semantic versioning
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

You can use the `scarf check-package ./path/to/your/package-file.dhall` to
validate your package file. Currently, it won't do things like check your
archive or test your release, but it will make sure you spec type-checks, and
that you have a valid license type and platform.

### Upload your release

Once you have a valid spec, it's time to upload! You'll need your
`SCARF_API_TOKEN`, which you can find by going to your [account
page](https://scarf.sh/#/user-account). To upload, run:

```bash
SCARF_API_TOKEN=${your_token} scarf upload ./path/to/your/validated-spec.dhall
```

**Packages on Scarf can't be deleted once they're uploaded!** This is part of
the reason scarf uses dhall for package files: Static types will help us enforce
that package specs are correct ahead of time.

Once your release is uploaded, your users can install your package with a simple:

```bash
scarf install ${your_package_name}
```

## View your package analytics

Once you've pushed a release to your package, you can head over to the
[dashboard](https://scarf.sh/#/home) to see your packages install and usage
stats!

