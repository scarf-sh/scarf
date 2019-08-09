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

If your users wish to use your package without reporting usage statistics, they
can pay you for it! You can easily create a Scarf-connected Stripe account, set
your desired price for your package, and your users can easily pay to use your
package without reporting usage statistics.

The best part is that you can get all of these features without writing any
code! You simply upload your package to Scarf. The end-user will install your
package via `scarf` which installs your program inside a wrapper so that Scarf
can capture your package's usage statistics and enforce permission levels.

## Install Scarf

On Debian-based linux systems, there is a dependency on `netbase`

```bash
$ sudo apt-get install netbase
```

To get Scarf, simply run:

```bash
$ curl -L https://scarf.sh/install | bash
```

You'll then need to add `~/.scarf/bin` to you `PATH`


## Create your Scarf account

Head over to [https://scarf.sh](https://scarf.sh) to register your developer
account.

## Create your first package

Once you're registered, you'll want to create your package on the "New Package"
page of the [Scarf website](https://scarf.sh/#/create-package). You'll need a
unique package name, and some description of what your package does. Scarf currently supports packages in the form of:

- A locally built archive with an executable that can be run directly on the target platform.
- An npm package that you upload to scarf rather than npm itself.

### Define your package specification

You can now add releases to your package that your users can install! A Scarf
package release primarily involves writing a small package specification or
uploading your npm package directly. 

#### Yaml specifications (archive based packages)

Standard archive based packages can be described in yaml.

```yaml
name:  "curl-runnings"
author: "Avi Press"
copyright: "2019 Avi Press"
license: "MIT"
version: "0.11.0"
# For each platform (currently MacOS and Linux_x86_64) you're distributing your release to, include an entry in distributions.
distributions:
  -
    platform: MacOS
    # For simple archive installations, set the name of the executable that will be invoked for your package
    simpleExecutableInstall: "curl-runnings"
    # uri can be a remote or local tar archive
    uri: "https://github.com/aviaviavi/curl-runnings/releases/download/0.11.0/curl-runnings-0.11.0-mac.tar.gz"
    # [Optional] if your archive has extra files that should be included, list them here
    includes:
      - "./directories"
      - "./or-files.txt"
  -
    platform: Linux_x86_64
    simpleExecutableInstall: "curl-runnings"
    uri: "./path/to/local/archive.tar.gz"
    includes: []
    depends:
      - scarf-packages
      - your-package
      - depends-on
```

Some notes: 

- `simpleExecutableInstall` is currently the only install type supported, but more will be coming soon.

You can use `scarf check-package ./path/to/your/package-file.yaml` to
validate your package file. Currently, it won't do things like check your
archive or test your release, but it will make sure you spec type-checks, and
that you have a valid license type and platform.

Dependency handling is still immature. Currently any dependencies you add in
your scarf.yaml will use the latest validated version.

#### Npm 

You can upload an npm based package to scarf rather than npm itself. It will be
globally installed by scarf just like any other scarf package. You'll need to
make sure your `package.json` includes a `main` entry that points to the script
that will be ultimately invoked, and the package must have a license.

You can use `scarf check-package ./path/to/your/package.json to
validate your package file.

### Upload your release

Once you have a valid spec, it's time to upload! You'll need your
`SCARF_API_TOKEN`, which you can find by going to your [account
page](https://scarf.sh/#/user-account). To upload, run:

```bash
SCARF_API_TOKEN=${your_token} scarf upload ./path/to/your/validated-spec.(yaml|json)
```

**Packages on Scarf can't be deleted once they're uploaded!** 

Once your release is uploaded, your users can install your package with a simple:

```bash
scarf install ${your_package_name}
```

## View your package analytics

Once you've pushed a release to your package, you can head over to the
[dashboard](https://scarf.sh/#/home) to see your packages install and usage
stats!

## Connect a Stripe Account

Navigate to your account details page by clicking your username in the nav bar.
Find the `Connect to Stripe` button at the bottom, and follow Stripe's
onboarding process. It only takes a few minutes. You'll be redirected back to
Scarf when the enrollment is complete.

## Add a private usage tier to your package

Now that you've connected a Stripe account, you're ready to start collecting
payments for your package! Navigate to your package detail page on Scarf and
click `Start monetizing <package>`. Set your price. That's it! Scarf handles all
user registration and payments so you are now fully ready to start making money
from your package. Stripe will send your payouts daily to your connected bank
account. (NOTE: your very first payout will go through 7 days after your account
is opened, so be patiend for your first payout.)

## Keeping Scarf up to date

A simple upgrade command is all you need to get the latest version of Scarf

```
$ scarf upgrade
```
