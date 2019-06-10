# Using Scarf as system package manager

## Overview

In addition to easy package management, Scarf supports installing
static binaries in a way that sends anonymized usage statistics to the package
author.

## Installation

```bash
$ curl -L https://scarf.sh/install | bash
```

You'll then need to add `~/.scarf/bin` to you `PATH`

## Install your packages

```bash
$ scarf install <package>
```

#### Version handling

You can install specific versions of a package with the --version flag by
supplying a version or a version range. The latest version within range will be
installed.

```bash
$ scarf install <package> --version=1.0.1
$ scarf install <other package> --version=">1.0 && <2.0"
```

#### System package file

Scarf keeps track of installed packages in `~/.scarf/scarf-package.json`. It can
be useful to keep this file in version control so you can keep a consistent
package setup across multiple computers. If you want to (re)install all packages
from your system package file, run:

```bash
$ scarf install --system-package-file
```

## Keeping Scarf up to date

A simple upgrade command is all you need to get the latest version of Scarf

```bash
$ scarf upgrade
```
