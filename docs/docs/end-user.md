# Using Scarf as system package manager

## Overview

In addition to easy package management, Scarf supports installing
static binaries in a way that sends anonymized usage statistics to the package
author.

## Installation

On Debian-based linux systems, there is a dependency on `netbase`

```bash
$ sudo apt-get install netbase
```

To get Scarf, simply run:

```bash
$ curl -L https://scarf.sh/install | bash
```

You'll then need to add `~/.scarf/bin` to you `PATH`

## Install your packages

```bash
$ scarf install <package>
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
