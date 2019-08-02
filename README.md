# Scarf (beta) [![Build Status](https://travis-ci.org/aviaviavi/scarf.svg?branch=master)](https://travis-ci.org/aviaviavi/scarf) [![Gitter](https://badges.gitter.im/scarfsh/community.svg)](https://gitter.im/scarfsh/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Scarf's mission is to help open source developers monetize their work. 

Concretely, Scarf is a developer-focused, cross-platform system package manager
that enables developers to see how their software is actually used and to get
paid for their work. If you distribute your software with Scarf, your users will
be able to easily install it with the `scarf` CLI, and you'll gain insights into
how your software is used such as:

- Install counts
- Exit codes when your program is invoked
- Execution times
- Sub-commands and flags that are passed on the command line

You can gain all these insights without writing any code! You simply upload your
package to Scarf. The end-user will install your package via `scarf` which
installs your program inside a wrapper so that Scarf can capture your package's
usage statistics.

Future work will be to allow you to charge users of your packages for
installation, usage, and/or support.

See the documentation at [https://docs.scarf.sh](https://docs.scarf.sh).

### Installing

You can install `scarf` by running:

```bash
curl -L https://scarf.sh/install | bash
```

### Status

Scarf is in very early stages, and is being developed rapidly. Lots of
functionality is either missing or buggy. Please feel encouraged to submit any
questions, comments, or issues you run into. All feedback is welcome and
appreciated!
