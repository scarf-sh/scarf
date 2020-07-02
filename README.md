# Scarf (beta) [![Build Status](https://travis-ci.org/aviaviavi/scarf.svg?branch=master)](https://travis-ci.org/aviaviavi/scarf) [![Gitter](https://badges.gitter.im/scarfsh/community.svg)](https://gitter.im/scarfsh/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Faviaviavi%2Fscarf.svg?type=shield)](https://app.fossa.com/projects/git%2Bgithub.com%2Faviaviavi%2Fscarf?ref=badge_shield)

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

Support your work by charging to use your package without sending these usage
statistics. Just create and connect a Stripe account through Scarf and you can
be making money from your work in minutes.

The best part is that you can get all of these features without writing any
code! You simply upload your package to Scarf. The end-user will install your
package via `scarf` which installs your program inside a wrapper so
that Scarf can capture your package's usage statistics or validate payment for
additional features.

See the documentation at [https://docs.scarf.sh](https://docs.scarf.sh).

### Installing

You can install `scarf` by running:

```bash
curl -L https://scarf.sh/install | bash
```


## License
[![FOSSA Status](https://app.fossa.com/api/projects/git%2Bgithub.com%2Faviaviavi%2Fscarf.svg?type=large)](https://app.fossa.com/projects/git%2Bgithub.com%2Faviaviavi%2Fscarf?ref=badge_large)