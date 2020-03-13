# Scarf SDK's for library authors

Scarf also offers analytics tools for libraries! By simply adding a dependency
to a Scarf language library, you can be getting better insight into how your
package is used

## scarf-js (npm)

You can find scarf-js on [Github](https://github.com/scarf-sh/scarf-js) or on
[npm](https://www.npmjs.com/package/@scarf/scarf) directly.

### Installation

You'll first need to create a library entry on Scarf. Once created, add a
dependency on this library to your own:

```bash
npm i --save @scarf/scarf
```

Once your library is published to npm with this change, Scarf will automatically
collect stats on install, no additional code is required!

Head to your package's dashboard on Scarf to see your reports when available.

## More languages coming soon

We're working to build out sibling libraries for various languages. If you're
interested in using Scarf in a language we haven't released yet, let us know!

