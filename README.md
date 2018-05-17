### Usage

First, install `yarn`:

```
$ npm install --global yarn
```

Then install the server dependencies:

```
$ yarn install
```

And finally, to build and run the server:

```
$ yarn
```

Note: You might need to disable CORS in your browser for the client-side app to communicate with the API.

#### Command listing

Run:

```
$ yarn <command>
```

| Command | Action |
| ------- |------- |
| `install` | Install Node.js and PureScript dependencies |
| `build` | Clean and build the project |
| `clean` | Remove all build artifacts |
| `watch` | Watch for file changes, rebuilding and restarting the server |
| `ide` | Start the interactive development tool |
| `start` | Run the server |
| `test` | Run the tests (currently no tests) |

### Todo

- Logging
- Metrics
- Refactoring and DSL development

### Further reading

**PureScript By Example**  
Free book by the creator of PureScript  
https://leanpub.com/purescript
