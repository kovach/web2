# Intro

an interaction language

see documentation [here](docs/index.md).

# Building

building the server requires [stack](https://docs.haskellstack.org/en/stable/README/)

building the client requires [npm](https://www.npmjs.com/get-npm)

## Server

```
stack build --profile
stack exec arrow-server
```

for tests:

```
stack test
```

## Client

```
cd ui/
npm install
```

this installs `underscore.js` and `CodeMirror`

open `index.html`.
