# What is this?
- a general purpose rule-based programming language
- an intepreter that exposes full derivations for each value it computes
- a server/client system that supports interactive GUI applications

# Motivation
- Software application users should be able ask simple questions about an app and get simple answers back.
- To change a small part of the program, they shouldn't have to learn all of its structure. Instead the evaluation context should make it easy to denote any small part of the application, and provide concise explanations localized to that part.
- This repo is a proof of concept. A user can ask a question (by clicking a visual element) and get an answer (a derivation for the tuple that backs the element). All data in the running system is accessible through this mechanism, and all the running rules can be modified.

# Documentation

[stale](docs/index.md).

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
