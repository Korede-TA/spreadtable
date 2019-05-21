# spreadtable
Experimental speadsheet-based interface for structured programming based on "structured grammars".

Think of it as a grid-based terminal of sorts. 

The idea came when i realized what the terminal represents as something of the base-level substrate for interacting with computers. 
The terminal is something of a relic of history, being an emulator of pre-CRT computers that utilized teletypes to display the output of computations. 

The spreadsheet metaphor is appealing because of it's seeming approachability & readability to the average user and it's unique suitability for [homoiconicity](https://en.wikipedia.org/wiki/Homoiconicity). 

Given that the terminal serves as both a REPL for interacting with the computer programs, as well as a platform for editors for creating/editing programs, i believe this interface can serve a similar purpose.

# Developing

* copy or clone this repository
* The bucklescript code goes into _src/*.ml_
* The _release_ folder contains an _index.html_ and rollup bundles your js here in _main.js_

## Install

```
npm install
```

## Build

```
npm run build
```

## Watch

```
npm run watch
```
