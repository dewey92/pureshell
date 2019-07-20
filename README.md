# PureShell 🖥
My own personal excersice to tame Purescript compiler 😄 PureShell is an attempt to reimplement common Shell commands like `cat`, `ls`, `find`, etc in Purescript. PRs and suggestions are very much welcomed

## Development
To try running in your local machine, some dependencies are required:
```
git clone https://github.com/dewey92/pureshell
cd pureshell

npm i -g purescript spago
spago install
```

To watch for changes, simpley run
```
yarn dev
```

## Build
Building is also easy. Run
```
yarn build
```

and you'll have `pureshell` in your global variable

## Usage
The easiest way to list all available commands along with their options, use
```
pureshell --help
```

Currently, it only support the following commands:
- cat
- ...
