# Up and running with ‘stack’

---

# Today's 🍕 sponsor

![inline 180%](bekk_logo.pdf)

---

# *Stack* in ten seconds

- Tool for developing Haskell projects
- Alternative to `cabal-install`
- Uses `.cabal` files

^ Will get back to what that means.

---

# Agenda

1. Introduction & Installing
1. Design goals
1. `stack.yaml`
1. Demos
1. Docker
1. What about *cabal*?

---

^ stack is still quite new:

    ANNOUNCING: first public beta of stack
    
    9 Jun 2015 Chris Done
    
    stack is a new, complete, cross-platform development
    tool aimed at both new and experienced Haskell
    developers alike, for installing and setting up the
    compiler, installing packages needed, and building,
    testing or benchmarking one or more packages in a
    project at a time. It's the whole stack.

---

# Main features

- Installing GHC automatically, in an isolated location
- Installing packages needed for your project
- Building your project
- Testing and benchmarking your project

^ The life-cycle of your project
^ Windows support

---

# Installing

Installing stack

- OS X: `brew install haskell-stack`
- Windows: [`docs.haskellstack.org/en/stable/install_and_upgrade.html#windows`](http://docs.haskellstack.org/en/stable/install_and_upgrade.html#windows)
- Ubuntu/Debian: add key + apt source and run `apt-get install stack`

^ Instructions for other distros
    
---

# Design goals

- *Reproducible builds*: If it builds today, it will build tomorrow—with the same result


- *User friendly, discoverable* interface: `stack --help` should be enough to get started

- *Isolated*: Will only make changes to some specific stack directories

^ Will not touch any system version of GHC or interfere with packages installed by cabal or any other build tools.

---

# Reproducible builds

The primary stack design point. If you run `stack build` today, you should get the same result running `stack build` tomorrow.

> stack applies to package management the same old recipe that made the success of functional programming: manage complexity by making the output of all actions proper functions of their inputs.

^ Running cabal install at different times can lead to wildly different install plans, without giving any good reason to the user.

^ There are some cases that can break that rule (changes in your operating system configuration, for example), but, overall, stack follows this design philosophy closely.

---

# User-friendly

- `stack --help`
- Will try to tell you what's wrong (i.e. that you need to run `stack setup`)

---

# Isolated

- `~/.stack` for shared files
- `.stack-work` for project-specific files
- Will share builds when it's safe

^ Explicit sandboxing is not required by stack. All builds are automatically isolated. You won’t accidentally corrupt your installed packages with actions taken in other projects.

           $ stack build
           lifted-base-0.2.3.6: build
           lifted-base-0.2.3.6: copy/register
           network-2.6.2.1: copying precompiled package
           primitive-0.6.1.0: copying precompiled package

---

# Stackage

Curated package sets called snapshots.

[`stackage.org`](https://www.stackage.org/)

Saves you the hassle of figuring out which packages that work together and with which compiler version.

Will reuse built packages across snapshots when it is safe to do so.

---

# `stack.yaml`

- Created with `stack init`/`stack new`.
- Contains a reference to the snapshot the package will be built against – called a *resolver*
- Should be committed together with the rest of the project (and the `.cabal` file)

^ To build your project, stack uses a stack.yaml file in the root directory of your project as a sort of blueprint. That file contains a reference, called a resolver, to the snapshot which your package will be built against.

---

# Demo 1: Hello, World

^ 	Creating a new project
^ 		`stack init`, look at stack.yaml

```
$ stack new my-project
$ cd my-project
$ stack setup
$ stack build
$ stack exec my-project-exe
```

---

# Demo 2: Using a template

- List available templates: `stack templates`
- Using a template: `stack new [project name] [template]`

[`github.com/commercialhaskell/stack-templates`](https://github.com/commercialhaskell/stack-templates)

---

# Demo 3: Tests and benchmark

- `stack test`
- `stack bench`

---

# Demo 4: Initialize a project based a `.cabal` file

`stack init` will search for a compatible resolver (snapshot) and then create
`stack.yaml`.

---

# Dependencies not in the stackage snapshot: hackage

    resolver: lts-4.0
    packages:
    - '.'
    - extra-deps
      - some-package-0.1.1
      - some-other-package-0.8

---

# Dependencies not in the stackage snapshot: directories/git repo

     resolver: lts-4.0
     packages:
     - '.'
     - '../someproject'
     - location:
         git: git@github.com:haskell-servant/servant
         commit: 357cc839b65601975cb186a3f00a729f7f3bb2de
       subdirs:
         - servant
         - servant-client
         - servant-server

^ git submodule or `location`

---

# Installing programs and libraries outside of a project

- Global: `~/.stack/global-project/stack.yaml`
- To install a library or an executable, simply run `stack install lens`
- Binaries get a symlink in: `~/.local/bin`
- Using a stack-installed ghc:

    $ stack ghc
    $ stack ghci

^ Will have access to libraries for the given resolver version

---

# Shell scripts

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-4.0 --install-ghc runghc --package pandoc

import Text.Pandoc
import Text.Pandoc.Error (handleError)

main = putStrLn
     . writemediawiki def
     . handleError
     . readMarkdown def
     =<< getContents
```

^ The second line, is a Haskell comment providing additional options to stack (due to the common limitation of the “shebang” line only being allowed a single argument). In this case, the options tell stack to use the lts-3.2 resolver, automatically install GHC if it is not already installed, and ensure the pandoc package is available.

---

# Shell scripts

    $ echo "# Heading\n\nThis is a [http://test.com](test)." \
    | ./md2mw
    Progress: 8/15
    
    $ echo "# Heading\n\nThis is a [http://test.com](test)." \
    | ./md2mw
    = Heading =
    
    This is a [[test|http://test.com]].

---

# A simple *Makefile* is all you need…

```makefile
all:
    stack setup
    stack build
    stack install --local-bin-path=${LOCAL_BIN_DIR}
```

^ Great for CI/build server

---

# *Docker*

Stack can integrate with *docker* in two ways

1. All `stack` commands can be run in a (transient) docker image.
2. Can create a *Docker* image that can be deployed to

# 🌧

^ All developers work in the same environment even if their machines are otherwise different


---

# What is the relationship between stack and cabal?

- The `Cabal` library is used by stack to build your Haskell code
- A `.cabal` file defines all package-level metadata just like in the `cabal-install` world

^ I.e. modules, executables, test suites, etc.
^ No change at all on this front.

- A `stack.yaml` file provides information on where dependencies come from

^ `stack build` currently initializes a stack.yaml from the existing .cabal file. Project initialization is something that is still being discussed and there may be more options here for new projects in the future (see issue 253)

^ `cabal-install` (the executable) is used by stack for its dependency solver functionality.

---

# What about *cabal*?

^ How is it different from Cabal?
^ Why was it developed as a separate project instead of being worked on with Cabal?

“Cabal” can mean three things:

- A package metadata format (`.cabal` files) and the corresponding spec
- An implementation of the spec as a framework

^ aka Cabal-the-library

- The `cabal-install` command-line tool

^ a command-line tool that uses Cabal-the-library

---

# What about *cabal*?

- You *can* achieve much of what stack brings with `cabal-install`
- Sandboxes
- Freezing

^ Sharing is hard.

- Future plans for `cabal-install`

---

# Loose ends

- Compiling your project whenever a file changes: `stack build --file-watch`

- How do I update my package index? (I.e. what is the equivalent of `cabal update`?)

^ Not needed. If the package index is missing, or if a snapshot refers to package/version that isn't available, stack will automatically update and then try again.

- What about version bounds?

^ Not needed since the stackage snapshot specifies the version of all packages
^ PVP for hackage

---

# Summary

- (`stack setup`)
- `stack new my-project`
- `stack new my-project scotty-hello-world`
- `stack build`/`stack test`/`stack bench`
- `stack ghci`
- `stack.yaml`

---

# More Information

- [`haskellstack.org`](http://haskellstack.org/)
- `#haskell-stack`

---

# Questions

---

# Next Meetup

- 1 February: *Servant: Defining web APIs at the type level*
- ?: *Your presentation?*
