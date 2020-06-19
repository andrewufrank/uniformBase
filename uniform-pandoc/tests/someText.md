---
title: An IDE for Haskell - a new start
keywords: Haskell IDE
abstract: The long struggle for an IDE to write Haskell code came to an end for me when I had to give up Leksah. It was not possible to install the program anymore and I switched to an all purpose editor (VScode) and search for support tools; less than perfect, but again a productive tool. Now, I try again for more...
date: 2020-06-16
---


# An new start for an IDE for Haskell

The current theorie is to combine components to achieve an Integrated Development Environment. The components must build on the existing base tools: 

- ghc compiler
- cabal and stack to manage packages and link them together
- git for versioning and storage management
- VScode, for editing and visualizationThis is a minimal Haskell project with a library and some routines which all do nothing. 

I used it for experimenting with data types, but the interest here is the integration with a Haskell IDE, namely the use of ghcide with VScode.

