# Uniform Rules

Constructing life size projects in Haskell is extremely time consuming, 
despite the fact that seemingly all necessary tools are collected and 
ready to use in hackage. The effort to select the "right" package when
several are offered and then to use the package till finally discovering
some obscure quirk which make it difficult and perhaps even impossible
to use in the context of the project. 

There are several efforts to counteract this recognized issue and to 
reduce the effort. The most important one is likely stack with the 
idea of currated sets of packages which are known to have a consistent
set of depenedencies and avoid "Cabal hell" to find a combination of 
versions for which dependencies can be satisfied. 

Other efforts include, for example, the .. , which start with a modified
prelude, which makes integration with other packages from outside. 
Others, e.g. MissingH, collected just missing functionality.

I want a less intrusive solution and try in the uniform collection of packages
to select - in the sense of curation - packages which can work together
and wrap them in an isolation layer to remove effects from changes 
in package from the code of the application. In the same layer, I want 
to bring the package to a common level of interaction.

## Issues of interactions

### Monads
A common problem are packages which are running in their own Monad and 
which are then difficult to combine with others. Especially cumbersome 
are methods to manage errors.

Uniform is standardizing to ErrIO monad, defined as a wrapper around IO 
with Text as the error message format. 

### Character sequences
The confusion between Strings, Text, various kinds of ByteStrings is just 
a permanent nuissance and sometimes posses more time consuming problems.

Uniform standardizes on Text (with rare exceptions where BytStrings are 
desirable for efficiency). The package `uniform-strings` gives short and
consistenly named functions to convert between any formats I have encountered
(with and without error checking).

## Functionality
A minimal subset of functionality is expected from a package. Primarily, 
representable types must have a `Show` and a corresponding `Read` instance, 
such that `read . show = id` (this is relied on in an effort to allow
organized regression testing). Nice formated output can be produce with 
instances of `NiceStrings`. 

A Generic instance is desirable to allow `ToJSON` and `FromJSON` instances 
automatically produced. 

