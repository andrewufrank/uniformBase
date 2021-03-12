`Error.hs` defines an uniform approach to error:
    any function which can fail returns a value of 
    - `ErrIO`, which is a wrapper around IO, returning a Text if the computation fails 
    - `ErrOrVal` (which is `Either Text`) for functions not in the IO monad

All cases where functions return other error signaling methods must be converted as part of their integration into the uniform framework. 

Especially important are the functions to convert to regular IO (and back): 
- `callIO` which calls an IO function and catches a possible error return.  
-`ErrIO a` conversion into `IO (ErrOrVal a)` which will be important to make the HTF package for testing usable with operations in the ErrIO monad. 

Some other functions are helpful to identify hard to track problems in Haskell: 

`undef` to find out where the undefined value is comming from 

The reminder is a collection of things which I found useful once; it will be interesting to see how much is actually useful and used. 

The package depends on uniform-string, which depends on unif0rm-error for its handling of tests.

The StartApp is a bridge between the ErrIO to the standard IO monad.

