# UniformBase

The package imports separately compiled packages and exports all its symbols. 

# design
- the individual packages must be listed in the cabal.project file (in packages which import as well!)
- the symbols are imported globally with UniformBase (not needed to import the subpackages individually and not needed to list them in the package.yaml)
