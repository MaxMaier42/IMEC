This is a resubmission. I replaced T with true and added a more detailed description. 

## Test environments
local windows 10: R 3.6.3, R 4.0.2, and R 4.1 (development version)

local Ubuntu 20.04: R 3.6.3

## R CMD check results

There were no errors, warnings, or notes.

However, there might be a warning of the tkplot functionality when running without display. 
I believe if this warning occurs it can safely be ignored since it should not be a 
problem in practice.

## Downstream dependencies

There are no downstream dependencies for this package.
