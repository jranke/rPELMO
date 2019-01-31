# rPELMO

The R package **rPELMO** provides some utilities for working with the pesticide
leaching model FOCUS PELMO.

## Installation

The easiest way to install the package is probably to use 
[drat](https://cran.r-project.org/package=drat):

```r
install.packages("drat")
drat::addRepo("jranke")
install.packages("rPELMO")
```

Alternatively you can install the package 
using the `devtools` package.  Using `quick = TRUE` skips docs,
multiple-architecture builds, demos, and vignettes.


```r
library(devtools)
install_github("jranke/rPELMO", quick = TRUE)
```

## Use

Please refer to the [reference](http://pkgdown.jrwb.de/rPELMO/reference/index.html).

## Examples

An example how FOCUS PELMO can be run in parallel under Linux is shown
[here](http://pkgdown.jrwb.de/rPELMO/reference/PELMO_runs.html).
