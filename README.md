# confinterpret

[![Travis-CI Build Status](https://travis-ci.org/jimvine/confinterpret.svg?branch=master)](https://travis-ci.org/jimvine/confinterpret)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/confinterpret)](http://cran.r-project.org/package=confinterpret)

An R package for interpreting confidence intervals.

The goal of confinterpret is to allow users to produce standardised 
descriptive interpretations from confidence intervals in R. The package
supports researchers to focus more on generating estimates with
associated ranges of uncertainty, rather than on single point-estimates, 
such as are common when conclusions are drawn from p-values.

The package includes (extensible) support for various test types, 
which are specified as sets of interpretations dependent on where the 
lower and upper confidence limits sit.

A collection of interpretation_set objects are provided in the package,
to cover certain common situations where the confidence interval is of
an effect size comparing two groups. Convenience / wrapper functions are 
provided for using each of these:

* Superiority tests: `interpret_superiority()`
* Non-inferiority tests: `interpret_noninferiority()`
* Equivalence tests: `interpret_equivalence()`

## Installation

confinterpret is on CRAN, so you can install it in the normal way:

```R
install.packages("confinterpret")
```

You can also install the development version of confinterpret from github with:

```R
# install.packages("devtools")
devtools::install_github("jimvine/confinterpret")
```

## Example

This is a basic example which shows you how to solve a common problem:

```R
# Set up a dummy confidence interval
ci_above <- matrix(c(0.15, 0.25), nrow = 1,
                   dimnames = list("estimate", c("2.5 %","97.5 %")))

ci_above_sup <- interpret_superiority(ci_above)

# Access the short-form interpretation:
ci_above_sup$interpretation_short

# Access the full interpretation:
ci_above_sup$interpretation

# Access the full interpretation, marked up with markdown:
ci_above_sup$interpretation_md
```
