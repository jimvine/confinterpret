# confinterpret 1.0.0

## S3 class for interpretation_result

* The output of `confinterpret()` now returns an object of class `interpretation_result.
* These results now include the `interpretation_set` object that were used to generate them (previous versions just included the name of the `interpretation_set`).

## Plotting

* Added plot method for `interpretation_result` objects. Plots the result on a chart, using the `interpretation_set` and associated parameters to draw a relevant canvas that shows the boundaries etc.
* Added a plotting function that works for a collection of `interpretation_result` objects. See `?plot_interpretation_result_list`.
* All plotting functions support (and default to) drawing the intervals as curves rather than simple boxes. (Boxes can still be produced by specifying `interval_type = "unif"`.)

## Printing

* Print method provided for `interpretation_set` objects.
* Print method provided for `interpretation_result` objects.

# confinterpret 0.2.0

## Plotting

* Improved `plot.interpretation_set()` function - now takes account of length of `extra_boundaries` names when setting the bottom margin.

## Documentation

* Corrections to help files (including errors in which `interpretation_set` some of the functions use).
* Added examples to help files for `interpret_superiority()`, `interpret_noninferiority()`, and `interpret_equivalence()`.
* Added documentation on placeholders in supplied `interpretation_set` objects.
* Added Value section to help files for `interpret_superiority()`, `interpret_noninferiority()`, and `interpret_equivalence()`.

# confinterpret 0.1.0

NB: v0.1.0 was accepted on CRAN (2017-03-15): https://CRAN.R-project.org/package=confinterpret

* Initial version.
* Added a `NEWS.md` file to track changes to the package.
