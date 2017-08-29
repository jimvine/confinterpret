# (c) Jim Vine
# Author: Jim Vine
# print method for interpretation_set objects.


# Further arguments to be passed to other methods (...) are ignored in this
# function. According to `?print.default` they are in that too, so this is not
# too out of the ordinary.

#' @export
#'
print.interpretation_set <- function(x, ...) {

  validate_interpretation_set(x)

  x_print <-
    cat("Object of class 'interpretation_set', with ", length(x$boundary_names),
        ifelse(length(x$boundary_names) == 1, " boundary", " boundaries"),
        " and ", length(x$interpretations), " interpretations.\n\n",

        ifelse(length(x$boundary_names) == 1,
               "Boundary name:\n  ",
               "Boundary names:\n  "),
        paste(x$boundary_names, sep = "\n  ", collapse = "\n  "),
        "\n\nPlaceholders:\n  ",

        paste(x$placeholders, sep = "\n  ", collapse = "\n  "),
        "\n\nInterpretations:\n",
        #as.data.frame(t(simplify2array(x$interpretations))),

        sep = "")

  invisible(x_print)

  print(interpretations_df(x), right = FALSE)

}



# Internal helper function to make the data.frame version of the
# interpretations used in printing.
#
# If exporting at any point, think whether it should be an as.data.frame method
#
interpretations_df <- function(x) {
  validate_interpretation_set(x)
  # Use same rownames as plot method.
  LETTERS78 <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))
  row_names <- LETTERS78[1 : length(x$interpretations)]
  as.data.frame(t(simplify2array(x$interpretations)), row.names = row_names)
}
