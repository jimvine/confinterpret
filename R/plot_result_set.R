# (c) Jim Vine
# Author: Jim Vine
# plotting function for sets of interpretation_result objects.


#' Plotting function for set of interpretation_result objects
#'
#' Produces a plot presenting a set of \code{\link{interpretation_result}}
#' objects on a single chart. If the \code{interpretation_result} objects are
#' named then the names will be used for labelling the relevant intervals on
#' the chart.
#'
#' @param x
#'   A list of \code{interpretation_result} objects, length at least 2. The
#'   objects may optionally be named.
#' @param estimates
#'   Estimate values that the intervals assessed in each
#'   \code{interpretation_result} object relate to. If not specified, a default
#'   of the central point between the two ends of each interval will be assumed.
#' @param ...
#'   Further arguments passed to and from methods.
#'
#' @inheritParams plot_region_canvas
#' @inheritParams plot_intervals
#' @inheritParams plot.interpretation_result
#' @inheritParams plot_intervals_norm
#'
#' @examples
#'
#' # Set up some intervals to test:
#' ci_stage_1 <- matrix(c(0.023, 0.131), nrow = 1,
#'                      dimnames = list("estimate", c("2.5 %", "97.5 %")))
#' ci_stage_2 <- matrix(c(-0.016, 0.096), nrow = 1,
#'                      dimnames = list("estimate", c("2.5 %", "97.5 %")))
#' # Conduct the interpretations:
#'interp_stage_1 <-  interpret_noninferiority(ci_stage_1, actual_null = 0,
#'                                            ni_margin = 0.05,
#'                                            groups = c("Business as usual",
#'                                                       "New approach"))
#'interp_stage_2 <-  interpret_noninferiority(ci_stage_2, actual_null = 0,
#'                                            ni_margin = 0.05,
#'                                            groups = c("Business as usual",
#'                                                       "New approach"))
#'
#' # Assemble the list object:
#' interp_1_and_2 <- list("Stage 1" = interp_stage_1,
#'                        "Stage 2" = interp_stage_2)
#' # Set a nice colour scheme
#' grDevices::palette(c("#FF671F99", "#F2A90099", "#0085CA99"))
#' plot_interpretation_result_set(interp_1_and_2, boundary_label_pos = "on top")
#'
#' @export
#'
plot_interpretation_result_set <- function(x,
                                           extra_boundaries = NULL,
                                           estimates = NULL,
                                           boundary_values = TRUE,
                                           boundary_label_pos = "below",
                                           interpretation_label_pos = "right",
                                           x_axis_pos = "below",
                                           y_axis_pos = "none",
                                           inner_margin = c(-0.1, 0.05,
                                                            -0.1, 0.05),
                                           edge_margin = c(0, 0.02, 0, 0.02),
                                           edge_type = "gradient",
                                           interval_type = "norm",
                                           y_scale = 0.75,
                                           interval_value_labels = TRUE,
                                           estimate_value_labels = TRUE,
                                           plot_estimate_marks = TRUE,
                                           ...) {

  validate_result_set(x)

  interpretation_set <- x[[1]]$parameters$interpretation_set
  names(x[[1]]$parameters$boundaries) <- interpretation_set$boundary_names

  # Extract values from each of the interpretation_result objects
  ci <- t(sapply(sapply(x, "[[", "parameters", simplify = FALSE), "[[", "ci"))
  interpretations_short <- sapply(x, "[[", "interpretation_short")

  plot_region_canvas(boundaries = x[[1]]$parameters$boundaries,
                     extra_boundaries = extra_boundaries,
                     values = ci,
                     interpretations = interpretations_short,
                     boundary_values = boundary_values,
                     boundary_label_pos = boundary_label_pos,
                     interpretation_label_pos = interpretation_label_pos,
                     x_axis_pos = x_axis_pos,
                     y_axis_pos = y_axis_pos,
                     inner_margin = inner_margin,
                     edge_margin = edge_margin,
                     edge_type = edge_type,
                     ...)

  plot_intervals(intervals = ci[(nrow(ci) : 1), ],
                 estimates = rev(estimates),
                 interval_type = interval_type,
                 y_scale = y_scale,
                 interval_value_labels = interval_value_labels,
                 estimate_value_labels = estimate_value_labels,
                 plot_estimate_marks = plot_estimate_marks,
                 ...)

  if(!is.null(names(x))) {
    graphics::text(x = rowMeans(ci),
                   y = (nrow(ci) : 1),
                   names(x))
  }
}

#' Validates a set of interpretation_result objects
#'
#' Checks that a set of interpretation_result objects has been correctly
#' assembled for use in the plotting function.
#'
#'
#' @inheritParams plot_interpretation_result_set
#'
validate_result_set <- function(x) {

  if(length(x) < 2) {
    stop(paste("The interpretation_result_set should be at least length 2",
               "to use this."))
  }

  # Check that the set is valid.

  # Check the first one alone, since for the loop can then go from 2:last
  validate_interpretation_result(x[[1]])

  for (i in 2 : length(x)) {

    # All items are valid results in their own right.
    validate_interpretation_result(x[[i]])

    # All items have the same interpretation_set_name
    if (x[[i]]$parameters$interpretation_set_name !=
        x[[1]]$parameters$interpretation_set_name) {
      stop(paste("All results in a set ought to have been generated using",
                 "the same interpretation_set. (Some values of",
                 "$parameters$interpretation_set_name are not equal.)"))
    }

    # All have the same interpretation_set objects
    if (!isTRUE(all.equal(x[[i]]$parameters$interpretation_set,
                          x[[1]]$parameters$interpretation_set))) {
      stop(paste("All results in a set ought to have been generated using",
                 "the same interpretation_set. (Some values of",
                 "$parameters$interpretation_set are not equal.)"))

    }

    # All items have the same boundaries
    if (!isTRUE(all.equal(x[[i]]$parameters$boundaries,
                          x[[1]]$parameters$boundaries))) {
      stop(paste("All results in a set ought to have been generated using",
                 "the same boundaries. (Some values of",
                 "$parameters$boundaries are not equal.)"))

    }

    # All items have the same low_to_high
    if (!isTRUE(all.equal(x[[i]]$parameters$low_to_high,
                          x[[1]]$parameters$low_to_high))) {
      stop(paste("All results in a set ought to have been generated using",
                 "the same boundary order. (Some values of",
                 "$parameters$low_to_high are not equal.)"))

    }
  }
}


