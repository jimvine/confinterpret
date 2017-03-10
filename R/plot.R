# (c) Jim Vine
# Author: Jim Vine
# plot method for interpretation_set objects.

#' Plot a diagram of the valid options for an interpretation_set object
#'
#' Produces a diagram that illustrates the set of pairs of lower and upper
#' confidence limits that are valid for a given
#' \code{\link{interpretation_set}} object. The output is presented as a set of
#' regions in different colours with boxes either within regions or spanning
#' them to illustrate where the lower and upper confidence limits sit. The
#' options are labelled alphabetically, and presented in the order in which
#' their associated interpretations should be provided in the
#' \code{interpretation_set}.
#'
#' Additional boundaries can be displayed using the \code{extra_boundaries}
#' parameter. This can be helpful if you want to show a position that is
#' of some practical relevance, but is not defined as a boundary for the
#' purposes of the \code{interpretation_set}. The boundaries specified by the
#' \code{interpretation_set} are plotted with spacing 1 and are centred about
#' 0: for an even number of boundaries the central pair of boundaries will be
#' at -0.5 and +0.5; for an odd number of boundaries the central one will be
#' at 0, and the next ones (if any) will be at -1 and +1, and so on.
#'
#' @param interpretation_set
#'   An \code{interpretation_set} object for plotting.
#' @param extra_boundaries
#'   A vector of numerical values specifying the position for displaying
#'   additional boundaries, not specified in the \code{interpretation_set}.
#'   See Details.
#'
#' @examples
#' # Plot the pre-defined interpretations_equivalence object with an additional
#' # central boundary to illustrate where the actual null point is.
#' plot(interpretations_equivalence, extra_boundaries = 0)
#'
#'@export
#'
plot.interpretation_set <- function(interpretation_set,
                                    extra_boundaries = NULL, ...) {

  validate_interpretation_set(interpretation_set)

  # Definitions ---------------------------------------------------------------

  boundary_spacing <- 1

  number_boundaries <- length(interpretation_set$boundary_names)
  number_regions    <- number_boundaries + 1


  # TODO: Rework to allow users to pass alternative colour sets if they want.

  # Define a colour_set to use for the backgrounds of the regions
  # colour_set <- terrain.colors(number_regions)
  colour_set <- colours_tol_sunset(number_regions)
  colour_set <- rev(colour_set)


  # Create a set of boundaries ------------------------------------------------

  # Is there an even number of boundaries?
  boundaries_even <- number_boundaries %% 2 == 0

  if (boundaries_even) {
    boundaries <- seq(from = boundary_spacing * (0.5 - number_boundaries / 2),
                      by = boundary_spacing,
                      length.out = number_boundaries)
  } else {
    boundaries <- seq(from = boundary_spacing * (number_boundaries - 1) / -2,
                      by = boundary_spacing,
                      length.out = number_boundaries)
  }


  # Generate the set of regions -----------------------------------------------

  n <- 0
  lower_regions <- 0
  upper_regions <- 0

  for (region_lower in 1:number_regions) {
    for (region_upper in region_lower:number_regions) {
      n <- n + 1

      lower_regions[[n]] <- region_lower
      upper_regions[[n]] <- region_upper

    }
  }

  region_list <- cbind(lower_regions, upper_regions)

  lower_value <- 0
  upper_value <- 0

  # Add an extra upper boundary on for the purposes of this routine.
  boundaries <- c(boundaries, boundaries[length(boundaries)] + boundary_spacing)

  for (option_counter in 1 : nrow(region_list)) {
    # Separate cases for the two ends being in the same or different regions
    if (lower_regions[option_counter] == upper_regions[option_counter]) {
      lower_value[[option_counter]] <- boundaries[[ lower_regions[[option_counter]] ]] - 0.75 * boundary_spacing
      upper_value[[option_counter]] <- boundaries[[ upper_regions[[option_counter]] ]] - 0.25 * boundary_spacing
    } else {
      lower_value[[option_counter]] <- boundaries[lower_regions[option_counter]] - 0.5 * boundary_spacing
      upper_value[[option_counter]] <- boundaries[upper_regions[option_counter]] - 0.5 * boundary_spacing
    }
  }

  # Special cases for first and last.
  # Keep those the same size as intervals that span 2 adjacent bands.
  # Should also support keeping the top and bottom region being drawn
  #   larger. Will typically be reasonable to imply that those regions extend
  #   to smaller / larger values (perhaps -Inf to +Inf in some cases).

  lower_value[[1]]                   <- upper_value[[1]] - boundary_spacing
  upper_value[[length(upper_value)]] <- lower_value[[length(upper_value)]] +
                                          boundary_spacing

  # Drop the extra top boundary
  boundaries <- boundaries[1:length(boundaries)-1]

  # Lose un-needed precision
  lower_value <- round(lower_value, 6)
  upper_value <- round(upper_value, 6)

  ci_perms <- cbind(lower_value, upper_value)

  # Labelling
  # stackoverflow: http://stackoverflow.com/a/25877009
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

  rownames(ci_perms) <- LETTERS702[1 : nrow(ci_perms)]


  # Actual plotting -----------------------------------------------------------

  last <- nrow(ci_perms)
  top <- last + 1



  boxplot.matrix(ci_perms[last : 1,],
                 use.cols = FALSE,
                 horizontal = TRUE,
                 medlty = "blank", las=2,
                 axes=FALSE)



  # Draw some background boxes

  rect(xleft = c(boundaries[1] - 2 * boundary_spacing, boundaries),
       ybottom = 0,
       xright = c(boundaries, boundaries[length(boundaries)] + 2 * boundary_spacing),
       ytop = top,
       col = colour_set)

  # Extra 'boundaries': a dotted line if anything is passed as extra_boundaries
  # Useful for equivalence: want to show actual_null but not treat it as a
  #   boundary. E.g.:
  # plot(interpretations_equivalence, extra_boundaries = 0)
  abline(v=extra_boundaries, lty="15151555")
  # TODO: Extract and display names / labels for these if present.

  boxplot.matrix(ci_perms[last:1,],
                 use.cols = FALSE,
                 horizontal = TRUE,
                 medlty = "blank", las=2,
                 add=TRUE,
                 axes=FALSE)

  # Label the boxes
  text(x=rowMeans(ci_perms), y=(last:1),
       rownames(ci_perms))

  # Label the boundaries.
  Axis(side = 1,
       at = boundaries,
       labels = interpretation_set$boundary_names,
       las = 2,  # Label text perpendicular to axis
       lwd = 0)

  # Label with the interpretations
  # Uses lapply and `[[` to extract the interpretation_short values.
  Axis(side = 4,
       at = 1 : last,
       labels = lapply(interpretation_set$interpretations,
                       `[[`,"interpretation_short"),
       las = 2,  # Label text perpendicular to axis
       lwd = 0)  # Line width - 0 supresses line.

}

