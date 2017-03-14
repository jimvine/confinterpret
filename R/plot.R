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
#' Plots use the current R Graphics Palette, so you may wish to set that to
#' something attractive before plotting. See ?palette.
#'
#' @param x
#'   An \code{interpretation_set} object.
#' @param extra_boundaries
#'   A vector of numerical values specifying the position for displaying
#'   additional boundaries, not specified in the \code{interpretation_set}.
#'   May optionally be named values; if named, the names will be labelled
#'   on the plot axis. See Details for information on specifying locations.
#' @param ...
#'   Further arguments passed to and from methods.
#'
#' @examples
#' # Set a nice colour scheme
#' grDevices::palette(c("#FF671F99", "#F2A90099", "#0085CA99"))
#' # Plot the pre-defined interpretations_equivalence object with an additional
#' #   central boundary to illustrate where the actual null point is.
#' plot(interpretations_equivalence, extra_boundaries = c("Actual null" = 0))
#'
#' @export
#'
plot.interpretation_set <- function(x, extra_boundaries = NULL, ...) {

  if (!requireNamespace("graphics", quietly = TRUE)) {
    stop(paste("graphics package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(paste("grDevices package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }

  validate_interpretation_set(x)

  # Definitions ---------------------------------------------------------------

  boundary_spacing <- 1

  number_boundaries <- length(x$boundary_names)
  number_regions    <- number_boundaries + 1

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
  boundaries <- boundaries[1 : length(boundaries) - 1]

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

  # Define width of the main background boxes.
  box_edge_left <- boundaries[1] - 1.5 * boundary_spacing
  box_edge_right <- boundaries[length(boundaries)] + 1.5 * boundary_spacing

  # Define width of the edge ragged boundary.
  edging_size <- 0.02 * (box_edge_right - box_edge_left)
  pic_edge_left <- box_edge_left - edging_size
  pic_edge_right <- box_edge_right + edging_size

  xlim <- c(pic_edge_left, pic_edge_right)
  ylim <- c(0, top)

  # Uses lapply and `[[` to extract the interpretation_short values.
  # (Used for calculating size of margin and for displaying these.)
  interpretation_labels <- lapply(x$interpretations, `[[`,
                                  "interpretation_short")

  # Margins that should be big enough for the labels.
  graphics::par(mar=c(max(nchar(x$boundary_names)) * 0.6, 1, 1,
            max(nchar(interpretation_labels)) * 0.6))

  graphics::plot.new()
  graphics::plot.window(xlim, ylim, xaxs = "r")

  # Draw the background boxes.
  graphics::rect(xleft = c(box_edge_left, boundaries),
       ybottom = 0,
       xright = c(boundaries, box_edge_right),
       ytop = top,
       col = grDevices::palette(),
       border = NA)

  # How many zigzags on each edge.
  edging_number <- 40

  # Polygons to plot the filled zigzags. Filled in same colours as edge
  # background boxes to extend them.
  poly_y <- c(seq(0, top, length.out = 1 + edging_number * 2), top, 0)
  poly_x <- c(pic_edge_left,
              rep(c(box_edge_left, pic_edge_left), edging_number),
              box_edge_left, box_edge_left)

  graphics::polygon(poly_x, poly_y, density = NULL, angle = 45,
          border = NA,
          col = grDevices::palette()[1], lty = graphics::par("lty"),
          fillOddEven = FALSE)


  poly_x <- c(pic_edge_right,
              rep(c(box_edge_right, pic_edge_right), edging_number),
              box_edge_right, box_edge_right)

  graphics::polygon(poly_x, poly_y, density = NULL, angle = 45,
          border = NA,
          col = grDevices::palette()[number_regions],
          lty = graphics::par("lty"),
          fillOddEven = FALSE)

  # Extra 'boundaries': a dotted line if anything is passed as extra_boundaries
  graphics::abline(v = extra_boundaries, lty="15151555")

  # Plot the options.
  graphics::boxplot.matrix(ci_perms[last : 1, ],
                 use.cols = FALSE,
                 horizontal = TRUE,
                 medlty = "blank", las = 2,
                 add = TRUE,
                 axes = FALSE)

  # Label the options.
  graphics::text(x = rowMeans(ci_perms),
       y = (last : 1),
       rownames(ci_perms))

  # Label the boundaries.
  graphics::Axis(side = 1, # 1=below
       at = boundaries,
       labels = x$boundary_names,
       las = 2,  # Label text perpendicular to axis
       lwd = 0)

  # Label the extra_boundaries, if at least one name exists.
  if(!is.null(names(extra_boundaries))) {
    graphics::Axis(side = 1,
         at = extra_boundaries,
         labels = names(extra_boundaries),
         las = 2,  # Label text perpendicular to axis
         lwd = 0)
  }

  # Label with the interpretations
  graphics::Axis(side = 4, # 4=right
       at = last : 1,
       labels = interpretation_labels,
       las = 2,  # Label text perpendicular to axis
       lwd = 0)  # Line width - 0 supresses line.

}

