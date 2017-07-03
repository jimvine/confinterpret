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

  # Check required packages ---------------------------------------------------
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

  # Limit the number of regions we will try to plot.
  # Picked 12 as this comes to 78 regions, which happens to be a multiple of
  # 26, so 3 complete alphabets for the labelling. Though a more realistic
  # number to plot is probably much smaller.
  if (number_regions > 12) {
    stop(paste("plot.interpretation_set only supports interpretation_set",
               "objects with up to 11 boundaries (12 regions)."))
  }

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

  lower_regions <- rep(1 : number_regions, number_regions : 1)
  upper_regions <- unlist(mapply(function(x) { x : number_regions },
                                 1 : number_regions))

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
  LETTERS78 <- c(LETTERS, paste0("A", LETTERS), paste0("B", LETTERS))

  rownames(ci_perms) <- LETTERS78[1 : nrow(ci_perms)]


  # Final setup ---------------------------------------------------------------

  names(boundaries) <- x$boundary_names

  # Uses lapply and `[[` to extract the interpretation_short values.
  # (Used for calculating size of margin and for displaying these.)
  interpretations <- lapply(x$interpretations, `[[`, "interpretation_short")


  # Actual plotting -----------------------------------------------------------

  plot_region_canvas(boundaries = boundaries,
                     extra_boundaries = extra_boundaries,
                     values = ci_perms,
                     interpretations = interpretations,
                     ...)


  # Plot the options.
  plot_intervals(intervals = ci_perms[nrow(ci_perms) : 1, ], ...)


  # Label the options.
  graphics::text(x = rowMeans(ci_perms),
                 y = (nrow(ci_perms) : 1),
                 rownames(ci_perms))


}




#' Plot a canvas backed with regions defined by a set of boundaries
#'
#' Produces a plot with all the background elements for plotting
#' interpretation_set objects and similar outputs.
#'
#' If using to plot interpretation_set objects as generic items, the
#' boundaries will typically be at arbitrary values selected for
#' visual clarity. In this case it will typically not make sense to
#' plot a numerical x axis. But boundaries can also be plotted as specific
#' values related to the intended interpretation, and x axis plotting is
#' normally appropriate in this case.
#'
#' The colours of the background regions are determined by
#' graphics::palette. Normally it will use the first n colours from the
#' palette, where n is the number of regions (which is the number of
#' boundaries + 1). If the left-most boundary is set to be at the edge of
#' the plot (by having no values lower than it and setting inner_margin[2]
#' and edge_margin[2] to 0), then the first colour in palette will be unused.
#' Similarly, if the right-most boundary is set to be the edge of the plot
#' then there will only be as many regions as boundaries, and elements 1:n-1
#' of the palette will be used. (And similarly, one fewer regions than
#' boundaries will be drawn if both the first and last boundaries are
#' the edges of the plot.)
#'
#' A pair of extra margins are defined for the purposes of this plot. Both
#' are technically drawn as part of the plotting area (i.e., not in the area
#' of the actual margin, which normally contains axes etc.).
#' Note that the order of edges used in these margins is the same as the
#' \code{graphics::par} parameters \code{mar} and \code{oma}, but the
#' scaling / units are not. These parameters are specified proportional to
#' the area of active plotting, rather than as lines.)
#'
#' @param boundaries
#' Named vector of numerical values of where boundaries should be drawn.
#'
#' @param extra_boundaries
#' Names optional.
#'
#' @param values
#' A matrix with either one or two columns containing the values of
#' point estimates (one column) or ranges (two columns). Row names
#' can specify labels.
#'
#' @param inner_margin
#' Numerical vector of the form \code{c(bottom, left, top, right)}, which
#' gives the amount of inner margin to be added, expressed as a proportion
#' of the plotted area. This is space designed to be past any plotted objects
#' but before the edging (defined separately via \code{edge_margin}).
#' See Details.
#'
#' @param edge_margin
#' Numerical vector of the form \code{c(bottom, left, top, right)}, which gives
#' the amount of 'edge margin' to be added, expressed as a proportion of the
#' plotted width. This is the space designed to be occupied by plot edges
#' (e.g. a gradient fading out). Currently only implemented for left and
#' right; top and bottom values are ignored. See Details.
#'
#' @param edge_type
#' What style of edge to draw at the sides of the plot. Currently supported
#' options are \code{"gradient"} (the default) and \code{"zigzag"}.
#'
#' @param boundary_label_pos
#' Where to put the boundary labels.
#' Options are \code{c("below", "above", "on top", "none")}.
#' If you are planning to plot values on the canvas and want the boundary
#' labels on top then you may want to choose "none" and make a call to
#' \code{label_ontop_boundaries()} after plotting values.
#'
#' @param boundary_values
#' A logical value indicating whether the values should be appended to
#' the boundaries' names.
#'
#' @param interpretation_label_pos
#' Options are \code{c("right", "left", "none")}
#'
#' @param interpretations
#' Character vector of interpretations to be used for labelling interpretations
#' or \code{NULL}. If provided, should be the same length as
#' \code{nrow(values)}.
#'
#' @param x_axis_pos
#' Location of a numerical x axis.
#' Options are \code{c("none", "below", "above")}.
#'
#' @param y_axis_pos
#' Location of a numerical y axis. Default "none" will almost always be right.
#' Options are c\code{("none", "left", "right")}.
#'
#' @param ...
#' Further parameters to be passed on.
#'
plot_region_canvas <- function(boundaries,
                               extra_boundaries = NULL,
                               values,
                               interpretations = NULL,
                               boundary_values = FALSE,
                               boundary_label_pos = "below",
                               interpretation_label_pos = "right",
                               x_axis_pos = "none",
                               y_axis_pos = "none",
                               inner_margin = c(0, 0.05, 0, 0.05),
                               edge_margin = c(0, 0.02, 0, 0.02),
                               edge_type = "gradient",
                               ...) {

  # Check required packages ---------------------------------------------------
  if (!requireNamespace("graphics", quietly = TRUE)) {
    stop(paste("graphics package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(paste("grDevices package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }

  # Definitions ---------------------------------------------------------------

  # If boundary_values == TRUE then append them into the relevant names.
  if(boundary_values) {
    names(boundaries) <-
      paste0(names(boundaries), "  ( ", boundaries, " )")
    if(!is.null(names(extra_boundaries))) {
      names(extra_boundaries) <-
        paste0(names(extra_boundaries), "  ( ", extra_boundaries, " )")
    }
  }

  # The maximum and minimum values to plot and distance between them.
  plot_min <- min(boundaries, values)
  plot_max <- max(boundaries, values)
  plot_width <- plot_max - plot_min

  # The extent of the boxes representing the regions (excluding any edge).
  regions_min <- plot_min - (plot_width * inner_margin[2])
  regions_max <- plot_max + (plot_width * inner_margin[4])

  # The extent of the edges.
  edge_min <- regions_min - (plot_width * edge_margin[2])
  edge_max <- regions_max + (plot_width * edge_margin[4])

  number_values <- nrow(values)
  plot_height <- number_values + 1
  top <- plot_height * (1 + inner_margin[3])
  bottom <- 0 - (plot_height * inner_margin[3])

  xlim <- c(edge_min, edge_max)
  ylim <- c(bottom, top)


  # Margins that should be big enough for the labels.
  # 0.5 all round to start, then add on 2 lines for an axis if on the side
  # and an amount for the longest labels if on the side.
  margin_above <- 0.5 +
    ifelse(x_axis_pos == "above", 2, 0) +
    ifelse(boundary_label_pos == "above",
           max(0, strwidthl(names(boundaries)),
               strwidthl(names(extra_boundaries))),
           0)

  margin_below <- 0.5 +
    ifelse(x_axis_pos == "below", 2, 0) +
    ifelse(boundary_label_pos == "below",
           max(0, strwidthl(names(boundaries)),
               strwidthl(names(extra_boundaries))),
           0)

  margin_left <- 0.5 +
    ifelse(y_axis_pos == "left", 2, 0) +
    ifelse(interpretation_label_pos == "left",
           max(0, strwidthl(interpretations)),
           0)

  margin_right <- 0.5 +
    ifelse(y_axis_pos == "right", 2, 0) +
    ifelse(interpretation_label_pos == "right",
           max(0, strwidthl(interpretations)),
           0)

  graphics::par(mar = c(margin_below, margin_left, margin_above, margin_right))

  graphics::plot.new()
  graphics::plot.window(xlim, ylim, xaxs = "r")


  # Draw the background boxes -------------------------------------------------
  graphics::rect(xleft = c(regions_min, boundaries),
                 ybottom = bottom,
                 xright = c(boundaries, regions_max),
                 ytop = top,
                 col = grDevices::palette(),
                 border = NA)


  # Draw the edges ------------------------------------------------------------
  if(edge_type == "gradient") {

    plot_edge_gradient(start_colour = grDevices::palette()[1],
                       end_colour = graphics::par("bg"),
                       # gradient_steps = 10,
                       xstart = regions_min,
                       xend = edge_min,
                       ybottom = bottom,
                       ytop = top,
                       ...)

    plot_edge_gradient(start_colour = grDevices::palette()[length(boundaries) + 1],
                       end_colour = graphics::par("bg"),
                       # gradient_steps = 10,
                       xstart = regions_max,
                       xend = edge_max,
                       ybottom = bottom,
                       ytop = top,
                       ...)

  }

  if(edge_type == "zigzag") {

    plot_edge_zigzag(colour = grDevices::palette()[1],
                     # zigzag_number = 40,
                     xstart = regions_min,
                     xend = edge_min,
                     ybottom = bottom,
                     ytop = top,
                     ...)

    plot_edge_zigzag(colour = grDevices::palette()[length(boundaries) + 1],
                     # zigzag_number = 40,
                     xstart = regions_max,
                     xend = edge_max,
                     ybottom = bottom,
                     ytop = top,
                     ...)

  }

  # Add any axes requested ----------------------------------------------------

  if(x_axis_pos == "below") {
    graphics::Axis(side = 1)
    graphics::grid(ny = NA)
  }

  if(x_axis_pos == "above") {
    graphics::Axis(side = 3)
    graphics::grid(ny = NA)
  }

  if(y_axis_pos == "left") {
    graphics::Axis(side = 2)
  }

  if(y_axis_pos == "right") {
    graphics::Axis(side = 4)
  }


  # Extra 'boundaries' --------------------------------------------------------

  # A dotted line if anything is passed as extra_boundaries
  graphics::abline(v = extra_boundaries, lty="15151555")


  # Add any labelling requested -----------------------------------------------

  # Label the boundaries.
  # And label the extra_boundaries, if at least one name exists.
  if(boundary_label_pos == "below" |
     boundary_label_pos == "above") {
    graphics::Axis(side = ifelse(boundary_label_pos == "below", 1, 3), # 1=below, 3=above
                   at = boundaries,
                   labels = names(boundaries),
                   las = 2,  # Label text perpendicular to axis
                   lwd = 0,  # Line width - 0 supresses line.
                   line = ifelse(boundary_label_pos == x_axis_pos, 2, 0))

    if(!is.null(names(extra_boundaries))) {
      graphics::Axis(side = ifelse(boundary_label_pos == "below", 1, 3),
                     at = extra_boundaries,
                     labels = names(extra_boundaries),
                     las = 2,  # Label text perpendicular to axis
                     lwd = 0,  # Line width - 0 supresses line.
                     line = ifelse(boundary_label_pos == x_axis_pos, 2, 0))
    }
  } else if(boundary_label_pos == "on top") {
    label_ontop_boundaries(boundaries, extra_boundaries)
  }

  # Label the interpretations
  if(interpretation_label_pos == "right" |
     interpretation_label_pos == "left") {

    graphics::Axis(side = ifelse(interpretation_label_pos == "right", 4, 2), # 4=right, 2=left
                   at = number_values : 1,
                   labels = interpretations,
                   las = 2,  # Label text perpendicular to axis
                   lwd = 0,  # Line width - 0 supresses line.
                   line = ifelse(interpretation_label_pos == y_axis_pos, 2, 0))

  }

}




# xstart, xend
# Specify the starting x coordinate and the ending one. xstart is where the
# start_colour is used. xend is where end_colour is used.
plot_edge_gradient <- function(start_colour,
                               end_colour,
                               gradient_steps = 10,
                               xstart, xend,
                               ybottom, ytop,
                               ...) {

  # Check required packages ---------------------------------------------------
  if (!requireNamespace("graphics", quietly = TRUE)) {
    stop(paste("graphics package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(paste("grDevices package needed for plot.interpretation_set to work.",
               "Please install it."), call. = FALSE)
  }


  # Colours -------------------------------------------------------------------

  shades <- grDevices::colorRampPalette(c(start_colour, end_colour),
                             space = "Lab")(gradient_steps)

  # colorRampPalette doesn't preserve alphas. Find it and add it on.
  alpha <- grDevices::col2rgb(start_colour, alpha = TRUE)[4,]
  shades <- grDevices::rgb(grDevices::col2rgb(shades)[1,] / 255,
                           grDevices::col2rgb(shades)[2,] / 255,
                           grDevices::col2rgb(shades)[3,] / 255,
                           alpha / 255)

  # Definitions ---------------------------------------------------------------

  shade_edges <- seq(from = xstart, to = xend, length.out = gradient_steps + 1)

  if(xstart > xend) {
    shades <- rev(shades)
    shade_edges <- rev(shade_edges)
  }

  # Drawing -------------------------------------------------------------------

  graphics::rect(xleft = shade_edges[1 : gradient_steps],
                 ybottom = ybottom,
                 xright = shade_edges[2 : (gradient_steps + 1)],
                 ytop = ytop,
                 col = shades,
                 border = NA)

}

plot_edge_zigzag <- function(colour,
                             zigzag_number = 40,
                             xstart, xend,
                             ybottom, ytop,
                             ...) {

  # Polygons to plot the filled zigzags. Filled in same colours as edge
  # background boxes to extend them.
  poly_y <- c(seq(ybottom, ytop, length.out = 1 + zigzag_number * 2),
              ytop, ybottom)
  poly_x <- c(xend, rep(c(xstart, xend), zigzag_number), xstart, xstart)

  graphics::polygon(poly_x, poly_y,
                    density = NULL,
                    angle = 45,
                    border = NA,
                    col = colour,
                    lty = graphics::par("lty"),
                    fillOddEven = FALSE)

}

#' Plot intervals
#'
#' Plot intervals on a canvas, typically prepared with
#' \code{plot_region_canvas()}.
#'
#' @param intervals
#' The interval(s) to be plotted. Two column matrix.
#'
#' @param estimates
#' Estimates for each of the intervals (optional).
#'
#' @param interval_type
#' Set the way the interval is presented. Current options are
#' \code{c("norm", "unif")} for a normal distribution-based curve
#' and a box, respectively.
#'
#' @param ...
#' Further parameters to be passed on.
#'
plot_intervals <- function(intervals,
                           estimates = NULL,
                           interval_type = "norm",
                           ...) {

  if(interval_type == "norm") {
    plot_intervals_norm(intervals = intervals, ...)
  } else if(interval_type == "unif") {
    plot_intervals_unif(intervals = intervals, ...)
  }


}

#' Plot intervals as curved (normal distribution) areas
#'
#' @param y_scale
#' How tall the interval plots are to be drawn
#'
#' @param interval_value_labels
#' Logical value specifying whether interval value labels are to be added.
#'
#' @param estimate_value_labels
#' Logical value specifying whether estimate value labels are to be added.
#'
#' @param interval_labels_offset
#' Amount to offset interval labels by from the centre of the end
#' of the interval's plot. \code{c(x1, x2, y1, y2)}.
#'
#' @param estimate_labels_offset
#' Amount to offset estimate labels by. \code{c(x, y)}.
#'
#' @inheritParams plot_intervals
plot_intervals_norm <- function(intervals,
                               estimates = NULL,
                               y_scale = 1,
                               interval_value_labels = FALSE,
                               estimate_value_labels = FALSE,
                               interval_labels_offset = c(0, 0, 0.2, 0.2),
                               estimate_labels_offset = c(0, 0.6),
                               ...) {

  # Check required packages ---------------------------------------------------
  if (!requireNamespace("graphics", quietly = TRUE)) {
    stop(paste("graphics package needed for plot_intervals_norm to work.",
               "Please install it."), call. = FALSE)
  }

  if (!requireNamespace("grDevices", quietly = TRUE)) {
    stop(paste("grDevices package needed for plot_intervals_norm to work.",
               "Please install it."), call. = FALSE)
  }

  if (!requireNamespace("stats", quietly = TRUE)) {
    stop(paste("stats package needed for plot_intervals_norm to work.",
               "Please install it."), call. = FALSE)
  }

  # Etc ----------------------------------------------------------------------

  ci_x_band <- mapply(seq, intervals[, 1], intervals[, 2], length.out = 100)
  sd <- (intervals[, 2] - intervals[, 1]) / 4
  if(is.null(estimates)) {
    estimates <- (intervals[, 1] + intervals[, 2]) / 2
  }

  for(i in 1 : nrow(intervals)) {

    y_mid <- i


    # Plot a polygon representing the CI as a normal distribution

    # Simple filled version
    # graphics::polygon(x = c(ci_x_band[, i], rev(ci_x_band[, i])),
    #                   y = c(y_mid + (dnorm(ci_x_band[, i], mean = estimate[i], sd[i]) * sd[i] * y_scale),
    #                         y_mid - rev(dnorm(ci_x_band[, i], mean = estimate[i], sd[i]) * sd[i] * y_scale)),
    #                   col = "#FFFFFFCC")


    n_curve <- function(x) {
      # Maximum value of normal distribution (where x=mean) is
      #     1 / (sqrt(2 * pi) * sd)
      # By multiplying by sd we get maximum of:
      #     1 / (2 * pi)
      # Which is about 0.4.
      stats::dnorm(x, mean = estimates[i], sd[i]) * sd[i] * y_scale
    }

    # graphics::polygon(x = c(ci_x_band[, i], rev(ci_x_band[, i])),
    #                   y = c(y_mid + (dnorm(ci_x_band[, i], mean = estimate[i], sd[i]) * sd[i] * y_scale),
    #                         y_mid - rev(dnorm(ci_x_band[, i], mean = estimate[i], sd[i]) * sd[i] * y_scale)),
    #                   col = NA)


    min_end <- (ci_x_band[1, i] - estimates[i]) * 1.2
    max_end <- (ci_x_band[length(ci_x_band[, i]), i] - estimates[i]) * 1.2

    for(j in 1 : (length(ci_x_band[, i]) - 1)) {

      graphics::polygon(x = c(ci_x_band[j,      i],
                              ci_x_band[j,      i],
                              ci_x_band[j + 1L, i],
                              ci_x_band[j + 1L, i]),
                        y = c(y_mid - n_curve(ci_x_band[j,      i]),
                              y_mid + n_curve(ci_x_band[j,      i]),
                              y_mid + n_curve(ci_x_band[j + 1L, i]),
                              y_mid - n_curve(ci_x_band[j + 1L, i])),
                        col = grDevices::rgb(1, 1, 1, 1 -
                                               max((ci_x_band[j, i] - estimates[i]) / min_end,
                                          (ci_x_band[j, i] - estimates[i]) / max_end)),
                        border = NA)
    }

    graphics::polygon(x = c(ci_x_band[, i], rev(ci_x_band[, i])),
                      y = c(y_mid + n_curve(ci_x_band[, i]),
                            y_mid - rev( n_curve(ci_x_band[, i]))),
                      col = NA)

    # Label the interval bounds
    if(interval_value_labels) {
      label_interval_values(intervals, interval_labels_offset, ...)
    }

    # Label the estimates
    if(estimate_value_labels) {
      label_estimate_values(estimates, estimate_labels_offset, ...)
    }
  }
}

#' Plot intervals as uniform (box) areas
#'
#' @inheritParams plot_intervals_norm
#'
plot_intervals_unif <- function(intervals,
                                estimates = NULL,
                                interval_value_labels = FALSE,
                                estimate_value_labels = FALSE,
                                interval_labels_offset = c(-0.1, 0.1, 0, 0),
                                estimate_labels_offset = c(0, 0.6),
                                ...) {

  if(is.null(estimates)) {
    estimates <- (intervals[, 1] + intervals[, 2]) / 2
  }

  graphics::boxplot.matrix(intervals,
                           use.cols = FALSE,
                           horizontal = TRUE,
                           medlty = "blank",
                           las = 2,
                           add = TRUE,
                           axes = FALSE)

  # Label the interval bounds
  if(interval_value_labels) {
    label_interval_values(intervals, interval_labels_offset, ...)
  }

  # Label the estimates
  if(estimate_value_labels) {
    label_estimate_values(estimates, estimate_labels_offset, ...)
  }
}

label_interval_values <- function(intervals,
                                  interval_labels_offset = c(0, 0, 0, 0),
                                  interval_labels_rounding = 3,
                                  ...) {
  graphics::text(x = cbind(intervals[, 1] + interval_labels_offset[1],
                           intervals[, 2] + interval_labels_offset[2]),
                 y = c((1 : nrow(intervals)) + interval_labels_offset[3],
                       (1 : nrow(intervals)) + interval_labels_offset[4]),
                 round(intervals, interval_labels_rounding))
}


label_estimate_values <- function(estimates,
                                  estimate_labels_offset = c(0, 0),
                                  estimate_labels_rounding = 3,
                                  ...) {
  graphics::text(x = estimates + estimate_labels_offset[1],
                 y = (1 : length(estimates)) + estimate_labels_offset[2],
                 round(estimates, estimate_labels_rounding))
}



#' Label the boundaries on top of the plot.
#'
#' If plotting values or ranges may want to call this directly last
#' to ensure it is on top, and specify no labels in the canvas plotting call.
#'
#' @inheritParams plot_region_canvas
#'
label_ontop_boundaries <- function(boundaries, extra_boundaries = NULL) {

  y_mid <- (graphics::par("usr")[4] + graphics::par("usr")[3]) / 2
  graphics::text(x = boundaries,
                 y = y_mid,
                 labels = names(boundaries),
                 srt = 90)

  if(!is.null(names(extra_boundaries))) {
    graphics::text(x = extra_boundaries,
                   y = y_mid,
                   labels = names(extra_boundaries),
                   srt = 90)
  }
}

#' Obtain string widths in (approximate) multiple of lines.
#'
#' @param s
#' A character vector whose width is to be determined.
strwidthl <- function(s) {
  graphics::strwidth(s, units = "inches") /
    graphics::strheight("M", unit = "inches")
}
