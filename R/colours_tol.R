
#' @importFrom grDevices rgb
colours_tol_sunset <- function(n) {

  # Translating P Tol's IDL code into R from
  #   https://personal.sron.nl/~pault/sunset_colors.pro

  xarr <- matrix(c(11, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                   11, 5, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                   11, 8, 5, NA, NA, NA, NA, NA, NA, NA, NA,
                   14, 10, 6, 2, NA, NA, NA, NA, NA, NA, NA,
                   14, 10, 8, 6, 2, NA, NA, NA, NA, NA, NA,
                   13, 11, 9, 7, 5, 3, NA, NA, NA, NA, NA,
                   13, 11, 9, 8, 7, 5, 3, NA, NA, NA, NA,
                   13, 12, 10, 9, 7, 6, 4, 3, NA, NA, NA,
                   13, 12, 10, 9, 8, 7, 6, 4, 3, NA, NA,
                   15, 13, 12, 10, 9, 7, 6, 4, 3, 1, NA,
                   15, 13, 12, 10, 9, 8, 7, 6, 4, 3, 1),
                 nrow=11, byrow=TRUE)

  x <- xarr[n, 1:n]

  sunsetred   <- c(174, 208, 210, 237, 245, 249, 255, 255, 230, 180, 153, 119,  58,   0,  61)
  sunsetgreen <- c( 28,  50,  77, 135, 162, 189, 227, 250, 245, 221, 199, 183, 137, 139,  82)
  sunsetblue  <- c( 62,  50,  62,  94, 117, 126, 170, 210, 254, 247, 236, 229, 201, 206, 161)

  red   <- sunsetred[x]
  green <- sunsetgreen[x]
  blue  <- sunsetblue[x]

  colour_set <- rgb(red, green, blue, maxColorValue = 255)

}

