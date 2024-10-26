

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method for printing \code{bdf} font objects
#'
#' @param x bdf font object as returned by \code{read_bdf()}
#' @param ... ignored
#'
#' @return None
#' @examples
#' # Print output from a font to the terminal
#' 1 + 1
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.bdf <- function(x, ...) {
  print(x$font_info)
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame of the given string and font
#'
#' @param bdf font
#' @param text text
#' @param line_height height
#'
#' @return data.frame of x,y coordinates
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_coords <- function(text, bdf, line_height = NULL) {
  codes <- utf8ToInt(text)
  dfs   <- vector('list', length(codes))

  line_height <- line_height %||% bdf$font_info$line_height
  yoffset     <- 0
  xoffset     <- 0

  for (i in seq_along(codes)) {
    code <- codes[i]

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If it's a carriage return (i.e. \n) then recalculate the offsets
    # and go to the next character
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (code == 10) {
      xoffset <- 0
      yoffset <- yoffset - line_height
      next
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get the character data for the given utf8 code
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    idx <- bdf$idx[code + 1L]
    if (is.null(idx) || is.na(idx)) {
      idx <- bdf$font_info$default_char
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get this character
    # Offset the x,y coords based upon the position of the character
    # add the coords data.frame to the list of all data.frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    this_bdf_char <- bdf$chars[[idx]]
    this_df      <- this_bdf_char$coords
    # If this is a 'space' then often this will have no coords
    if (!is.null(this_df) && nrow(this_df) > 0) {
      this_df$x    <- this_df$x + xoffset
      this_df$y    <- this_df$y + yoffset
      this_df$idx  <- i
      dfs[[i]]     <- this_df
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Even if a character has no coordinates (e.g. 'space') always add the
    # character dwidth to the xoffset for positioning the next character
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    xoffset <- xoffset + this_bdf_char$dwidth

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Bind all the data.frames of coordinates together.
  # Shift the entire set of coordinates "up" so that it nominally starts
  # at y = 0
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res_df   <- do.call(rbind, dfs)
  res_df$y <- res_df$y - yoffset

  class(res_df) <- c('tbl_df', 'tbl', 'data.frame')
  res_df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a data.frame of (x,y) coords into a matrix
#'
#' @param df data.frame with x and y coords
#'
#' @return matrix to hold the coords
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
coords_to_mat <- function(df) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # y coords can sometimes be negative because of descenders/offsets
  # so push them all to be at least "1", so that (x,y) coords can be used
  # as matrix indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (any(df$y < 1)) {
    df$y <- df$y + abs(min(df$y)) + 1L
  }
  if (any(df$x < 1)) {
    df$x <- df$x + abs(min(df$x)) + 1L
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a matrix of the appropriate size
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  height <- max(df$y)
  width  <- max(df$x)
  mat    <- matrix(0L, height, width)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set all the pixel locations to 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[ (df$x - 1) * height + df$y ] <- 1L

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pixel coordinates have (x, y) on the bottom left, but matrices have
  # origin on the top-left, so invert the matrix in the 'y' direction so
  # that it comes out the right way up
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  mat[rev(seq(nrow(mat))), , drop = FALSE]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a matrix of the given string and font
#'
#' @inheritParams create_coords
#'
#' @return matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_matrix <- function(text, bdf, line_height = NULL) {
  df <- create_coords(text, bdf, line_height = line_height)
  1L - coords_to_mat(df)  # invert black/white
}







