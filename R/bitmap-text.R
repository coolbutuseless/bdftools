
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Font information
#' 
#' Currently on includes font names and unicode codepoints available within
#' each font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"font_info"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Bitmap font names
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_fonts <- c(
  "unifont",
  "cozette", 
  "creep2-11", 
  "spleen-5x8", "spleen-6x12", "spleen-8x16", "spleen-12x24", "spleen-16x32", "spleen-32x64"
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame of the given string and font
#'
#' @param font bdf font name
#' @param text text
#' @param line_height height
#'
#' @return data.frame of x,y coordinates
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_coords <- function(text, font = "unifont", line_height = NULL) {
  
  if (!font %in% names(bdfs)) {
    stop("No such bdf font: ", font)
  }
  bdf <- bdfs[[font]]
  
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
    if (is.null(idx) || is.na(idx) || idx > length(bdf$chars)) {
      code <- bdf$font_info$default_char %||% 63 # Default to question mark
      idx <- bdf$idx[code + 1L]
    }
    
    # cat("idx / len = ", idx, length(bdf$chars), idx > length(bdf$chars), "\n")
    # cat("def: ", bdf$font_info$default_char, "\n")

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Get this character
    # Offset the x,y coords based upon the position of the character
    # add the coords data.frame to the list of all data.frames
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    this_bdf_char <- bdf$chars[[idx]]
    this_df      <- this_bdf_char$coords
    this_df$x0   <- this_df$x
    this_df$y0   <- this_df$y
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
#' @inheritParams bitmap_text_coords
#' @param scale Integer size scale factor. Default: 1.  Must be an integer value >= 1.
#'        Scale up the matrix or raster result by this factor
#'
#' @return matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_matrix <- function(text, font = "unifont", scale = 1, line_height = NULL) {
  
  scale <- as.integer(scale)
  stopifnot(scale >= 1)
  
  df <- bitmap_text_coords(text, font, line_height = line_height)
  mat <- coords_to_mat(df)  
  
  if (scale > 1) {
    mat <- kronecker(mat, matrix(1L, scale, scale))
  }
  
  mat
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a raster
#' 
#' @inheritParams bitmap_text_coords
#' 
#' @return raster
#' @importFrom grDevices as.raster
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_text_raster <- function(text, font = "unifont", scale = 1, line_height = NULL) {
  mat <- bitmap_text_matrix(text = text, font = font, scale = scale, line_height = line_height)
  mat <- 1L - mat
  grDevices::as.raster(mat)
}








