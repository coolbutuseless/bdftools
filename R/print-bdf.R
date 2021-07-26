


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' S3 method for printing \code{bdf} font objects
#'
#' @param x bdf font object as returned by \code{read_bdf()}
#' @param text Example string to output to console in this font. Default: "Handgloves"
#' @param zero,one characters to use to represent zero and one
#' @param ... ignored
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.bdf <- function(x, text = 'Handgloves', zero = ' ', one = '#', ...) {
  # print(x$font_info)
  info <- x$font_info

  info$bitmap <- paste(info$bitmap, collapse=",")

  nn <- names(info)
  res <- paste(nn, unlist(info), sep="=", collapse = ", ")
  cat(res, "\n\n")

  bdf_print_sample(x, text, zero = zero, one = one)

  invisible(x)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a binary matrix to the terminal
#'
#' @param mat matrix of 0/1 values
#' @param zero,one characters to use to represent zero and one
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print_binary_matrix <- function(mat, trim = TRUE, zero = ' ', one = '#') {

  stopifnot(nchar(zero) == nchar(one))

  width <- getOption('width', 80L) - 1L
  if (isTRUE(trim) && ncol(mat) > width) {
    mat <- mat[,seq(width),drop=FALSE]
  }

  mat[mat == 0] <- zero
  mat[mat == 1] <- one
  res <- apply(mat, 1, paste, collapse="")
  cat(res, sep = "\n")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print sample text to screen
#'
#' @param bdf \code{bdf} font object as returned by \code{read_bdf()}
#' @param text string
#' @param wrap wrap the text into separate lines using \code{base::strwrap()}.
#'        Default: TRUE.   Note that \code{strwrap()} will only break lines
#'        at whitespace, and resulting text may still overflow terminal width.
#'        See \code{trim} argument.
#' @param trim trim the text at the consolve width. default: TRUE
#' @param width desired output width for output. Default: NULL means to
#'        use the current console width
#' @param line_height This value is used for vertical spacing between lines of text when
#'        the text is wrapped.  If \code{line_height} is NULL, then use the pixel size
#'        speficied in the font.  You may wish to set a value here if you to
#'        ensure ascenders/descenders do not overlap on subsequent lines, or just
#'        to space out the text a bit more.
#' @param zero,one characters to use to represent zero and one
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_print_sample <- function(bdf, text, width = NULL, wrap = TRUE, trim = TRUE,
                             line_height = NULL, zero = ' ', one = '#') {

  stopifnot(inherits(bdf, 'bdf'))

  if (isTRUE(wrap)) {
    width          <- (width %||% getOption('width', 80L)) - 1L
    char_width     <- bdf$font_info$bbox[1]
    chars_per_line <- floor(width/char_width)

    text <- strwrap(text, chars_per_line)
    text <- paste(text, collapse="\n")
  }

  mat <- bdf_create_mat(bdf, text, line_height = line_height)
  print_binary_matrix(mat, trim = trim, zero = zero, one = one)

  invisible(bdf)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' View a single \code{bdf_char}
#'
#' @param bdf_char single \code{bdf_char}
#' @param zero,one characters to use to represent zero and one
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_print_char <- function(bdf_char, zero = ' ', one = '#') {
  stopifnot(inherits(bdf_char, 'bdf_char'))
  mat <- coords_df_to_mat(bdf_char$coords)
  print_binary_matrix(mat, zero = zero, one = one)

  invisible(bdf_char)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get a list of \code{bdf_char} objects from a \code{bdf} font
#'
#' @inheritParams bdf_print_sample
#'
#' @return list of \code{bdf_char} objects. A space is used for any
#'         unknown characters, or the default character from the font if one is
#'         specified in the font header.
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_extract_chars <- function(bdf, text) {
  codes <- utf8ToInt(text)
  idx   <- bdf$idx[codes + 1L]
  idx[is.na(idx)] <- bdf$idx[bdf$font_info$default_char + 1L]

  bdf$chars[idx]
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a data.frame of the given string and font
#'
#' @inheritParams bdf_print_sample
#'
#' @return data.frame of x,y coordinates
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_create_df <- function(bdf, text, line_height = NULL) {
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
coords_df_to_mat <- function(df) {
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
#' @inheritParams bdf_print_sample
#' @inheritParams bdf_create_df
#'
#' @return matrix
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdf_create_mat <- function(bdf, text, line_height = NULL) {

  df <- bdf_create_df(bdf, text, line_height = line_height)

  coords_df_to_mat(df)
}







