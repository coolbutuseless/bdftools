

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Simple wrapper for \code{grid::gpar()} which exposes the valid arguments
#'
#' The \code{grid::gpar()} does not have any named arguments, and when used
#' in a modern IDE, it is difficult to quicklyl know which graphical parameters
#' are available.
#'
#' This function (\code{ingrid::gp()}) is a thin wrapper around \code{grid::gpar()}
#' which exposes all the parameters as named arguments to make autocomplete
#' a bit more useful.
#'
#' @param col Colour for lines and borders.
#' @param fill Colour for filling rectangles, polygons, ...
#' @param alpha Alpha channel for transparency
#' @param lty Line type
#' @param lwd Line width
#' @param lex Multiplier applied to line width
#' @param lineend Line end style ('round', 'butt', 'square')
#' @param linejoin Line join style ('round', 'mitre', 'bevel')
#' @param linemitre Line mitre limit (number greater than 1)
#' @param fontsize The size of text (in points)
#' @param cex Multiplier applied to fontsize
#' @param fontfamily The font family
#' @param fontface The font face ('bold', 'italic', ...)
#' @param lineheight The height of a line as a multiple of the size of text
#' @param ... other arguments ignored
#'
#' @examples
#' \dontrun{
#' gp(fill = 'red')
#' }
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gpar2 <- function(col, fill, alpha, lty, lwd, lex, lineend, linejoin, linemitre,
               fontsize, cex, fontfamily, fontface, lineheight, ...) {

  args <- find_args()
  do.call(grid::gpar, args)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Expand a single gpar into multiple gpars
#'
#' This is needed to proprly give a gpar-per-string, otheriwse it'll end
#' up being a gpar-per-rect which is not what I want
#'
#' @param gp_ref the reference gpar
#' @param N number of child gpars to generate
#'
#' @noRd
#'
#' @importFrom stats setNames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expand_gpar <- function(gp_ref, N) {

  null_seq <- rep(list(NULL), N)

  if (is.null(gp_ref)) {
    res <- null_seq
    return(res)
  }

  gp_arg_names <- c(
    "col", "fill", "alpha", "lty", "lwd", "lex", "lineend", "linejoin",
    "linemitre", "fontsize", "cex", "fontfamily", "fontface", "lineheight"
  )
  gp_arg_names <- setNames(gp_arg_names, gp_arg_names)

  gp_args <- lapply(gp_arg_names, function(arg_name) {
    if (is.null(gp_ref[[arg_name]])) null_seq else rep_len(gp_ref[[arg_name]], N)
  })

  gpars <- vector('list', N)

  for (i in seq(N)) {
    this_args <- lapply(gp_args, function(x) x[[i]])
    this_args <- Filter(Negate(is.null), this_args)
    gpars[[i]] <- do.call(gpar, this_args)
  }

  gpars
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a \code{bdf_str} object into grid grob object
#'
#' @param bdf \code{bdf} font
#' @param text string
#' @param wrap use \code{strwrap()} to wrap text into lines? Default: TRUE
#' @param width number of characters to use when wrap = TRUE. default: 12
#' @param x,y,hjust,vjust,rot,default.units,name,gp,size,shrink other
#' @param col,fill,alpha,lty,lwd,lex,lineend,linejoin,linemitre,fontsize,cex,fontfamily,fontface,lineheight
#'        See documentation for \code{grid::gpar()}
#'
#' @import grid
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bdfGrob <- function(
  bdf, text,
  x             = unit(0.5, 'npc'),
  y             = unit(0.5, 'npc'),
  hjust         = NULL,
  vjust         = NULL,
  rot           = 0,  # polygrob? or just roate vp?
  default.units = 'points',
  name          = NULL,
  gp            = NULL,
  size          = 2,
  shrink        = 0.9,
  wrap          = FALSE,
  width         = 12,
  col           = 'black',
  fill          = 'black',
  alpha, lty, lwd, lex, lineend, linejoin, linemitre, fontsize, cex, fontfamily, fontface, lineheight
) {

  stopifnot(inherits(bdf, 'bdf'))
  stopifnot(is.character(text))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a gpar() from the inline arguments if given gp arg is not NULL
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(gp)) {
    gp_args <- find_args()
    gp      <- do.call(gpar2, gp_args)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the gpar, so that there's 1 gpar per string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  N  <- length(text)

  # If tehre's more than 1 string, then expand the 'gp' to cover them all.
  # If it's just a single string, then create a list of length 1 which
  # contains that gp.
  if (N > 1) {
    gp <- expand_gpar(gp, N)
  } else {
    gp <- list(gp)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Justifcation defaults to centre of device
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hjust <- hjust %||% 0.5
  vjust <- vjust %||% 0.5

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure (x, y) coords are actual units
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.unit(x))  x <- unit(x, default.units)
  if (!is.unit(y))  y <- unit(y, default.units)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replicate all the arguments as necessary to make a new grob of rectables
  # for each word
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  x      <- rep_len(x     , N)
  y      <- rep_len(y     , N)
  hjust  <- rep_len(hjust , N)
  vjust  <- rep_len(vjust , N)
  rot    <- rep_len(rot   , N)
  wrap   <- rep_len(wrap  , N)
  width  <- rep_len(width , N)
  size   <- rep_len(size  , N)
  shrink <- rep_len(shrink, N)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pre-allocated space for the output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grobs <- vector('list', length(text))

  for (i in seq_along(text)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # strwrap() the string if asked
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    this_str <- text[i]
    if (isTRUE(wrap[i])) {
      this_str <- strwrap(this_str, width = width[i])
      this_str <- paste(this_str, collapse = "\n")
    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Create a data.frame for the chars in this particular string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    df   <- bdf_create_df(bdf, this_str)
    text_width  <- diff(range(df$x))
    text_height <- diff(range(df$y))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Justify the string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    df$x <- df$x - text_width  * hjust[i] - 0.5
    df$y <- df$y - text_height * vjust[i] - 0.5


    vp <- grid::viewport(x = x[i], y = y[i], angle = rot[i])

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Creae a grob for this string
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    grobs[[i]] <- rectGrob(
      x              = unit(0.5, "npc") + grid::unit(size[i] * df$x, 'points'),
      y              = unit(0.5, "npc") + grid::unit(size[i] * df$y, 'points'),
      width          = grid::unit(size[i] * shrink[i], 'points'),
      height         = grid::unit(size[i] * shrink[i], 'points'),
      gp             = gp[[i]],
      vp             = vp,
      default.units  = default.units
    )
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return a grob tree containing a set of rectGrobs (one for each string)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  do.call(grobTree, grobs)
}






if (FALSE) {

  library(grid)
  bdf_file <- "./working/fonts/spleen-8x16.bdf"
  bdf_file <- "./working/fonts/tom-thumb.bdf"
  bdf_file <- "./working/fonts/creep2-11.bdf"
  bdf_file <- "./working/fonts/spleen-5x8.bdf"
  bdf_file <- "./working/fonts/spleen-12x24.bdf"
  myfont <- NULL
  myfont <- read_bdf(bdf_file)
  myfont

  text <- 'hello #*-=+;:"'
  # text <- "You should render them recursively, r made up of little r"
  text <- c('hello', 'there')
  y   <- unit(0.5, 'npc') + unit(c(0.1, 0.4), 'npc')
  grob <- bdfGrob(
    myfont, text,
    y = y,
    size  = c(4, 8),
    shrink = c(0.7, 1.2),
    # vp    = viewport(angle = 10),
    wrap  = TRUE,
    width = 15,
    # hjust = 0.5,
    # vjust = 1.5,
    rot  = 90,
    fill = rainbow(6)
  )
  grid.newpage(); grid.draw(grob)

  bdf_print_sample(myfont, text)

}

