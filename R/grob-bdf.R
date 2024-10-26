



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a \code{bdf_str} object into grid grob object
#'
#' @param bdf \code{bdf} font
#' @param text string
#' @param wrap use \code{strwrap()} to wrap text into lines? Default: TRUE
#' @param width number of characters to use when wrap = TRUE. default: 12
#' @param x,y,hjust,vjust,rot,default.units,name,gp,size,shrink other
#'
#' @return grid grob object
#' @examples
#' 1 + 1
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
    size          = 2,
    shrink        = 0.9,
    wrap          = FALSE,
    width         = 12,
    gp = gpar(
      col  = 'black',
      fill = 'black'
    )
) {
  
  stopifnot(inherits(bdf, 'bdf'))
  stopifnot(is.character(text))
  
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
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # strwrap() the string if asked
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (isTRUE(wrap)) {
    this_str <- strwrap(text, width = width)
    this_str <- paste(this_str, collapse = "\n")
  } else {
    this_str <- text
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a data.frame for the chars in this particular string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df          <- bdf_create_df(bdf, this_str)
  text_width  <- diff(range(df$x))
  text_height <- diff(range(df$y))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Justify the string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df$x <- df$x - text_width  * hjust - 0.5
  df$y <- df$y - text_height * vjust - 0.5
  
  
  vp <- grid::viewport(x = x, y = y, angle = rot)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Creae a grob for this string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  this_grob <- rectGrob(
    x              = unit(0.5, "npc") + grid::unit(size * df$x, 'points'),
    y              = unit(0.5, "npc") + grid::unit(size * df$y, 'points'),
    width          = grid::unit(size * shrink, 'points'),
    height         = grid::unit(size * shrink, 'points'),
    gp             = gp,
    vp             = vp,
    default.units  = default.units
  )
  
  
  this_grob
}






if (FALSE) {
  
  library(grid)
  bdf_file <- "./inst/fonts/spleen-8x16.bdf"
  myfont <- NULL
  myfont <- read_bdf(bdf_file)
  myfont
  
  text <- 'hello #*-=+;:"'
  y   <- unit(0.5, 'npc') + unit(c(0.1, 0.4), 'npc')
  grob <- bdfGrob(
    myfont, text,
    y = y,
    size  = 7,
    shrink = 0.7,
    # vp    = viewport(angle = 10),
    wrap  = TRUE,
    width = 15,
    # hjust = 0.5,
    # vjust = 1.5,
    rot  = 0,
    fill = rainbow(6)
  )
  grid.newpage(); grid.draw(grob)
  
  bdf_print_sample(myfont, text)
  
}

