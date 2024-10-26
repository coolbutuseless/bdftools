
globalVariables(c('idx', 'x', 'xoffset', 'stroke'))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given line of text.
#'
#' @inheritParams vector_text_coords
#'
#' @return data.frame with coordinates for all the glyphs with characters offset
#'        appropriately.  \code{char_idx} is the index of the character within
#'        the given text string
#'
#' @importFrom utils head
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_coords_single_row <- function(text, font, dx = 0) {

  stopifnot(length(text) == 1)

  if (nchar(text) == 0) {
    return(data.frame())
  }


  font_df <- switch(
    font,
    gridfont        = gridfont,
    gridfont_smooth = gridfont_smooth,
    arcade          = arcade,
    stop("No such font: ", font)
  )
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # split text into characters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (font == 'arcade') {
    text <- toupper(text)
  } else {
    text <- tolower(text)
  }
  text <- strsplit(text, '')[[1]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replace any unknown chars with a blank space
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bad_idx <- !(text %in% font_df$char)
  text[bad_idx] <- ' '

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Merge the text info with the data.frame for each character from `arcade_df`
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- data.frame(
    char     = text,
    char_idx = seq_along(text),
    stringsAsFactors = FALSE
  )

  string_df <- merge(string_df, font_df, sort = FALSE, all.x = TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke, idx),])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate the character offset
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  width_df <- subset(string_df, stroke == 1 & idx == 1)
  width_df <- width_df[, c('char_idx', 'width')]
  width_df$xoffset <- c(0, cumsum(head(width_df$width, -1) + dx))
  width_df$width <- NULL

  string_df <- merge(string_df, width_df, sort = FALSE, all.x = TRUE)
  string_df <- transform(string_df, x = x + xoffset)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure correct ordering
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  string_df <- with(string_df, string_df[order(char_idx, stroke, idx),])


  string_df
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create data.frame of glyph information for the given text.
#'
#' Text input can contain multiple lines separated by carriage returns
#'
#' @param text single character string
#' @param dx character spacing in original units
#' @param dy character spacing in original units
#' @param font choose font: 'original' or 'smooth'. default: original
#'
#' @return data.frame with coordinates for all the glyphs with characters offset
#'        appropriately.  \code{char_idx} is the index of the character within
#'        the given text string
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_coords <- function(text, font = c('gridfont', 'gridfont_smooth', 'arcade'), dx = 0, dy = 0) {

  stopifnot(length(text) == 1)
  font <- match.arg(font)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the text at "\n" boundaries
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  texts  <- strsplit(text, "\n")[[1]]
  nchars <- cumsum(nchar(texts))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a string for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dfs <- lapply(texts, vector_text_coords_single_row, font = font, dx = dx)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Update line numbering and character indices
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(dfs)) {
    if (nrow(dfs[[i]]) == 0) next
    dfs[[i]]$line     <- i
    if (i > 1) {
      dfs[[i]]$char_idx <- dfs[[i]]$char_idx + nchars[i-1]
    }
  }

  font_df <- switch(
    font,
    gridfont        = gridfont,
    gridfont_smooth = gridfont_smooth,
    arcade          = arcade,
    stop("No such font: ", font)
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combined all data.frames for each line, offset the y for each line
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- do.call(rbind, dfs)
  res$y <- res$y - (res$line - 1) * (font_df$height[1] + dy)

  # Reposition so that bottom of text is (1, 1)
  res$y <- res$y - min(res$y, na.rm = TRUE)
  
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw a line on a matrix with bresenham
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
line <- function(mat, x1, y1,  x2,  y2) {
  
  xdelta <- abs(x2 - x1)
  ydelta <- abs(y2 - y1)
  
  if (xdelta > ydelta) {
    x <- x1:x2
    y <- seq(y1, y2, length.out = length(x))
  } else {
    y <- y1:y2
    x <- seq(x1, x2, length.out = length(y))
  }

  x <- as.integer(round(x))
  y <- as.integer(round(y))
  y <- nrow(mat) - y + 1L
  mat[cbind(y, x)] <- 1L

  mat
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a binary matrix rendering of the text 
#' 
#' @param text string
#' @param font fontname
#' @param scale scale factor for text rendering
#' @param dx extra spacing
#' @param dy extra spacing
#' @return raster image
#' @examples
#' vector_text_matrix("Hello", font = "gridfont", scale = 1)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_matrix <- function(text, font, scale = 10, dx = NULL, dy = NULL) {
  
  if (is.null(dx) && scale < 2) {
    dx <- 1L
  }
  if (is.null(dy) && scale < 2) {
    dy <- 1L
  }
  
  dx <- dx %||% 0
  dy <- dy %||% 0
  
  
  df <- vector_text_coords(text = text, font = font, dx = dx, dy = dy)
  
  df$x <- df$x * scale + 1L
  df$y <- df$y * scale + 1L
  
  width  <- max(df$x)
  height <- max(df$y)
  
  mat <- matrix(0L, nrow = height, ncol = width)
  
  df$j <- with(df, interaction(char_idx, stroke, drop = TRUE))
  strokes <- split(df, df$j)
  
  for (stroke in strokes) {
    for (i in seq_len(nrow(stroke) - 1)) {
      mat <- line(mat, stroke$x[i], stroke$y[i], stroke$x[i + 1L], stroke$y[i + 1L])
    }
  }
  
  
  mat
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create a raster rendering of the font
#' 
#' @inheritParams vector_text_matrix
#' @return raster image
#' @examples
#' ras <- vector_text_raster("Hello", font = "gridfont", scale = 15)
#' plot(ras, interpolate = FALSE)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
vector_text_raster <- function(text, font, scale = 10, dx = NULL, dy = NULL) {
  
  mat <- vector_text_matrix(text = text, font = font, scale = scale, dx = dx, dy = dy)
  as.raster(1L - mat)
}




if (FALSE) {
  library(ggplot2)
  plot_df <- vector_text_coords('Country Road\nTake me Home', font='gridfont_smooth')

  ggplot(plot_df, aes(x, y)) +
    geom_path(aes(group = interaction(char_idx, stroke)), na.rm=TRUE) +
    coord_equal() +
    theme_void()
}


if (FALSE) {
  
  df <- vector_text_coords('Hello', font='gridfont', dx = 1)
  df  
  with(df, plot(x, y))
  
  vector_text_raster("Hello", font = "gridfont", dx = 0, scale = 30) |> plot(interpolate = FALSE)
  
  mat <- matrix(0L, nrow = 12, ncol = 8)
  plot(as.raster(mat), interpolate = FALSE)
  
  x1 <- 1
  y1 <- 1
  x2 <- 8
  y2 <- 12
  mat <- line(mat, x1, y1, x2, y2)
  plot(as.raster(mat), interpolate = FALSE)
  
  
}













