

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read a BDF file
#'
#' @param filename path to BDF file
#'
#' @return list containing
#' \itemize{
#' \item{font_info - named list of global information about the font}
#' \item{font_info_raw - original representation of global information about the font}
#' \item{chars - list of \code{bdf_char} objects}
#' \item{idx - an index which maps from utf8 codepoint to position in \code{chars}}
#' }
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_bdf <- function(filename) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Read in the raw bdf text
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  txt <- trimws(readLines(filename))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Ensure it starts/ends with "STARTFONT", "ENDFONT", and then remove
  # "ENDFONT" to make some other processing easier.
  # In some weird fonts (like c64.bdf), there is a COMMENT before STARTFONT.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  txt0 <- txt[!grepl("^COMMENT", txt)]
  stopifnot(startsWith(txt0[1], 'STARTFONT'))
  stopifnot(txt[length(txt)] == 'ENDFONT')

  txt <- txt[-length(txt)]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the font text into chunks delineated by the start of each
  # character i.e. where the line starts with "STARTCHAR"
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chunks <- split_char_vec_into_chunks(txt, '^STARTCHAR')

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The first chunk is the header with global font information
  # All remaiing chunks each represent a character in the font
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  header_chunk <- chunks[[1]]
  char_chunks  <- chunks[-1]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the header into a named list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  font_info <- parse_font_info(header_chunk)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the characters into a list of 'bdf_char' objects containing
  # information for a sinle character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars <- lapply(char_chunks, parse_char, height = font_info$size)

  char_names <- vapply(chars, function(x) x$desc, character(1))
  names(chars) <- char_names


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # The 'encodings' of each character are the utf8 codepoints, but they do
  # not necessarily occur at that index in the 'chars' list.
  # e.g. the 'space' character has encoding = 32, but it is usually the
  # first character in a font i.e. list index = 1
  #
  # Here I biuld a lookup table that translates from utf8 codepoint to
  # list index in the the 'chars' list
  #
  # Note!!  Because utf8 codepoint = 0 is a valid enttiy in these fonts,
  # I have to "+1" to all codepoints so that they actually work with
  # R's 1-indexed vectors.
  #
  # This also means when I lookup an index, I need to always add 1 to the codepoint
  #
  # Note: some fonts have encodings which are negative. Just dropping these
  # altogether for now
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  encodings <- vapply(chars, function(x) x$encoding, integer(1))
  good      <- encodings >= 0
  chars     <- chars[good]
  encodings <- encodings[good]
  idx       <- rep(NA_integer_, length(encodings))
  idx[encodings + 1L] <- seq_along(encodings)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # List of all font information and character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- list(
    font_info     = font_info,
    font_info_raw = chunks[[1]],
    chars         = chars,
    idx           = idx
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make this of class 'bdf' so I can make use of S3 print.bdf() for
  # compact/informative output
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  class(res) <- 'bdf'
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse out font header information
#' @param header character vector of PDF font data prior to fist STARTCHAR
#'
#' @return list of info from the header
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_font_info <- function(header) {

  res <- list()

  for (elem in header) {
    if (startsWith(elem, "SIZE")) {
      res$size <- as.integer(strsplit(elem, "\\s")[[1]][2])
    } else if (startsWith(elem, "DEFAULT_CHAR")) {
      res$default_char <- as.integer(strsplit(elem, "\\s")[[1]][2])
    } else if (startsWith(elem, "FONTBOUNDINGBOX")) {
      res$bbox <- as.integer(strsplit(elem, "\\s")[[1]][-1])
    } else if (startsWith(elem, "PIXEL_SIZE")) {
      res$pixel_size <- as.integer(strsplit(elem, "\\s")[[1]][2])
    } else if (startsWith(elem, "FONT_ASCENT")) {
      res$font_ascent <- as.integer(strsplit(elem, "\\s")[[1]][2])
    } else if (startsWith(elem, "FONT_DESCENT")) {
      res$font_descent <- as.integer(strsplit(elem, "\\s")[[1]][2])
    } else {
      # cat("Unknwon: ", elem, "\n")
    }
  }

  ascent_descent <- NULL
  if (!is.null(res$font_ascent) && !is.null(res$font_descent)) {
    ascent_descent <- res$font_ascent + res$font_descent
  }


  res$line_height <- res$pixel_size %||%
    ascent_descent %||%
    res$size %||%
    res$bounding_box[2]

  if (is.null(res$line_height)) {
    warning("Couldn't determine font height from header. Using 12")
    res$line_height <- 12
  }



  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # if a default_char has not been specified, then use 'SPACE'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res$default_char <- res$default_char %||% 32L

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper function to split a single long vector into chunks
#'
#' @param char_vec character vector
#' @param regex regex for the elements to split on
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
split_char_vec_into_chunks <- function(txt, regex) {
  idx <- grep(regex, txt)
  start <- c(1L, idx)
  end   <- c(idx-1L, length(txt))
  mapply(
    function(x,y) {
      txt[seq(x, y)]
    },
    start, end, SIMPLIFY = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Parse a single BDF char
#'
#' @param txt character vector. must have initial 'STARTCHAR' line and final
#'        'ENDCHAR'
#' @param height height of font in pixels
#'
#' @return list of met-information and font information
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_char <- function(txt, height) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(startsWith(txt[1]            , "STARTCHAR"))
  stopifnot(           txt[length(txt)] == "ENDCHAR"  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split txt into
  #    header - everything before 'BITMAP'
  #    bitmap - everything after 'BITMAP'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chunks <- split_char_vec_into_chunks(txt, "^BITMAP")
  stopifnot(length(chunks) == 2L)

  header_txt <- chunks[[1]]
  bitmap_txt <- chunks[[2]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse out the raw bitmap sequence
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  bitmap_ints <- parse_bitmap_ints(bitmap_txt)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Parse the header information for this character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (i in seq_along(header_txt)) {
    elem <- header_txt[[i]]
    if (startsWith(elem, "ENCODING")) {
      encoding <- as.integer(substr(elem, start = 10, stop = nchar(elem)))
    } else if (startsWith(elem, "BBX")) {
      bbox <- as.integer(strsplit(elem, "\\s")[[1]][-1])
    } else if (startsWith(elem, "DWIDTH")) {
      dwidth <- as.integer(strsplit(elem, "\\s")[[1]][2])
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert the raw bitmap information into a data.frame of x,y coords
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  coords <- bitmap_ints_to_df(bitmap_ints, dwidth = dwidth)
  if (!is.null(coords) && nrow(coords) > 0) {
    coords$x <- coords$x + bbox[3]
    coords$y <- coords$y + bbox[4]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This characters definition
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- list(
    desc     = substr(txt[1], start = 11, stop = nchar(txt[1])),
    bitmap   = bitmap_ints,
    coords   = coords,
    encoding = encoding,
    dwidth   = dwidth,
    bbox     = bbox,
    raw      = txt
  )

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Finalise and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  class(res) <- 'bdf_char'
  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert BITMAP/ENDCHAR text sequence into an integer vector
#'
#' @param bitmap_vec character vector. Must start with 'BITMAP' and end with
#'        'ENDCHAR'
#'
#' @return vector of bitmap integers
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
parse_bitmap_ints <- function(txt) {
  stopifnot(txt[1]           == 'BITMAP')
  stopifnot(txt[length(txt)] == 'ENDCHAR')

  strtoi(txt[seq(2, length(txt)-1)], base = 16)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a vector of ints (representing a bitmap) into aata.frame of x,y coords
#'
#' @param bitmap_ints vector of integers representing a bitmap
#' @param dwidth character width (i.e. number of pixels)
#'
#' @return data.frame of x,y coords
#'
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
bitmap_ints_to_df <- function(bitmap_ints, dwidth) {

  Nbitmaps <- length(bitmap_ints)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # based upon the character width, decide how many bytes/bits we need
  # from each 'bitmap_int'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nbytes <- ceiling(dwidth/8)
  nbits  <- nbytes * 8

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the matrix of bits representing the character
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  allxs        <- vector('list', Nbitmaps)
  allbits      <- intToBits(bitmap_ints)
  dim(allbits) <- c(32, Nbitmaps)
  char_bits    <- allbits[seq(nbits), , drop=FALSE]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find the first and last rows which have any bits set
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (FALSE) {
    any_bits   <- which(apply(char_bits, 1, function(x) {any(x > 0)}))
    if (length(any_bits) == 0) {
      res <- data.frame(x = integer(0), y = integer(0))
      return(res)
    }
    first_last <- range(any_bits)
    char_bits <- char_bits[first_last[1]:first_last[2], ,drop=FALSE]
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Grab the last 'bboxw' rows in the matrix
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  char_bits <- char_bits[(nbits - dwidth + 1L):nbits, , drop = FALSE]


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a data.frame of x,y coordinates of all the bits set to 1
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- arrayInd(which(char_bits > 0), dim(char_bits))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Need to flip and mirror these coords so that they're the right way up
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (nrow(res) > 0  && ncol(res) > 0) {
    res <- as.data.frame(res)
    colnames(res) <- c('x', 'y')
    # res$x <- dwidth   + 1L - res$x
    res$x <- dwidth   + 1L - res$x
    res$y <- Nbitmaps + 1L - res$y
  } else {
    res <- data.frame(x = integer(0), y = integer(0))
  }

  res
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert \code{bdf_char} object into a data.frame
#'
#' @param x \code{bdf_char} object
#' @param ... ignored
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.data.frame.bdf_char <- function(x, ...) {
  df <- x$coords
  if (nrow(df) == 0) {
    df <- data.frame(x = NA_real_, y = NA_real_)
  }

  meta_df <- data.frame(
    encoding = x$encoding,
    desc     = x$desc,
    dwidth   = x$dwidth
  )

  cbind(meta_df, df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert \code{bdf} font into a data.frame
#'
#' @param x \code{bdf} font object
#' @param ... ignored
#'
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as.data.frame.bdf <- function(x, ...) {

  char_dfs <- lapply(x$chars, as.data.frame.bdf_char)

  df <- do.call(rbind, char_dfs)

  df$size         <- x$font_info$size
  df$pixel_size   <- x$font_info$pixel_size
  df$font_ascent  <- x$font_info$font_ascent
  df$font_descent <- x$font_info$font_descent
  df$line_height  <- x$font_info$line_height

  class(df) <- c('tbl_df', 'tbl', 'data.frame')
  df
}






if (FALSE) {

  library(grid)
  # bdf_file <- "./working/spleen-32x64.bdf"
  # bdf_file <- "./working/tom-thumb.bdf"
  bdf_file <- "./working/creep2-11.bdf"
  # bdf_file <- "./working/spleen-5x8.bdf"
  myfont <- NULL
  myfont <- read_bdf(bdf_file)
  myfont

  text <- 'hello #*-=+;:"'
  text <- "abc"
  text <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Morbi nec rutrum dui. Sed eleifend turpis in egestas tincidunt. Pellentesque nec eros cursus, condimentum lacus sed, tempor ipsum. Quisque id condimentum metus, id suscipit magna. Vestibulum tempor lorem augue. Pellentesque ultricies dui magna. Morbi euismod mauris eu feugiat mattis. Vestibulum ut odio non ipsum ultricies finibus at vitae nibh. Quisque a aliquam sapien. "
  grob <- bdfGrob(
    myfont, text,
    wrap = TRUE,
    width = 12,
    gp = gpar(fill=viridis::viridis(50), lwd = 0.1),
    size = 5,
    shrink = 0.7,
    vp = viewport(angle = 0)
  )
  grid.newpage(); grid.draw(grob)

  # bdf_print_sample(myfont, text)


  start <- Sys.time()
  text <- "#RStats"
  size <- displease::seq_ease(0.1, 400, n = 100, type = 'exp-in')
  for (i in seq_along(size)) {
    grob <- bdfGrob(
      myfont, text,
      # wrap = 10,
      gp = gpar(fill='black'),
      size = size[i],
      shrink = 0.8,
      vp = viewport(angle = 10)
    )

    file <- sprintf("./working/anim/%04i.png", i)
    png(file)
    grid.newpage(); grid.draw(grob)
    dev.off()
  }
  Sys.time() - start

}




if (FALSE) {

  bdf_file <- '/Users/mike/projects/pixelfont-annexe/ucs-fonts-75dpi100dpi/100dpi/courBO10.bdf'
  # bdf_file <- '/Users/mike/projects/pixelfont-annexe/ucs-fonts-75dpi100dpi/100dpi/timI12.bdf'
  myfont <- read_bdf(bdf_file)
  myfont

  grob <- bdfGrob(
    myfont, "Hello\n#RStats",
    # wrap = 10,
    gp = gpar(fill='black'),
    size = 10,
    shrink = 1,
    vp = viewport(angle = 0)
  )
  grid.newpage(); grid.draw(grob)

}



if (FALSE) {

  library(ggplot2)

  bdf_file <- "./working/spleen-5x8.bdf"
  myfont <- NULL
  myfont <- read_bdf(bdf_file)
  myfont

  df <- as.data.frame(myfont)

  pdf("working/sample.pdf", width = 10)
  ggplot(df[df$encoding < 104,]) +
    geom_tile(aes(x, y), width=0.9, height = 0.9, na.rm = TRUE) +
    facet_wrap(~encoding + desc, ncol = 12) +
    theme_void(4) +
    coord_equal()
  dev.off()

}






