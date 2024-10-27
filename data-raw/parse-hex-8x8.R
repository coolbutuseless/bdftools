

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Byte to coordinate lookup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs8 <- lapply(0:255, \(i) {
  which(rev(intToBits(i)[1:8]) > 0)
}) |> setNames(
  toupper(sprintf("%02x", 0:255))
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 8 byte hex to a data.frame of coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_8x8_to_coords <- function(hex) {
  hex <- stringr::str_sub(hex, seq(1, 15, 2), seq(2, 16, 2))
  coords <- lapply(8:1, \(i) {
    x <- xs8[[hex[[i]]]]
    if (length(x) == 0) {
      data.frame(x = integer(0), y = integer(0))
    } else {
      data.frame(x = x, y = 9L - i)
    }
  }) 
  char_coords <- do.call(rbind, coords)
  char_coords
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read an 8x8 hex font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_8x8 <- function(hex_file) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Load hexfile and split codepoint from hex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  hex_raw  <- readLines(hex_file) 
  hex_raw  <- strsplit(hex_raw, ":")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert hex to data.frames
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  chars <- lapply(hex_raw, \(hr) {
    hex <- hr[2]
    list(
      coords = hex_8x8_to_coords(hex),
      dwidth = 8L
    )
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert codes to index into 'chars'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codes <- vapply(hex_raw, \(x) x[1], character(1))
  codes <- strtoi(codes, base = 16)
  
  max_code <- max(codes)
  idx <- rep(NA_integer_, max_code+1L)
  
  idx[codes + 1] <- seq_along(codes)
  
  list(
    chars = chars, 
    idx   = idx
  )
  
}



unscii_8         <- read_hex_8x8("data-raw/unscii/unscii-8.hex")
# unscii_8_alt     <- read_hex8("data-raw/unscii/unscii-8-alt.hex")
# unscii_8_fantasy <- read_hex8("data-raw/unscii/unscii-8-fantasy.hex")
# unscii_8_mcr     <- read_hex8("data-raw/unscii/unscii-8-mcr.hex")
# unscii_8_tall    <- read_hex8("data-raw/unscii/unscii-8-tall.hex")
unscii_8_thin    <- read_hex_8x8("data-raw/unscii/unscii-8-thin.hex")

















