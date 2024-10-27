


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Byte to coordinate lookup
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
xs8 <- lapply(0:255, \(i) {
  which(rev(intToBits(i)[1:8]) > 0)
}) |> setNames(
  toupper(sprintf("%02x", 0:255))
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert 16 byte hex to a data.frame of coordinates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hex_8x16_to_coords <- function(hex) {
  
  hex <- stringr::str_sub(hex, seq(1, 31, 2), seq(2, 32, 2))
  coords <- lapply(16:1, \(i) {
    x <- xs8[[hex[[i]]]]
    if (length(x) == 0) {
      data.frame(x = integer(0), y = integer(0))
    } else {
      data.frame(x = x, y = 17L - i)
    }
  }) 
  char_coords <- do.call(rbind, coords)
  char_coords
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Readh hex 8x16 font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_hex_8x16 <- function(hex_file) {
  
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
      coords = hex_8x16_to_coords(hex),
      dwidth = 8L
    )
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert codes to index into 'chars'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  codes <- vapply(hex_raw, \(x) x[1], character(1))
  codes <- strtoi(codes, base = 16)
  
  # print(codes)
  max_code <- max(codes)
  # print(max_code)
  idx <- rep(NA_integer_, max_code+1L)
  
  idx[codes + 1] <- seq_along(codes)
  
  list(
    chars = chars, 
    idx   = idx,
    font_info = list(
      line_height = 16L
    )
  )
  
}




hex_file <- "data-raw/unscii/unscii-16.hex"
unscii_16 <- read_hex_8x16(hex_file)

# hex_file       <- "data-raw/unscii/unscii-16-full.hex"
# unscii_16_full <- read_hex_8x16(hex_file)
















