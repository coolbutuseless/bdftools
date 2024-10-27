
source("data-raw/parse-arcade.R")
source("data-raw/parse-bdfs.R")
source("data-raw/parse-gridfont.R")
source("data-raw/parse-hex-8x8.R")
source("data-raw/parse-hex-8x16.R")



bitmaps[['unscii-8']]      <- unscii_8
bitmaps[['unscii-8-thin']] <- unscii_8_thin
bitmaps[['unscii-16']]     <- unscii_16


usethis::use_data(
  bitmaps,
  arcade,
  gridfont, gridfont_smooth,
  internal = TRUE, overwrite = TRUE, compress = 'bzip2'
)




bdf_names

font_info <- list()
font_info$bitmap <- lapply(bitmaps, function(bdf) {
  cp <- which(!is.na(bdf$idx)) - 1L
  list(codepoints = sort(cp))
})




cp_arcade <- vapply(unique(arcade$char), utf8ToInt, integer(1)) |> 
  unname() |> 
  sort()


cp_gridfont <- vapply(unique(gridfont$char), utf8ToInt, integer(1)) |> 
  unname() |> 
  sort()

cp_gridfont_smooth <- vapply(unique(gridfont_smooth$char), utf8ToInt, integer(1)) |> 
  unname() |> 
  sort()

font_info$vector <- list()
font_info$vector$arcade          <- list(codepoints = cp_arcade)
font_info$vector$gridfont        <- list(codepoints = cp_gridfont)
font_info$vector$gridfont_smooth <- list(codepoints = cp_gridfont_smooth)


usethis::use_data(
  font_info,
  internal = FALSE,
  overwrite = TRUE
)
