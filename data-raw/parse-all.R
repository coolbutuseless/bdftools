
source("data-raw/parse-arcade.R")
source("data-raw/parse-bdfs.R")
source("data-raw/parse-gridfont.R")


# usethis::use_data(bdfs, overwrite = TRUE, internal = TRUE, compress = 'bzip2')
# usethis::use_data(arcade, internal = FALSE, overwrite = TRUE)

usethis::use_data(
  bdfs,
  arcade,
  gridfont, gridfont_smooth,
  internal = TRUE, overwrite = TRUE, compress = 'bzip2'
)
