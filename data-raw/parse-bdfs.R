## code to prepare `DATASET` dataset goes here


bdf_files <- list.files("inst/fonts/", pattern = "bdf$", full.names = TRUE)

bdf_names <- basename(tools::file_path_sans_ext(bdf_files))

bdfs <- lapply(bdf_files, bdftools:::read_bdf) |>
  setNames(bdf_names)


usethis::use_data(bdfs, overwrite = TRUE, internal = TRUE, compress = 'xz')
