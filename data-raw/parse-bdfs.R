## code to prepare `DATASET` dataset goes here


bdf_files <- list.files("data-raw/fonts/", pattern = "bdf$", full.names = TRUE)
bdf_files

bdf_names <- basename(tools::file_path_sans_ext(bdf_files))
bdf_names[bdf_names == 'unifont-16.0.01'] <- 'unifont'


bdfs <- lapply(bdf_files, bdftools:::read_bdf) |>
  setNames(bdf_names)




usethis::use_data(bdfs, overwrite = TRUE, internal = TRUE, compress = 'bzip2')
