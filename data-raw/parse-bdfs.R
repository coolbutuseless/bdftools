## code to prepare `DATASET` dataset goes here


bdf_files <- list.files("data-raw/fonts/", pattern = "bdf$", full.names = TRUE)
bdf_files

bdf_names <- basename(tools::file_path_sans_ext(bdf_files))
bdf_names[bdf_names == 'unifont-16.0.01'] <- 'unifont'


bitmaps <- lapply(bdf_files, function(f) {
  message(f)
  bdftools:::read_bdf(f)
  }) |>
  setNames(bdf_names)



