myfont <- read_bdf_builtin(font_name = "spleen-8x16.bdf")

test_that("bdfGrob()", {
  skip_if_not_installed("vdiffr")
  library("vdiffr")

  text <- c('hello', 'there')
  y   <- unit(0.5, 'npc') + unit(c(0.1, 0.4), 'npc')
  grob <- bdfGrob(
    myfont, text,
    y = y,
    size  = c(4, 8),
    shrink = c(0.7, 1.2),
    # vp    = viewport(angle = 10),
    wrap  = TRUE,
    width = 15,
    # hjust = 0.5,
    # vjust = 1.5,
    rot  = 90,
    fill = grDevices::rainbow(6)
  )

  expect_doppelganger("bdfGrob", grob)
})
