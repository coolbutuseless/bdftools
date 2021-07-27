test_that("geom_text_bdf()", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("vdiffr")
  library("ggplot2")
  library("vdiffr")

  fontfile <- system.file("spleen-5x8.bdf", package = "bdftools", mustWork = TRUE)

  expect_doppelganger("geom_text_bdf", {

    plot_df <- head(mtcars)
    plot_df$car <- rownames(plot_df)
    plot_df$cyl <- as.factor(plot_df$cyl)

    ggplot(plot_df, aes(mpg, wt)) +
      geom_point(col = 'red') +
      geom_text_bdf(
        aes(mpg, wt, label=car, fill = cyl),
        col          = NA,
        linewidth    = 0.5,
        bdf_filename = fontfile,
        shrink       = 1,
        size         = 9,
        hjust        = -0.1
      ) +
      theme_bw() +
      scale_fill_brewer(palette = 'Dark2') +
      labs(title = "geom_text_bdf() - bitmap font rendering in ggplot2")
  })
})
