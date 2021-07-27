fontfile <- system.file("spleen-5x8.bdf", package = "bdftools", mustWork = TRUE)
myfont <- read_bdf(fontfile)

test_that("bdf_print_sample()", {
    verify_output("text_output/bdf_print_sample.txt",
        bdf_print_sample(myfont, "Hello RStats", wrap = FALSE)
    )
    skip_on_os("windows")
    verify_output("text_output/bdf_print_sample_wide.txt",
        bdf_print_sample(myfont, "Hello RStats", wrap = FALSE,
                         zero = '  ', one = '\u2588\u2588')
    )
})

test_that("bdf_print_compact()", {
    skip_on_os("windows")
    verify_output("text_output/bdf_print_compact.txt",
        bdf_print_sample_compact(myfont, 'Hello #RStats')
    )
})
