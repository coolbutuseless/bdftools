
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bdftools

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
<!-- badges: end -->

`bdftools` provides some tools for reading, manipulating and outputting
BDF bitmap fonts.

### What’s in the box

- `read_bdf(filename)` reads a BDF pixel font file into an R
  representation i.e. an object of class `bdf`
- `print.bdf()` prints meta information and a font sample.
- `bdf_create_df(bdf, text)` Create a data.frame of points for the given
  string
- `bdf_create_mat(bdf, text)` Create a matrix representation for the
  given string
- `bdfGrob(bdf, text, ...)` create a simple grob representation of the
  given string using squares for pixels
- `as.data.frame.bdf(bdf)` converts the full `bdf` font into a
  rectangular data.frame of all characters and their (x, y) coordinates.
- `read_bdf_builtin()` to read in a font included with this package:
  - [Cozette](https://github.com/slavfox/Cozette) License: MIT. See
    `LICENSE-cozette`
  - [Creep2](https://github.com/raymond-w-ko/creep2) License: MIT. See
    `LICENSE-creep2`
  - [Spleen](https://github.com/fcambus/spleen) License: BSD 2-clause.
    See `LICENSE-spleen`

You can install from
[GitHub](https://github.com/coolbutuseless/bdftools) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/bdftools')
```

## Example: Loading and displaying a BDF bitmap font.

``` r
library(grid)
library(ggplot2)
library(bdftools)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load a BDF font
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fontfile <- system.file("fonts", "spleen-5x8.bdf", package = "bdftools", mustWork = TRUE)
myfont <- read_bdf(fontfile)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# By default, printing a font will print some header info, and a text 
# sample rendered in that font in the console
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
myfont
```

    $size
    [1] 8

    $bbox
    [1]  5  8  0 -1

    $pixel_size
    [1] 8

    $font_descent
    [1] 1

    $font_ascent
    [1] 7

    $default_char
    [1] 32

    $line_height
    [1] 8

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate some sample text in the console
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_matrix("Hello RStats", myfont) |> 
  as.raster() |>
  plot(interpolate = FALSE)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

``` r
coords <- create_coords("Hello\n#RStats!", myfont)
head(coords)
```

    # A tibble: 6 × 3
          x     y   idx
      <dbl> <dbl> <int>
    1     4    14     1
    2     1    14     1
    3     4    13     1
    4     1    13     1
    5     4    12     1
    6     3    12     1

``` r
grid.newpage()
grid.rect(coords$x * 4, coords$y * 4, width = 3, height = 3, default.units = 'mm',
          gp = gpar(fill = 'lightblue'))
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

# An example of a larger font

``` r
myfont <- bdftools::read_bdf_builtin("spleen-16x32.bdf")
create_matrix("Frak!", myfont) |> 
  as.raster() |>
  plot(interpolate = FALSE)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

## Font sample sheet

An example of how the data.frame representation of the font can be
plotted in `ggplot2`.

``` r
library(ggplot2)
library(bdftools)

myfont <- read_bdf_builtin("cozette.bdf")

plot_df <- as.data.frame(myfont)
plot_df <- plot_df[plot_df$encoding >= utf8ToInt('A') & plot_df$encoding <= utf8ToInt('|'),]

ggplot(plot_df) +
  geom_tile(aes(x, y), width=0.9, height = 0.9, na.rm = TRUE) +
  facet_wrap(~encoding + desc, ncol = 12)+
  theme_void(10) +
  coord_equal()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

## BDF Bitmap Font Resources

- Fonts
  - [Tom Thumb tiny
    font](https://robey.lag.net/2010/01/23/tiny-monospace-font.html)
    (License: Dual licensed CC0 or CC-BY 3.0 license.)
  - [github repo of lots of
    fonts](https://github.com/Tecate/bitmap-fonts) (License:
    Various/unknown)
  - [unifont](https://www.unifoundry.com/unifont/index.html) a BDF font
    with glyphs for every printable codepoint from `U+0000` to `U+FFFF`.
    (License: Dual licensed SIL Open Font License (OFL) version 1.1 and
    the GNU GPL 2+ with the GNU font embedding exception)
  - [X11 fonts](https://www.cl.cam.ac.uk/~mgk25/ucs-fonts.html)
    (License: Public Domain)
- BDF Font Specification
  - [BDF on
    Wikipedia](https://en.wikipedia.org/wiki/Glyph_Bitmap_Distribution_Format)
  - [BDF Specification Document
    (Adobe)](https://adobe-type-tools.github.io/font-tech-notes/pdfs/5005.BDF_Spec.pdf)

## Related Software

Other retro/vector/pixel fonts in R

- [arcade font](https://github.com/coolbutuseless/arcadefont)
- [gridfont](https://github.com/coolbutuseless/gridfont)
- [hershey](https://github.com/coolbutuseless/hershey)
