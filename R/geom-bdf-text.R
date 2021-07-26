


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add text in a bitmap font to a ggplot
#'
#' @param mapping,data,stat,position,...,nudge_x,nudge_y,na.rm,show.legend,inherit.aes
#'        See documentation for \code{ggplot2::geom_text()}
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
geom_text_bdf <- function(
  mapping       = NULL,
  data          = NULL,
  stat          = "identity",
  position      = "identity",
  ...,
  nudge_x       = 0,
  nudge_y       = 0,
  na.rm         = FALSE,
  show.legend   = NA,
  inherit.aes   = TRUE
) {

  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      stop("You must specify either `position` or `nudge_x`/`nudge_y`.")
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data        = data,
    mapping     = mapping,
    stat        = stat,
    geom        = GeomTextBDF,
    position    = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm         = na.rm,
      ...
    )
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Custom key for bdf fonts. Just a slightly modified draw_key_polygon
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw_key_bdf <- function(data, params, size) {
  data$size <- data$linewidth
  ggplot2::draw_key_polygon(data, params, size)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' GeomTextBDF
#'
#' @format NULL
#' @usage NULL
#'
#' @import ggplot2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GeomTextBDF <- ggplot2::ggproto(
  "GeomTextBDF",
  ggplot2::GeomText,
  required_aes = c("x", "y", "label"),

  default_aes = ggplot2::aes(
    colour       = "black",
    fill         = 'black',
    size         = 3.88,
    angle        = 0,
    hjust        = 0.5,
    vjust        = 0.5,
    alpha        = NA,
    family       = "",
    fontface     = 1,
    lineheight   = 1.2,
    linewidth    = 1,
    linetype     = 1,
    bdf_filename = system.file('spleen-5x8.bdf', package = 'bdftools', mustWork = TRUE),
    shrink       = 1
  ),

  draw_panel = function(data, panel_params, coord, na.rm = FALSE) {
    lab <- data$label
    data <- coord$transform(data, panel_params)

    # this_text_grob <- textGrob(
    #   lab,
    #   data$x, data$y, default.units = "native",
    #   hjust = data$hjust, vjust = data$vjust,
    #   rot = data$angle,
    #   gp = gpar(
    #     col = alpha(data$colour, data$alpha),
    #     fontsize = data$size * .pt,
    #     fontfamily = data$family,
    #     fontface = data$fontface,
    #     lineheight = data$lineheight
    #   )
    # )

    bdf_filename <- data$bdf_filename[1]
    if (!file.exists(bdf_filename)) {
      stop("Can't find bdf fontfile: bdf_filename = ", deparse(bdf_filename))
    }

    bdf <- read_bdf(bdf_filename)

    # print(data)

    this_bdf_grob <- bdfGrob(
      bdf           = bdf,
      text           = lab,
      x             = data$x,
      y             = data$y,
      default.units = 'native',
      hjust         = data$hjust,
      vjust         = data$vjust,
      size          = data$size * .pt / bdf$font_info$size,
      shrink        = data$shrink,
      wrap          = FALSE,
      rot           = data$angle,
      gp = gpar(
        col  = alpha(data$colour, data$alpha),
        fill = alpha(data$fill, data$alpha),
        lwd  = data$linewidth,
        lty  = data$linetype
      )
      # rot         = data$angle
    )


    # grobTree(this_bdf_grob, this_text_grob)
    this_bdf_grob
  },

  draw_key = draw_key_bdf
)


if (FALSE) {
  library(ggplot2)

  df <- data.frame(x = c(10, 20), y = c(10, 20), label = c('hello', 'there'))

  ggplot(df) +
    geom_text_bdf(
      aes(
        x     = x,
        y     = y,
        label = label,
        vjust = 0,
        hjust = 0,
        shrink = 1
      ),
      size = 20) +
    geom_text_bdf(label = "#RStats", x = 0, y = 0, hjust = 0, vjust = 0, size = 30,
                  shrink = 1) +
    ylim(0, 30) +
    xlim(0, 30) +
    theme_bw()


  ggplot(mtcars, aes(wt, mpg, label = rownames(mtcars))) +
    geom_point(color = 'blue') +
    geom_text_bdf(
      size = 10,
      shrink = 0.8,
      hjust = 0,
      fill = 'skyblue',
      angle = 45
    ) +
    theme_bw() +
    xlim(1.5, 2.5) +
    ylim(20, 40)


  system.time({
  unifont <- read_bdf("./working/unifont.bdf")
  })

  myfont <- read_bdf("/Users/mike/projects/bdftools-annexe/ucs-fonts-75dpi100dpi/100dpi/timR18.bdf")
  myfont



}


