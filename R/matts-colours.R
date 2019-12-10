#' @title Matts Custom Colours
#'
#' @description Functions for building and accessing Matt's favourite colours and palettes for use in ggplot visualizations.
#'
#' @param palette Name of palette.
#' @param discrete Logical. Is the colour palette for a discrete mapping?
#' @param reverse Logical. Should the colours/palette be reverse?
#' @param ... Additional arguments passed on to relevant function.
#'
#' @details Here is a breakdown of the important functions and what they do:
#'
#' \itemize{
#'  \item \code{scale_color_matt} a ggplot-type function for providing colours for coloring a mapping.
#'  \item \code{scale_fill_matt} a ggplot-type function for providing colours for filling a mapping.
#'  \item \code{matts_colours} is a character vector of my favourite colours.
#'  \item \code{matts_palettes} is a list of curated palette sets.
#'  \item \code{matts_cols} is a function for access colours in \code{matts_colours} by name (hex value is returned).
#'  \item \code{matts_pal} returns a function to interpolate a colour palette from \code{matts_palettes}.
#' }
#'
#' See \code{matts_palettes()} for a list of the available palettes.
#'
#' @name matt
#'
#' @export

scale_color_matt <- function(palette = 'rainbow', discrete = TRUE, reverse = FALSE, ...) {
  pal <- matts_pals(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("matts_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' @export
#' @rdname matt
scale_fill_matt <- function(palette = "rainbow", discrete = TRUE, reverse = FALSE, ...) {
  pal <- matts_pals(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("matts_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}

#' @export
#' @rdname matt
matts_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return(matts_colours)

  matts_colours[cols]
}

#' @export
#' @rdname matt
matts_pals <- function(palette = 'rainbow', reverse = FALSE, ...) {
  pal <- matts_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

#' @export
#' @rdname matt
matts_colours <- c(`blue` = '#BAE1FF',
                   `harvard` = '#C90016',
                   `leafs` = '#00205B')

#' @export
#' @rdname matt
matts_palettes <- list(
  `rainbow` = c('#FFB3BA', '#FFDFBA', '#FFFFBA', '#BAFFC9', '#BAE1FF'),
  `bootstrap` = c('#D9534F', '#F9F9F9', '#5BC0DE', '#5CB85C', '#428BCA'),
  `cool` = c('#005073', '#107DAC', '#189AD3', '#1EBBD7', '#71C7EC'),
  `warm` = c('#DE0C1C', '#FE2D2D', '#FB7830', '#FECF02', '#FFDD47'),

  `blues` = c('#011F4B', '#03396C', '#005B96', '#6497B1', '#B3CDE0'),
  `greys` = c('#343D46', '#4F5B66', '#65737E', '#A7ADBA', '#C0C5CE'),
  `grays` = c('#343D46', '#4F5B66', '#65737E', '#A7ADBA', '#C0C5CE'),
  `bw` = c('#1B1B1B', '#434343', '#6D6D6D', '#969696', '#BEBEBE', '#E1E1E1', '#F9F9F9'),
  `reds` = c('#FF0000', '#BF0000', '#800000', '#400000', '#000000'),
  `red orange` = c('#FFC100', '#FF9A00', '#FF7400', '#FF4D00', '#FF0000'),
  `pastel1` = c('#FFC5D0', '#EAD1AB', '#BBDEB1', '#99E2D8', '#B8D8F8', '#EDC8F5', '#FFC5D0'),
  `blue red` = c('#023FA5', '#7D87B9', '#BEC1D4', '#E2E2E2', '#D6BCC0', '#BB7784', '#8E063B'),

  `lunenfeld` = c('#44C7F4', '#F04E63', '#FFCD02'),
  `toronto` = c('#25355A', '#007FA3', '#000000', '#F2F4F7'),

  `earthy` = c('#46211A', '#693D3D', '#BA5536', '#A43820'),
  `mountains` = c('#90AFC5', '#336B87', '#2A3132', '#763626'),
  `outdoors` = c('#2E4600', '#486B00', '#A2C523', '#7D4427'),
  `autumn` = c('#8D230F', '#1E434C', '#9B4F0F', '#C99E10'),
  `tropical` = c('#4897D8', '#FFDB5C', '#FA6E59', '#F8A055'),
  `summer` = c('#BB3366','#FF1188', '#FFEE22', '#11BBCC', '#1199BB'),

  `kian` = c('#EED284', '#96384E', '#04354B', '#0297A0', '#037665'),
  `louche` = c('#FDE8F1', '#FFCEDF', '#A1F3FF', '#9CDFFF', '#8ED3F4'),
  `throaway` = c('#14202B', '#264067', '#5080AE', '#B2C6DD', '#E14A4C'),
  `stay cool` = c('#C6DD52', '#FBB819', '#DF3D5E', '#6D4A76', '#3CA9A1'),
  `blow pop` = c('#F46969', '#F4F4F4', '#91B5D6', '#5691C7', '#31709F'),
  `abrasion` = c('#47BD9B', '#3994E2', '#EA6ED0', '#00F2FF', '#F88E8E'),
  `dragons eye` = c('#772C78', '#7441A9', '#C71F85', '#F0A413', '#3A7FCC'),
  `beauty1` = c('#F8B195', '#F67280', '#C06C84', '#6C5B7B', '#355C7D'),
  `beauty2` = c('#99B898', '#FECEAB', '#FF847C', '#E84A5F', '#2A363B'),
  `beauty3` = c('#E5FCC2', '#9DE0AD', '#45ADA8', '#547980', '#594F4F'),
  `hunt1` = c('#7189BF', '#DF7599', '#FFC785', '#72D6C9'),
  `hunt2` = c('#99E1E5', '#F3E8CB', '#F2C6B4', '#FBAFAF'),
  `hunt3` = c('#EAAFAF', '#A2738C', '#645C84', '#427996'),
  `hunt4` = c('#FFB6B9', '#FAE3D9', '#BBDED6', '#8AC6D1')
)
