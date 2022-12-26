#' Colors from Leder Games' Root, specifically the art of Kyle Ferrin, extracted
#' from the Steam workshop mod files
root_colors <- c(
  cats = rgb(205, 110, 50, 255, maxColorValue = 255),
  birds = rgb(58, 100, 161, 255, maxColorValue = 255),
  woodland = rgb(92, 167, 76, 255, maxColorValue = 255),
  vagabond1 = rgb(210, 211, 212, 255, maxColorValue = 255),
  otters = rgb(81, 177, 171, 255, maxColorValue = 255),
  lizards = rgb(227, 219, 50, 255, maxColorValue = 255),
  vagabond2 = rgb(53, 53, 53, 255, maxColorValue = 255),
  moles = rgb(212, 174, 145, 255, maxColorValue = 255),
  corvids = rgb(74, 39, 105, 255, maxColorValue = 255),
  badgers = rgb(161, 175, 175, 255, maxColorValue = 255),
  rats = rgb(227, 34, 54, 255, maxColorValue = 255)
)


#' Get names and hex codes from color palette
#' @param ... List of colors to return. If \code{NULL}, returns the entire set of options.
#' @param palette Named character vector of hex codes; defaults to \code{root_colors}.
#' @examples
#' get_colors()
#' get_colors("cats")
#' get_colors("cats", "birds")
#' @export

get_colors <- function(..., palette = root_colors) {
  colors <- c(...)
  if (is.null(colors))
    return (palette)
  palette[colors]
}

# Palettes of colors

root_pal_list <- list(
  full = get_colors(),
  base = get_colors("cats", "birds", "woodland", "vagabond1"),
  expansion = get_colors("otters", "lizards", "vagabond2", "corvids", "moles",
                         "badgers", "rats")
)

#' Color scales
#'
#' These functions provide \code{ggplot2}-compatible color and fill scales using
#' custom colors
#' 
#' The underlying code is adapted from \code{@drsimonj}'s tutorial at
#' \url{https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2}.
#'
#' @param ... options passed to \code{scale_color_manual} or
#'   \code{scale_color_gradientn}
#' @param palette character, which palette to use. Options are \code{"full"}
#'  (default), \code{"base"}, and \code{"expansion"}.
#' @param discrete logical, whether palette is discrete or continuous.
#'   Continuous will interpolate between colors; discrete has a maximum number
#'   of values.
#' @param reverse logical, whether to reverse the order of the palette.
#' @param pal_list Named list of palettes, used by palette argument. Each
#'   element is a vector of hex codes.
#' @param named logical, whether the colors in the palette have names
#'   corresponding to the data. This is usually not true.

#'
#' @describeIn scale_color_root Color scale
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 3)
#' p + scale_color_root(palette = "base")
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Petal.Length)) +
#'   geom_point(size = 3)
#' p + scale_color_root(discrete = FALSE)
#' p + scale_color_root(discrete = FALSE, reverse = TRUE)
#'
#' @export

scale_color_root <- function(...,
                             palette = "full",
                             discrete = TRUE,
                             reverse = FALSE,
                             pal_list = root_pal_list,
                             named = FALSE) {
  
  pal <- pal_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    ggplot2::scale_color_manual(..., values = pal)
  } else {
    ggplot2::scale_color_gradientn(..., colors = pal)
  }
}

#' @describeIn scale_color_root Fill scale
#' @examples
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Species)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#'
#' p + scale_fill_root(palette = "expansion")
#'
#' p <- ggplot(iris, aes(Sepal.Width, Sepal.Length, fill = Petal.Length)) +
#'   geom_point(aes(size = Petal.Width), shape = 21)
#' p + scale_fill_root(discrete = FALSE)
#' p + scale_fill_root(discrete = FALSE, reverse = TRUE)
#'
#' @export

scale_fill_root <- function(...,
                            palette = "full",
                            discrete = TRUE,
                            reverse = FALSE,
                            pal_list = root_pal_list,
                            named = FALSE) {
  
  pal <- pal_list[[palette]]
  
  if(!named)
    names(pal) <- NULL
  
  if (reverse) pal <- rev(pal)
  if (discrete) {
    ggplot2::scale_fill_manual(..., values = pal)
  } else {
    ggplot2::scale_fill_gradientn(..., colors = pal)
  }
}