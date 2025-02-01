#' @title Aggregate sf objects
#'
#' @description
#' Geometries and attributes are aggregated.
#' Source: https://gist.github.com/rCarto/bb47aff0a02e808d2bf64f2d8c5db7d8#file-st_aggregate-r
#'
#' @param x sf object
#' @param by name of the variable of grouping elements
#' @param var name(s) of the variable(s) to aggregate
#' @param fun function(s) to compute the summary statistics
#'
#' @return An sf object is returned
#'
#' @examples
#' \dontrun{
#' library(sf)
#' nc <- sf::st_read(system.file("shape/nc.shp", package="sf"))
#' nc$dummy <- "ZONE_A"
#' nc$dummy[25:50] <- "ZONE_B"
#' nc$dummy[51:100] <- "ZONE_C"
#' r <- st_aggregate(nc, "dummy", c("BIR74", "NWBIR74"), c("mean", "median"))
#' plot(nc)
#' plot(r)
#' }
#'
#'@export
st_aggregate <- function(x, by, var, fun){
  var = base::c(by, var)
  fun = base::c("head", fun)
  n <- base::length(var)
  l <- base::vector("list", n)
  for (i in 1:n){
    l[[i]] <- base::tapply(x[[var[i]]], x[[by]], add_args(fun[[i]]), n = 1, na.rm = TRUE)
  }
  base::names(l) <- var
  r <- sf::st_sf(base::do.call(data.frame, l),
                 geometry = base::tapply(x[attr(x, "sf_column")], x[[by]], sf::st_union),
                 crs = sf::st_crs(x))
  r
}

add_args <- function(x){
  rx <- base::get(x)
  fx <- base::formals(rx)
  fx$na.rm = TRUE
  fx$n = 1
  base::formals(rx) <- fx
  rx
}
