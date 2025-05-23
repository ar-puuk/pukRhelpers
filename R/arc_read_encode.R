#' Read an ArcGIS layer (or table or ImageServer) and optionally encode domains
#'
#' @param url The URL of the FeatureLayer, Table, or ImageServer.
#' @param col_names Default `TRUE`. Column names or name handling rule. `col_names` can be `TRUE`, `FALSE`, `NULL`, or a character vector:
#'   - If `TRUE`, use existing default column names for the layer or table. If `FALSE` or `NULL`, column names will be generated automatically: X1, X2, X3 etc.
#'   - If `col_names` is a character vector, values replace the existing column names. `col_names` can't be length 0 or longer than the number of fields in the returned layer.
#' @param col_select Default `NULL`. A character vector of the field names to be returned. By default, all fields are returned.
#' @param n_max Defaults to `Inf` or an option set with `options("arcgislayers.n_max" = <max records>)`. Maximum number of records to return.
#' @param name_repair Default `"unique"`. See `vctrs::vec_as_names()` for details. If `name_repair = NULL` and `alias = "replace"` may include invalid names.
#' @param crs the spatial reference to be returned. If the CRS is different than the CRS for the input FeatureLayer, a transformation will occur server-side. Ignored if x is a `Table`.
#' @param ... Additional arguments passed to `arcgislayers::arc_select()` if URL is a `FeatureLayer` or `Table` or `arcgislayers::arc_raster()` if URL is an `ImageLayer.`
#' @param fields Default `NULL` a character vector of the field names to returned. By default all fields are returned. Ignored if `col_names` is supplied.
#' @param alias Use of field alias values. Default `c("drop", "label", "replace")`,. There are three options:
#'   - `"drop"`, field alias values are ignored.
#'   - `"label"`: field alias values are assigned as a label attribute for each field.
#'   - `"replace"`: field alias values replace existing column names. `col_names`
#' @param token your authorization token.
#' @param encode_field_values Logical, character vector, or list of character
#'   vectors.
#'   - `FALSE` (the default): do _not_ encode any fields.
#'   - `TRUE`: encode _all_ fields that have coded-value domains.
#'   - Specific field or list of fields to replace. Fields that do not have coded value domains are ignored.
#' @param codes Character scalar, one of `"replace"` or `"label"`. Passed
#'   through to `arcgislayers::encode_field_values()` to control whether domain labels
#'   replace the raw codes or just attach as an attribute.
#'
#' @return If `url` is an `ImageServer`, a `SpatRaster`; otherwise an `sf` or
#'   `data.frame`.
#' @export
arc_read_encode <- function (
    url,
    col_names = TRUE,
    col_select = NULL,
    n_max = Inf,
    name_repair = "unique",
    crs = NULL,
    ...,
    fields = NULL,
    alias = "drop",
    token = arcgisutils::arc_token(),
    encode_field_values = FALSE,
    codes = c("replace", "label")
) {
  # validate url and basic args
  arcgisutils:::check_string(url, allow_empty = FALSE)
  arcgisutils:::check_character(fields, allow_null = TRUE)
  arcgisutils:::check_character(col_select, allow_null = TRUE)

  # validate alias
  alias <- alias %||% "drop"
  alias <- rlang::arg_match(alias, values = c("drop", "label", "replace"))

  # validate col_names
  is_valid_col_names_arg <- rlang::is_logical(col_names, 1L) ||
    rlang::is_null(col_names) ||
    rlang::is_character(col_names)
  if (!is_valid_col_names_arg) {
    cli::cli_abort(
      "{.arg col_names} must be one of {.val TRUE}, {.val FALSE}, {.val NULL}, or a character vector of the new column names"
    )
  }

  # validate n_max
  if (!rlang::is_integerish(n_max, 1L)) {
    cli::cli_abort("{.arg n_max} must be a scalar integer.")
  }

  # validate token
  if (!is.null(token)) {
    arcgisutils::obj_check_token(token)
  }

  # validate field encoding arguments
  codes <- rlang::arg_match(codes, values = c("replace", "label"))
  if (! (rlang::is_logical(encode_field_values, 1L) ||
         rlang::is_character(encode_field_values)) ) {
    cli::cli_abort("`encode_field_values` must be a single logical or a character vector of field names.")
  }

  # open the resource
  x <- arcgislayers::arc_open(url = url, token = token)
  crs <- crs %||% sf::st_crs(x)

  # ImageServer case
  if (inherits(x, "ImageServer")) {
    layer <- arcgislayers::arc_raster(x = x, ..., crs = crs, token = token)
    return(layer)
  } else if (!arcgislayers:::obj_is_layer(x)) {
    cli::cli_abort(c(
      "{.arg url} is not a supported type: {.val FeatureLayer}, {.val Table}, or {.val ImageServer}",
      i = "found {.val {class(x)[1]}}"
    ))
  }

  # apply global n_max override
  if (is.infinite(n_max) && is.numeric(getOption("arcgislayers.n_max"))) {
    n_max <- getOption("arcgislayers.n_max")
  }

  # select data
  layer <- arcgislayers::arc_select(
    x      = x,
    fields = col_select %||% fields,
    crs    = crs,
    n_max  = n_max,
    token  = token,
    ...
  )

  # name/alias logic
  if (identical(col_names, "alias")) {
    alias     <- "replace"
    col_names <- NULL
    lifecycle::deprecate_soft(
      "deprecated",
      what = "arcgislayers::arc_read(col_names = \"can't be alias\")",
      with = "arcgislayers::arc_read(alias = \"replace\")"
    )
  }

  if (identical(alias, "drop") || is.character(col_names) || isFALSE(col_names)) {
    layer <- arcgislayers:::set_col_names(.data = layer, col_names = col_names, name_repair = name_repair)
  } else {
    layer <- arcgislayers::set_layer_aliases(.data = layer, .layer = x, name_repair = name_repair, alias = alias)
  }

  # Optional domain encoding
  if (isTRUE(encode_field_values) || rlang::is_character(encode_field_values)) {
    fld <- if (isTRUE(encode_field_values)) NULL else encode_field_values
    layer <- arcgislayers::encode_field_values(
      .data  = layer,
      .layer = x,
      field  = fld,
      codes  = codes
    )
  }

  layer
}
