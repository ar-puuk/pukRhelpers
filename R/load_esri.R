#' @title Load and Query an ESRI Feature Service Layer
#'
#' @description
#' This function connects to an ESRI Feature Service, displays available layers if no ID is provided,
#' and allows users to download and query a specific layer.
#'
#' @param furl Character string. URL to the ESRI Feature Service endpoint.
#' @param ... Additional arguments passed to `arcgislayers::arc_select` for querying the layer.
#' @param id Integer, optional. The ID of the layer to download. If NULL, available layers are displayed,
#'   and the user is prompted to specify an ID interactively.
#'
#' @details
#' The function opens a connection to the specified ESRI Feature Service, lists all available layers
#' if no `id` is provided, and then downloads and queries the specified layer. Users can pass query
#' arguments via `...` to filter the results.
#'
#' @return An `sf` object containing the queried features from the selected layer.
#'
#' @examples
#' \dontrun{
#' # Example 1: Load and query a specific layer by ID
#' sf_data <- load_esri("https://sampleserver6.arcgisonline.com/arcgis/rest/services/USA/MapServer", id = 1)
#'
#' # Example 2: Interactively select a layer
#' sf_data <- load_esri("https://sampleserver6.arcgisonline.com/arcgis/rest/services/USA/MapServer")
#' }
#'
#' @export
load_esri <- function(furl, ..., id = NULL) {
  rlang::check_dots_used()

  # Open connection to remote resource
  server <- arcgislayers::arc_open(furl)

  # Extract layer information
  layer_info <- server$layers

  # Check if id is specified
  if (base::is.null(id)) {
    # Display available layers
    base::cat("Available Layers:\n\n")
    base::cat(base::sprintf("%-3s %-30s\n", "ID", "Name"))
    for (i in base::seq_along(layer_info$id)) {
      base::cat(base::sprintf("%-3s %-30s\n", base::paste0(layer_info$id[i], ":"), layer_info$name[i]))
    }

    base::cat("\n")

    # Prompt user for id
    id <- base::as.integer(base::readline("Enter ID of the layer you want to download: "))

    # Check if entered id is valid
    while (!(id %in% layer_info$id)) {
      stop("Invalid layer ID. Please enter a valid ID.")
    }
  }

  # Get the index of the specified id
  index <- base::which(layer_info$id == id)

  # Extract the layer based on the specified id
  layer <- arcgislayers::get_layer(server, id = id)

  # Query the Feature Service
  sf <- arcgislayers::arc_select(layer, ...)

  # Return
  tryCatch(
    {
      return(sf)
    },
    error = function(e) {
      stop("Can't load ESRI layer")
    }
  )
}
