#' Convert FlatGeoBuf (FGB) Files to Geodatabase (GDB) Layers
#'
#' This function converts all FlatGeoBuf (.FGB) files within a specified folder into layers of a specified Geodatabase (GDB) file.
#'
#' @param input_folder Character. Path to the folder containing FGB files.
#' @param gdb_path Character. File path to the output GDB file.
#' @param overwrite Logical. Whether to overwrite existing layers in the GDB. Default is FALSE.
#' @param crs Character. Coordinate Reference System to transform the data. Default is "EPSG:3857".
#'
#' @import sf
#' @export
#'
#' @examples
#' \dontrun{
#' convert_fgb_to_gdb("/path/to/fgb_folder", "/path/to/output.gdb", overwrite = FALSE, crs = "EPSG:3857")
#' }
convert_fgb_to_gdb <- function(input_folder, gdb_path, overwrite = FALSE, crs = "EPSG:3857") {
  # Ensure input folder exists
  if (!base::dir.exists(input_folder)) {
    base::stop("Input folder does not exist.")
  }

  # Detect all .FGB files in the folder
  fgb_files <- base::list.files(input_folder, pattern = "\\.fgb$", full.names = TRUE)

  if (base::length(fgb_files) == 0) {
    base::stop("No FGB files found in the specified folder.")
  }

  # Get existing layers in GDB if overwrite is FALSE
  existing_layers <- if (!overwrite && base::file.exists(gdb_path)) {
    base::tryCatch({
      sf::st_layers(gdb_path)$name
    }, error = function(e) base::character(0))
  } else {
    base::character(0)
  }

  # Process each FGB file
  for (fgb_file in fgb_files) {
    layer_name <- tools::file_path_sans_ext(base::basename(fgb_file))

    # Skip if layer exists and overwrite is FALSE
    if (!overwrite && layer_name %in% existing_layers) {
      base::message("Skipping ", layer_name, " as it already exists in the GDB.")
      next
    }

    # Read the FGB file and convert CRS
    sf_data <- sf::st_read(fgb_file, quiet = TRUE) |> sf::st_transform(crs)

    # Replace '.' in column names with '_'
    base::names(sf_data) <- base::gsub("\\.", "_", base::names(sf_data))

    # Write to the GDB
    sf::st_write(sf_data, dsn = gdb_path, layer = layer_name, driver = "OpenFileGDB", append = !overwrite, quiet = TRUE)

    base::message("Successfully written ", layer_name, " to ", gdb_path)
  }
}
