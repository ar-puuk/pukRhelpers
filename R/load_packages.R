#' @title Load and Install R Packages
#'
#' @description
#' This function checks if the specified R packages are installed and loads them.
#' If a package is not installed, it attempts to install it from the specified CRAN repositories.
#'
#' @param ... A list of package names to be installed and/or loaded. Names should be provided as unquoted identifiers.
#' @param silent Logical, default is FALSE. If TRUE, suppresses printing of package loading statuses.
#'
#' @details
#' This function ensures that all specified packages are installed and loaded in the current R session.
#' Missing packages are automatically installed from CRAN. The function also provides an option
#' to suppress output messages.
#'
#' @return A named logical vector indicating whether each package was successfully loaded.
#'
#' @examples
#' \dontrun{
#' # Example 1: Load and install ggplot2 and dplyr
#' load_packages(ggplot2, dplyr)
#'
#' # Example 2: Load packages silently
#' load_packages(ggplot2, dplyr, silent = TRUE)
#' }
#'
#' @export
load_packages <- function(..., silent = FALSE) {
  # Convert input arguments to a character vector of package names
  packages <- base::as.character(base::substitute(base::list(...)))[-1]

  # Function to check if a package is installed and load it if available
  requirePkg <- function(pkg) {
    if (base::length(base::setdiff(pkg, base::rownames(utils::installed.packages()))) == 0)
      require(pkg, quietly = TRUE, character.only = TRUE)
  }

  if (base::length(packages) == 0) {
    stop("No package names provided. Input must contain package names to install and load")
  }

  # Install missing packages
  if (base::length(base::setdiff(packages, base::rownames(utils::installed.packages()))) > 0)
    utils::install.packages(base::setdiff(packages, base::rownames(utils::installed.packages())),
                     repos = c("https://cloud.r-project.org", "http://owi.usgs.gov/R/"))

  # Load the packages and store results in 'res'
  res <- base::unlist(base::sapply(packages, requirePkg))

  # Print package installation and loading status
  if (!silent && !base::is.null(res)) {
    cat("\nPackage Installation and Loading Status:\n\n")
    status <- base::sapply(packages, function(pkg) pkg %in% base::names(res))
    base::print(status)
  }

  # Return the loading results
  return(res)
}
