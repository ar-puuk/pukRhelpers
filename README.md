# **pukRhelpers**

<!-- badges: start -->
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/ar-puuk/pukRhelpers/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ar-puuk/pukRhelpers/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/ar-puuk/pukRhelpers/graph/badge.svg)](https://app.codecov.io/gh/ar-puuk/pukRhelpers)
<!-- badges: end -->

**pukRhelpers** is a collection of helper functions designed to streamline and simplify workflows in R. This package compiles useful utilities that I've used across various projects, providing convenience for data manipulation, analysis, and visualization tasks.

------------------------------------------------------------------------

## **Installation**

You can install the **pukRhelpers** package from GitHub using one of the following methods, depending on the package manager you prefer.

### Install using `devtools`

``` r
# install.packages("devtools")
devtools::install_github("ar-puuk/pukRhelpers")
```

### Install using `remotes`

``` r
# install.packages("remotes")
remotes::install_github("ar-puuk/pukRhelpers")
```

### Install using `pak`

``` r
# install.packages("pak")
pak::pak("ar-puuk/pukRhelpers")
```

### Install from `r-universe`

``` r
install.packages('pukRhelpers', repos = 'https://ar-puuk.r-universe.dev')
```

After installation, load the package with:

``` r
library(pukRhelpers)
```

## **Usage**

Once you’ve installed the package, you can start using its functions immediately.

Here is an example of using one of the functions in `pukRhelpers`:

``` r
# Work in progress
```

### Available Functions

For a full list of functions and examples, check out the package [documentation](https://ar-puuk.github.io/pukRhelpers/reference/).

-   `convert_fgb_to_gdb()`: Convert FlatGeoBuf (FGB) Files to Geodatabase (GDB) Layers
-   `convert_xml_html()`: Convert XML to HTML using XSLT
-   `extract_html_table()`: Extract table following a specific anchor tag by name attribute
-   `geo_split_lines()`: Split Lines by Maximum Length (Credit: [dblodgett-usgs](https://gist.github.com/dblodgett-usgs/cf87392c02d73f1b7d16153d2b66a8f3))
-   `load_esri()`: Load and Query an ESRI Feature Service Layer
-   `load_kml_sf()`: Convert KML to Simple Features (sf) Object with Preserved Attributes (Credit: [mdsumner](https://gist.github.com/mdsumner/1469b4ab53058e33bafc4fd9cda454eb))
-   `load_packages()`: Load and Install R Packages
-   `load_ugrc_data()`: Load Dataset from UGRC SGID
-   `load_ugrc_vars()`: Load List of Datasets from UGRC SGID
-   `st_aggregate()`: Aggregate sf objects (Credit: [rCarto](https://gist.github.com/rCarto/bb47aff0a02e808d2bf64f2d8c5db7d8))
-   `st_split_lines()`: Split Lines by Maximum Length (Credit: [dblodgett-usgs](https://gist.github.com/dblodgett-usgs/cf87392c02d73f1b7d16153d2b66a8f3))
-   `arc_read_encode()`: Enables `arcgislayers::encode_field_value()` as an internal argument to the `arcgislayers::arc_read()` to avoid unnecessary objects in the environment and simpler workflow. (See [arcgislayers/237](https://github.com/R-ArcGIS/arcgislayers/issues/237) and [arcgislayers/234](https://github.com/R-ArcGIS/arcgislayers/discussions/234))

## **Contributing**

Contributions are welcome! If you have any suggestions or improvements, feel free to [open an issue](https://github.com/ar-puuk/pukRhelpers/issues/new) or submit a [pull request](https://github.com/ar-puuk/pukRhelpers/compare).

### How to contribute

1.  Fork this repository.
2.  Create a new branch for your feature or fix.
3.  Submit a pull request with a detailed description of your changes.

Please ensure that your code adheres to the [Tidyverse style guide](https://style.tidyverse.org/) and passes any unit tests if applicable.

## **License**

This package is licensed under the [GPL-3 License](https://github.com/ar-puuk/pukRhelpers/blob/master/LICENSE.md). See the [LICENSE](https://github.com/ar-puuk/pukRhelpers/blob/master/LICENSE) file for more details.

## Acknowledgements

Thanks to the R community and contributors for inspiring and building great packages that made the development of **pukRhelpers** possible.
