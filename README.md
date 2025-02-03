# **pukRhelpers**

[![License](https://img.shields.io/github/license/ar-puuk/pukRhelpers)](https://opensource.org/license/GPL-3-0)

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

After installation, load the package with:

``` r
library(pukRhelpers)
```

## **Usage**

Once you’ve installed the package, you can start using its functions immediately.

Here is an example of using one of the functions in pukRhelpers:

-   `convert_fgb_to_gdb()`: Convert FlatGeoBuf (FGB) Files to Geodatabase (GDB) Layers
-   `convert_xml_html()`: Convert XML to HTML using XSLT
-   `extract_html_table()`: Extract table following a specific anchor tag by name attribute
-   `geo_split_lines()`: Split Lines by Maximum Length (Credit: [dblodgett-usgs](https://gist.github.com/dblodgett-usgs))
-   `load_esri()`: Load and Query an ESRI Feature Service Layer
-   `load_kml_sf()`: Convert KML to Simple Features (sf) Object with Preserved Attributes (Credit: [mdsumner](https://gist.github.com/mdsumner))
-   `load_packages()`: Load and Install R Packages
-   `load_ugrc_data()`: Load Dataset from UGRC SGID
-   `load_ugrc_vars()`: Load List of Datasets from UGRC SGID
-   `st_aggregate()`: Aggregate sf objects (Credit: [rCarto](https://gist.github.com/rCarto))
-   `st_split_lines()`: Split Lines by Maximum Length (Credit: [dblodgett-usgs](https://gist.github.com/dblodgett-usgs))

### Available Functions

For a full list of functions and examples, check out the package [documentation](https://ar-puuk.github.io/pukRhelpers/reference/).

## **Contributing**

Contributions are welcome! If you have any suggestions or improvements, feel free to open an issue or submit a pull request.

### How to contribute

1.  Fork this repository.
2.  Create a new branch for your feature or fix.
3.  Submit a pull request with a detailed description of your changes.

Please ensure that your code adheres to the [Tidyverse style guide](https://style.tidyverse.org/) and passes any unit tests if applicable.

## **License**

This package is licensed under the [GPL-3 License](https://github.com/ar-puuk/pukRhelpers/blob/master/LICENSE.md). See the [LICENSE](https://github.com/ar-puuk/pukRhelpers/blob/master/LICENSE) file for more details.

## Acknowledgements

Thanks to the R community and contributors for inspiring and building great packages that made the development of **pukRhelpers** possible.
