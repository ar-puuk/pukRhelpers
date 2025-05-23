% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/convert_xml_html.R
\name{convert_xml_html}
\alias{convert_xml_html}
\title{Convert XML to HTML using XSLT}
\usage{
convert_xml_html(root, xml_filename, xsl_filename, output_filename)
}
\arguments{
\item{root}{A character string specifying the root directory where the XML and XSL files are located.}

\item{xml_filename}{A character string specifying the name of the XML file.}

\item{xsl_filename}{A character string specifying the name of the XSL file.}

\item{output_filename}{A character string specifying the name of the output HTML file.}
}
\value{
This function does not return a value but saves the transformed HTML file to the specified output path.
}
\description{
This function applies an XSLT transformation to an XML file and saves the output as an HTML file.
}
\examples{
\dontrun{
convert_xml_html("/path/to/files", "input.xml", "stylesheet.xsl", "output.html")
}

}
