# contains all libraries used by all other files
# - pdf_to_xml.R
# - extract_sections.R
# - extract_citations.R
# - parse_citations.R
#
# Note, for the XML package be sure to install latest XML from source
# download from http://www.omegahat.org/R/src/contrib/
# install.packages( "~/Downloads/XML_3.99-0.tar.gz", repos = NULL, type = "source" )

require(XML)
require(RCurl)
require(fulltext)
require(rplos)
require(aRxiv)
require(rentrez)
require(RJSONIO)
require(rcrossref)
require(jsonlite)
require(stringr)
require(magrittr)
require(zoo) 
require(png)
require(grid)
