# R code overview

The main files are:

* pdf_to_xml.R - converts PDFs to XML
* extract_sections.R - finds sections from a parsed XML CV
* extract_citations.R - extracts individual citations from the publication section
* parse_citations.R - queries crossref API with citation strings

We also use the files:

* procMiner.R - Duncan's code called by pdf_to_xml.R to run pdfminer and clean XML nodes
* pdf_to_xml_wrapper.R - called by pdf_to_xml.R to deal with R memory issues
* libraries.R - all libraries needed for all files in this directory

Also see each file for a full description and the important functions. The most important functions overall are:

* pdf_to_xml() - from pdf_to_xml.R
* parse_cv() - from extract_sections.R
* get_section_locations() - from extract_citations.R
* get_citations() - from extract_citations.R
* query_crossref() - from parse_citations.R
* main() - from extract_citations.R (for running entire algorithm)

