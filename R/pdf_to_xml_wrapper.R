#!/usr/bin/env Rscript
# enable arguments from command line
args = commandArgs( trailingOnly = TRUE )

require(XML)
require(RCurl)

source("~/Dropbox/GSR/CVRead/R/procMiner.R")

pdf_to_xml_helper = 
    # INPUT:
    #   - input_full_path: full path to input .pdf file
    #   - output_full_path: full path to output .xml file
    #   - error_file: full path to the error log file
    #
    # OUTPUT: NULL, output is either saved as .xml or an entry is
    #   added to the error_file
    # DOC: converts pdf to xml, will be called via command line using a subset of the 
    # files to avoid R running out of memory issues (currently called from R)
function( input_full_path, output_full_path, error_file )
{
    mapply( function(pdf_doc, filename, error_file) {

        # in case we get an error with a file, store the filename and move on
        # use sink to suppress output and error message (we can ignore these)
        sink( "~/Documents/cv/temp_files/pdf_xml_output.txt", type = "output" )
        
        doc = try( pdfMinerDoc(pdf_doc), silent = TRUE)
        
        sink()
        
        if( class(doc) != "try-error" ) {
            saveXML( doc, filename )
        } else {
            # on error, write filename to error_file
            write( pdf_doc, error_file, append = TRUE, sep = "\n" )
        }
        return( NULL )
    }, input_full_path, output_full_path, error_file )
    
    return( NULL )
}

####################
# Running the Code #
####################

# args[i] are paths to files containing the files of interest
input_full_path = readLines( args[1] )
output_full_path = readLines( args[2] )
error_file = args[3]

if(TRUE) 
    pdf_to_xml_helper( input_full_path, output_full_path, error_file )
