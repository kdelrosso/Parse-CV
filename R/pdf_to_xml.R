# This code relies on pdfminer, available here: https://pypi.python.org/pypi/pdfminer/
# and some of Duncan's code available here: https://github.com/dsidavis/CVRead
#
# We are sourcing in Duncan's procMiner.R to use the function pdfMinerDoc(), which
# converts a PDF to an XML document via a system call, as well as some initial cleaning
# and organizing of the XML nodes. The purpose of the code is to convert PDF to XML in batch, 
# and save the XML using the convention original_filename.xml.
#
# Note 1: while trying to process thousands of pdfs at a time, R tends to crash after several 
# hours and running out of memory. The functions split_vect() and pdf_to_xml_helper() were added 
# to solve the issue. If running thousands of PDFs, set parameter via_cmd = TRUE in the 
# function pdf_to_xml().
#
# Note 2: the code is currently setup to handle PDFs only, not .doc / .docx at this time

require(XML)
require(RCurl)

source("~/Dropbox/GSR/CVRead/R/procMiner.R")
source("libraries.R")

# set path to pdfminer
options(PDF2TXT = "~/anaconda/bin/pdf2txt.py")

escape_char_fix = 
    # INPUT: character vector of length 1 and optionally a string of characters to fix
    # DOC: fixes filenames, adds '\' before special character to avoid error in bash
function(file, char_to_fix = "[]{}() ,!@$^&*`'")
{
    char_to_fix = unlist( strsplit( char_to_fix, "" ) ); char_to_fix
    
    # add '\' a forward slash before each char_to_fix
    for( c in char_to_fix ) {
        file = gsub( c, paste0("\\", c), file, fixed = TRUE )
    }
    file
}


update_file_list = 
    # INPUT: 
    #   all_files: all .pdf files in the input directory
    #   error_file: full path to the error log file
    #   output_dir: output directory with .xml files
    # OUTPUT: 
    #   - all remaining files from all_files not contained in either 
    #   the error log file or the output directory
function( all_files, error_file, output_dir )
{
    # change .xml to .pdf to compare with all_files
    xml_files = list.files( output_dir )
    xml_files = gsub( "\\.xml$", ".pdf", xml_files )
    
    # skip pdfs in error_file if it exists
    # keep only file name, not full path
    pdf_to_skip = 
        if( file.exists( error_file ) ) {
            readLines( error_file )
        } else {
            NULL
        }
    pdf_to_skip = gsub( ".*/(.*)", "\\1", pdf_to_skip )
    
    # combine results
    files_to_skip = union( xml_files, pdf_to_skip )
    
    # return vector of files we haven't processed yet
    setdiff( all_files, files_to_skip )
}

split_vect = 
    # INPUT: a number (len) and the size of each split
    # OUTPUT: vector of length len, containing
    #   - 1 repeated size times, 2 repeated size times, etc.
function( len, size = 1000 )
{
    n = ceiling( len / size )
    rep( 1:n, each = size)[ 1:len ]
}

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

pdf_to_xml = 
    # INPUT: an input and output directory location
    # DOC: 
    #   - batch convert .pdf file to .xml in the given directory
    #   - use option via_cmd = TRUE to run the main pdf_to_xml_helper() 
    #   function in a new R session via and command line and avoid the running
    #   out of memory issue
function( input_dir, output_dir = "../CV_XML", via_cmd = FALSE ) 
{
    # get all the pdf files in the input directory
    all_files = list.files(input_dir, pattern = "\\.pdf$")
    
    # create directory for xml files
    output_dir = file.path(input_dir, output_dir)
    dir.create(output_dir, showWarnings = FALSE)
    
    error_file = file.path(output_dir, "_error_log.txt")
    all_files = update_file_list( all_files, error_file, output_dir )
    
    # fix file names with problem characters
    all_files_pdf = sapply(all_files, escape_char_fix)
    
    # output file names (keep original names) and full paths
    new_filenames = gsub( "\\.pdf$", ".xml", all_files)
    output_full_path = file.path(output_dir, new_filenames)
    input_full_path = file.path(input_dir, all_files_pdf)
    n = length( input_full_path )
    
    input_full_path = split( input_full_path, split_vect( n ) )
    output_full_path = split( output_full_path, split_vect( n ) )
    n = length( input_full_path )
    
    start = proc.time()
    
    count = 1
    for( i in 1:n ) {
        
        # print progress
        print( sprintf( "%s of %s", count, n ) )
        print( (proc.time() - start) / 3600 )
        print( sprintf( "%s .xml files processed", length( list.files(output_dir) ) ) )
        
        # if via_cmd = TRUE, call pdf_to_xml_helper using the terminal
        # the input and output files are written to files, and the path to those files
        # is passed as arguments to pdf_to_xml_wrapper.R
        if( via_cmd ) {
            input_filename = "~/Documents/cv/temp_files/pdf_input_path.txt"
            output_filename = "~/Documents/cv/temp_files/xml_output_path.txt"
            
            write( input_full_path[[i]], file = input_filename, sep = "\n" )
            write( output_full_path[[i]], file = output_filename, sep = "\n" )
            
            cmd = sprintf( "Rscript pdf_to_xml_wrapper.R %s %s %s", 
                           input_filename, output_filename, error_file )
            
            system( cmd )
            
            # if still getting R crash issues, can manually kill R each time
            system( "killall R" )
        } else {
            pdf_to_xml_helper( input_full_path[[i]], output_full_path[[i]], error_file )
        }
        
        count = count + 1
    }
    
    # number of .pdf files converted to .xml
    length( list.files(output_dir) )
}


####################
# Running the Code #
####################

if(FALSE) {
    dir = "~/Documents/cv"
    university = list.files( dir )[[4]]
    input_dir = file.path( dir, university )
    input_dir
    output_dir = paste0( "../", university, "_xml" )
    output_dir
    
    pdf_to_xml( input_dir, output_dir, via_cmd = TRUE )
}
