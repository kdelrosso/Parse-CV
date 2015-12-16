# This code takes a .pdf or .xml file as input and attempts to identify all the CV
# section names. We first extract important text features from every line in the CV, 
# features such as text size, capitalization, indentation, font, bold/italics, etc.
# We then group the text based on common features and try to identify a single group
# that contains all the sections by comparing each group with a known list of common
# section names. The main function is parse_cv().
#
# Specifically, on the first pass we'll group using the text features:
# - capitalization
# - text size ( not rounded )
# - bold
# - italics
# - left indentation ( rounded )
#
# The goal of the first grouping is to identify capitalized, bolded / italicized, or larger 
# text size section titles which have the same left indentation. We achieved better results
# using the rounded left indentation since many CVs are not super precise with their margins.
# We chose to use the non-rounded text size since CVs may use different text sizes which are
# very near one another.
#
# On the second pass we remove the left indentation requirement and search for centered text. 
# We chose to stop after two passes since we were able to extract most sections. We also
# have a secondary methed to find the publication later on, so this step doesn't need to be 100%.
#
# The known section names are located: "~/Dropbox/GSR/parse_citations/text_files/section_names.txt"
#
# If unable to find sections in future CVs:
#   - add more passes to cv_search_tree() with different ways to group the text,
#   possibly using the rounded text size.
#   - look for match_sum in cv_search_tree() and the related note

source("pdf_to_xml.R")
source("libraries.R")

# for reading in saved section names from a txt file
common_sections = function(filename) {
    readLines(filename)
}

bbox_to_df = 
    # INPUT: character vector of bbox location
    # OUTPUT: data.frame with numeric columns
    #   columns: left, bottom, right, top
    #   bottom and top are measured from the bottom of page
    # DOC: bbox is an XML attribute of the text, it defines a 
    #   region on the page containing text.
function(bbox)
{
    df = do.call( rbind, strsplit( bbox, ",") )
    df = apply(df, 2, as.numeric)
    df = as.data.frame( df )
    
    set_colnames( df, c("left", "bottom", "right", "top"))
}

size_text = 
    # INPUT: XML doc and a page number
    # OUTPUT: the text size from every line of the doc
    # DOC: currently assigns the text size as the most common character size
function(doc, page_num)
{
    # get charcterSizes for every character in a textline on page page_num
    char_size = xpathSApply(doc, sprintf("//page[@id=%s]//textbox/textline", page_num), xmlGetAttr, "charcterSizes")
    by_line = strsplit( char_size, ",")
    
    # we'll give a line the size of it's most common character size
    common_size = lapply( by_line, function(line) names( which.max( table(line) ) ) )
    unlist(common_size)
}

get_text_info = 
    # INPUT: XML doc and a page number
    # OUTPUT: 
    #   - data.frame of text lines, in the correct order (from top to bottom)
    #   - currently contains columns for text, text location, size, and font
    # DOC: pdfminer creates textboxes, which don't necessarily go line-by-line
    # thus it's possible for lines to be shuffled in their vertical order. Taking the document
    # page-by-page and ordering the text by their vertical location solves this issue.
function(doc, page_num = 8)
{
    # get bbox dimensions for every textline on page page_num, convert to a data frame
    bbox = xpathSApply(doc, sprintf("//page[@id=%s]//textbox/textline", page_num), xmlGetAttr, "bbox")
    bbox_df = bbox_to_df(bbox)
    
    bbox_df$text = xpathSApply(doc, sprintf("//page[@id=%s]//textbox/textline/text", page_num), xmlValue)
    bbox_df$text_size = as.numeric( size_text(doc, page_num) )
    
    bbox_df$font = xpathSApply(doc, sprintf("//page[@id=%s]//textbox/textline/text", page_num), xmlGetAttr, "font")
    
    # make sure textline are in the correct order
    ordering = order( bbox_df$top, decreasing = TRUE)
    bbox_df[ordering, ]
}

white_space = 
    # INPUT: a data.frame with columns for text location ( specifically top and bottom )
    # OUTPUT: 2 element list, with space above and below text
    # DOC: still working to understand the spacing (i.e. sometimes we get negative values??)
    # not an issue with the code but the info from pdfminer
function(df)
{
    top = df$top
    bottom = df$bottom
    
    # at the top/bottom of a page we'll mark with NA
    below = bottom - c(top[-1], NA)
    above = c(NA, bottom[-length(bottom)]) - top

    list(below = below, above = above)    
}


all_caps = 
    # INPUT: data frame with text as column
    # OUTPUT: the longest continuous all caps text
function(df, text_col = "text")
{
    text = df[[text_col]]
    setNames( sapply( text, find_upper ), NULL )
}

find_upper = 
    # INPUT: character vector
    # OUTPUT: character vector with only upper case text from beginning of a line
function( text, min_length = 6 )
{
    # for simplicity, remove all punctuation
    text = remove_punct(text)
    text = trim( str_extract( text, "^[[:upper:]\\s]+") )
    if( is.na(text) | nchar(text) < min_length ) return("")
    
    text
}


trim = 
    # trim leading and training whitespace from a character vector,
    # also remove punctuation with punct = TRUE
function( char_vector, punct = FALSE) 
{
    reg = 
        if( punct ) {
            "[[:space:][:punct:]]"
        } else {
            "[[:space:]]"
        }
    
    rx = sprintf( "^%s+|%s+$", reg, reg )
    gsub( rx, "", char_vector )
}

# remove everything not alphanumeric or space
remove_punct = function(char_vector) {
    gsub( "[^[:alnum:][:space:]]", "", char_vector )
}


df_per_page = 
    # INPUT: XML doc and a page number
    # OUTPUT: a data.frame with text features as columns
    # DOC: calls previous functions to get text, capitalization, and spacing info
    # for each line of text on a page
function(doc, page_number)
{
    # get text and location info
    df = get_text_info(doc, page_number)
    
    # get capitalization info
    df$caps = all_caps( df )
    df$caps_tf = as.integer( nchar( df$caps ) != 0 )
    
    # get spacing info
    space = white_space(df)
    df$above = space$above
    df$below = space$below
    
    df$page_num = page_number
    
    df 
}

doc_to_df = 
    # INPUT: XML doc and option to make text short (for display)
    # OUTPUT: data.frame with relevant text features
    # DOC: features / columns include:
    #   - text, number of characters, and text size
    #   - font, bold / italics, and capitalization
    #   - left / right indentation and spacing above / below
function(doc, short_text = FALSE)
{
    # get the number of pages
    root <- xmlRoot(doc)
    num_pages = as.numeric( xmlGetAttr(root, "numPages") )
    
    # create a df for each page, return combined result
    df_list = lapply( 1:num_pages, df_per_page, doc = doc  )
    df = do.call(rbind, df_list)
    
    df$n_char = nchar( df$text )
    
    # remove blank and lines with only spaces (i.e. don't contain alphanumeric or punctuation)
    df = df[ grepl("[[:alnum:][:punct:]]", df$text), ] 
    
    # round text size and indentation for text grouping used later on
    df$text_size_norm = round( df$text_size, 1 )
    df$left_norm = round(df[, "left"], 0)
    df$right_norm = round(df[, "right"], 0)
    
    # bold / italics
    df$font_bold = as.integer( grepl("bold", df$font, ignore.case = TRUE) )
    df$font_italic = as.integer( grepl("italic", df$font, ignore.case = TRUE) )
    
    # get spacing above / below the line of text
    pos_above = na.fill( df$above, Inf ) > median(df$above, na.rm = TRUE)
    pos_below = na.fill( df$below, Inf ) > median(df$below, na.rm = TRUE)
    df$space_ab = as.integer( pos_above & pos_below )
    
    # shorten text to first 20 characters (nicer to display in R)
    if( short_text ) {
        df$text = substring(df$text, 1, 20)
    }
    
    df
}

# append additional section names the current file
update_section_names = 
function(new_sections, filename = "~/Dropbox/GSR/parse_citations/text_files/section_names.txt")
{
    write( tolower(new_sections), filename, append = TRUE)
}

# count all words in a character vector
word_count = function(vect) {
    length( unlist( strsplit(vect, "[[:space:]]") ) )
}

search_section_names = 
    # INPUT: character vector and list of section names
    # OUTPUT: either the sum or percentage, depending on use_sum
    # DOC: 
    #   - returns the total number or percentage of elements in vect 
    #   that are in sections_names
function(vect, section_names, use_sum = TRUE)
{
    vect = tolower( as.character(vect) )
    sections_found = vect %in% section_names
    
    if( use_sum ) {
        return( sum(sections_found) )
    } else {
        return( mean(sections_found) )
    }
}

# use number of pages to estimate the minimum allowed sections
get_min_sections = function(df) {
    pages = max(df$page_num)
    
    # min_sections in the interval (3, 7)
    min_sect = 0.5 * pages + 2
    min_sect = max( c(3, min_sect) )
    min_sect = min( c(7, min_sect) )
    
    floor(min_sect)
}


# ask for user input to identify sections (not currently used)
get_user_input = function(group_by_list) {
    print(group_by_list)
    
    n = length(group_by_list)
    index = Inf
    while( index > n ) {
        cat("\n", "Enter group number: (-1 for none)", "\n")
        index = as.numeric( readline() )
        if(index == -1) return(NULL) 
        if(index == 0) index = Inf
    }
    tolower( group_by_list[[index]] )
}


fix_group_text = 
    # fix leading and trailing white space
    # use fix = TRUE to fix " T H I S  E X A M P L E "
function(vect, fix = FALSE)
{
    vect = trim( remove_punct( as.character(vect) ) )
    if( fix ) {
        vect = sapply( strsplit(vect, "  "), function(text) {
            paste( gsub("[[:space:]]", "", text), collapse = " " ) 
            })
    }
    vect
}


compare_with_sections = 
    # INPUT:
    #   - group_by: list of character vectors, for each group
    #   - section_names: previously found sections names to match against
    #   - min_match: use to determine how many sections_names must be found in a group 
    #
    # OUTPUT: TRUE/FALSE mask if the number of matched elements is greater than 1
    # DOC: for each character vecter in group_by, get the count of the number of occurances in section_names
function( group_by, section_names, min_match = 1 )
{
    match_sum = sapply( group_by, function(vect) search_section_names( vect, section_names ) )
    mask = match_sum > min_match
    if( sum(mask) == 0 ){
        # similar to above, but we first fix the character vector and then search
        # see fix_group_text() for the fix used
        match_sum = sapply( group_by, function(vect) {
            search_section_names( fix_group_text(vect, fix = TRUE), section_names ) 
            })
    }
    match_sum
}

cv_search_tree =
    # INPUT:
    #   - df: data.frame with features for text
    #   - section_names: found section names to compare with groups that are created
    #   - group_list: the ways to group the text features
    #
    # OUTPUT: the sections found ( if any )
    #
    # DOC: 
    #   - function is called somewhat recursively, by specifying a new group_list and iterating pass
    #   - pass 1, we'll use uppercase and left justified as a give away for sections
    #   - pass 2, we'll search for centered text and add font and grouping variable
function(df, section_names, group_list, min_sections = get_min_sections(df), max_sections = 50, 
         pass = 1, to_print = FALSE) 
{
    if( to_print ) print( sprintf( "Pass: %s", pass) )
    
    # for testing
    if( FALSE ) {
        min_sections = get_min_sections(df)
        max_sections = 50
        group_list = list(df$caps_tf, df$text_size, df$font_bold, df$font_italic, df$font)
    }
    
    # group_by contains all the text for each group found
    group_by = aggregate( df$text, by = group_list, function(x) x, simplify = FALSE )$x
    
    # filter group_by list, to those between min/max section length, and convert to character
    mask = sapply(group_by, function(vect) (length(vect) > min_sections) & (length(vect) < max_sections) )
    
    # remove punctuation and leading and trailing white space
    group_by = unname( lapply( group_by[mask], fix_group_text ) )
    
    # print group_by, used with 5 examples doc
    if( to_print ) {
        group_by_tmp = group_by
        group_names = sprintf( "group %s", 1:length(group_by_tmp) )
        names( group_by_tmp ) = group_names
        
        # make all groups have the same length, pad with "" is not
        max_length = max( sapply( group_by_tmp, length ) )
        mapply( function(group, name) {
            df = as.data.frame(group)
            colnames(df) = name
            print(df)
            cat("\n")
            }, group_by_tmp, names(group_by_tmp) )
    }

    # can adjust this parameter, important to determine when a group is considered the sections
    match_sum = compare_with_sections( group_by, section_names )
    mask = match_sum > 1
    
    if( length(group_by) != 0) {
        if( sum(mask) != 0 ) {
            # if more than one potential section group, take the one with the most matches
            mask = which.max( match_sum )
            return( tolower( group_by[mask][[1]]) )
        } 
    }
    
    if( pass == 1 ) {
        # searching for centered text, also adding font
        group_list = list(df$caps_tf, df$text_size, df$font_bold, df$font_italic, df$font)
        cv_search_tree(df, section_names, group_list, min_sections, max_sections, pass = 2)
    } else if ( pass == 2 ){
        # next pass here
    }  
}


fix_sections = 
    # remove common sections which often have similar features to section names
function( sections )
{
    to_replace = c( "curriculum vitae", "mailing address", "address", "affiliations", 
                    "contact", "contact information")
    
    if( length(sections) > 0 ) {
        
        to_replace = paste( to_replace, collapse = "|" )
        sections = grep(to_replace, sections, invert = TRUE, value = TRUE)
        
        # in case we included a line with their name (using the assumption it's first)
        if( grepl("phd", sections[1]) ) sections = sections[-1]
        
    }
    sections
}

is_column_format = 
    # INPUT: data.frame of text features
    # OUTPUT: TRUE / FALSE
    # DOC: 
    #   - return TRUE if CV is column format, FALSE otherwise
    #   - skip the first and last n rows while determining column format
function( df, n = 30 ) 
{
    # remove first / last n rows of text
    end = nrow( df ) - n
    rownames( df ) = NULL
    df = df[ n : end, ]
    
    # split by section text
    df_split = split( df, df$section_tf )
    
    most_right_section = max( df_split$'1'$right )

    # find that minimum left indentation which is used more than 5 times
    mc_left = table( df_split$'0'$left )
    most_left_text = min( as.numeric( names( mc_left[ mc_left > 5 ] ) ) )
    
    # we found column format
    if( most_right_section < most_left_text ) return( TRUE )
    
    FALSE
}

group_section_lines = 
    # DOC: creates a numeric vector for grouping column CV sections
    #   - changes for example 0010101001011010100 into 0011111002222033300
    #   - used by column_format()
function( section_tf )
{
    new_section_tf = numeric( length(section_tf) )

    one_count = 0
    group = 1
    pass = FALSE
    
    for( i in 1:(length( section_tf ) - 2) ) {
        
        if( section_tf[i] == 1 ) {
            one_count = one_count + 1
            new_section_tf[i] = group
            
            # if following a 1 we have two consecutive zeros the group ends
            if( section_tf[i + 1] + section_tf[i + 2] == 0 ) pass = TRUE
            
        } else if( one_count != 0 ) {
            new_section_tf[i] = group
        }
    
        if( one_count == 3 | pass ) {
            one_count = 0
            group = group + 1
            pass = FALSE
        }
    }
    
    new_section_tf
}


column_format = 
    # INPUT: data.frame of text features and section names found
    # OUTPUT: list containing the inputs, updated if column format
    # DOC: 
    #   - first we determines if a CV is column format and makes changes if so
    #   - we combine section names on different lines (but which are one section name)
    #   - we re-position section names above their section's text
function( df, sections )
{
    text = tolower( fix_group_text( df$text ) )
    section_tf = as.integer( text %in% sections )
    
    df$section_tf = section_tf
    
    # if we have column format, we'll look for sections in sections_tf with pattern 101 or 10101
    # and merge section titles; we'll merge at most three consecutive lines
    if( is_column_format( df ) ) {
        section_groups = group_section_lines( section_tf )
        
        for( group in 1:max(section_groups) ) {
            tmp_tf = section_groups == group & section_tf
            section_text = paste( df$text[ tmp_tf ], collapse = " " )
            first = which.max( tmp_tf )
            
            df[ first, "text" ] = section_text
            tmp_tf[first] = FALSE
            
            # will remove all these NA rows at the end
            df[ tmp_tf, ] = NA
        }
        
        # remove rows which we just combined
        df = na.omit( df )
        
        # update the new section names
        text = df$text[ as.logical( df$section_tf ) ]
        sections = tolower( fix_group_text( text ) )
    }
    
    list( df, sections )
}


parse_cv =
    # INPUT:
    #   - filename: path to CV in either .pdf or .xml format
    #   - section_filename: path to .txt file containing found section names (one per line)
    #   - short_text: TRUE/FALSE whether or not to include the full line text in df
    #
    # OUTPUT: 
    #   - 2 element list: a data.frame with text features and the sections found
    #
    # DOC: 
    #   - main function calls all other function
    #   - contains two blocks of test code of exploring XML, and the other functions
function(filename, section_filename = "~/Dropbox/GSR/parse_citations/text_files/section_names.txt", 
         short_text = FALSE, print_sections = FALSE)
{
    # for testing
    if( FALSE ) {
        filename = "~/Dropbox/GSR/CV_examples/CV_XML/rubincv.xml"
        section_filename = "~/Dropbox/GSR/parse_citations/text_files/section_names.txt"
        short_text = FALSE
    }
    
    # get the file extension
    extension = tolower( substring( filename, nchar(filename) - 3) )
    
    # parse xml
    doc = 
        if( extension == ".pdf" ) {
            pdfMinerDoc(filename)
        } else if( extension == ".xml" ) {
            xmlParse(filename)
        } else {
            stop( "Input file must be either .pdf or .xml" )
        }
    
    # for testing / exploring XML
    if( FALSE ) {
        getNodeSet(doc, "//page[@id=1]//textbox")
        xpathSApply(doc, "//page[@id=1]//textbox/textline", xmlGetAttr, "bbox")
    }
    
    df = doc_to_df( doc, short_text )
    
    # section_names are from file, new_sections are those found in CV
    section_names = unique( common_sections( section_filename ) )
    
    # initial groups for searching for subsections
    group_list = list(df$caps_tf, df$text_size, df$font_bold, df$font_italic, df$left_norm)
    new_sections = cv_search_tree(df, section_names, group_list, to_print = FALSE)
    
    new_sections = fix_sections( new_sections )
    
    if( print_sections ) {
        print( new_sections )
        cat("\n\n")
    }
    
    # update df if we have a column_format
    df$section_tf = 0
    
    # the return from this function is the format list( df, new_sections )
    column_format( df, new_sections )
}

####################
# Running the Code #
####################

# Results from extracting sections from "~/Documents/cv/ucrecruit_ucb_xml/"
# -------------------------------------------------------------------------
# total files: 45606
# found citation count: 38880, percent: 0.855
# error count: 1173
# time: 2.7 hours
# look at number_of_sections.png for histogram of the number of sections extracted from each CV

if( FALSE ) {
    filenames = list.files( "~/Documents/cv/ucrecruit_ucb_xml/", pattern = "\\.xml$", full.names = TRUE )
    head( filenames )
    
    total = 1
    count = 1
    error_count = 1
    section_length = list()
    
    start = proc.time()
    for( f in filenames ) {
        
        if( ( total %% 500 ) == 0 ) {
            tmp = sprintf( "count: %s, perc.: %s", count, count / total )
            print( tmp )
            print( sprintf( "error count: %s", error_count ) )
            print( ( proc.time() - start ) / 60 )
            hist( unlist( section_length ) )
        }
        
        output = try( parse_cv( f ), silent = TRUE)
        
        if( class(output) != "try-error" ) {
            n = length( output[[2]] )
        } else {
            # on error save filename
            e_filename = "~/Dropbox/GSR/parse_citations/text_files/section_errors.txt"
            write( f, file = e_filename, sep = "\n", append = TRUE )
            error_count = error_count + 1
        }
        
        if( n > 3 ) count = count + 1
        section_length[[ total ]] = n
        
        total = total + 1
    }
}
