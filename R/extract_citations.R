# The main() function at the bottom ties everything together, takes a .pdf or .xml
# filename as input and outputs a data.frame of parsed citation results. Specifically,
# however, the primary goal of this code is to extract all the text between the publication
# section and the next section, and split the text appropriately into individual citation 
# strings.
#
# Get citation text
# -----------------
# We proceed down two paths to extract the citation text, either we successfully found section 
# names or we didn't. In the first case, we'll locate the publication section and extract all text 
# until the next section (which we know). In the other case, we'll walk through the CV looking for 
# the publication section, and then extract all text until we find any previously identified section 
# which isn't publication. 
#
# Parse citation text
# -------------------
# Now that we have the citation text, we'll attempt to parse citations using the following methods. 
# For each method, we'll check if the citation parse was successful and if not we proceed to the next 
# most likely parsing method.
# - pass 1: numbered list
# - pass 2: author’s name
# - pass 3: year ( used as a list )
# - pass 4: textbox ( using the grouping returned from pdfminer )
# - pass 5: left indentation
# - pass 6: most common starting word
#
# The known publication names are located: "~/Dropbox/GSR/parse_citations/text_files/publications.txt"
# The known section names are located: "~/Dropbox/GSR/parse_citations/text_files/section_names.txt"
#
# The parsed citation strings are passed to parse_citations.R and the crossref API.
#
# Note: when writing new functions to improve the code, be sure to use function fix_group_text() 
# to turn the original text into the text used for finding sections.

source("parse_citations.R")
source("libraries.R")

find_sequence = 
    # INPUT: numeric vector
    # OUTPUT: the longest sequential vector of numbers from the input
    # DOC: 
    #   - used to find a numbered list
    #   - searches both ascending and descending
    #   - longest sequence starting from 1 or longest descending
    #   - find_sequence( c(10, 4, 4, 3, 2, 2, 1) ) return c(4, 3, 2, 1)
    #   - find_sequence( c(1, 2, 4, 5, 6) ) return c(1, 2)
function( numbers, starting = 1 ) 
{
    # returns TRUE if we found a descending match
    find_match = function( biggest, sequence ) {
        tf = biggest:1 %in% sequence
        mean(tf) == 1
    }
    
    # after 5 iterations with no sequence found, return NULL
    if( (starting == 6) | (starting == length(numbers)) ) return( NULL )
    
    numbers = as.numeric(numbers)
    start = numbers[starting]
    
    # if the first number is 1, search ascending otherwise descending
    if( start == 1 ) {
        max_num = Inf
        num_temp = numbers
        while( max_num > 1 ) {
            max_index = which.max( num_temp )
            max_num = num_temp[max_index]
            if( find_match( max_num, numbers) ) {
                return( 1:max_num )
            } else {
                num_temp = num_temp[-max_index]
            }
        }
    } else {
        if( find_match( start, numbers) ) return( start:1 )
    }
    
    # no match found start with the second number and try again 
    find_sequence( numbers, starting + 1 )
}


extract_numbered_list = 
    # INPUT: each line of a the citation text
    # OUTPUT: NA, or the potential numbers in the numbered list
function(text) 
{
    nums = na.omit( str_extract( text, "^[[:digit:]]{1,2}[^[:digit:]]") )

    if( length(nums) == 0 ) return( NA )
    
    # exclude numbers from list that don't end with most common ending character
    last_char = str_extract( nums, ".$")
    keep_char = names( which.max( table(last_char) ) )
    nums = nums[ last_char == keep_char]
    
    as.numeric( gsub( "[^[:digit:]]", "", nums ) )
}


tf_numbered_list = 
    # INPUT: 
    #   - text: each line of the publication text
    #   - list_numbers: potential numbered list
    # OUTPUT: numeric vector of citation line groups
    # DOC:
    #   - if citation use a numbered list scheme, uses the numbers to 
    #   group each line of text into corresponding citations
    #   - example output: c(1, 2, 2, 3, 3, 3, 4, 4)
    #   - the 1 is most likely not a citation since it's only a single line
    #   while 2, 3, and 4 are the lines to combine to form those citations
function( text, list_numbers = extract_numbered_list( text ) ) 
{
    sequence = find_sequence(list_numbers)
    current_index = 1
    
    # count used for incrementing through groups vector
    count = 1
    groups = numeric( length(text) )
    
    for( string in text ) {
        # looking for a line of text starting with the next number in the list
        num_to_match = sequence[ current_index ]
        num = extract_numbered_list( string )
        
        skip = 
            if( is.na(num) | current_index > length(sequence) ) {
                TRUE
            } else {
                FALSE
            }
        
        # if we found the next number, increment groups and add current line to the next group
        if( !skip & num == num_to_match ) {
            current_index = current_index + 1
        }
        
        groups[count] = current_index
        count = count + 1
    }
    groups
    
    # for the last group
    if( which.max( groups ) + 2 <= length(groups) ) {
        groups[ (which.max( groups ) + 2) : length(groups) ] = max(groups) + 1
    }
    
    groups
}


right_left_groups = 
    # INPUT: 
    #   - df: data.frame with columns for line indentation
    #   - which_side: either "left" or "right
    #   - side_norm: either "left_norm" or "right_norm"
    # OUTPUT: grouping, see example from tf_numbered_list()
    # DOC:
    #   - used to create citation groups based on right and left side indentation
    #   - right side groups work when text is justified (currently not used)
function( df, which_side = "right", side_norm = "right_norm" ) 
{
    side = table( df[[side_norm]] )
    
    # most common indentation value
    value = names( which.max( side ) )
    
    # different methods to combine groups if "left" or "right
    side_tf = 
        if( which_side == "right" ) {
            side = as.integer( df[[side_norm]] == value ) 
            side + c( 0, side[-length(side)] ) == 2
        } else {
            side = as.integer( df[[side_norm]] != value ) 
            side + c( side[-1], 0 ) == 2
        }
    
    side[ side_tf ] = 0
    setNames( list( side, cumsum( side ) + 1 ), c("tf", "group") )
}

get_name_lines = 
    # INPUT: CV author's name and CV text
    # OUTPUT: 0/1 vector indicating if line contains the author's name
    # DOC: 
    #   - find lines with author's name (or et al)
    #   - use for grouping citations
function( cv_name, text, pass = 1 ) 
{
    min_names = 
        if( length(text) < 10 ) {
            1
        } else {
            round( length(text) / 10, 0 )
        }
    
    # find lines containing CV author's name
    rx = sprintf( "([^[:alpha:]]|^)%s([^[:alpha:]]|$)", cv_name )
    names_found = as.integer( grepl( rx, text, ignore.case = TRUE ) )
    
    if( sum(names_found) < min_names ) { 
        # if author's name not found, search again using "et al"
        if( pass == 1 ) {
            get_name_lines( "et al", text, 2 )
        } else {
            return( 0 )
        }
    }
    
    names_found
}

name_group = 
    # DOC: called after get_name_lines(), form citations groups 
    # by using the CV author's name
function( name_tf )
{
    # remove second line if back-to-back lines contain the author's name
    mask = ( name_tf + c( 0, name_tf[ -length(name_tf) ] ) ) == 2
    name_tf[ mask ] = 0
    
    cumsum( name_tf )
}

get_year_lines = 
    # INPUT: CV text
    # OUTPUT: 0/1 vector indicating if line contains a year
    # DOC: find all lines that begin with a year
function(text)
{
    text = trim( text )
    rx = "^(19[0-9]{2}|200[0-9]|201[0-9])($|[[:space:]])"
    
    as.integer( grepl( rx, text ) )
}

start_word_group = 
    # DOC: group CV text using the most common first word in the citation
    # similar idea to get_year_lines() above
function( cite_text ) 
{
    cite_text = tolower( trim( cite_text, punct = TRUE ) )
    first_word = str_extract( cite_text, "^[[:alpha:]]+?[[:space:],]" )
    
    # find the most common first word
    max_word = names( which.max( table( first_word ) ) )
    word_tf = na.fill( first_word == max_word, FALSE )
    
    cumsum( as.integer( word_tf ) )
}

get_citation_group = 
    # INPUT: 
    #   - df: data.frame with columns for citation groupings
    #   - method: c( "numbered", "bulleted", "left", "right", "start_word", "name", "year" )
    # OUTPUT: numeric vector for now to group citations
    # DOC: 
    #   - bulleted_list is not implemented
    #   - name refers to authors name
function( df, method ) 
{
    split_group = 
    if( method == "numbered" ) {
        df$numbered_list
    } else if( method == "bulleted" ) {
        df$bulleted_list
    } else if( method == "left" ) {
        df$left_group
    } else if( method == "right" ) {
        df$right_group
    } else if( method == "start_word" ) {
        df$start_word
    } else if( method == "name" ) {
        df$name
    } else if( method == "year" ) {
        df$year
    }
    
    split_group
}

combine_citations = 
    # INPUT: 
    #   - character vector of citation strings
    #   - groupings for citation text
    #
    # OUTPUT: citation strings combined into groups 
function( citation_text, groups ) 
{
    # split on groups and combine all text for each group
    text_groups = split( citation_text, groups )
    citations = sapply( text_groups, function(group) paste( group, collapse = " " ) )
    
    # some lines end with "-" for continuing a word and should be removed, other times the hyphen 
    # is supposed to be there. The below code attempts to fix this issue
    rx = "([[:alpha:]])- ([[:alpha:]])"
    citations = gsub( rx, "\\1\\2", citations )
    
    rx = "([[:digit:]])- ([[:digit:]])"
    citations = gsub( rx, "\\1-\\2", citations )
    
    # if citations are too long or short, remove
    cite_length = nchar( citations )
    mask = cite_length > 85 & cite_length < 375
    
    citations[mask]
}

##### NOTE ##### 
# The following three function: keep_values_between(), groups_by_period(), get_citation_textbox()
# are used to combine text using the textbox output by pdfminer

keep_values_between = 
    # INPUT: logical vector with 1 or 2 TRUE values
    # OUTPUT: logical vector with all TRUE values between the original two
function(tf)
{
    tf = cumsum(tf)
    tf[ tf > 1 ] = 0
    
    # remove first TRUE value
    tf[ which.max(tf) ] = 0
    as.logical(tf)
}

groups_by_period = 
    # INPUT: logical vector whether or not line ends with a period
    # OUTPUT: grouping for text ending with a period
function(period)
{
    period = as.integer(period)

    count = 1
    group = 1
    groups = NULL
    for( p in period ) {
        if( p == 0 ) {
            count = count + 1
        } else {
            groups = c( groups, rep(group, count) )
            group = group + 1
            count = 1
        }
    }
    c( groups, rep(group, count - 1) )
}

get_citation_textbox = 
    # INPUT: 
    #   - filename_xml: filename of the xml file
    #   - name of starting / ending sections
    # OUTPUT: parsed citations using textboxes
    # DOC: 
    #   - attempts to fix line breaks due to incorrect citation grouping by pdfminer
    #   and those due to page breaks.
    #   - combine lines so they end with a period
function( filename_xml, start_section, end_section )
{
    # for testing
    if( FALSE ) {
        filename_xml = cv_filename
        start_section = name_index$start_section 
        end_section = name_index$end_section
    }
    
    # get the textbox from original XML
    doc = xmlParse(filename_xml)
    textbox = xpathSApply(doc, "//textbox", xmlValue )
    
    # clean data in the same fashion as section names
    text_tf = fix_group_text( tolower(textbox) ) %in% c(start_section, end_section)
    text_tf = keep_values_between( text_tf )
    
    citation_text = textbox[ text_tf ]
    
    # remove blank lines
    citation_text = citation_text[ nchar(citation_text) > 0 ]
    
    # find lines ending with a period and group appropriately
    tf = grepl( "\\.$", citation_text )
    split_group = groups_by_period(tf)
    combine_citations( citation_text, split_group )
}


start_end_section = 
    # INPUT: 
    #   - text: all lines of text in CV
    #   - found_sections: character vector of section names (or NULL is none found)
    #   - pub_reg: regular expression for finding publication section
    #   - other_reg: regular expression for finding sections not publication
    # 
    # OUTPUT: 4 element list: "start_index", "start_section", "end_index", "end_section"
    #   - start_section will be a publication section
    #   - end_section will be the next section after publication
    #   - start_index, index of first line of text after start_section
    #   - end_index, index of last line of text before end_section
    #
    # DOC: if we found sections, we identify the publication section and next section
    # if no sections found, we walk through the text of CV and look for lines
    # that start with something like "publications." We then identify the next
    # section by looking for lines that begin with "any other section name."
function( text, found_sections, pub_reg, other_reg )
{
    # convert every space into a single space
    fix_space = function( vect ) {
        vect = gsub( "[ ]{2,}", ";:;", vect )
        vect = gsub( " ", "", vect )
        gsub( ";:;", " ", vect )
    }
    
    index = grep( pub_reg, found_sections )
    
    # attempt to fix spacing and find publication section, otherwise use start_end_no_section() method
    if( length(index) == 0 ) {
        index = grep( pub_reg, fix_space( found_sections ) )  
        
        if( length(index) == 0 ) return( start_end_no_section( text, pub_reg, other_reg ) )
    }
    
    # index location of the publication section, take minimum in case of multiple matches
    index = min( index )
    
    start_index = which( text == found_sections[index] ) + 1
    
    # either we take the next section, or publications is the last section
    # and we extract all text until the end of document
    end_index = 
        if( index == length(found_sections) ) {
            length(text)
        } else {
            # in case we have sections with the same name, take the smallest
            min( which( text == found_sections[index + 1] ) - 1 )
        }
    
    # also get the actual section names
    start_section = found_sections[ index ]
    end_section = 
        if( index == length(found_sections) ) {
            NULL
        } else {
            found_sections[index + 1]
        }
    
    list_names = c( "start_index", "start_section", "end_index", "end_section" )
    setNames( list( start_index, start_section, end_index, end_section ), list_names )
}

start_end_no_section = 
    # see start_end_section()
    # use if no sections found
function( text, pub_reg, other_reg )
{
    get_index_name = 
    function(text, mask, start = TRUE)
    {
        if( start ) {
            list_names = c( "start_index", "start_section" )
            add_term = 1
        } else {
            list_names = c( "end_index", "end_section" )
            add_term = -1
        } 
        
        if( sum(mask) > 0 ) {
            index = which.max( mask ) + add_term
            section = text[ index + (-1) * add_term ]
        } else {
            index = NULL
            section = NULL
        }
        setNames( list(index, section), list_names )
    }

    mask_pub = grepl( paste0("^", pub_reg), text )
    
    # if we can't find the publication section, return NULL
    if( sum(mask_pub) == 0 ) return( NULL )
    
    start = get_index_name( text, mask_pub, TRUE )
    
    # skipping index location of publications
    mask_pub = as.logical( cumsum( mask_pub ) )
    mask_pub[ start$start_index - 1 ] = FALSE
    
    # get first section that appears after publication
    other_mask = grepl( paste0("^", other_reg), text )
    other_mask = other_mask & mask_pub

    end = get_index_name( text, other_mask, FALSE )
    
    c( start, end )
}
    

get_section_locations = 
    # INPUT: 
    #   - text: all lines of text in CV
    #   - found_sections: character vector of section names (or NULL is none found)
    #   - pub_filename: file containing publication section
    #   - section_filename: file containing all other sections
    # OUTPUT: a list containing "start_index", "start_section", "end_index", "end_section" 
    # DOC: see start_end_section() for more details
function( text, found_sections, pub_filename, section_filename )
{
    # build regular expression from character vector of section names
    create_reg = function(vect) {
        vect = paste( vect, collapse = "|")
        sprintf( "(%s)", vect )
    }
    
    # for testing
    if( FALSE ) {
        text = df$text
        found_sections = output[[2]]
        pub_filename = "~/Dropbox/GSR/parse_citations/text_files/publications.txt"
        section_filename = "~/Dropbox/GSR/parse_citations/text_files/section_names.txt"
    }
    
    # sections found for publication section
    pub_sections = readLines( pub_filename )
    pub_reg = create_reg( pub_sections )
    
    # sections found for everything but publication section
    other_sections = readLines( section_filename )
    other_reg = create_reg( other_sections )
    
    text = fix_group_text( tolower(text) )
    
    name_index =
    if( length(found_sections) != 0 ) {
        start_end_section( text, found_sections, pub_reg, other_reg )
    } else {
        start_end_no_section( text, pub_reg, other_reg )
    }
    
    name_index
}

get_citations_helper = 
    # see get_citations()
function( df, method = "numbered" ) 
{
    # groups for left / right indentation
    right = right_left_groups( df )
    left = right_left_groups( df, "left", "left_norm" )
    
    df$right_side = right$tf
    df$right_group = right$group
    df$left_side = left$tf
    df$left_group = left$group
    
    df$start_word = start_word_group( df$text )
    df$name = name_group( df$name )
    df$year = cumsum( df$year )
    
    # no lines contian the author's name 
    if( method == "name" & sum(df$name) == 0 ) return( NULL )
    
    # get the group to split on and combine citations by that grouping
    split_group = get_citation_group( df, method )
    combine_citations( df$text, split_group )
}

get_citations = 
    # INPUT: 
    #   - filename for XML 
    #   - data.frame containing ways to group citation text
    #   - name_index: list containing "start_index", "start_section", "end_index", "end_section"    
    #
    # OUTPUT: character vector of citation strings
    #
    # DOC: possible methods (in decreasing chance of success)
    #   - pass 1: numbered list
    #   - pass 2: author’s name
    #   - pass 3: year ( used as a list )
    #   - pass 4: textbox ( using the grouping returned from pdfminer )
    #   - pass 5: left indentation
    #   - pass 6: most common starting word
function( filename, df, name_index, pass = 1 )
{
    # for testing
    if( FALSE ) {
        filename = cv_filename
        df = df_text
    }
    
    print( sprintf( "Citation Pass: %s", pass ) )
    # couldn't extract any citations
    if( pass == 5 ) return( NULL )
    
    citations = 
    if( pass == 1 ) {
        # groups for numbered list
        text = df$text
        nums = find_sequence( extract_numbered_list( text ) )
        
        # assume we don't have numbered list in this case
        if( length(nums) < 4 ) return( get_citations(filename, df, name_index, pass + 1 ) )
        
        df$numbered_list = tf_numbered_list( text, nums )
        
        get_citations_helper( df, "numbered" )
    } else if( pass == 2 ) {
        get_citations_helper( df, "name" )
    } else if( pass == 3 ) {
        get_citations_helper( df, "year" )
    } else if( pass == 4 ) {
        get_citation_textbox( filename, name_index$start_section, name_index$end_section )
    } else if( pass == 5 ) {
        get_citations_helper( df, "left" )
    } else if( pass == 6 ) {
        get_citations_helper( df, "start_word" )
    }
    
    # we didn't find any
    if( length(citations) < 1 ) {
        return( get_citations(filename, df, name_index, pass + 1 ) )
    }
    
    citations
}


main = 
    # INPUT: 
    #   - cv_filename: path to .pdf or .xml CV
    #   - cv_name: CV author's last name
    #   - pub_filename: file containing publication section
    #   - section_filename: file containing all other sections
    # OUTPUT: data.frame of results from crossref API, see parse_citations.R
    # DOC: our main function
    #   - finds sections, identifies publication and next section
    #   - extracts all citations text
    #   - groups citations text into individual citations
    #   - passes each citation to crossref API and returns those results
function( cv_filename, cv_name, pub_filename, section_filename, print_text = FALSE)
{
    # for testing
    if( FALSE ) {
        cv_name = "rubin"
        cv_filename = "~/Dropbox/GSR/CV_examples/CV_XML/rubincv.xml"
        pub_filename = "~/Dropbox/GSR/parse_citations/text_files/publications.txt"
        section_filename = "~/Dropbox/GSR/parse_citations/text_files/section_names.txt"
    }
    
    output = parse_cv( cv_filename, short_text = FALSE )
    df = output[[1]]
    found_sections = output[[2]]
    
    # contains: "start_index", "start_section", "end_index", "end_section"
    name_index = get_section_locations( df$text, found_sections, pub_filename, section_filename )
    
    if( print_text ) {
        # print sections using to extract citations
        # print( unlist(name_index) )
        print( t (as.data.frame(name_index) ) )
        cat( "\n" )
    }
    
    df_text = df[ name_index$start_index : name_index$end_index, c("left_norm", "right_norm", "text")]
    
    # add auther's name and year
    df_text$name = get_name_lines( cv_name, df_text$text )
    df_text$year = get_year_lines( df_text$text )
    
    if( print_text ) {
        # print first 10 citation lines
        print( df_text$text[1:10] )
    }
    
    # group text into citations
    citations = get_citations( cv_filename, df_text, name_index )
    citations = trim( citations )
    
    # extract doi and year (if they appear in citation)
    doi = get_doi( citations )
    year = get_year( citations )
    
    # make citations begin with a letter (remove numbering)
    citations = str_extract( citations, "[[:alpha:]].*" )
    
    citation_id = gsub( ".*/(.*)\\.xml$", "\\1", cv_filename )
    query_crossref( citations, doi, year, id = citation_id, cv_name = cv_name ) 
}
