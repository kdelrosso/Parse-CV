# The purpose of this code is take as input a list of publication citations and output
# a data.frame of results with columns for: title, authors, journal, doi, year, and several score metrics:
# - a score which is returned from crossref, unclear how this value is computed but above 1 appears good
# - a title_score, see check_title_v2()
# - a merge_score, see parse_results()
#
# We currently keep at most 5 results with the highest merge scores, and only keep results with a 
# matching CV author's name. The citation_rank column holds this information, i.e. citation_rank = 1
# is the best match and citation_rank = 5 is the worst match for a particular citation.

source("libraries.R")

get_doi = 
    # INPUT: list of citations
    # OUTPUT: 
    #   - vector the same length as input, with doi if found
    #   empty string otherwise
    # DOC: finding doi with regular expression
function(cite_list)
{
    # looks for pattern 10.****/...
    cite_list = gsub( "[[:space:]]", "", cite_list)
    reg = regexpr("10\\.[0-9]{4}/.+([[[:space:]],;.]|$)", cite_list) 
    doi = substring(cite_list, reg, reg + attributes(reg)$match.length)
    doi = gsub("[\\s,;\\.]+$", "", doi)

    doi 
}

get_year = 
    # INPUT: list of citations
    # OUTPUT: 
    #   - vector the same length as input, with year if found
    #   empty string otherwise
    # DOC: finding year with regular expression
function(cite_list)
{
    # building regular expression
    before_after_year = "[\\s,\\.;\\(\\)]"
    to_match = paste0( before_after_year, "19[0-9]{2}|200[0-9]|201[0-9]", before_after_year )

    reg = regexpr( to_match, cite_list )
    year = substring(cite_list, reg, reg + attributes(reg)$match.length)
    year = gsub("[^0-9]", "", year)
    
    year 
}


###### pass citation to anystyle #####

# Note: this code works, however the crossref method below works much better
# so parsing with anystyle isn't used anywhere currently

anystyle = 
    # INPUT: vector of citations to parse with anystyle
    #
    # OUTPUT: results in json format
    # DOC: writes a ruby file which calls Anystyle.parse, runs file with system call
function(cite_list, filename = "~/Dropbox/GSR/parse_citations/ruby/citations.rb") 
{
    # builts the text for the ruby file
    anystyle_ruby = function(x) c( 'print "\n"', sprintf('c = Anystyle.parse"%s"', x), "print c.to_json" )
    to_ruby = sapply(citations, anystyle_ruby)
    to_write = c("require 'anystyle/parser'", "require 'json'", to_ruby)
    
    # write and runs the ruby file, returns json output
    write(to_write, filename)  
    system( sprintf("ruby %s", filename), intern = TRUE)
}

json_to_df = 
    # INPTUT: vector of json strings
    # OUTPUT: data frame of the data contained in json strings
function(json_string)
{
    json_string = json_string[nchar(json_string) > 0]
    
    df_list = lapply( json_string, function(x) as.data.frame( fromJSON(x) ) )
    df_list
}

merge_df = 
    # perform a full outer join of all dfs in input list
function( list_of_dfs )
{
    if ( length(list_of_dfs) == 0 ) {
        return( NULL )
    } else if ( length(list_of_dfs) == 1 ) {
        return( list_of_dfs )
    }
    
    # initializing all_df with first df from list
    all_df = list_of_dfs[[1]]
    list_of_dfs[[1]] = NULL
    
    # each df in list is merged
    for (df in list_of_dfs){
        all_df = merge(x = all_df, y = df, all = TRUE) 
    }
    
    all_df
}

###### query crossref with citation ######

check_title_v2 = 
    # INPUT: 
    #   - titles: list of titles returned from crossref
    #   - citations: original citations
    # OUTPUT: pseudo percentage match between title and original citation
    # DOC: 
    #   - for each title in titles we return the minimum agrep distance needed for a match
    #   - we'll use 1 - distance to get a pseudo "percentage" title match 
function(titles, citation) 
{
    titles = tolower( gsub( "[^[:alnum:]]", "", titles ) )
    citation = tolower( gsub( "[^[:alnum:]]", "", citation ) )
    rx = sprintf( ".*%s.*", titles )
    
    distances = c(0.05, seq(0.1, 0.9, 0.1))
    indexes = unlist( lapply( rx, function(r) {
        
        # if we match exactly, return 1
        if( grepl( r, citation ) ) return( 1 )
        
        found = sapply( distances, function(d) {
            agrep( r, citation, max.distance = d, fixed = FALSE ) 
        })
        names(found) = distances
        val = 1 - as.numeric( names( which.min( unlist(found) ) ) )
        if( length(val) == 0 ) val = 0
        
        val
    }))
    
    if( length(indexes) == 0 ) {
		return( rep( 0, length(titles) ) )	
    }
	
    unname(indexes)
}


extract_more_info = 
    # INPUT: data.frame with parsed citation results
    # OUTPUT: data.frame with additional information extracted
    # DOC:
    #   - find citation counts from crossref via doi
    #   - extracts author and journal info from crossref full citation
function( df_result ) 
{    
    citation = df_result$fullCitation
    
    # extract authors
    rx_author = sprintf( "(^.*?),? ?%s.*", df_result$year )
    df_result$authors = mapply( function(rx, cite) gsub( rx, "\\1", cite ), 
                                rx_author, citation )
    
    # extract journal
    rx_journal = ".*<i>(.*)</i>.*"
    df_result$journal = gsub( rx_journal, "\\1", citation )
    
    # extract doi
    rx_doi = "http://dx.doi.org/(.*)"
    doi_vect = gsub( rx_doi, "\\1", df_result$doi )
    
    # get citation counts with doi via crossref
    citation_count = sapply( doi_vect, function(doi) { 
        cite_count = try( cr_citation_count(doi), silent = TRUE)
        
        if( class(cite_count) == "try-error" ) {
            # try second method to get doi, otherwise cite_count = NA
            doi = get_doi( df_result$doi )
            cite_count = try( cr_citation_count(doi), silent = TRUE)
            
            if( class(cite_count) == "try-error" ) cite_count = NA
        }
        cite_count
        })
        
    df_result$cite_count_crossref = citation_count
    df_result
}

search = 
    # INPUT: q (query), doi, and year as character strings
    # OUTPUT: a data.frame of search results
    # DOC: 
    #   - query crossref with given inputs, if no results found remove
    #    year/doi and search again (in case of errors with either)
function(q, doi = NULL, year = NULL, search_num, search = 1) 
{
    update_doi_year = 
        # if value has length 0 (i.e. "") or is NULL, return NULL
        # otherwise return value
    function( value ) {
        value = 
        if( is.null( value ) ) {
            NULL
        } else if( nchar( value ) == 0 ) {
            NULL
        } 
        
        value
    }
    
    if( FALSE ) print( c(substring(q, 1, 55), doi) )
    
    # if empty string or NULL, doi / year get NULL
    doi = update_doi_year( doi )
    year = update_doi_year( year )

    # query crossref
    df = cr_search(query = q, sort = "score", doi = doi, year = year, 
              type = "Journal Article", rows = search_num)
    
    # if no matches found, try again removing year and doi
    if( search == 1 ) {
        # if year already NULL, we won't re-search
        if( is.null( year ) ) {
            search = 2
        } else {
            year = NULL
        }
    } 
    
    if( search == 2 ) {
        # if doi already NULL, we won't re-search
        if( is.null( doi ) ) {
            search = 3
        } else {
            doi = NULL
        }
    }
    
    if( (nrow(df) > 0) | (search == 3) ) {
        return( df )
    }
    
    search(q, doi, year, search_num, search + 1)
}


parse_results = 
    # INPUT:
    #   - search_result: data.frame returned from crossref query
    #   - original_cite: original citation string
    # OUTPUT: data.frame of results filtered by various criteria
    # DOC:
    #   - we compute a title percentage score, see check_title_v2()
    #   - a merge score equal to title_score times score (original score returned from crossref)
    #   - we'll only keep results which contain the CV author's name
    #   - keep the top 5 results by merge score for each citation
function( search_result, original_cite, cv_name ) 
{
    keep_cols = c("doi", "score", "title", "year", "title_score", "merge_score", "fullCitation")
    all_cols = c(keep_cols, "authors", "journal", "cite_count_crossref")
    
    # if no search results returned, add one with NA values for original citation
    if( nrow(search_result) == 0 ) {
        search_result = as.list( rep( NA, length(all_cols) ) )
        names( search_result ) = all_cols
        search_result$original_citation = original_cite
        return( search_result )
    }

    # determine percentage of title matches with original citation    
    search_result$title_score = check_title_v2( search_result$title, original_cite )
    search_result$merge_score = search_result$score * search_result$title_score

    # get additional citation information including citation counts
    filtered_results = search_result[ ,keep_cols ]
    filtered_results = extract_more_info( filtered_results ) 
    
    # keep results which contain the CV author's name
    to_keep = grepl( cv_name, filtered_results$authors, ignore.case = TRUE )
    filtered_results = filtered_results[ to_keep, ]
    
    # if all results have been removed, return NULL
    if( nrow(filtered_results) == 0 ) return( NULL )
    
    # if found journal in the original citation
    filtered_results$journal_tf = as.integer( sapply( filtered_results$journal, function(j) {
        grepl( j, original_cite, ignore.case = TRUE ) 
        }) )
    
    # keep the top 5 results, ordered by title_score * score (via crossref)
    ordering = order( filtered_results$merge_score, decreasing = TRUE )
    filtered_results = head( filtered_results[ ordering, ], 5 )
    
    # include the original citation
    filtered_results$original_citation = original_cite
    
    # reset rownames, then include them as the ranking of the found citations
    rownames( filtered_results ) = NULL
    filtered_results$citation_rank = rownames( filtered_results )
    
    filtered_results
}

query_crossref = 
    # INPUT: 
    #   - list of citations, doi, and year
    #   - if no doi or year, an empty string ("") should be used
    #   - cv_name: the CV author's last name, see parse_results()
    #   - search_num: number of search results for crossref to return
    #   - id: unique identifier for each CV
    # OUTPUT: data.frame with parsed citation results
    # DOC: main function which calls all others for each citation
function(cite_list, doi_list, year_list, search_num = 10, id = "sample_id", cv_name)
{
    # query crossref with each citation
    results = mapply( function( c, d, y ) search( c, d, y, search_num, 1 ), 
                      cite_list, doi_list, year_list, SIMPLIFY = FALSE )
    
    # clean query results and return the top five for each citation
    parsed_citations = mapply( parse_results, results, cite_list, cv_name, SIMPLIFY = FALSE )
    
    # combine all results
    parsed_citations = do.call( rbind, parsed_citations )
    rownames( parsed_citations ) = NULL
    
    # add the citation_id
    parsed_citations$id = id
    parsed_citations
}

# for testing
if( FALSE ) {
    example_citation = "Shukla S., Safeeq M., AghaKouchak A., Guan K., Funk C., 2015, Temperature Impacts on the Water Year 2014 Drought in California, Geophysical Research Letters, 42, 4384- 4393, doi: 10.1002/2015GL063666."
    query_crossref( example_citation, "", "", cv_name = "AghaKouchak" )
}
