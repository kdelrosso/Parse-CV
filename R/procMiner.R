cleanText =
    #
    # This is for removing non-printing characters and other such text in
    # XML nodes. Kim's document is an example of this.
    #
    # extend this.
function(x)
  gsub("\\t\\r", " ", gsub("
", "", gsub("  ", " ", x))   )


pdfMinerDoc =
    #
    # read a PDF or already converted XML document and "tidy" it up
    # by collapsing the character nodes into text nodes,
    # removing the <page> nodes
    # and any running header content
    #
function(doc, removePageNodes = FALSE, removeHeader = TRUE, sub = cleanText)
{
   if(is.character(doc)) {
       if(grepl("\\.pdf$", doc))
          doc = convertPDF(doc)
       
       doc = xmlParse(doc)
   }
    
   xpathApply(doc, "//textline", collapseTextLine, sub)

   xpathApply(doc, "//layout | //line[@linewidth = 0]", removeNodes)

   pages = getNodeSet(doc, "//page")
   root = xmlRoot(doc)
   xmlAttrs(root) = c(numPages = length(pages))

   
   #XXX before we do this,
   # a) do we even need to do it?, and
   # b) can we get the header and footer at this time.
   # c) renumber the top-level <textbox> within a page to contain
   #  the page number


     # The text may contain page numbers, or these may be in a separate node.
   if(removeHeader) 
       removeHeaderFooter(pages, doc)

# Was thinking we may have to recognized and use different margin values on alternating pages   
#   margins = sapply(pages, function(x) getMargins(xmlChildren(x)))
#   if(!all(apply(margins, 1, function(x) length(unique(x)) ) == 1)) {
#       browser()
#   }   
   
   if(removePageNodes)
       removePages(, pages)

   doc
#   structure(doc, class = c("PDFMinerDoc", class(doc)))
}

removePages =
function(doc = as(pages[[1]], "XMLInternalDocument"), pages = getNodeSet(doc, "//page"))
{
    lapply(pages, replaceNodeWithChildren)
    doc
}

removeHeaderFooter =
function(pages, doc, above = 792 - 60, below = 72*.75, ...)
{
      # Or use removeNodes( getHeaderByText(pages, doc) )
    lapply(pages, function(x) removeNodes(getHeaderByPosition(x, above, ...)))

      # remove the footer
    lapply(pages, function(x) removeNodes(getFooterByPosition(x, below, ...)))

}

getFooterByPosition =
    #
    # Find text lower than below.  3/4 of an inch.
    #
    #  We could check for a pattern to make certain it is not just
    #  additional text, e.g. overflow, footnote, etc.
    #
function(page, below = 72*.75)
{
    nodes = getNodeSet(page, sprintf(".//textline[@bbox and get-bottom(@bbox) < %f]", below),
                         xpathFuns = list('get-bottom' = getBottom))
}
    

getHeaderByPosition =
function(page, above = 792 - 60) # half inch
{
    nodes = getNodeSet(page, sprintf(".//textline[@bbox and getBBoxEl(@bbox, 2) > %f]", above),
                         xpathFuns = list('getBBoxEl' = getBBoxEl))
}


getHeaderByText =
function(pages, doc)
{
        # Assumes the header is the first thing!!
    header = lapply(pages, `[[`, 1)
    htext = sapply(header, xmlValue)

    bbox = getBBox(header)
    bottom = max(bbox[,2])

       # if none duplicated, then we have not identified the 
    if(any(duplicated(htext))) {
       others = getNodeSet(doc, sprintf("//textbox[@bbox and  get-bottom(@bbox) >= %f]", bottom),
                         xpathFuns = list('get-bottom' = getBottom))

#        removeNodes(others)
    } else
       others = list()

    c( header[ duplicated(htext) ], others)
}


convertPDF =
    #
    # If given a pdf, call pdfminer's pdf2txt to create the XML file.
    #
    #XXX Need to locate the pdfminer/tools/pdf2txt.py script
    #
function(filename, pdfminer = getOption("PDF2TXT", "pdf2txt.py"))
{
    # update path to pdf2txt.py if not option given above
    if( pdfminer == "pdf2txt.py" ) {
        pdfminer = "~/anaconda/bin/pdf2txt.py"
    }
    
    # cmd = sprintf("%s -t xml %s", pdfminer, filename)
    cmd = sprintf("%s -t xml -F 1.0 %s", pdfminer, filename)
    system(cmd, intern = TRUE)
}


getMargins =
function(doc, m = getIndentations(doc))    
{
  structure(c(apply(m[, c(1, 2)], 2, min), apply(m[, c(3, 4)], 2, max)) , names = c("left", "bottom", "right", "top"))
}

getSections =
    #
    #XXX Needs to get much smarter
    #  Not just bold, but font size different from next lines, indentation
    #  underlines of text  (not getting this in the XML)
    #
    #
    #
    #
function(doc, setClass = TRUE, asNodes = TRUE, useIndent = TRUE)
{
   if(is.character(doc))
      doc = pdfMinerDoc(doc)

  
   ans  = if(useIndent)
             getSectionsByIndent(doc, getMargins(doc)["left"], asNodes)

   if(!length(ans)) 
      ans = getNodeSet(doc, "//text[contains(@font, 'Bold')]")

   if(setClass)
       markSection(ans)

   txt = sapply(ans, xmlValue)   
   if(asNodes)
       structure(ans, names = txt)
   else
       txt
}

getSectionsByIndent =
    #
function(doc, margin = NA, asNodes = TRUE)
{
   xpquery = "//textbox[ not(ancestor::layout) and %s get-indent(following-sibling::textbox[1]/@bbox) > get-indent(@bbox) ]"
   if(!is.na(margin))
      margin = sprintf("get-indent(@bbox) = %f and", margin)
   else
      margin = ""
   
   xpquery = sprintf(xpquery, margin)

#   if(is(doc, "XMLInternalNode"))
#      xpquery = paste0(".", xpquery)
   
   getNodeSet(doc, xpquery,  xpathFuns = list('get-indent' = getIndent))
}


groupByIndent =
    #
    #  We run into a problem here with cv_amir
    #  The publications numbered from 9. to 1. have a different indentation than left.
    #  So we add the condition that if a node has multiple <textline>, then it is self-contained
    #  regardless of its indentation. But then we end up with 2 publications that
    #  are split across two groups because the second part of each has multiple <textline>
    #  If we had the pages, we could do this by page and we wouldn't have the problem.
    #  However, if a publication spanned 2 pages, we had have an entirely different problem.

    #
    #  If the publications are ordered as a list (with numbers or bullets), we can use that
    #  to detect errors and regroup.
    #
function(nodes, bbox = getIndentations(nodes))
{
   left = min(bbox[,1])
   hasMultipleLines = sapply(nodes, function(x) sum(names(x) == "textline") > 1)
   atLeft = (bbox[,1] == left)
#   prevStartsAtLeft = c(TRUE, atLeft[- length(atLeft)])
   w = cumsum(  atLeft | hasMultipleLines)   #  & prevStartsAtLeft) )
   ans = split(nodes, w)

   txt = sapply(ans, getAllText)
   firstWord = sapply(strsplit(txt, "[[:space:]]"), `[[`, 1)
   isNum = grepl("^[0-9]+[[:punct:]]?$", firstWord)
   if(sum(isNum)/length(txt) > .9) {
       # we have numbers
       if(any(!isNum)) {
           i = which(!isNum)
browser()           
           ans[i - 1L] = mapply(c, ans[i-1L], ans[i], SIMPLIFY = FALSE)
           ans = ans[-i]
       }
   } else {
       tt = table(first)
       if(max(tt)/length(txt) > .9 && length(tt) > 1) {
           # we have a common identifier
           browser()
       }
   }
   
   ans
}

getAllText =
function(nodes, collapse = "\n")
{
  paste(sapply(nodes, xmlValue), collapse = collapse)
} 

getSectionElements =
function(nodes, convert = getAllText, ...)
{
  e = groupByIndent(nodes)
  sapply(e, convert, ...) 
}


markSection =
function(nodes)
{
  invisible(mapply(function(x, idx) xmlAttrs(x) = c(class = "sectionTitle", sectionNum = idx),
                      nodes, seq(along = nodes)))
}


getWithinSection =
    #
    # Get the nodes within each section identified by getSections() and marked with a class = 'sectionTitle' attribute and a @sectionNum attribute.
    # This @class attribute means we can work with the document object here.
    #
function(doc, sectionNodes = getNodeSet(doc, "//textbox[@class = 'sectionTitle']"))
{
  ans = lapply(sectionNodes, function(x) {
                                 idx = as.integer(xmlGetAttr(x, "sectionNum"))
                                 getNodeSet(x, sprintf("./following-sibling::textbox[ following-sibling::textbox[@sectionNum = %d]]", idx + 1L))
                              })

  names(ans) = sapply(sectionNodes, xmlValue)
  ans
}


getIndent =
    # This is an R function that we use as a function in an XPath query.
    # It is passed a list with one element which is the bbox attribute.
    # This could be an empty list so we return -1 for this case.
function(bbox)    
{
  if(length(bbox))
     as.numeric(strsplit(bbox[[1]], ",")[[1]][1])
  else
      -1
}

getBottom =
    # Also called from XPath.
function(bbox)
{
  as.numeric(strsplit(bbox[[1]], ",")[[1]][2])
}

getBBoxEl =
    # Also called from XPath.
function(bbox, pos)
{
  as.numeric(strsplit(bbox[[1]], ",")[[1]][pos])
}



getFontSizes =
    #
    # return a table of the counts for different font sizes
    # If name = TRUE, then we get a  2-way table of the font size and description
    #
    # Font sizes are complicated. The font name may have the size (e.g. mulhearn's CV)
    # Otherwise, we use the size attribute which is not the font size, but the box size
    # but can be a proxy, but not always a good one.
    # The size can be larger for text within a section eventhough the section title/header
    # is physically larger. It depends on the content of the box.
    # This is where we may want to keep the size of the individual characters.
function(doc, name = FALSE)
{
    fonts = getFonts(doc)
    if(all(grepl("[0-9]+$$", names(fonts)))) {
        vals = unlist(getNodeSet(doc, "//text/@font"))
        return( table( gsub(".*[^0-9]", "", vals) ) )
    }
        
    if(!name)
       table(as.numeric(unlist(getNodeSet(doc, "//text/@size"))))
    else {
       nodes = getNodeSet(doc, "//text")
       info = sapply(nodes, function(x) xmlAttrs(x)[c("font", "size")])
       table(info["size",], info[ "font", ])
    }
}

getFonts =
    #
    # Get the table of counts of the font names/descriptions used in the documents
    # either by number of <text> nodes or by the number of characters using that font.
    # By character allows us to better guess the default font for the document.
    #
    #
function(doc, byChar = FALSE)
{
    if(byChar) {
        nodes = getNodeSet(doc, "//text")
        font = sapply(nodes, xmlGetAttr, "font")
        nchar = nchar(sapply(nodes, xmlValue, trim = TRUE))
        tapply(nchar, font, sum)
    } else
        table(unlist(getNodeSet(doc, "//text/@font")))
}


getSectionsByFont =
    #
    # This is another approach to try to identify section titles
    #
    # This examines all the fonts used in the document and then
    # for all but the most commonly used font (assumed to be general text)
    # it collects all of the lines that are resaonably short that use
    # these fonts.
    #
function(doc, ..., asDataFrame = TRUE)
{
    tt = sort(getFonts(doc, TRUE))
    fonts = names(tt)[ - length(tt) ]
    ans = lapply(fonts, function(f)
                           findShortLines(doc, font = f, ...))
    
    names(ans) = fonts

    if(asDataFrame) {
        tmp = lapply(ans, function(x) sapply(unlist(x), xmlValue))
        page = lapply(ans, function(x)  rep(seq(along = x), sapply(x, length)))
        font = rep(fonts, sapply(tmp, length))
        size = lapply(ans, function(x) sapply(unlist(x), getNodeSet, ".//text/@size"))
        data.frame(text = unlist(tmp), font = font, size = as.numeric(unlist(size)), page = unlist(page), stringsAsFactors = FALSE, row.names = NULL)
    } else
        ans
}


getBBox = getIndentations =
    #
    # a matrix of bboxes for the <textline> nodes
    #
function(doc, bbox = getNodeSet(doc, ".//textline/@bbox"))
{
  if(missing(bbox) && is.list(doc) && all(sapply(doc, inherits, "XMLInternalElementNode")))
     bbox = lapply(doc, xmlGetAttr, "bbox")

  if(length(bbox) == 0)
     return(NULL)
      
  m = do.call(rbind, strsplit(unlist(bbox), ","))
  mode(m) = "numeric"
  colnames(m) = c("left", "bottom", "right", "top")
  m
}



collapseTextLine =
    #
    # Combine the individual nodes containing a single character into a single node within a <textline> node
    # and put the resulting text into the first <text> node of that <textline> node, and delete the remaining
    # <text> nodes.
    #
    # If the font changes across characters, we lose that information currently.
    # 
function(node, sub = NULL)
{
    txt = xmlValue(node)
    if(!is.null(sub))
        txt = sub(txt)
    
    xmlValue(node[[1]]) = txt

    sizes = sapply(xmlChildren(node), xmlGetAttr, "size", "")
    xmlAttrs(node) = c(charcterSizes = paste(sizes, collapse = ","))
    
    removeNodes(xmlChildren(node)[-1])
    node
}


plot.PDFMinerDoc = showBoxes =
    #
    # showBoxes(pdfMinerDoc("SampleCVs/cv_amir.pdf")))
    #
    # pamir = pdfMinerDoc("SampleCVs/cv_amir.pdf", FALSE)
    # showBoxes(getNodeSet(pamir, "/*/page[2]//*[not(ancestor::layout)]"))
    # par(mfrow = c(5, 5), mar = c(0, 0, 0, 0))
    # invisible(lapply(1:25, function(i) showBoxes(getNodeSet(pamir, sprintf("/*/page[%d]//*[not(ancestor::layout)]", i)), axes = FALSE)))
function(doc, bbox = getIndentations(doc), margins = getMargins(, bbox), ...)
{
    if(!missing(doc)) {
      if(is.character(doc)) 
         doc = pdfMinerDoc(doc, removePageNodes = FALSE, removeHeader = FALSE)

      if(is(doc, "XMLInternalDocument")) {
          pages = getNodeSet(doc, "//page")
          if(length(pages)) {
                 # Show all the pages separately
              r = ceiling(sqrt(length(pages)))
              par(mfrow = c(r, ceiling(length(pages)/r)), mar = c(0, 0, 0, 0))
              lapply(pages, function(p) showBoxes(getNodeSet(p, ".//*[not(ancestor::layout) and @bbox]"), axes = FALSE))
              return(NULL)
          }
      }
    }


    plot(0, xlim = margins[c("left", "right")], ylim = margins[c("bottom", "top")], type = "n", xlab = "", ylab = "", ...)
    rect(bbox[, 1], bbox[, 2], bbox[, 3], bbox[, 4])
}




textWidth =
function(nodes)
{
  UseMethod("textWidth")
}

textWidth.XMLInternalDocument =
function(nodes)
{
    textWidth(getNodeSet(nodes, "//textline"))
}

textWidth.list = textWidth.XMLNodeSet =
function(nodes)
{
   bbox = getBBox(nodes)
   widths = bbox[,3] - bbox[,1]
   structure(widths, names = sapply(nodes, xmlValue))
}



findUnderlines =
function(doc, ...)
    UseMethod("findUnderlines")

findUnderlines.character =
function(doc, ...)
  findUnderlines(pdfMinerDoc(doc, removePageNodes = FALSE))

findUnderlines.XMLInternalDocument =
function(doc, ...)
{
    findUnderlines(getNodeSet(doc, "//page"))
}

findUnderlines.XMLNodeSet =
function(doc, ...)
{
    lapply(doc, findUnderlines)
}

findUnderlines.XMLInternalElementNode =
    #
    # Add the tests for the line going all the way across, or that there is no other text on that line.
    #
    #
function(doc, threshold = 4, ...)
{
 rects = getNodeSet(doc, ".//rect | .//line[@linewidth > 0]")
 if(length(rects) == 0)
     return(NULL)
 
 rects = getBBox(rects)


 lines = lapply(rects[, "bottom"],
                function(y) {
                    els = getNodeSet(doc, sprintf(".//textline[@bbox and abs(get-bottom(@bbox) - %f) < %f and not(normalize-space(.) = '')]", y, threshold),
                                      xpathFuns = list('get-bottom' = getBottom))

                    if(length(els)) {
                        pos = getBBox(els)[, 2]
                        tmp = lapply(pos, findTextOnSameLine, doc)
                        i = sapply(tmp, length) == 1
if(!all(i))    browser()
                        els = els[i]
                    }
                    els
                })

 lines[ sapply(lines, length) > 0 ]
}

findTextOnSameLine =
function(y, doc)
{
   lines =  getNodeSet(doc, sprintf(".//textline[@bbox and get-bottom(@bbox) = %f and not(normalize-space(.) = '') ]", y),
                        xpathFuns = list('get-bottom' = getBottom))
}




findNumberedSectionTitle =
function(doc)
{
  els = getNodeSet(doc, "//textbox[ starts-with-a-number(.) ]/preceding-sibling::textbox[ not(starts-with-a-number(.)) ][1]",
                     xpathFuns = list('starts-with-a-number' = startsWithANumber))
}

startsWithANumber =
function(x)
{
  grepl("^[0-9]+[[:punct:]]?", xmlValue(x[[1]]))
}


findBulletedSectionTitle =
function(doc)
{
  els = getNodeSet(doc, "//textbox[ starts-with(., '•') ]/preceding-sibling::textbox[ not(starts-with(., '•')) ]")
}



findShortLines =
function(doc, widthPercent = .7, allCaps = FALSE, font = NA)
  UseMethod("findShortLines")

findShortLines.character =
function(doc, widthPercent = .7, allCaps = FALSE, font = NA)    
  findShortLines(pdfMinerDoc(doc, removePageNodes = FALSE), widthPercent, allCaps, font)

findShortLines.XMLInternalDocument =
function(doc, widthPercent = .7, allCaps = FALSE, font = NA)    
  xpathApply(doc, "//page", findShortLines, widthPercent, allCaps, font)


findShortLines.XMLInternalElementNode =
    #
    #XXX We have an issue with some <text> nodes have the wrong bbox that are too narrow, (e.g. Mulhearn's "Education and Training")
    # and some <textline> nodes that are too big
    #
function(doc, widthPercent = .7, allCaps = FALSE, font = NA, marThreshold = 12)
{
#cat("Page", xmlGetAttr(doc, "id"), font, "\n")
    mar = getMargins(doc)
    fontQuery = if(!is.na(font))
                  sprintf("text/@font = '%s' and", font)
                else
                   ""
    xp = sprintf(".//textline[@bbox and string-length(normalize-space(.)) > 1 and %s abs(get-indent(@bbox) - %f) < %f and getBBoxEl(@bbox, 3) < %f]",
                  fontQuery, mar[1],  marThreshold, mar[3]*widthPercent)
    
    
    nodes = getNodeSet(doc, xp, xpathFuns = list('get-indent' = getIndent, 'getBBoxEl' = getBBoxEl))

    if(length(nodes) == 0)
       return(NULL)
    
    pos = getBBox(nodes)
    ok = sapply(pos[, 2], function(pos)  length(findTextOnSameLine(pos, doc)) == 1)

    if(allCaps)
       nodes[ok][ isUpperCase(sapply(nodes[ok], xmlValue)) ]
    else
       nodes[ok]
}


isUpperCase =
function(x)
{
    x == toupper(x)
}
