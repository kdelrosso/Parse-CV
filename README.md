# Parse-CV

Extract publication citation information from curriculum vitae using R


### Overview

The goal of this project is to extract and parse citations from the publication section of a curriculum vitae (CV). We take a CV in PDF format and convert the document into XML. We then identify all section headings in the CV and extract the text in the publication section. We parse the publication text into individual citation strings, which we pass to the CrossRef API. The final output is a data frame of results with fields for citation doi, title, authors, journal, year, citation counts, and various scoring metrics.


### PDF to XML

We first convert the PDF to XML using pdfminer (available <a href = "https://pypi.python.org/pypi/pdfminer/">here</a>) and Python. This initial step uses code from my professor Duncan Temple Lang and is available <a href = "https://github.com/dsidavis/CVRead">here</a>. The code uses a system call to interface with pdfminer, as well as performs some initial cleaning and organizing of the XML nodes.


### Extract Sections

We first extract important text features from every line of the CV: text size, capitalization, left / right indentation, font, bold / italics, etc. We then group the text based on common features and try to identify a single group that contains all the sections by comparing each group with a known list of common section names. Specifically, on the first pass we'll group using the text features: 

- capitalization 
- text size
- bold 
- italics 
- left indentation

The goal of the first grouping is to identify capitalized, bolded / italicized, or larger text size section titles which have the same left indentation. With the second pass, we remove the left indentation requirement and search for centered text or those CVs with imperfect left indentation. The known section names are located in <code>/text_files/section_names.txt</code>


### Extract Citations

Now that we have the section names, we need to extract all the text between the publication
section and the next section and then split that text appropriately into individual citation 
strings.

#### Step 1: Get citation text

We proceed down two paths to extract the citation text, either we successfully found section 
names in the previous step or we didn't. In the first case, we'll locate the publication section and extract all text 
until the next section (which we know). In the other case, we'll walk through the CV looking for 
the publication section, and then extract all text until we find any previously identified section 
which isn't publication. The known section names are located in <code>/text_files/publications.txt</code>

#### Step 2: Parse citation text

Now that we have the citation text, we'll attempt to parse citations using the following methods. 

- numbered list
- author’s name
- year (used as a list)
- textbox (a grouping returned from pdfminer)
- left indentation
- most common starting word

For each method above, we'll check if the citation parse was successful and if not we proceed to the next most likely parsing method.


### Parse Citations

The final step is to pass our parsed citation strings to the CrossRef API available as an R package. <pre><code>install.packages( "rcrossref" )</code></pre>

CrossRef returns the doi, title, authors, journal, year, and a score for the match. We first remove results which don't contain the CV author's name. We then create a fuzzy match between the title CrossRef returns and the original citation. This gives us a pseudo percentage for the title match. We multiply the returned score by the title score to give use a better sense of which citation is correct. This isn't need when the citation is obvious, but many times none of the returned results seem likely based solely on the score. If we get a good title match, however, we can be more confident we've found the actual citation.


### Preliminary Results

We attempted to extracted section names from over 45,000 CVs. We successfully found all section names in nearly 39,000 CVs, a success rate of 85.3%.

We also ran the full algorithm on 65 training CVs. We found the section names in 98.5% and extracted citations from 86.2% of those 65 CVs. In total we successfully parsed 514 citations with a median title percentage match of 90%.