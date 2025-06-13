library(dplyr)
library(purrr)
library(ggplot2)
library(ggraph)
library(igraph)
library(readr)
library(litsearchr)

#-------------------------------------------------------------------------------
# Parameters
#-------------------------------------------------------------------------------

FILE      <- ""       # Necessary (.bib or .ris file)
STOPWORDS <- ""       # Optional

KEYWORD_MIN_N    <- 2
KEYWORD_MIN_FREQ <- 2

TITLE_MIN_N    <- 2
TITLE_MIN_FREQ <- 2

#-------------------------------------------------------------------------------
# Loading results and
#-------------------------------------------------------------------------------

pubs.tib <- readRDS( system.file( "extdata/sample_pubs.rds", package = "bibliokit" ) )

if( STOPWORDS == "" ){
  all_stopwords <- get_stopwords("English")
} else {
  all_stopwords <- c( get_stopwords("English"), read_lines( STOPWORDS ) )
}

keywords_raw <- separate_rows( pubs.tib, "authkeywords", sep = " \\| " ) |> pull( authkeywords )

keywords <- extract_terms(
  keywords =  keywords_raw,
  method =    "tagged",
  min_n =     KEYWORD_MIN_N,
  min_freq =  KEYWORD_MIN_FREQ,
  stopwords = all_stopwords
)

title_terms <- extract_terms (
  text      = pubs.tib$`dc:title`,
  method    = "fakerake",
  min_n     = TITLE_MIN_N,
  min_freq  = TITLE_MIN_FREQ,
  stopwords = all_stopwords
)

terms <- unique( c( keywords, title_terms ) )

#-------------------------------------------------------------------------------
# Network analysis
#-------------------------------------------------------------------------------

docs <- paste( pubs.tib$`dc:title`, pubs.tib$`dc:description` )
dfm  <- create_dfm( elements = docs, features = terms )

g <- create_network( dfm, min_studies = 10 )

left_join(
  strength(g)  |> as_tibble( rownames = "name" ) |> rename( strength = value ),
  colSums(dfm) |> as_tibble( rownames = "name" ) |> rename( freq = value ),
  by = "name"
) |> write.csv( file = "outputs/quantum_keywords.csv" )

write_search(
  grouped_terms,
  languages   = "English",
  exactphrase = TRUE,
  stemming    = FALSE,
  closure     = "left",
  writesearch = TRUE
)

