library(dplyr)
library(purrr)
library(ggplot2)
library(ggforce)
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


categories.vec <- c( "Core Terms", "Direct Synonyms", "Sub-Domain Terms", "Use Cases", "Generic Terms" )

nodes.tib <- read.csv( "outputs/quantum_keywords.csv" ) |>
  select( -ID ) |>
  mutate( category = factor( category, levels = categories.vec ) )

g_reordered <- graph_from_data_frame( igraph::as_data_frame( g, what = "edges" ),
                                      vertices = nodes.tib,
                                      directed = FALSE )

# Visualisation - pie chart of categories
category_counts.tib <- g_reordered |>
  igraph::as_data_frame( what = "vertices" ) |>
  as_tibble() |>
  count( category ) |>
  mutate( category = factor( category, levels = categories.vec ) )

category_counts.tib |>
  ggplot( aes( x = "", y = -n, fill = category ) ) +
    geom_col( width = 1, color = "white", show.legend = FALSE ) +
    geom_text( aes( label = paste0( category, "\n", n ) ),
               position = position_stack( vjust = 0.5 ), lineheight = 0.8,
               hjust = "outward", vjust = "outward", show.legend = FALSE ) +
    coord_polar( "y", start = 0 ) +
    labs( title = "Distribution of Terms by Category" ) +
    scale_fill_brewer( type = "qual", palette = "Pastel1" ) +
    scale_colour_identity() +
    theme_void()

g_reordered |>
  igraph::as_data_frame( what = "vertices" ) |>
  as_tibble()

clusters <- cluster_louvain( g_reordered )
membership <- membership( clusters )
V(g_reordered)$cluster <- membership

layout_coords <- igraph::layout_with_fr( g_reordered )
V(g_reordered)$x <- layout_coords[, 1]
V(g_reordered)$y <- layout_coords[, 2]

g_reordered |>
  ggraph( layout = "manual", x = V(g_reordered)$x, y = V(g_reordered)$y ) +
    geom_edge_link( aes( alpha = weight ), show.legend = FALSE ) +
    geom_node_point( aes( color = category, size = freq ), show.legend = TRUE ) +
    scale_color_brewer( type = "qual", palette = "Set1" ) +
    theme_void()

g_reordered |>
  igraph::as_data_frame( what = "vertices" ) |>
  ggplot( aes( x = x, y = y ) ) +
    stat_ellipse( aes( group = factor( cluster ) ) ) +
    geom_point( aes( colour = category ), size = 3, alpha = 0.8 ) +
    scale_color_brewer( palette = "Set1", name = "Cluster" ) +
    theme_minimal() +
    labs( title = "Clusters of Terms in Quantum Computing Literature" ) +
    theme_minimal()

mod_score <- igraph::modularity( x = g_reordered, membership = membership )
print( mod_score )
