library( tibble )
library( dplyr )
library( tidyr )
library( purrr )
library( stringr )

library( igraph )
library( ggraph )
library( tidygraph )
library( ggrepel )

library( litsearchr )
library( multinets )
library( bibliokit )

# ------------------------------------------------------------------------------
# Prep
# ------------------------------------------------------------------------------

# Load the bibliographic data
load("~/research-bibanalysis/data/scopus-2025_03_28-quantum_250328_2024.rda")
length( res$entries )

df <- bibliokit::entries_to_df( res$entries )
authors.tib <- bibliokit::df_to_authors( df )

# Define a safe version of combn to handle cases with fewer than 2 unique countries
safe_combn <- possibly(
  ~ combn( .x, 2, simplify = FALSE),
  otherwise = list()
)

# ------------------------------------------------------------------------------
# Network Analysis
# ------------------------------------------------------------------------------

# Create edge tibbles
macro_edges.tib <- authors.tib |>
  distinct( entry_number, country ) |>
  reframe(
    .by = entry_number,
    pair = safe_combn( sort( unique( country ) ) )
  ) |>
  reframe(
    node1 = map_chr( pair, 1 ),
    node2 = map_chr( pair, 2 )
  ) |>
  count( node1, node2, level = "Macro" )

meso_edges.tib <- authors.tib |>
  reframe(
    node1 = authid,
    node2 = country
  ) |>
  count( node1, node2, level = "Meso" )

micro_edges.tib <- authors.tib |>
  distinct( entry_number, authid ) |>
  reframe(
    .by = entry_number,
    pair = safe_combn( sort( unique( authid ) ) )
  ) |>
  reframe(
    node1 = map_chr( pair, 1 ),
    node2 = map_chr( pair, 2 )
  ) |>
  count( node1, node2, level = "Micro" )

edges.tib <- bind_rows(
  macro_edges.tib,
  meso_edges.tib,
  micro_edges.tib
)

# Create network

countries.vec <- unique( authors.tib$country )

g <- as_tbl_graph( edges.tib, directed = FALSE )

combined_freq.tib <- bind_rows(
  authors.tib |> count( authid )  |> rename( name = authid ),
  authors.tib |> count( country ) |> rename( name = country ) ) |>
  rename( freq = n )

id_label_map.tib <- bind_rows(
  authors.tib |> distinct( name = authid,  .keep_all = TRUE ),
  authors.tib |> distinct( name = country, .keep_all = TRUE ) ) |>
  reframe( name, label = ifelse( name %in% countries.vec, name, authname ) )

g <- g |>
  activate( nodes ) |>
    left_join( combined_freq.tib, by = "name" ) |>
    left_join( id_label_map.tib, by = "name" ) |>
    mutate( type = ifelse( V(g)$name %in% countries.vec, TRUE, FALSE ),
            type_label = ifelse( V(g)$name %in% countries.vec, "Country", "Researcher" ) ) |>
  activate( edges ) |>
    rename( weight = n )

# ------------------------------------------------------------------------------
# Visualisation
# ------------------------------------------------------------------------------

plot_multi_network <- function( G, LAYOUT, COUNTRY_N, AUTHOR_N, MIN_EDGE ){

  colours.vec <- c(
    "Country" = "red3", "Researcher" = "blue3",
    "Macro" = "red", "Micro" = "blue", "Meso" = "green4"
  )

  shapes.vec <- c( "Country" = "square", "Researcher" = "circle" )

  linetypes.vec <- c( "Macro" = "solid", "Micro" = "dashed", "Meso" = "dotted" )

  top_nodes <- bind_rows(
    g |> activate( nodes ) |> as_tibble() |>
      filter( type == TRUE ) |>
      slice_max( order_by = freq, n = COUNTRY_N ),

    g |> activate( nodes ) |> as_tibble() |>
      filter( type == FALSE ) |>
      slice_max( order_by = freq, n = AUTHOR_N )
  )

  g_redux <- g |>
    activate( nodes ) |>
    filter( name %in% top_nodes$name ) |>
    activate( edges ) |>
    filter( weight >= MIN_EDGE ) |>
    activate( nodes ) |>
    filter( centrality_degree() > 0 )

  g_redux <- delete_vertices( V( g_redux )[ degree( g_redux ) == 0 ] )

  if( LAYOUT == "multi" ){
    l <- multinets::layout_multilevel( g_redux )
  } else {
    l <- LAYOUT
  }

  p <- ggraph( g_redux, layout = l ) +
    geom_edge_link( aes( color = level, alpha = weight, linetype = level ) ) +
    geom_node_point( aes( shape = type_label, color = type_label, size = freq ) ) +
    geom_node_text( aes( label = label ), repel = TRUE, hjust = "outward", size = 3 ) +
    scale_color_manual( values = colours.vec ) +
    scale_shape_manual( values = shapes.vec ) +
    scale_linetype_manual( values = linetypes.vec ) +
    labs( caption = glue::glue( "Layout, Top {AUTHOR_N} Authors, Top {COUNTRY_N} Countries, Min Edge Weight = {MIN_EDGE}" ) )

  filename <- paste0( "figures/multinet_", LAYOUT,
                  "_author", AUTHOR_N, "_country", COUNTRY_N,
                  "_minedge", MIN_EDGE, ".pdf" )
  ggsave( filename = filename, plot = p, width = 10, height = 8 )

  return( p )
}

# Plot with different parameters
plot_multi_network( g, "multi", 15, 15, 1 )

