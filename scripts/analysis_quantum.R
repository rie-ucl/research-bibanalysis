# ------------------------------------------------------------------------------
# Analysis on `Quantum Computing`
# ------------------------------------------------------------------------------

## Download data from scopus
# res_qc <- bibliokit::download_scopus_data( '"quantum computing*"', n = 100 )

## Import data and convert to data.frame
load( "data/rscopus-2025_03_03-quantum_computing.rda" )
df_qc <- bibliokit::entries_to_df( res$entries )

## Basic visualisation
bibliokit::create_word_cloud( df_qc )
bibliokit::plot_publication_trend( df_qc )
bibliokit::plot_top_countries( df_qc, max_n = 10, weighted = FALSE )

## Co-authorship Analysis
df_country <- bibliokit::add_country_to_author( df_qc )

df_country |>
  count( country, sort = TRUE ) |>
  head( 10 )

detach( "package:bibliokit", unload = TRUE )
