library( tibble )
library( dplyr )
library( tidyr )
library( igraph )
library( ggraph )
library( litsearchr )
library( multinets )

FILE = "domains/pos_nav_timing/scopus_pos_nav_timing_200_bycitation.ris"

data.tib <- import_results( file = FILE ) |> as_tibble()

