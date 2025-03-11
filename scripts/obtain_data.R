q_terms <- bibliokit::expand_search_terms("quantum computing")

res_qc <- bibliokit::download_scopus_data( '"quantum computing*"', n = 100 )

res_nb <- bibliokit::download_scopus_data( '"Nanobiolog*" OR "Nano biolog*"', n = 100 )
