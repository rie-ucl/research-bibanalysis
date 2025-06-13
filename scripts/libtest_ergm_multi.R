# install.packages("ergm.multi")
library(ergm.multi)

vignette( package = "ergm.multi" ) # Goeyvaerts_reproduction
data( package = "ergm.multi" ) # Goeyvaerts and Lazega

data( "Goeyvaerts", package = "ergm.multi" )
data( "Lazega", package = "ergm.multi" )

str( Lazega, max.level = 1 )
names( Lazega )

summary( Lazega )
Lazega$mel[1] |> unlist()
Lazega$gal |> unlist()
Lazega$val[1] |> unlist()
Lazega$iel[1] |> unlist()
Lazega$oel[1] |> unlist()

# Visualisation

library( network )
library( igraph )
library( ggraph )

g <- intergraph::asIgraph( Lazega )
vertex_attr( g )

ggraph( g, layout = "fr" ) +
  geom_edge_link( aes( )) +
  geom_node_point() +
  theme_void()
