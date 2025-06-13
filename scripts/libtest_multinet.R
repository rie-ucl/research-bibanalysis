#' @title Sandbox for Multinet Analysis
#' https://cran.r-project.org/web/packages/multinets/readme/README.html

library(ggplot2)
library(igraph)
library(ggraph)
library(tidygraph)

install.packages( "multinets", type = "source" )
library(multinets)

data( "linked_sim" )
is_multilevel( linked_sim )

str( linked_sim )

V(linked_sim)
vertex_attr( linked_sim )
linked_sim <- set_shape_multilevel( linked_sim )
vertex_attr( linked_sim )

E(linked_sim)
edge_attr( linked_sim )
linked_sim <- linked_sim |> set_color_multilevel()
edge_attr( linked_sim )
E(linked_sim)$color <- substr( E(linked_sim)$color, 1, 7 )

l <- layout_multilevel( linked_sim, layout = layout_with_kk )
linked_sim <- set_color_multilevel( linked_sim )
plot( linked_sim, layout = l, 
      vertex.size = 5, vertex.level = NA )

# ------------------------------------------------------------------------------

organizations <- extract_highlevel( linked_sim ) # edges between vertices of different levels are kept
individuals <- extract_lowlevel( linked_sim ) # edges between vertices of different levels are kept
affiliations <- extract_mesolevel( linked_sim ) # edges between vertices from different levels


plot( organizations, layout = l_org, 
      vertex.size = 5, vertex.level = NA )

plot( individuals, layout = l,
      vertex.size = 5, vertex.level = NA )

plot( affiliations, layout = l,
      vertex.size = 5, vertex.level = NA )

transformed <- mode_transformation( affiliations )
high_transformed <- mode_transformation( affiliations, which = "high" )

# ------------------------------------------------------------------------------

linked_sim <- set_color_multilevel( linked_sim, color.true = "#FF9900", color.false = "#9900CC" )
linked_sim <- set_shape_multilevel( linked_sim, shape.true = "circle filled", shape.false = "star" )

g <- as_tbl_graph( linked_sim )
vertex_attr_names( g )
E(g)$color <- substr(E(g)$color, 1, nchar(E(g)$color) - 2)
V(g)$shape

g <- linked_sim

ggraph( g, layout = l ) +
  geom_edge_link( aes( color = E(g)$color ) ) +
  geom_node_point( aes( shape = shape ) ) +
  scale_color_identity() +
  scale_shape_identity() +
  theme_void()
