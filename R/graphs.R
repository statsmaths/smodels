#' Add network metadata to edge and node lists
#'
#' @import        igraph
#'
#' @param .data   a data_frame containing the variables in the model
#' @param ...     variables to group by.
#' @param sort    logical. Should the data be sorted on output.
#'
#' @export
graph_data <- function(nodes, edges, directed = FALSE) {

  # make sure edges have associated nodes
  names(nodes)[1] <- "id"


  # create the graph object
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = directed)

  # layout is stochastic, so make it here with a fixed seed
  set.seed(1)
  L <- igraph::layout.fruchterman.reingold(H)

  # get id's from the graph back into the edge list
  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  ids <- cbind(match(es[,1], names(vs)),
               match(es[,2], names(vs)))

  # create node output
  cmp <- igraph::components(H)
  node_out <- data_frame(id = names(vs),
                          x = L[,1], y = L[,2],
                          eigen = igraph::eigen_centrality(H)$vector,
                          close = igraph::closeness(H),
                          between = igraph::betweenness(H),
                          cluster = igraph::membership(igraph::cluster_walktrap(H)),
                          component = as.integer(cmp$membership),
                          component_size = cmp$csize[component])

  node_out <- inner_join(nodes, node_out, by = "id")

  # create output edge data
  edge_out <- data_frame(x0 = L[ids[,1],1],
                         x1 = L[ids[,2],1],
                         y0 = L[ids[,1],2],
                         y1 = L[ids[,2],2])

  edge_out <- bind_cols(edges, edge_out)

  list(nodes = node_out, edges = edge_out)
}

# library(dplyr)
# edges <- data_frame(e1 = c("A", "B"), e2 = c("B", "C"))
# nodes <- data_frame(id = c("A", "B", "C"), lower = c("a", "b", "c"))


