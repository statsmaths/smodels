#' Add network metadata to edge and node lists
#'
#' @import        igraph
#'
#' @param nodes     data frame describing the nodes
#' @param edges     data frame describing the edges
#' @param directed  logical. Is this a directed graph?
#' @param bipartite logical. Is this a directed graph?
#'
#' @export
graph_data <- function(edges, nodes = NULL, nodes2 = NULL,
                       directed = FALSE, layout = NULL) {

  # if no nodes data frame, create it
  if (is.null(nodes))
    nodes <- dplyr::data_frame(id = unique(c(edges[[1]], edges[[2]])))

  # is this a bipartite graph?
  bipartite <- !is.null(nodes2)

  # make sure edges have associated nodes
  names(nodes)[1] <- "id"
  names(edges)[1:2] <- c("id", "id_out")
  nodes[[1]] <- as.character(nodes[[1]])
  edges[[1]] <- as.character(edges[[1]])
  edges[[2]] <- as.character(edges[[2]])
  node_names <- nodes$id
  bad_ids <- c(which(is.na(match(edges[[1]], node_names))),
               which(is.na(match(edges[[2]], node_names))))
  if (length(bad_ids) > 0)
    edges <- edges[-bad_ids,]

  # create the graph object
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = directed)

  # layout is stochastic, so make it here with a fixed seed
  set.seed(1)
  if (is.null(layout)) {
    if (bipartite) {
      L <- igraph::layout_as_bipartite(H)
    } else {
      L <- igraph::layout_nicely(H)
    }
  } else {
    L <- as.matrix(layout)
  }

  # get id's from the graph back into the edge list
  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  noms <- names(vs)
  if (is.null(noms)) noms <- seq_along(vs)
  ids <- cbind(match(es[,1], noms),
               match(es[,2], noms))

  # create node output
  cmp <- igraph::components(H)
  node_out <- dplyr::data_frame(id = as.character(noms),
                          x = L[,1], y = L[,2],
                          degree = igraph::degree(H, mode = "all"),
                          degree_in = igraph::degree(H, mode = "in"),
                          degree_out = igraph::degree(H, mode = "out"),
                          eigen = igraph::eigen_centrality(H, directed = FALSE)$vector,
                          close = igraph::closeness(H),
                          between = igraph::betweenness(H),
                          cluster = as.character(as.integer(igraph::membership(igraph::cluster_walktrap(H)))),
                          component = as.integer(cmp$membership),
                          component_size = cmp$csize[as.integer(cmp$membership)])

  these <- which(!is.na(match(names(nodes)[-1], names(node_out)[-1]))) + 1
  if (length(these))
    nodes <- nodes[,-these]
  node_out <- dplyr::inner_join(nodes, node_out, by = "id")

  # create output edge data
  edge_out <- dplyr::data_frame(x = L[ids[,1],1],
                                xend = L[ids[,2],1],
                                y = L[ids[,1],2],
                                yend = L[ids[,2],2])

  these <- which(!is.na(match(names(edges), names(edge_out))))
  if (length(these))
    edges <- edges[,-these]
  edge_out <- dplyr::bind_cols(edges, edge_out)

  list(nodes = node_out, edges = edge_out)
}

#' Add network metadata to edge and node lists
#'
#' @import        igraph
#'
#' @param edges     data frame describing the edges
#'
#' @export
co_cite <- function(edges) {

  # create the graph object
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]))
  cocitation(H)

  X <- cocitation(H)
  df <- data_frame(id = rownames(X)[row(X)],
                   id_out = rownames(X)[col(X)],
                   count = as.numeric(X))
  df[df$count > 0,]
}


