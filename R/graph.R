#' Add network metadata and layout information
#'
#' @import        igraph
#'
#' @param edges       data frame describing the edges
#' @param node_name   name of the node id column; defaults to "id"
#' @param directed    logical; is the graph directed?
#'
#' @export
sm_graph_layout <- function(edges, node_name = "id", directed = FALSE)
{
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = directed)
  L <- igraph::layout_nicely(H)

  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  noms <- names(vs)
  cmp <- igraph::components(H)
  ids <- cbind(match(es[,1], noms),  match(es[,2], noms))

  # Properties from the whole graph
  if (directed)
  {
    node_out <- tibble::tibble(
      id = as.character(noms),
      x = L[,1],
      y = L[,2],
      degree_out = igraph::degree(H, mode = "out"),
      degree_in = igraph::degree(H, mode = "in"),
      degree_total = igraph::degree(H, mode = "total"),
      eigen = NA_real_,
      between = NA_real_,
      cluster = as.character(as.integer(
        igraph::membership(igraph::cluster_walktrap(H))
      )),
      component = as.integer(cmp$membership),
      component_size = cmp$csize[as.integer(cmp$membership)]
    )
  } else {
    node_out <- tibble::tibble(
      id = as.character(noms),
      x = L[,1],
      y = L[,2],
      degree = igraph::degree(H, mode = "all"),
      eigen = NA_real_,
      close = NA_real_,
      between = NA_real_,
      cluster = as.character(as.integer(
        igraph::membership(igraph::cluster_walktrap(H))
      )),
      component = as.integer(cmp$membership),
      component_size = cmp$csize[as.integer(cmp$membership)]
    )
  }

  names(node_out)[1] <- node_name

  # Properties by component
  membership <- as.integer(cmp$membership)
  for (i in unique(membership))
  {
    Hs <- induced_subgraph(H, membership == i)
    index <- which(node_out$component == i)
    node_out$eigen[index] <- igraph::eigen_centrality(Hs, directed = FALSE)$vector
    if (!directed) { node_out$close[index] <- igraph::closeness(Hs) }
    node_out$between[index] <- igraph::betweenness(Hs, directed = directed)
  }

  # Reorder components by size
  tab <- as.integer(names(sort(table(membership), decreasing = TRUE)))
  node_out$component <- match(node_out$component, tab)

  edge_out <- tibble::tibble(
    x = L[ids[,1],1],
    xend = L[ids[,2],1],
    y = L[ids[,1],2],
    yend = L[ids[,2],2]
  )

  list(node = node_out, edge = edge_out)
}

#' Add network metadata to edge and node lists
#'
#' @import        igraph
#'
#' @param edges       data frame describing the edges
#' @param node_name   name of the node id column; defaults to "id"
#'
#' @export
sm_graph_metadata <- function(edges, node_name = "id")
{
  H <- igraph::graph.edgelist(as.matrix(edges[,1:2]), directed = FALSE)

  vs <- igraph::V(H)
  es <- igraph::get.edgelist(H)
  noms <- names(vs)
  cmp <- igraph::components(H)
  ids <- cbind(match(es[,1], noms),  match(es[,2], noms))

  node_out <- tibble::tibble(
    id = as.character(noms),
    degree = igraph::degree(H, mode = "all"),
    eigen = igraph::eigen_centrality(H, directed = FALSE)$vector,
    close = igraph::closeness(H),
    between = igraph::betweenness(H),
    cluster = as.character(as.integer(
      igraph::membership(igraph::cluster_walktrap(H))
    )),
    component = as.integer(cmp$membership),
    component_size = cmp$csize[as.integer(cmp$membership)]
  )
  names(node_out)[1] <- node_name

  node_out
}
