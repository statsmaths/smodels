#' Tidy a Matrix Object
#'
#' Creates a data frame out of a Matrix that has column and row names. Name
#' defaults assume a term frequency matrix, but it is possible to give other
#' names.
#'
#' @param x           the matrix to tidy.
#' @param rows_to     names of the column containing the rownames; defaults to
#'                    "document"
#' @param cols_to     names of the column containing the rownames; defaults to
#'                    "term"
#' @param values_to   names of the column containing the rownames; defaults to
#'                    "count"
#' @export
sm_tidy_matrix <- function(
  x, rows_to = "document", cols_to = "term", values_to = "count"
) {
  if (is.null(rownames(x))) stop("input must have row names")
  if (is.null(colnames(x))) stop("input must have column names")

  x <- as.matrix(x)
  out <- tibble::tibble(
    var1 = rownames(x)[row(x)],
    var2 = colnames(x)[col(x)],
    var3 = as.numeric(x)
  )

  names(out) <- c(rows_to, cols_to, values_to)
  out
}

#' Create a data frame of pairwise (Euclidiean) distances
#'
#' @param x           the matrix to compute distances between rows.
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @export
sm_tidy_distance <- function(x, item_name = "document")
{
  d <- as.matrix(stats::dist(as.matrix(x)))
  rownames(d) <- rownames(x)
  colnames(d) <- rownames(x)

  sm_tidy_matrix(
    d,
    sprintf("%s1", item_name),
    sprintf("%s2", item_name),
    "distance"
  )
}

#' Create a data frame of pairwise Cosine similarities
#'
#' @param x           the matrix to compute similarities between rows.
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @export
sm_tidy_angle_distance <- function(x, item_name = "document")
{
  x <- as.matrix(x)
  sim <- x / sqrt(rowSums(x * x))
  sim <- sim %*% t(sim)

  out <- sm_tidy_matrix(
    sim, sprintf("%s1", item_name), sprintf("%s2", item_name), "distance"
  )
  out$distance[out$distance > 1] <- 1
  out$distance[out$distance < -1] <- -1
  out$distance <- acos(out$distance) / pi
  out
}

#' Create a data frame of principal components
#'
#' @param x       the matrix to compute principal components from.
#' @param n       number of components to compute
#' @param scale   logical; should columns by scaled before PCA
#' @param item_name   name to give to the set of rows; defaults to "document"
#'
#' @export
sm_tidy_pca <- function(x, n = 3, scale = TRUE, item_name = "document")
{
  x <- as.matrix(x)
  df <- tibble::as_tibble(irlba::prcomp_irlba(t(x), n = n, scale. = scale)$rotation)
  names(df) <- sprintf("v%d", seq_len(ncol(df)))
  if (!is.null(rownames(x)))
  {
    df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
    names(df)[1] <- item_name
  }
  df
}

#' Create a data frame of umap components
#'
#' @param x              the matrix to compute principal components from.
#' @param n              number of components to compute
#' @param random_state   integer to set the random state of the algorithm
#' @param item_name      name to give to the set of rows; defaults to "document"
#' @param ...            other options passed to the umap function
#'
#' @export
sm_tidy_umap <- function(x, n = 2, random_state = 1, item_name = "document", ...)
{
  x <- as.matrix(x)
  df <- umap::umap(x, n_components = n, random_state = random_state, ...)$layout
  df <- tibble::as_tibble(df)
  names(df) <- sprintf("v%d", seq_len(ncol(df)))
  if (!is.null(rownames(x)))
  {
    df <- dplyr::bind_cols(tibble::tibble(rownames(x)), df)
    names(df)[1] <- item_name
  }
  df
}

#' Create a data frame of elastic net coefficents
#'
#' @param X              the data matrix, possibly sparse
#' @param y              the response vector
#' @param item_name      name to give to the set of rows; defaults to "var"
#' @param binomial       logical; is this a binomial regression?
#'
#' @export
sm_tidy_glmnet <- function(X, y, item_name = "var", binomial = FALSE)
{
  model <- glmnet::glmnet(
    X, y, family = ifelse(binomial, "binomial", "gaussian")
  )

  beta <- as.matrix(model$beta)
  df <- tibble::tibble(
    var = rownames(beta)[row(beta)],
    step = seq_len(ncol(beta))[col(beta)],
    lambda = model$lambda[col(beta)],
    coef = as.numeric(beta)
  )
  names(df)[1] <- item_name
  df
}
