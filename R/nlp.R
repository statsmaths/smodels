#' Convert a list of terms to a data frame
#'
#' @param tokens   list of tokens
#'
#' @export
term_list_to_df <- function(tokens) {

  id <- mapply(function(u, v) rep(u, length(v)),
                               seq_along(tokens), tokens)
  df <- dplyr::data_frame(id = unlist(id),
                   token = unlist(tokens))

  df
}

#' Convert a data frame of terms to a sparse matrix
#'
#' @param x   data frame
#' @param min_df   minimum fraction of documents to cut off at
#' @param max_df   maximum fraction of documents to cut off at
#' @param max_features   maximum number of features
#' @param scale  should data be scaled by rows
#' @param vocabulary   optional vocabulary vector; if given
#'                     min_df, max_df, and max_features are ignored
#'
#' @export
term_df_to_matrix <- function(x, min_df = 0.0, max_df = 1.0,
                              max_features = 10000,
                              scale = FALSE,
                              vocabulary = NULL) {

  N <- max(x$id)
  if (is.null(vocabulary)) {
    possible_vocab <- unique(x)
    possible_vocab <- dplyr::group_by_(possible_vocab, "token")
    possible_vocab <- dplyr::summarize_(possible_vocab, prop = "n()")
    possible_vocab$prop <- possible_vocab$prop/N
    possible_vocab <- dplyr::filter_(possible_vocab, ~prop >
        min_df, ~prop < max_df)
    possible_vocab <- possible_vocab$token
    vocabulary <- dplyr::filter_(x, ~token %in% possible_vocab)
    vocabulary <- dplyr::group_by_(vocabulary, "token")
    vocabulary <- dplyr::summarize_(vocabulary, n = "n()")
    vocabulary <- dplyr::arrange_(vocabulary, "dplyr::desc(n)")
    index <- 1:min(c(max_features, nrow(vocabulary)))
    vocabulary <- vocabulary[["token"]][index]
  }
  if (length(vocabulary) <= 2) {
      stop("vocabulary length is too small to continue")
  }
  x <- dplyr::filter_(x, ~token %in% vocabulary)
  x$token <- factor(x$token, levels = vocabulary)

  id <- x[["id"]]
  mat <- methods::as(Matrix::sparse.model.matrix(~token - 1,
      data = x), "dgTMatrix")
  df <- dplyr::data_frame(id = id[mat@i + 1], lid = mat@j,
      count = mat@x)
  df <- dplyr::group_by_(df, "id", "lid")
  df <- dplyr::summarize_(df, count = "sum(count)")

  count <- NULL
  if (scale) {
    df <- dplyr::group_by_(df, "id")
    df <- dplyr::mutate(df, count = count / sum(count))
    df <- ungroup(df)
  }

  term_counts <- Matrix::spMatrix(nrow = N, ncol = ncol(mat),
      i = df$id, j = df$lid + 1, x = df$count)

  colnames(term_counts) <- vocabulary
  term_counts
}

