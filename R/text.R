#' Construct the TF-IDF Matrix from Annotation or Data Frame
#'
#' Given annotations, this function returns the term-frequency inverse
#' document frequency (tf-idf) matrix from the extracted lemmas.
#'
#' @param  object       a data frame containing an identifier for the document
#'                      (set with \code{doc_var}) and token (set with
#'                      \code{token_var})
#' @param min_df        the minimum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_df        the maximum proportion of documents a token
#'                      should be in to be included in the vocabulary
#' @param max_features  the maximum number of tokens in the vocabulary
#' @param doc_var       character vector. The name of the column in
#'                      \code{object} that contains the document ids. Defaults
#'                      to "doc_id".
#' @param token_var     character vector. The name of the column in
#'                      \code{object} that contains the tokens. Defaults to
#'                      "lemma".
#' @param vocabulary    character vector. The vocabulary set to use in
#'                      constructing the matrices. Will be computed
#'                      within the function if set to \code{NULL}. When
#'                      supplied, the options \code{min_df}, \code{max_df},
#'                      and \code{max_features} are ignored.
#'
#' @return  a tibble in wide format with term frequencies and tf-idf values.
#'
#' @export
#' @name cnlp_utils_tfidf
sm_text_tfidf <- function(
  object,
  min_df=0.1,
  max_df=0.9,
  max_features=1e4,
  doc_var="doc_id",
  token_var="lemma",
  vocabulary=NULL
) {

  assert(inherits(object, "data.frame"), "'input' must be a data frame.")
  assert(doc_var %in% names(object), "no valid 'doc_var' found")
  assert(token_var %in% names(object), "no valid 'token_var' found")

  x <- data.frame(
    doc_id = object[[doc_var]],
    token = object[[token_var]],
    stringsAsFactors=FALSE
  )

  N <- length(unique(x$doc_id))

  if (is.null(vocabulary)) {
    possible_vocab <- table(x[!duplicated(x),]$token) / N
    possible_vocab <- possible_vocab[
      possible_vocab >= min_df & possible_vocab <= max_df
    ]
    possible_vocab <- sort(possible_vocab, decreasing=TRUE)
    vocabulary <- names(possible_vocab[
      seq(1, min(max_features, length(possible_vocab)))
    ])
  }

  assert(length(vocabulary) >= 1, "vocabulary length is too small to continue")

  # create counts
  x <- x[x$token %in% vocabulary, ]
  tf_tibble <- dplyr::group_by(x, doc_id, token)
  tf_tibble <- dplyr::summarize(tf_tibble, tf = dplyr::n())
  tf_tibble <- dplyr::group_by(tf_tibble, token)
  tf_tibble <- dplyr::mutate(tf_tibble,
    tfidf = (1 + log2(tf)) * log2(N / dplyr::n())
  )
  tf_tibble <- dplyr::ungroup(tf_tibble)

  return(tf_tibble)
}
