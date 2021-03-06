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

  doc_id <- token <- tf <- NULL
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

#' Keywords in Context
#'
#' @param term          search term as a string
#' @param text          vector of text to search
#' @param n             number of results to return; default is Inf
#' @param ignore_case   should search ignore case? default is TRUE
#' @param width         how many characters to show as context
#'
#' @return  a character vector of the search results
#'
#' @export
sm_kwic <- function(term, text, n = Inf, ignore_case = TRUE, width = 20L)
{
  if (ignore_case) {pre <- "(?i)"} else {pre <- ""}
  these <- stringi::stri_match(text,
    regex = sprintf("%s(.{0,%d})(\\W%s\\W)(.{0,%d})", pre, width, term, width))
  these <- these[!is.na(these[,1]),,drop=FALSE]
  these <- sprintf("%20s|%s|%s", these[,2], these[,3], these[,4])
  if (n < length(these)) { these <- sample(these, n) }
  return(these)
}

#' Skip N-Grams
#'
#' @param object          data frame
#' @param n_min           minimum number of terms
#' @param n               maximum number of terms
#' @param k               number of skips to consider
#' @param doc_var         name of the variable containing the document id
#' @param token_var       name of the variable containing the tokens
#'
#' @return  a data frame
#'
#' @export
sm_skip_ngram <- function (
  object, n_min = 1, n = 3, k = 1, doc_var = "doc_id", token_var = "lemma"
)
{
  words <- split(object[[token_var]], object[[doc_var]])
  skips <- unique(unlist(lapply(n_min:n, tokenizers:::get_valid_skips, k),
                  recursive = FALSE, use.names = FALSE))
  ngrams <- tokenizers:::skip_ngrams_vectorised(words, skips, character())
  out <- tibble::tibble(
    doc_id = rep(names(words), sapply(ngrams, length)), token = unlist(ngrams)
  )
  out <- out[!is.na(out$token),]
  out
}


#' N-Grams
#'
#' @param object          data frame
#' @param n_min           minimum number of terms
#' @param n               maximum number of terms
#' @param doc_var         name of the variable containing the document id
#' @param token_var       name of the variable containing the tokens
#'
#' @return  a data frame
#'
#' @export
sm_ngram <- function (
  object, n_min = 1, n = 3, doc_var = "doc_id", token_var = "lemma"
)
{
  words <- split(object[[token_var]], object[[doc_var]])
  ngrams <- tokenizers:::generate_ngrams_batch(words, ngram_min = n_min,
      ngram_max = n, stopwords = character(), ngram_delim = " ")
  out <- tibble::tibble(
    doc_id = rep(names(words), sapply(ngrams, length)), token = unlist(ngrams)
  )
  out <- out[!is.na(out$token),]
  out
}



#' Compute Dunn Likelihood scores
#'
#' @param object          data frame
#' @param group_name      name of the variable containing the groups
#' @param token_name      name of the variable containing the tokens
#'
#' @return  a data frame
#'
#' @export
sm_dunn_ll <- function(object, group_name, token_name = "lemma")
{
  groups <- object[[group_name]]
  tokens <- object[[token_name]]
  stopifnot(length(grps <- unique(groups)) == 2)

  lvl <- unique(tokens)
  a <- table(factor(tokens[groups == grps[1]], levels = lvl))
  b <- table(factor(tokens[groups == grps[2]], levels = lvl))
  c <- sum(groups == grps[1])
  d <- sum(groups == grps[2])

  e1 <- c * (a + b) / (c + d)
  e2 <- d * (a + b) / (c + d)

  ll1 <- a * log(a / e1)
  ll2 <- b * log(b / e2)
  ll1[is.na(ll1)] <- 0
  ll2[is.na(ll2)] <- 0
  ll <- 2 * (ll1 + ll2)

  index <- which(a * log(a / e1) < 0)
  ll[index] <- ll[index] * -1
  group_dom <- ""
  group_dom[ll > 0] <- grps[1]
  group_dom[ll < 0] <- grps[2]

  tibble::tibble(lemma = names(ll), dunn = as.numeric(ll), group = group_dom)
}

#' Compute Tidy LDA Topic Model
#'
#' @param X              A term-frequency matrix
#' @param num_topics     Integer. Number of topics to learn
#'
#' @return  A named list of two data frames.
#'
#' @export
sm_lda_topics <- function(X, num_topics = 16)
{
  lda_model <- topicmodels::LDA(
    x = X, k = num_topics, control = list(seed = 2811, verbose = 1)
  )

  docs <- tibble::tibble(
    doc_id = rep(rownames(X), num_topics),
    topic = rep(seq_len(num_topics), each = nrow(X)),
    prob = as.numeric(lda_model@gamma)
  )
  terms <- tibble::tibble(
    token = rep(colnames(X), each = num_topics),
    topic = rep(seq_len(num_topics), ncol(X)),
    beta = as.numeric(lda_model@beta)
  )

  return(list(docs = docs, terms = terms))
}

#' Create a data frame of Kmeans clusters
#'
#' @param df             A data frame with a all numeric columns except the
#'                       one containing the document ID
#' @param clusters       Integer. Number of clusters to identify.
#' @param item_name      Name of the column containing the item id.
#'
#' @return  A data frame with a column indicating the cluster ID.
#'
#' @export
sm_kmeans <- function(df, clusters = 3, item_name = "document")
{
  X <- as.matrix(df[,-which(names(df) == item_name)])
  df$cluster <- as.numeric(stats::kmeans(X, centers = clusters)$cluster)
  df
}

#' Create a data frame of Kmeans clusters
#'
#' @param X              A TF-IDF matrix
#' @param max_depth      Depth of the hierarchical clustering algorithm.
#'                       The maximum number of clusters will be 2^(max_depth).
#' @param balance        Should clusters be made to have uniform size.
#'
#' @return  A data frame.
#'
#' @export
sm_spectral_cluster <- function(X, max_depth = 4, balance = FALSE)
{
  tfidf <- scale(X, center=FALSE)
  A <- tcrossprod(tfidf) / ncol(tfidf)
  diag(A) <- 0
  Dinvroot <- diag(1 / sqrt(apply(A, 1, sum)))
  L <- diag(nrow(A)) - Dinvroot %*% A %*% Dinvroot

  # cycle over the second eigenvalues
  groups <- rep(0, nrow(L))
  for (depth in seq(1, max_depth)) {

    new_groups <- groups
    second_vals <- rep(0, length(groups))

    for (g in unique(groups)) {
      index <- which(groups == g)
      e <- eigen(L[index,index])
      vals <- e$vector[,ncol(e$vector)-1]
      if (balance) { m <- stats::median(vals) } else { m <- 0 }
      new_groups[index][vals > m]  <- g * 10 + 1
      new_groups[index][vals <= m] <- g * 10
      second_vals[index] <- vals
    }
    groups <- new_groups
  }
  groups <- sprintf(sprintf("%%0%dd", max_depth), groups)

  ntopics <- length(unique(groups))
  docs <- matrix(0, nrow=nrow(L), ncol=ntopics)
  for (i in 1:length(unique(groups)))
  {
    docs[groups == unique(groups)[i],i] <- 1
  }

  return(tibble::tibble(
    doc_id = rownames(X),
    cluster = as.numeric(factor(groups)),
    groups = groups
  ))
}
