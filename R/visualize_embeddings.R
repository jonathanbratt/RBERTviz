# Copyright 2019 Bedford Freeman & Worth Pub Grp LLC DBA Macmillan Learning.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# filter_layer_embeddings ------------------------------------------------------


#' Filter and Combine Embedding Vectors
#'
#' Applies a filter + summarize to the given table of embeddings. Can be used to
#' filter to specific layer outputs, or to average the outputs from multiple
#' layers.
#'
#' @param embedding_df A tbl_df of embedding vectors; the "output" component of
#'   \code{\link[RBERT]{extract_features}}.
#' @param layer_indices Integer vector; which layers embeddings to keep.
#' @param sum_fun A summarizing function to apply to the embedding vector
#'   components of the retained layers. Will be passed as the \code{.funs}
#'   argument to \code{\link[dplyr]{summarize_at}}. By default,
#'   \code{\link{mean}} is used, but other functions are possible. If this
#'   parameter is given a NULL value, no summarizing will be done, and only the
#'   filtered tbl_df will be returned.
#'
#' @return The input tbl_df of embedding vectors, with the specified filtering
#'   and summarizing applied.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' embeddings <- feats$output
#' embeddings_layer1 <- embeddings %>%
#'     filter_layer_embeddings(layer_indices = 1L)
#' # To keep the layer_index column, use sum_fun = NULL:
#' embeddings_layer1b <- embeddings %>%
#'     filter_layer_embeddings(layer_indices = 1L, sum_fun = NULL)
#' # Take the component-wise mean of the embeddings across layers 10-12.
#' embeddings_layer_mean <- embeddings %>%
#'     filter_layer_embeddings(layer_indices = 10:12)
#' # Take the component-wise maximum of the embeddings across layers 8-12.
#' # (Why would we do this? I don't know.)
#' embeddings_layer_max <- embeddings %>%
#'     filter_layer_embeddings(layer_indices = 8:12, sum_fun = max)
#' }
filter_layer_embeddings <- function(embedding_df,
                                    layer_indices = 12L,
                                    sum_fun = mean) {
  layers_filtered <- dplyr::filter(embedding_df,
                                   layer_index %in% layer_indices)

  if (is.null(sum_fun)) {
    return(layers_filtered)
  }

  layers_combined <- layers_filtered %>%
    dplyr::group_by(sequence_index, segment_index, token_index, token)
  layers_combined <- layers_combined %>%
    dplyr::summarize_at(.vars = dplyr::vars(dplyr::matches("V[0-9]+")),
                        .funs = sum_fun) %>%
    dplyr::ungroup()
  return(layers_combined)
}

# feats_com <- combine_layer_embeddings(vecs_df, 10:12)
# feats_com <- filter_layer_embeddings(vecs_df, 10:12, sum_fun=NULL)
# useful::corner(vecs_df %>% filter(layer_index>=10), c=7, r=10)
# useful::corner(feats_com, c=7, r=6)

# keep_tokens -------------------------------------------------------------

#' Filter Tokens
#'
#' Keeps only specified tokens in the given table of embeddings.
#'
#' @param embedding_df A tbl_df of embedding vectors; the output of
#'   \code{\link[RBERT]{extract_features}}.
#' @param tokens Character vector; which tokens to keep.
#'
#' @return The input tbl_df of embedding vectors, with the specified filtering
#'   applied.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' embeddings_layer12_cls <- feats$output %>%
#'     filter_layer_embeddings(layer_indices = 12L) %>%
#'     keep_tokens("[CLS]")
#' }
keep_tokens <- function(embedding_df, tokens = "[CLS]") {
  filtered_df <- dplyr::filter(embedding_df,
                               token %in% tokens)
  return(filtered_df)
}



# display_pca -------------------------------------------------------------

#' Display PCA of Embeddings
#'
#' Display a 2D PCA plot of a collection of embedding vectors.
#'
#' @param embedding_df A tbl_df of embedding vectors; from the output of
#'   \code{\link[RBERT]{extract_features}}.
#' @param project_vectors A tbl_df of embedding vectors to be used for
#'   calculating the PCA projection matrix. Defaults to \code{embedding_df}.
#'   This makes it possible to more consistently select the PCA "perspective",
#'   even as the set of vectors may change.
#' @param class Character vector; which tokens to keep.
#' @param color_field Character scalar; optional column name to assign to color
#'   aesthetic in the plot.
#' @param disambiguate_tokens Logical; whether to append example and token
#'   index to the literal token for display purposes.
#'
#' @return A ggplot2 plot of the embedding vectors projected onto two principal
#'   axes.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' feats$output %>%
#'     filter_layer_embeddings(layer_indices = 12L) %>%
#'     keep_tokens("[CLS]") %>%
#'     display_pca()
#' }
display_pca <- function(embedding_df,
                        project_vectors = embedding_df,
                        color_field = NULL,
                        disambiguate_tokens = TRUE) {
  pca_df <- do_pca(embedding_df = embedding_df,
                   project_vectors = project_vectors,
                   disambiguate_tokens = disambiguate_tokens)
  return(
    plot_pca(pca_df = pca_df,
             color_field = color_field)
  )
}

# do_pca -------------------------------------------------------------

#' Do PCA on Embeddings
#'
#' Calculate the 2D PCA for a collection of embedding vectors, in preparation
#' for plotting.
#'
#' @inheritParams display_pca
#'
#' @return A tbl_df of the embedding vectors projected onto two principal
#'   axes.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' pca_df <- feats$output %>%
#'     filter_layer_embeddings(layer_indices = 12L) %>%
#'     keep_tokens("[CLS]") %>%
#'     do_pca()
#' }
do_pca <- function(embedding_df,
                   project_vectors = embedding_df,
                   disambiguate_tokens = TRUE) {
  num_rows <- nrow(embedding_df)
  num_rows_proj <- nrow(project_vectors)
  if (num_rows_proj < 3) {
    stop("At least three vectors are required for a ",
         "meaningful PCA plot.")
  }
  # Use just the indicated vectors to select the PCA projection.
  proj_mat <- as.matrix(
    dplyr::select(project_vectors,
                  dplyr::matches("V[0-9]+"))
  )
  pcs <- stats::prcomp(proj_mat,
                       retx = TRUE, center = TRUE, scale. = TRUE, rank. = 2L)

  if (disambiguate_tokens) {
    embedding_df <- dplyr::mutate(embedding_df,
                                  token = paste(token,
                                                sequence_index,
                                                token_index,
                                                sep = "."))
  }
  # Keep all the non-vector columns, for possible use in plotting.
  tok_labels <- dplyr::select(embedding_df,
                              -dplyr::matches("V[0-9]+"))

  vec_mat <- as.matrix(
    dplyr::select(embedding_df,
                  dplyr::matches("V[0-9]+"))
  )
  # Instead of doing PCA here, apply the scaling and projection manually.
  vec_mat <- scale(vec_mat, center = pcs$center, scale = pcs$scale)
  projected <- vec_mat %*% pcs$rotation

  pca_df <- dplyr::bind_cols(tok_labels, tibble::as_tibble(projected))
  class(pca_df) <- c("rbert_pca", class(pca_df)) # for autoplot later?
  return(pca_df)
}

# plot_pca -------------------------------------------------------------

#' Plot PCA
#'
#' Given the output of \code{do_pca}, make the plot object.
#'
#' @param pca_df A tbl_df of pca vectors; output from a call to \code{do_pca}.
#' @inheritParams display_pca
#'
#' @return A ggplot2 plot of the embedding vectors projected onto two principal
#'   axes.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' feats$output %>%
#'     filter_layer_embeddings(layer_indices = 12L) %>%
#'     keep_tokens("[CLS]") %>%
#'     do_pca() %>%
#'     autoplot()
#' }
plot_pca <- function(pca_df,
                     color_field = NULL) {
  # Make the base ggplot object...
  ggp <- ggplot2::ggplot(pca_df, ggplot2::aes(x = PC1, y = PC2,
                                              label = token))

  # ...iff `color_field` was specified, handle all the color stuff here...
  if (!is.null(color_field)) {
    if (color_field %in% names(pca_df)) {
      # Avoid collisions with existing column names by explicitly specifying
      # environment of variable passed to `aes`.
      this_env <- new.env()
      this_env$class <- dplyr::pull(
        dplyr::select(pca_df, dplyr::one_of(color_field))
      )
      this_env$class <- as.factor(this_env$class)
      unique_classes <- unique(this_env$class)
      num_class <- length(unique_classes)
      getPalette <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(8, "Dark2")
      )

      pal <- getPalette(num_class)
      class_colors <- pal
      names(class_colors) <- unique_classes
      ggp <- ggp +
        ggplot2::aes(color = this_env$class) +
        ggplot2::scale_color_manual(values = class_colors,
                                    name = color_field)
    } else {
      warning("Column ", color_field, " not found in input table." )
    }
  }

  # ... finally, apply the geoms.
  ggp <- ggp +
    ggplot2::geom_text(vjust = 0, nudge_y = 0.5) +
    ggplot2::geom_point()

  return(ggp)
}


