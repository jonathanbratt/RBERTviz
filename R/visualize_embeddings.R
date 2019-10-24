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

# extract_vectors_df ---------------------------------------------------------

#' Extract Embedding Vectors
#'
#' Extract the embedding vector values from output of
#' \code{\link[RBERT]{extract_features}}. The resulting tbl_df will typically
#' have a large number of columns (> 768), so it will be rather slow to
#' \code{\link{View}}. Consider using \code{\link[dplyr]{glimpse}} or
#' \code{\link[useful]{corner}} if you just want to peek at the values.
#'
#' @param layer_outputs The \code{layer_outputs} component of
#'   \code{\link[RBERT]{extract_features}} output.
#'
#' @return The embedding vector components as a tbl_df, for all tokens and all
#'   layers.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' embeddings <- extract_vectors_df(feats$layer_outputs)
#' }
extract_vectors_df <- function(layer_outputs) {
    vals <- .extract_values(layer_outputs)
    labs <- .extract_labels(layer_outputs)
    return(dplyr::bind_cols(labs, vals))
}





# extract embeddings values and labels ----------------------------------------

#' Extract Embeddings
#'
#' Extract the embedding vector values from output of
#' \code{\link[RBERT]{extract_features}}. The columns identifying example
#' sequence, segment, token, and row are extracted separately, by
#' \code{.extract_labels}.
#'
#' @param layer_outputs The \code{layer_outputs} component of
#'   \code{\link[RBERT]{extract_features}} output.
#'
#' @return The embedding vector components as a tbl_df, for all tokens and all
#'   layers.
#' @keywords internal
#' @noRd
.extract_values <- function(layer_outputs) {
    vec_len <- length(
        layer_outputs$example_1$features$token_1$layers[[1]]$values
    )
    tmat <- purrr::map(
        layer_outputs,
        function(seq_data) {
            tmat2 <- purrr::map(
                seq_along(seq_data$features),
                function(tok_index) {
                    tok_data <- seq_data$features[[tok_index]]
                    t(vapply(
                        tok_data$layers,
                        function(layer_data) {layer_data$values},
                        FUN.VALUE = numeric(vec_len) ))
                })
            do.call(rbind, tmat2)
        })
    tmat <- do.call(rbind, tmat)
    colnames(tmat) <- paste0("V", seq_len(vec_len))
    return(tibble::as_tibble(tmat))
}

#' Extract Labels for Embeddings
#'
#' Extract the label columns for embedding vector values from output of
#' \code{\link[RBERT]{extract_features}}.
#'
#' @param layer_outputs The \code{layer_outputs} component of
#'   \code{\link[RBERT]{extract_features}} output.
#'
#' @return The embedding vector components as a tbl_df, for all tokens and all
#'   layers.
#' @keywords internal
#' @noRd
.extract_labels <- function(layer_outputs) {
    lab_df <- purrr::map_dfr(
        layer_outputs,
        function(ex_data) {
            purrr::map_dfr(
                seq_along(ex_data$features),
                function(tok_index) {
                    tok_data <- ex_data$features[[tok_index]]
                    purrr::map_dfr(
                        tok_data$layers,
                        function(layer_data) {
                            layer_index <- layer_data$index
                            ex_index <- ex_data$linex_index
                            tok_str <- tok_data$token
                            tib <- tibble::tibble(sequence_index = ex_index,
                                                  token_index = tok_index,
                                                  token = tok_str,
                                                  layer_index = layer_index)
                        })
                })
        })
    # We want to add a column to index which segment (within each example
    # sequence; either 1 or 2) each token belongs to. By the time we get to this
    # point in the process, the only way to identify tokens in the second
    # segment is the rule that every token after the first [SEP] token is in the
    # second segment.
    lab_df <- lab_df %>%
        dplyr::mutate(is_sep = token == "[SEP]") %>%
        dplyr::group_by(sequence_index, layer_index) %>%
        dplyr::mutate(segment_index = cumsum(is_sep) - is_sep + 1) %>%
        dplyr::select(sequence_index,
                      segment_index,
                      token_index,
                      token,
                      layer_index) %>%
        dplyr::ungroup()
    return(lab_df)
}



# filter_layer_embeddings ------------------------------------------------------


#' Filter and Combine Embedding Vectors
#'
#' Applies a filter + summarize to the given table of embeddings. Can be used to
#' filter to specific layer outputs, or to average the outputs from multiple
#' layers.
#'
#' @param embedding_df A tbl_df of embedding vectors; the output of
#'   \code{\link{extract_vectors_df}}.
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
#' embeddings <- extract_vectors_df(feats$layer_outputs)
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
#'   \code{\link{extract_vectors_df}}.
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
#' embeddings <- extract_vectors_df(feats$layer_outputs)
#' embeddings_layer12_cls <- embeddings %>%
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
#' @param embedding_df A tbl_df of embedding vectors; the output of
#'   \code{\link{extract_vectors_df}}.
#' @param class Character vector; which tokens to keep.
#' @param color_field Character scalar; optional column name to assign to color
#'   aesthetic in the plot.
#' @param disambiguate_tokens Logical; whether to append example and token
#'   index to the literal token for display purposes.
#' @param hide Optional logical vector of same length as
#'   \code{nrow(embedding_df)}; if present, only rows corresponding to TRUE will
#'   be displayed on the plot (all rows will still be used in the computation of
#'   the plot). This makes it easy to selectively display points without
#'   redefining the principal axes.
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
#' embeddings <- extract_vectors_df(feats$layer_outputs)
#' embeddings %>%
#'     filter_layer_embeddings(layer_indices = 12L) %>%
#'     keep_tokens("[CLS]") %>%
#'     display_pca()
#' }
display_pca <- function(embedding_df,
                        color_field = NULL,
                        disambiguate_tokens = TRUE,
                        hide = NULL) {
    num_rows <- nrow(embedding_df)
    if (num_rows < 3) {
        stop("At least three vectors are required for a ",
                "meaningful PCA plot.")
    }
    if (disambiguate_tokens) {
        embedding_df <- dplyr::mutate(embedding_df,
                                      token = paste(token,
                                                    sequence_index,
                                                    token_index,
                                                    sep = "."))
    }
    # Just keep all the non-vector columns, for possible use in plotting.
    tok_labels <- dplyr::select(embedding_df,
                                -dplyr::matches("V[0-9]+"))

    vec_mat <- as.matrix(
        dplyr::select(embedding_df,
                      dplyr::matches("V[0-9]+"))
    )
    pcs <- stats::prcomp(vec_mat,
                         retx = TRUE, center = TRUE, scale. = TRUE, rank. = 2L)
    pc_tbl <- dplyr::bind_cols(tok_labels, tibble::as_tibble(pcs$x))

    class <- rep("a", num_rows)
    if (!is.null(color_field)) {
        if (color_field %in% names(embedding_df)) {
            class <- dplyr::pull(
                dplyr::select(embedding_df, dplyr::one_of(color_field))
            )
        } else {
            warning("Column ", color_field, " not found in input table." )
        }
    }
    class <- as.factor(class)

    unique_classes <- unique(class)
    num_class <- length(unique_classes)
    getPalette <- grDevices::colorRampPalette(
        RColorBrewer::brewer.pal(8, "Dark2")
    )
    pal <- getPalette(num_class)
    class_colors <- pal
    names(class_colors) <- unique_classes

    if (!is.null(hide)) {
        if (length(hide) == nrow(pc_tbl)) {
            pc_tbl <- pc_tbl[!hide, ]
            class <- class[!hide]
        } else {
            warning("Length of hide parameter doesn't match size of ",
                    "input table, and will be ignored.")
        }
    }

    ggp <- ggplot2::ggplot(pc_tbl, ggplot2::aes(x = PC1, y = PC2,
                              label = token,
                              col = class)) +
        ggplot2::scale_color_manual(values = class_colors,
                           name = color_field) +
        ggplot2::geom_text(vjust = 0, nudge_y = 0.5) +
        ggplot2::geom_point()
    if (num_class <= 1) {
        ggp <- ggp + ggplot2::theme(legend.position = "none")
    }
    return(ggp)
}




