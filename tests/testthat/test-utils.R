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

test_that("format_attention and visualize_attention works", {
    # First two examples are single-segment, third is two-segment.
    chicken <- list(
        "The chicken didn't cross the road, because it was too tired.",
        "The chicken didn't cross the road, because it was too wide.",
        c("Why did the chicken cross the road?",
          "To get to the other side.")
    )
    chicken_ex <- RBERT::make_examples_simple(chicken)

    feats_chicken <- RBERT::extract_features(examples = chicken_ex,
                                      vocab_file = vocab_file,
                                      bert_config_file = bert_config_file,
                                      init_checkpoint = init_checkpoint,
                                      layer_indexes = 1:12,
                                      batch_size = 2L)
    attn1 <- format_attention(feats_chicken$attention_probs, seq_num = 1)
    testthat::expect_identical(names(attn1), c("all", "a", "b", "ab", "ba"))
    testthat::expect_identical(attn1$all$top_text[[15]], "tired")
    testthat::expect_identical(length(attn1$all$att), 12L)  # 12 layers
    testthat::expect_identical(length(attn1$all$att[[1]]), 12L) # 12 heads
    testthat::expect_identical(length(attn1$all$att[[1]][[1]]), 17L) # 17 tokens

    # Technically, these tests should be in a different file. But it's so much
    # faster to put them here.
    va1 <- visualize_attention(attn1)
    testthat::expect_s3_class(va1, "htmlwidget")
    testthat::expect_identical(va1$x, attn1)

    attn3 <- format_attention(feats_chicken$attention_probs, seq_num = 3)
    testthat::expect_identical(length(attn3$ab$top_text), 10L) # 10 tokens
    testthat::expect_identical(length(attn3$ab$bot_text), 8L) # 8 tokens

    embeddings <- extract_vectors_df(feats_chicken$layer_outputs)
    testthat::expect_identical(dim(embeddings), c(676L, 773L))

    embeddings2 <- filter_layer_embeddings(embeddings, layer_indices = 12L,
                                           sum_fun = NULL)
    # explicitly passing NULL summary function preserves the layer_index column.
    # We kept 1/13 of the rows (layers).
    testthat::expect_identical(dim(embeddings2), c(52L, 773L))

    embeddings <- filter_layer_embeddings(embeddings, layer_indices = 12L)
    # This way we don't keep the redundant layer_index column.
    testthat::expect_identical(dim(embeddings), c(52L, 772L))

    embeddings <- keep_tokens(embeddings, c("chicken", "road", "it"))
    # down to eight rows (the number of instances of the given words)
    testthat::expect_identical(dim(embeddings), c(8L, 772L))

    pca_plot <- display_pca(embeddings)
    testthat::expect_identical(as.character(pca_plot$labels),
                               c("PC1", "PC2", "token", "class"))
    testthat::expect_equal(length(pca_plot$data$token), 8)

    embeddings2 <- keep_tokens(embeddings, c("it"))
    # too few rows to make a meaningful plot
    testthat::expect_error(pca_plot <- display_pca(embeddings2),
                           "At least three")

    # add color class
    embeddings <- dplyr::mutate(embeddings,
                                b_or_r = c("bird", "road", "bird",
                                           "bird", "road", "road",
                                           "bird", "road"))
    pca_plot <- display_pca(embeddings, color_field = "b_or_r")
    # should be two distinct colors used now
    testthat::expect_equal(length(pca_plot$plot_env$class_colors), 2)
    testthat::expect_warning(pca_plot <- display_pca(embeddings,
                                                     color_field = "typo"),
                             "not found")

    pca_plot <- display_pca(embeddings, hide = c(FALSE, FALSE, FALSE,
                                                 FALSE, FALSE, FALSE,
                                                 TRUE, TRUE))
    testthat::expect_equal(length(pca_plot$data$token), 6)
    testthat::expect_warning(pca_plot <- display_pca(embeddings,
                                                     hide = c(FALSE, FALSE,
                                                              TRUE, TRUE)),
                             "ignored")

    # A package update mysteriously broke the visualization display. Installing
    # (but not necessarily keeping) an old version of Rcpp seemed to fix it. If
    # bug reappears, see if we can test for it somehow using
    # htmlwidgets::saveWidget.
})
