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
  # See RBERT_data.R for how feats_chicken is created.

  # feats_chicken <- readRDS(
  #   here::here("tests", "testthat", "feats_chicken.rds")
  # )
  # Goodpractice doesn't like here, so also provide a path for that, and
  # comment out the more convenient here version.
  feats_chicken <- readRDS("feats_chicken.rds")
  # attn1_expected <- readRDS(
  #   here::here("tests", "testthat", "attn1.rds")
  # )
  attn1_expected <- readRDS("attn1.rds")
  attn1 <- .format_attention(feats_chicken$attention, sequence_index = 1)
  testthat::expect_identical(names(attn1), c("all", "a", "b", "ab", "ba"))
  testthat::expect_identical(attn1$all$top_text[[15]], "tired")
  testthat::expect_identical(length(attn1$all$att), 12L)  # 12 layers
  testthat::expect_identical(length(attn1$all$att[[1]]), 12L) # 12 heads
  testthat::expect_identical(length(attn1$all$att[[1]][[1]]), 17L) # 17 tokens

  # Leave some room for slight differences between systems.
  tol <- 1e-5
  expect_length(
    unlist(attn1$a$att)[
      abs(unlist(attn1$a$att) - unlist(attn1_expected$a$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn1$b$att)[
      abs(unlist(attn1$b$att) - unlist(attn1_expected$b$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn1$ab$att)[
      abs(unlist(attn1$ab$att) - unlist(attn1_expected$ab$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn1$ba$att)[
      abs(unlist(attn1$ba$att) - unlist(attn1_expected$ba$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn1$all$att)[
      abs(unlist(attn1$all$att) - unlist(attn1_expected$all$att)) > tol
      ],
    0
  )

  # Technically, these tests should be in a different file. But it's so much
  # faster to put them here.
  va1 <- visualize_attention(feats_chicken$attention, 1)
  testthat::expect_s3_class(va1, "htmlwidget")
  testthat::expect_identical(va1$x, attn1)

  # attn3_expected <- readRDS(
  #   here::here("tests", "testthat", "attn3.rds")
  # )
  attn3_expected <- readRDS("attn3.rds")
  attn3 <- .format_attention(feats_chicken$attention, sequence_index = 3)
  testthat::expect_identical(length(attn3$ab$top_text), 10L) # 10 tokens
  testthat::expect_identical(length(attn3$ab$bot_text), 8L) # 8 tokens

  expect_length(
    unlist(attn3$a$att)[
      abs(unlist(attn3$a$att) - unlist(attn3_expected$a$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn3$b$att)[
      abs(unlist(attn3$b$att) - unlist(attn3_expected$b$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn3$ab$att)[
      abs(unlist(attn3$ab$att) - unlist(attn3_expected$ab$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn3$ba$att)[
      abs(unlist(attn3$ba$att) - unlist(attn3_expected$ba$att)) > tol
      ],
    0
  )
  expect_length(
    unlist(attn3$all$att)[
      abs(unlist(attn3$all$att) - unlist(attn3_expected$all$att)) > tol
      ],
    0
  )


  embeddings <- feats_chicken$output
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
                             c("PC1", "PC2", "token"))
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
  # Test against bug. Should not be a collision if "class" is already a field.
  embeddings3 <- dplyr::mutate(embeddings,
                              b_or_r = "thing",
                              class = c("bird", "road", "bird",
                                        "bird", "thing", "road",
                                        "bird", "road"))
  display_pca(embeddings3, color_field = "b_or_r")
  pca_plot <- display_pca(embeddings3, color_field = "b_or_r")
  testthat::expect_error(print(pca_plot), NA) # ~ "expect_no_error"

  # A package update mysteriously broke the visualization display. Installing
  # (but not necessarily keeping) an old version of Rcpp seemed to fix it. If
  # bug reappears, see if we can test for it somehow using
  # htmlwidgets::saveWidget.
})
