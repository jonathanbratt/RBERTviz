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

# format_attention --------------------------------------------------------


#' Format BERT Attention Weights for Visualizing
#'
#' Given data from an RBERT model, format the attention weights matrices in the
#' structure expected by the javascript visualization code.
#'
#' @inheritParams visualize_attention
#'
#' @return an object structured appropriately for input (as eventual json) to
#'   the javascript visualization code.
#' @keywords internal
.format_attention <- function(attention, sequence_index = 1) {
  in_seq_index <- sequence_index
  this_seq_data <- attention %>%
    dplyr::filter(sequence_index == in_seq_index) %>%
    dplyr::select(
      layer_index, head_index,
      segment_index,
      token_index, token,
      attention_segment_index,
      attention_token_index, attention_token,
      attention_weight
    )

  token_list_a <- .get_attention_tokens(this_seq_data, 1)
  token_list_b <- .get_attention_tokens(this_seq_data, 2)
  token_list <- c(token_list_a, token_list_b)

  return(
    list(
      all = list(
        att = .enlist_attention(this_seq_data, c(1, 2), c(1, 2)),
        top_text = as.list(token_list),
        bot_text = as.list(token_list)
      ),
      a = list(
        att = .enlist_attention(this_seq_data, 1, 1),
        top_text = as.list(token_list_a),
        bot_text = as.list(token_list_a)
      ),
      b = list(
        att = .enlist_attention(this_seq_data, 2, 2),
        top_text = as.list(token_list_b),
        bot_text = as.list(token_list_b)
      ),
      ab = list(
        att = .enlist_attention(this_seq_data, 1, 2),
        top_text = as.list(token_list_a),
        bot_text = as.list(token_list_b)
      ),
      ba = list(
        att = .enlist_attention(this_seq_data, 2, 1),
        top_text = as.list(token_list_b),
        bot_text = as.list(token_list_a)
      )
    )
  )
}

#' Convert an Attention df to a List
#'
#' @param attention_tibble A tibble of attention, with minimal columns
#'   layer_index, head_index, token_index, attention_token_index, and
#'   attention_weight.
#' @param top_segment_index Integer; the index of the first/top segment for this
#'   set. Can be a vector (ie, \code{c(1, 2)} for "all").
#' @param bottom_segment_index Integer; the index of the second/bottom segment
#'   for this set. Can be a vector (ie, \code{c(1, 2)} for "all").
#'
#' @return A list as expected by \code{\link{.format_attention}}.
#' @keywords internal
.enlist_attention <- function(attention_tibble,
                              top_segment_index,
                              bottom_segment_index) {
  return(
    attention_tibble %>%
      dplyr::filter(
        segment_index %in% top_segment_index,
        attention_segment_index %in% bottom_segment_index
      ) %>%
      dplyr::select(
        layer_index,
        head_index,
        token_index,
        attention_token_index,
        attention_weight
      ) %>%
      tidyr::nest(
        target_weights = c(attention_token_index, attention_weight)
      ) %>%
      tidyr::nest(token_weights = c(token_index, target_weights)) %>%
      tidyr::nest(head_weights = c(head_index, token_weights)) %>%
      dplyr::pull(head_weights) %>%
      purrr::map(
        function(this_head) {
          purrr::map(
            this_head$token_weights,
            function(this_token) {
              purrr::map(
                this_token$target_weights,
                function(this_target) {
                  as.list(this_target$attention_weight)
                }
              )
            }
          )
        }
      )
  )
}

#' Pull the Designated Tokens from the Attention tibble
#'
#' @param attention The attention component from
#'   \code{\link[RBERT]{extract_features}(..., features = "attention")}.
#' @param this_segment_index The segment to pull.
#'
#' @return A character vector of tokens.
#' @keywords internal
.get_attention_tokens <- function(attention, this_segment_index) {
  return(
    attention %>%
      dplyr::filter(
        layer_index == min(layer_index),
        head_index == min(head_index),
        attention_token_index == min(attention_token_index),
        segment_index == this_segment_index
      ) %>%
      dplyr::arrange(token_index) %>%
      dplyr::pull(token)
  )
}
