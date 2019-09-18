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
#' @param att_mat The "attention_probs" component of the output from
#'   \code{RBERT::extract_features()} (details to come).
#' @param seq_num Integer; which example sequence from the input to visualize.
#'
#' @return an object structured appropriately for input (as eventual json) to
#'   the javascript visualization code.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' attn <- format_attention(feats$attention_probs)
#' }
format_attention <- function(att_mat, seq_num = 1) {
    seq_key <- paste0("example_", seq_num)
    this_seq_data <- att_mat[[seq_key]]
    token_list <- this_seq_data$sequence
    split_seq <- split_sequences(token_list)
    a_len <- length(split_seq$a_text)
    b_len <- length(split_seq$b_text)

    all_att_mat <- this_seq_data[grepl(names(this_seq_data),
                                       pattern = "attention")]

    # make one piece at a time...
    all_list <- list("att" = convert_attention_matrix(all_att_mat),
                     "top_text" = as.list(token_list),
                     "bot_text" = as.list(token_list))

    a_att_mat <- purrr::map(all_att_mat, function(att_ten) {
        att_ten[ , 1:a_len, 1:a_len]
    })
    a_list <- list("att" = convert_attention_matrix(a_att_mat),
                   "top_text" = split_seq$a_text,
                   "bot_text" = split_seq$a_text)

    b_att_mat <- purrr::map(all_att_mat, function(att_ten) {
        if (b_len > 0 ) {
            att_ten[ , (a_len+1):(a_len+b_len), (a_len+1):(a_len+b_len)]
        } else {
            list()
        }
    })
    b_list <- list("att" = convert_attention_matrix(b_att_mat),
                   "top_text" = split_seq$b_text,
                   "bot_text" = split_seq$b_text)

    ab_att_mat <- purrr::map(all_att_mat, function(att_ten) {
        if (b_len > 0 ) {
            att_ten[ , 1:a_len, (a_len+1):(a_len+b_len)]
        } else {
            list()
        }
    })
    ab_list <- list("att" = convert_attention_matrix(ab_att_mat),
                    "top_text" = split_seq$a_text,
                    "bot_text" = split_seq$b_text)

    ba_att_mat <- purrr::map(all_att_mat, function(att_ten) {
        if (b_len > 0 ) {
            att_ten[ , (a_len+1):(a_len+b_len), 1:a_len]
        } else {
            list()
        }
    })
    ba_list <- list("att" = convert_attention_matrix(ba_att_mat),
                    "top_text" = split_seq$b_text,
                    "bot_text" = split_seq$a_text)
    ret_list <- list("all" = all_list,
                     "a" = a_list,
                     "b" = b_list,
                     "ab" = ab_list,
                     "ba" = ba_list)
    return(ret_list)
}


# convert_attention_matrix ----------------------------------------------------

#' Restructure Attention Data
#'
#' Converts a list of attention tensors to the nested list format required
#' downstream.
#'
#' @param att_ten_list List of numerical tensors of attention weights. Expected
#'   structure: \code{[layer_index][head_index, token_from_index,
#'   token_to_index]}
#'
#' @return The input attention weights, restructured as nested lists.
#' @keywords internal
convert_attention_matrix <- function(att_ten_list) {
    att_ten_list <- unname(att_ten_list)
    att_list <- purrr::map(att_ten_list, function(this_tensor) {
        # this_tensor should be 3-dimensional, or else empty.
        if (length(dim(this_tensor)) != 3) {
            return(list())
        }

        # Reverse the order of the dimensions
        this_tensor <- aperm(this_tensor)

        # Count the matrices.
        third_dimension <- dim(this_tensor)[[3]]
        purrr::map(seq_len(third_dimension), function(i) {
            # We want it to be a list of lists of lists. A data.frame is really
            # close to that.
            this_list <- this_tensor[,,i] %>%
                as.data.frame() %>%
                dplyr::mutate_all(as.list) %>%
                unclass() %>%
                unname()
            attr(this_list, "row.names") <- NULL
            return(this_list)
        })
    })
    return(att_list)
}


# split_sequences ---------------------------------------------------------

#' Split a Token Sequence
#'
#' Splits a sequence of tokens on the first \code{"[SEP]"} token. Everything
#' up to and including the first \code{"[SEP]"} is in the first segment,
#' everything following is in the second.
#'
#' @param token_list List of tokens.
#'
#' @return Two lists of tokens, divided on the first \code{"[SEP]"} token.
#' @keywords internal
split_sequences <- function(token_list) {
    sep_tok <- "[SEP]"
    first_sep <- token_list == sep_tok
    sep_index <- min(which(first_sep))
    first_seg <- as.list(token_list[seq_len(sep_index)])
    if (sep_index < length(token_list)) {
        second_seg <- as.list(token_list[(sep_index+1):length(token_list)])
    } else {
        second_seg <- list()
    }
    return(list("a_text" = first_seg,
                "b_text" = second_seg))
}
