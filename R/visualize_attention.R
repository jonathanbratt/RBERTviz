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

# visualize_attention -----------------------------------------------------

#' Visualize BERT Attention Weights
#'
#' Given data from an RBERT model, display an interactive visualization of the
#' attention weights.
#'
#' @param attention The "attention" component of the output from
#'   \code{\link[RBERT]{extract_features}(..., features = "attention")}.
#' @param sequence_index Integer; which example sequence from the input to
#'   visualize.
#' @param height,width  Characters; height and width of the htmlwidget
#'          specified in any valid \code{CSS} size unit.
#' @param elementId Character; a valid \code{CSS} element id.
#'
#' @return the output from \code{htmlwidgets::createWidget}.
#' @export
#'
#' @examples
#' \dontrun{
#' # assuming something like the following has been run:
#' # feats <- RBERT::extract_features(...) # See RBERT documentation
#' # Then:
#' visualize_attention(feats$attention)
#' }
visualize_attention <- function(attention,
                                sequence_index = 1,
                                width = NULL,
                                height = NULL,
                                elementId = NULL) {
  attn_obj <- .format_attention(attention, sequence_index)

  # create widget
  htmlwidgets::createWidget(
    name = 'visualize_attention',
    x = attn_obj,
    width = width,
    height = height,
    package = 'RBERTviz',
    elementId = elementId,
    # The [[1]] weirdness is because the r2d3 function already wraps its
    # return in a list.
    dependencies = list(r2d3::html_dependencies_d3(version = "3")[[1]],
                        rmarkdown::html_dependency_jquery())
  )
}


# automatically generated helper functions --------------------------------


#' Shiny bindings for visualize_attention
#'
#' Output and render functions for using visualize_attention within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a visualize_attention
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name visualize_attention-shiny
#'
#' @export
visualize_attentionOutput <- function(outputId,
                                      width = '100%', height = '400px'){
  # nocov start
  htmlwidgets::shinyWidgetOutput(outputId,
                                 'visualize_attention',
                                 width, height,
                                 package = 'RBERTviz')
  # nocov end
}

#' @rdname visualize_attention-shiny
#' @export
renderVisualize_attention <- function(expr,
                                      env = parent.frame(),
                                      quoted = FALSE) {
  # nocov start

  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr,
                                 visualize_attentionOutput,
                                 env,
                                 quoted = TRUE)
  # nocov end
}


# custom html helper function ---------------------------------------------


#' Customize Visualizer html
#'
#' "This function is looked up within the package implementing the widget by the
#' convention widgetname_html so it need not be formally exported from your
#' package or otherwise registered with htmlwidgets."
#'
#' You shouldn't need to call this function directly.
#'
#' @param id Parameter used by \code{htmlwidgets}.
#' @param style Parameter used by \code{htmlwidgets}.
#' @param class Parameter used by \code{htmlwidgets}.
#' @param ... Other parameters used by \code{htmlwidgets}.
#'
#' @return The output from htmltools::tags$span(). I think this should be
#'   properly formatted html, as a string.
#' @keywords internal
visualize_attention_html <- function(id, style, class, ...) {
  # nocov start
  html_str <- '<span style="user-select:none">
                Layer: <select id="layer"></select>
                    Attention: <select id="att_type">
                        <option value="all">All</option>
                        <option value="a">Sentence A self-attention</option>
                        <option value="b">Sentence B self-attention</option>
                        <option value="ab">Sentence A -> Sentence B</option>
                        <option value="ba">Sentence B -> Sentence A</option>
                    </select>
                </span>
                <div id="vis"></div>'
  htmltools::tags$span(id = id, class = class, htmltools::HTML(html_str))
  # nocov end
}


