#' @importFrom base64enc dataURI
#' @importFrom diffviewer visual_diff
#' @importFrom dplyr n
#' @importFrom ellmer chat_anthropic chat_openai_compatible
#'   content_image_file ContentToolResult tool
#'   type_array type_boolean type_number type_string
#' @importFrom grDevices dev.off
#' @importFrom graphics rect strheight strwidth text
#' @importFrom htmlwidgets saveWidget
#' @importFrom igraph add_edges components E make_empty_graph shortest_paths
#'   vcount
#' @importFrom later later
#' @importFrom magick image_convert image_draw image_info image_quantize
#'   image_raster image_read image_write
#' @importFrom markdown mark
#' @importFrom promises promise then
#' @importFrom R6 R6Class
#' @importFrom R.utils withTimeout
#' @importFrom tesseract ocr_data tesseract
#' @importFrom tools file_ext
#' @importFrom utils globalVariables write.csv
NULL

utils::globalVariables(c("x", "y"))
