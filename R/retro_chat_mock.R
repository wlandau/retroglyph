#' @title Mock chat constructor
#' @export
#' @family chats
#' @description Create an `ellmer` chat object that does not
#'   authenticate or connect to any real provider. Useful for
#'   testing and development without network access.
#' @details The returned chat supports registering tools, setting
#'   system prompts, and all other local operations. It will
#'   error only if you attempt to send a message to the model.
#' @return An `ellmer` chat object.
#' @examples
#'   chat <- retro_chat_mock()
#'   chat$get_system_prompt()
retro_chat_mock <- function() {
  ellmer::chat_openai_compatible(
    base_url = "http://localhost:57000",
    model = "mock"
  )
}
