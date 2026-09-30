test_that("retro_chat_mock() returns a Chat object", {
  chat <- retro_chat_mock()
  expect_s3_class(chat, "Chat")
})

test_that("retro_chat_mock() starts with no tools", {
  chat <- retro_chat_mock()
  expect_length(chat$get_tools(), 0L)
})

test_that("retro_chat_mock() can register tools", {
  chat <- retro_chat_mock()
  chat$register_tool(ellmer::tool(
    fun = function(x) x,
    name = "dummy",
    description = "a dummy tool",
    arguments = list(x = ellmer::type_number("x"))
  ))
  expect_length(chat$get_tools(), 1L)
})

test_that("retro_chat_mock() can set system prompt", {
  chat <- retro_chat_mock()
  chat$set_system_prompt("hello")
  expect_equal(chat$get_system_prompt(), "hello")
})
