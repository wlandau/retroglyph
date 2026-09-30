test_that("retro_tool_quantize() returns a ToolDef", {
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_quantize(state)
  expect_s3_class(tool, "ellmer::ToolDef")
})

test_that("retro_tool_quantize() populates state correctly", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  tool <- retro_tool_quantize(state)
  result <- tool()
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_true(file.exists(state$image_quantized))
})

test_that("retro_tool_quantize() result value is a single image", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  tool <- retro_tool_quantize(state)
  result <- tool()
  expect_true(is.list(result@value))
  expect_length(result@value, 1L)
  expect_s3_class(result@value[[1L]], "ellmer::ContentImage")
})

test_that("retro_tool_quantize() extra$display has quantized image", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  tool <- retro_tool_quantize(state)
  result <- tool()
  expect_equal(result@extra$display$title, "Quantized image")
  expect_true(nchar(result@extra$display$html) > 0L)
})

test_that("retro_tool_quantize() errors on a second call", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  tool <- retro_tool_quantize(state)
  tool()
  expect_error(tool(), "already been called once")
})
