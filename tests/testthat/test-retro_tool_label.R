test_that("retro_tool_label() returns a ToolDef", {
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_label(state)
  expect_s3_class(tool, "ellmer::ToolDef")
})

# Both tests below check ToolDef plumbing (state population, result shape),
# not OCR accuracy, so Tesseract is mocked and the source is a tiny
# synthetic image - see test-retro_do_label.R for the one test that
# exercises real OCR end to end.

test_that("retro_tool_label() populates state correctly", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  state$image_quantized <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(word = "0", confidence = 90, bbox = "1,1,5,5")
    },
    .package = "tesseract"
  )
  tool <- retro_tool_label(state)
  result <- tool()
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_true(file.exists(state$image_label))
  expect_s3_class(state$data_label, "tbl_df")
  expect_true(nrow(state$data_label) >= 1L)
})

test_that("retro_tool_label() result value is a single image", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  state$image_quantized <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(word = "0", confidence = 90, bbox = "1,1,5,5")
    },
    .package = "tesseract"
  )
  tool <- retro_tool_label(state)
  result <- tool()
  expect_true(is.list(result@value))
  expect_length(result@value, 1L)
  expect_s3_class(result@value[[1L]], "ellmer::ContentImage")
})

test_that("retro_tool_label() errors on a second call", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  state$image_quantized <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  local_mocked_bindings(
    tesseract = function(...) NULL,
    ocr_data = function(...) {
      tibble::tibble(word = "0", confidence = 90, bbox = "1,1,5,5")
    },
    .package = "tesseract"
  )
  tool <- retro_tool_label(state)
  tool()
  expect_error(tool(), "already been called once")
})

test_that("retro_tool_label() errors when quantize has not been run", {
  state <- new.env(parent = emptyenv())
  state$image_source <- retro_test_png(matrix(
    "#ffffffff",
    nrow = 20L,
    ncol = 20L
  ))
  tool <- retro_tool_label(state)
  expect_error(tool(), "Call the quantize tool before label")
})
