test_that("retro_image_png() converts JPEG to PNG", {
  input <- system.file("images", "building.jpg", package = "magick")
  skip_if(input == "", "magick sample JPEG not available")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(output))
  retro_image_png(input = input, output = output)
  expect_true(file.exists(output))
  info <- magick::image_info(magick::image_read(output))
  expect_equal(info$format, "PNG")
})

test_that("retro_image_png() rasterizes SVG to PNG", {
  svg <- paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"30\">",
    "<rect width=\"40\" height=\"30\" fill=\"#3030a0\"/></svg>"
  )
  input <- tempfile(fileext = ".svg")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  writeLines(svg, input)
  retro_image_png(input = input, output = output)
  info <- magick::image_info(magick::image_read(output))
  expect_equal(info$format, "PNG")
  # The blue fill should dominate the rasterized output.
  counts <- retro_color_count(output, c("#3030a0", "#000000"))
  expect_true(counts[1L] > counts[2L])
})

test_that("retro_image_png() preserves a non-white background", {
  # A blue rect on a solid black background must stay black-dominant: the
  # conversion never forces a white (or any) background.
  svg <- paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"40\" height=\"30\">",
    "<rect width=\"40\" height=\"30\" fill=\"#000000\"/>",
    "<rect x=\"0\" y=\"0\" width=\"8\" height=\"30\" fill=\"#3030a0\"/></svg>"
  )
  input <- tempfile(fileext = ".svg")
  output <- tempfile(fileext = ".png")
  on.exit(unlink(c(input, output)))
  writeLines(svg, input)
  retro_image_png(input = input, output = output)
  counts <- retro_color_count(output, c("#000000", "#3030a0"))
  expect_true(counts[1L] > counts[2L])
})

test_that("retro_image_png() validates its arguments", {
  output <- tempfile(fileext = ".png")
  expect_error(
    retro_image_png(input = c("a.png", "b.png"), output = output),
    "input must be a single string"
  )
  expect_error(
    retro_image_png(input = tempfile(fileext = ".png"), output = output),
    "input must exist"
  )
})
