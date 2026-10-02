test_that("retro_color_rgb() strips alpha from 8-char hex", {
  expect_equal(retro_color_rgb("#DC3030FF"), "#dc3030")
  expect_equal(retro_color_rgb("#3030A0ff"), "#3030a0")
})

test_that("retro_color_rgb() passes through 6-char hex", {
  expect_equal(retro_color_rgb("#dc3030"), "#dc3030")
  expect_equal(retro_color_rgb("#FFFFFF"), "#ffffff")
})

test_that("retro_color_rgb() lowercases", {
  expect_equal(retro_color_rgb("#DC3030"), "#dc3030")
  expect_equal(retro_color_rgb("#ABCDEF"), "#abcdef")
})

test_that("retro_color_rgb() is idempotent", {
  input <- c("#dc3030", "#ffffff", "#3030a0")
  expect_equal(retro_color_rgb(retro_color_rgb(input)), retro_color_rgb(input))
})

test_that("retro_color_rgb() handles vectors", {
  input <- c("#DC3030FF", "#3030A0FF", "#FFFFFFFF")
  expected <- c("#dc3030", "#3030a0", "#ffffff")
  expect_equal(retro_color_rgb(input), expected)
})

test_that("retro_color_rgba() appends ff to 6-char hex", {
  expect_equal(retro_color_rgba("#dc3030"), "#dc3030ff")
  expect_equal(retro_color_rgba("#FFFFFF"), "#ffffffff")
})

test_that("retro_color_rgba() passes through 8-char hex", {
  expect_equal(retro_color_rgba("#dc3030ff"), "#dc3030ff")
  expect_equal(retro_color_rgba("#FFFFFFFF"), "#ffffffff")
})

test_that("retro_color_rgba() lowercases", {
  expect_equal(retro_color_rgba("#DC3030"), "#dc3030ff")
  expect_equal(retro_color_rgba("#DC3030FF"), "#dc3030ff")
})

test_that("retro_color_rgba() is idempotent", {
  input <- c("#dc3030ff", "#ffffffff", "#3030a0ff")
  expect_equal(
    retro_color_rgba(retro_color_rgba(input)),
    retro_color_rgba(input)
  )
})

test_that("retro_color_rgba() handles vectors", {
  input <- c("#DC3030", "#3030A0", "#FFFFFF")
  expected <- c("#dc3030ff", "#3030a0ff", "#ffffffff")
  expect_equal(retro_color_rgba(input), expected)
})

test_that("retro_color_rgb() and retro_color_rgba() are inverses", {
  six_char <- c("#dc3030", "#3030a0", "#ffffff")
  expect_equal(retro_color_rgb(retro_color_rgba(six_char)), six_char)
  eight_char <- c("#dc3030ff", "#3030a0ff", "#ffffffff")
  expect_equal(retro_color_rgba(retro_color_rgb(eight_char)), eight_char)
})

test_that("retro_color_rgb() converts color names to hex", {
  # magick::image_raster() reports a fully transparent pixel as the name
  # "transparent". Slicing it to 7 characters used to give "transpa", an
  # invalid color name that stopped the pipeline downstream.
  expect_equal(retro_color_rgb("transparent"), "#ffffff")
  expect_equal(retro_color_rgb("white"), "#ffffff")
  expect_equal(retro_color_rgb("black"), "#000000")
})

test_that("retro_color_rgba() converts color names to hex", {
  # Appending "ff" to "transparent" used to give the invalid "transpaff".
  expect_equal(retro_color_rgba("transparent"), "#ffffff00")
  expect_equal(retro_color_rgba("white"), "#ffffffff")
})

test_that("retro_color_rgba() preserves a partial alpha channel", {
  expect_equal(retro_color_rgba("#ff000080"), "#ff000080")
  expect_equal(retro_color_rgba("#00000000"), "#00000000")
})

test_that("retro_color_rgb() drops a partial alpha channel", {
  expect_equal(retro_color_rgb("#ff000080"), "#ff0000")
})

test_that("retro_color_rgb() and retro_color_rgba() accept empty input", {
  expect_equal(retro_color_rgb(character(0L)), character(0L))
  expect_equal(retro_color_rgba(character(0L)), character(0L))
})

test_that("retro_color_rgb() converts a pixel matrix in caller order", {
  # The matrix callers assign back with pixel_matrix[] <- , which keeps
  # the dimensions, so the helper only owes them values in the same order.
  input <- matrix(
    c("#dc3030ff", "#3030a0ff", "#ffffffff", "#000000ff"),
    nrow = 2L
  )
  result <- input
  result[] <- retro_color_rgb(input)
  expect_equal(dim(result), c(2L, 2L))
  expect_equal(result[2L, 1L], "#3030a0")
  expect_equal(result[1L, 2L], "#ffffff")
})

test_that("retro_color_rgb() survives a raster with transparent pixels", {
  # The regression this guards: internal steps such as retro_image_mask()
  # build images from character matrices and never pass through
  # retro_image_png(), so they can meet a raster that still has an alpha
  # channel. Writing the normalized colors back must not error. Assigning
  # with raster[] <- mirrors how those callers use the result.
  raster <- magick::image_raster(
    magick::image_blank(10L, 10L, color = "transparent"),
    tidy = FALSE
  )
  expect_equal(unique(retro_color_rgb(raster)), "#ffffff")
  raster[] <- retro_color_rgba(retro_color_rgb(raster))
  expect_no_error(magick::image_read(raster))
})

test_that("retro_color_valid() accepts 6- and 8-char hex colors", {
  expect_true(retro_color_valid("#DC3030"))
  expect_true(retro_color_valid("#dc3030ff"))
  expect_equal(
    retro_color_valid(c("#FFFFFF", "#00ff00ff")),
    c(TRUE, TRUE)
  )
})

test_that("retro_color_valid() rejects malformed colors", {
  expect_equal(
    retro_color_valid(c("red", "#12", "#GGGGGG", "dc3030", "#1234567")),
    rep(FALSE, 5L)
  )
})

test_that("retro_color_count() counts pixels for each supplied color", {
  raster <- matrix(
    c(rep("#ffffff", 3L), rep("#ff0000", 2L), "#0000ff"),
    nrow = 1L
  )
  image <- retro_test_png(raster)
  on.exit(unlink(image))
  expect_equal(
    retro_color_count(image, c("#ffffff", "#ff0000", "#0000ff")),
    c(3L, 2L, 1L)
  )
})

test_that("retro_color_count() returns 0 for colors absent from the image", {
  raster <- matrix(c("#ffffff", "#ffffff", "#00ff00", "#0000ff"), nrow = 1L)
  image <- retro_test_png(raster)
  on.exit(unlink(image))
  expect_equal(
    retro_color_count(image, c("#ffffff", "#ff0000")),
    c(2L, 0L)
  )
})

test_that("retro_color_name() finds the exact match for primary colors", {
  expect_equal(retro_color_name("#ff0000"), "red")
  expect_equal(retro_color_name("#0000ff"), "blue")
  expect_equal(retro_color_name("#ffffff"), "white")
  expect_equal(retro_color_name("#000000"), "black")
})

test_that("retro_color_name() is vectorized", {
  expect_equal(
    retro_color_name(c("#ff0000", "#0000ff")),
    c("red", "blue")
  )
})

test_that("retro_color_name() ignores the alpha channel", {
  expect_equal(retro_color_name("#ff0000ff"), retro_color_name("#ff0000"))
})

test_that("retro_color_count() matches regardless of input hex case", {
  raster <- matrix(c("#FFFFFF", "#DC3030", "#DC3030"), nrow = 1L)
  image <- retro_test_png(raster)
  on.exit(unlink(image))
  expect_equal(
    retro_color_count(image, c("#ffffff", "#DC3030")),
    c(1L, 2L)
  )
})
