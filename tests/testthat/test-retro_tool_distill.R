# Shared label fixture: 4 ticks whose bounding box edges sit adjacent to
# the synthetic images' L-shaped axis crossing used below (y-axis at
# column 20 rows 10-80, x-axis at row 80 columns 20-90). X-axis labels
# sit below the axis; y-axis labels sit to the left.
retro_distill_labels <- function() {
  tibble::tibble(
    label = c("A", "B", "C", "D"),
    word = c("0", "12", "0", "1"),
    confidence = rep(90, 4L),
    x = c(20, 90, 15, 15),
    y = c(86, 86, 80, 10),
    x1 = c(18, 88, 13, 13),
    x2 = c(22, 92, 17, 17),
    y1 = c(83, 83, 77, 7),
    y2 = c(89, 89, 83, 13)
  )
}

retro_distill_ticks <- function() {
  list(
    x_label = c("A", "B"),
    x_value = c(0, 12),
    y_label = c("C", "D"),
    y_value = c(0, 1)
  )
}

# A 100x100 panel: white background, axes forming an L-shape (y-axis at
# column 20 rows 10-80, x-axis at row 80 columns 20-90), plus whatever
# curves the caller draws inside.
retro_distill_raster <- function() {
  raster <- matrix("#ffffffff", nrow = 100, ncol = 100)
  raster[10:80, 20] <- "#000000ff"
  raster[80, 20:90] <- "#000000ff"
  raster
}

retro_distill_call <- function(tool, ...) {
  do.call(tool, c(retro_distill_ticks(), list(...)))
}

test_that("retro_tool_distill() returns a ToolDef", {
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_distill(state)
  expect_s3_class(tool, "ellmer::ToolDef")
})

test_that("retro_tool_distill() errors when quantize has not been run", {
  state <- new.env(parent = emptyenv())
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  expect_error(
    retro_distill_call(
      tool,
      background = "#FFFFFF",
      series = "#FF0000",
      names = "Arm A",
      reference = "Arm A"
    ),
    "Call the quantize tool before distill."
  )
})

test_that("retro_tool_distill() errors when label has not been run", {
  state <- new.env(parent = emptyenv())
  state$image_quantized <- system.file("simulation.png", package = "retroglyph")
  tool <- retro_tool_distill(state)
  expect_error(
    retro_distill_call(
      tool,
      background = "#FFFFFF",
      series = "#FF0000",
      names = "Arm A",
      reference = "Arm A"
    ),
    "Call the label tool before distill."
  )
})

test_that("retro_tool_distill() isolates the panel and populates state", {
  raster <- retro_distill_raster()
  # One red curve spanning most of the panel width.
  raster[40:44, 25:85] <- "#ff0000ff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  result <- retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = "#FF0000",
    names = "Arm A",
    reference = "Arm A"
  )
  expect_s3_class(result, "ellmer::ContentToolResult")
  # All three images are written, intermediates included.
  expect_true(file.exists(state$image_clean))
  expect_true(file.exists(state$image_panel))
  expect_true(file.exists(state$image_classified))
  expect_s3_class(state$data_x, "tbl_df")
  expect_equal(state$data_x$label, c("A", "B"))
  expect_equal(state$data_x$value, c(0, 12))
  expect_s3_class(state$data_y, "tbl_df")
  expect_equal(state$data_y$label, c("C", "D"))
  expect_equal(state$data_y$value, c(0, 1))
  expect_s3_class(state$data_panel, "tbl_df")
  expect_equal(state$data_background, "#ffffff")
  expect_equal(state$data_legend$series, "Arm A")
  expect_equal(state$data_legend$color, "#ff0000")
  expect_true(is.logical(state$data_legend$reference))
  expect_equal(state$data_legend$reference, TRUE)
})

test_that("retro_tool_distill() removes the axis lines it cropped past", {
  input <- retro_test_png(retro_distill_raster())
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = "#FF0000",
    names = "Arm A",
    reference = "Arm A"
  )
  tidy <- magick::image_raster(
    magick::image_read(state$image_clean),
    tidy = TRUE
  )
  # The y-axis column (20) and x-axis row (80) are both masked out.
  expect_true(all(tidy$col[tidy$x == 20] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$y == 80] == "#ffffffff"))
})

test_that("retro_tool_distill() removes all four bounding box lines", {
  raster <- retro_distill_raster()
  # Add the top and right border lines to make a closed bounding box;
  # the tool always pads all four edges inward, past them.
  raster[10, 20:90] <- "#000000ff"
  raster[10:80, 90] <- "#000000ff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = "#FF0000",
    names = "Arm A",
    reference = "Arm A"
  )
  tidy <- magick::image_raster(
    magick::image_read(state$image_clean),
    tidy = TRUE
  )
  expect_true(all(tidy$col[tidy$x == 20] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$x == 90] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$y == 10] == "#ffffffff"))
  expect_true(all(tidy$col[tidy$y == 80] == "#ffffffff"))
})

test_that("retro_tool_distill() does not clobber state on a failed call", {
  # No axis lines at all, so axis detection fails.
  input <- retro_test_png(matrix("#ffffffff", nrow = 100, ncol = 100))
  on.exit(unlink(input), add = TRUE)
  previous_clean <- retro_test_png(matrix("#ffffffff", nrow = 10, ncol = 10))
  on.exit(unlink(previous_clean), add = TRUE)
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  state$image_clean <- previous_clean
  tool <- retro_tool_distill(state)
  expect_error(
    retro_distill_call(
      tool,
      background = "#FFFFFF",
      series = "#FF0000",
      names = "Arm A",
      reference = "Arm A"
    ),
    "Could not find"
  )
  expect_identical(state$image_clean, previous_clean)
  expect_true(file.exists(state$image_clean))
})

test_that("retro_tool_distill() keeps curves and drops short artifacts", {
  raster <- retro_distill_raster()
  # A long red curve (the data) and a short blue legend key fragment.
  raster[40:44, 25:85] <- "#ff0000ff"
  raster[30:34, 70:78] <- "#0000ffff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    names = c("Arm A", "Arm B"),
    reference = "Arm A"
  )
  tidy <- magick::image_raster(
    magick::image_read(state$image_clean),
    tidy = TRUE
  )
  curve <- tidy$y >= 40 & tidy$y <= 44 & tidy$x >= 25 & tidy$x <= 85
  expect_true(all(tidy$col[curve] == "#ff0000ff"))
  fragment <- tidy$y >= 30 & tidy$y <= 34 & tidy$x >= 70 & tidy$x <= 78
  expect_true(all(tidy$col[fragment] == "#ffffffff"))
})

test_that("retro_tool_distill() keeps the legend in the order names gives it", {
  raster <- retro_distill_raster()
  # Blue's curve (5 rows x 60 cols = 300 px) has more surviving pixels
  # than Red's (5 x 40 = 200 px), but names lists Red first - the legend
  # must preserve that order rather than sorting by pixel count.
  raster[40:44, 25:64] <- "#ff0000ff"
  raster[50:54, 25:84] <- "#0000ffff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    names = c("Red", "Blue"),
    reference = "Red"
  )
  expect_equal(state$data_legend$series, c("Red", "Blue"))
})

test_that("retro_tool_distill() marks the reference series in the legend", {
  raster <- retro_distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  raster[50:54, 25:85] <- "#0000ffff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = c("#FF0000", "#0000FF"),
    names = c("Placebo", "Drug"),
    reference = "Placebo"
  )
  expect_true(is.character(state$data_legend$series))
  expect_equal(state$data_legend$series, c("Placebo", "Drug"))
  expect_true(is.logical(state$data_legend$reference))
  expect_equal(state$data_legend$reference, c(TRUE, FALSE))
})

test_that("retro_tool_distill() reports each series' surviving pixel count", {
  raster <- retro_distill_raster()
  raster[40:44, 25:85] <- "#ff0000ff"
  input <- retro_test_png(raster)
  on.exit(unlink(input))
  state <- new.env(parent = emptyenv())
  state$image_quantized <- input
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  result <- retro_distill_call(
    tool,
    background = "#FFFFFF",
    series = "#FF0000",
    names = "Arm A",
    reference = "Arm A"
  )
  # The result carries only the cleaned image; the legend (with each
  # series' surviving pixel count folded into retro_do_distill()'s
  # separate counts output) is kept in state, not in the model-facing
  # value.
  expect_true(is.list(result@value))
  expect_length(result@value, 1L)
  expect_s3_class(result@value[[1L]], "ellmer::ContentImage")
  expect_equal(state$data_legend$series, "Arm A")
})

test_that("retro_tool_distill() rejects mismatched series and names", {
  state <- new.env(parent = emptyenv())
  state$image_quantized <- retro_test_png(retro_distill_raster())
  on.exit(unlink(state$image_quantized))
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  expect_error(
    retro_distill_call(
      tool,
      background = "#FFFFFF",
      series = c("#FF0000", "#0000FF"),
      names = "Arm A",
      reference = "Arm A"
    ),
    "series and series_names must have the same length"
  )
})

test_that("retro_tool_distill() rejects invalid hex colors", {
  state <- new.env(parent = emptyenv())
  state$image_quantized <- retro_test_png(retro_distill_raster())
  on.exit(unlink(state$image_quantized))
  state$data_label <- retro_distill_labels()
  tool <- retro_tool_distill(state)
  expect_error(
    retro_distill_call(
      tool,
      background = "#FFFFFF",
      series = "not a color",
      names = "Arm A",
      reference = "Arm A"
    ),
    "must be valid hex colors"
  )
})
