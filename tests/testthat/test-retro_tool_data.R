label_data <- tibble::tibble(
  label = c("A", "B", "C", "D", "E"),
  word = c("0", "12", "0", "1", "100"),
  confidence = c(90, 85, 92, 88, 91),
  x = c(1, 20, 1, 1, 15),
  y = c(5, 5, 5, 1, 5),
  x1 = c(1L, 19L, 1L, 1L, 14L),
  x2 = c(2L, 20L, 2L, 2L, 16L),
  y1 = c(4L, 4L, 4L, 1L, 4L),
  y2 = c(5L, 5L, 5L, 2L, 5L)
)

# Axis calibration, as if the distill tool had already run. x/y pixel
# columns mirror label_data's centroids for labels A, B, C, D.
data_x <- tibble::tibble(label = c("A", "B"), value = c(0, 12), x = c(1, 20))
data_y <- tibble::tibble(label = c("C", "D"), value = c(0, 1), y = c(5, 1))

# A distilled image: two step curves that overlap, so path-finding has
# something to recover. Red spans the left, blue the right, and each is a
# single connected run of pixels.
retro_data_image <- function() {
  raster <- matrix("#ffffff", nrow = 5L, ncol = 20L)
  raster[2L, 1L:12L] <- "#dc3030"
  raster[2L:3L, 12L] <- "#dc3030"
  raster[3L, 8L:20L] <- "#3030a0"
  retro_test_png(raster)
}

retro_data_legend <- function() {
  tibble::tibble(
    color = c("#dc3030", "#3030a0"),
    series = c("Placebo", "Drug"),
    reference = c(TRUE, FALSE)
  )
}

# Panel bounding box, as if the distill tool had already run, covering
# the full 20x5 fixture image.
retro_data_panel <- function() {
  tibble::tibble(
    x1 = 1L,
    x2 = 20L,
    y1 = 1L,
    y2 = 5L,
    pad_x1 = 1L,
    pad_y2 = -1L
  )
}

# A state populated as if the label and distill tools had both run.
retro_data_state <- function() {
  state <- new.env(parent = emptyenv())
  state$image_clean <- retro_data_image()
  state$data_label <- label_data
  state$data_x <- data_x
  state$data_y <- data_y
  state$data_legend <- retro_data_legend()
  state$data_background <- "#ffffff"
  state$data_panel <- retro_data_panel()
  state
}

# A risk table consistent with the two-series fixture: non-increasing
# within each arm, same number of entries per arm.
retro_data_risk <- function() {
  list(
    risk_patients = c(100, 90, 80, 70),
    risk_series = rep(c("Placebo", "Drug"), each = 2L),
    risk_x = rep(c(0, 10), 2L)
  )
}

test_that("retro_tool_data() returns a ToolDef", {
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_data(state)
  expect_s3_class(tool, "ellmer::ToolDef")
})

test_that("retro_tool_data() errors when distill has not been run", {
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_data(state)
  expect_error(tool(), "Call the distill tool before the data tool.")
})

test_that("retro_tool_data() finds paths and populates data_path", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  result <- do.call(tool, retro_data_risk())
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_s3_class(state$data_path, "tbl_df")
  expect_true(nrow(state$data_path) > 0L)
  expect_equal(
    names(state$data_path),
    c("x", "y", "color", "line_width", "width", "height")
  )
  expect_true(all(state$data_path$width == 20L))
  expect_true(all(state$data_path$height == 5L))
})

test_that("retro_tool_data() scales both series into data coordinates", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  do.call(tool, retro_data_risk())
  expect_s3_class(state$data_scaled, "tbl_df")
  expect_equal(
    names(state$data_scaled),
    c("series", "x", "y", "max_y", "increasing")
  )
  expect_setequal(
    unique(state$data_scaled$series),
    c("Placebo", "Drug")
  )
  # series values agree with the legend the distill tool produced.
  expect_setequal(
    unique(state$data_scaled$series),
    state$data_legend$series
  )
})

test_that("retro_tool_data() requires risk_series and risk_x", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  expect_error(
    tool(risk_patients = 100),
    'argument "risk_series" is missing'
  )
})

test_that("retro_tool_data() validates series names", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  expect_error(
    tool(risk_patients = 100, risk_series = "Unknown Arm", risk_x = 0),
    "Invalid series names"
  )
})

test_that("retro_tool_data() rejects a risk table that goes back up", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  expect_error(
    tool(
      risk_patients = c(100, 120, 80, 70),
      risk_series = rep(c("Placebo", "Drug"), each = 2L),
      risk_x = rep(c(0, 10), 2L)
    ),
    "non-increasing"
  )
})

test_that("retro_tool_data() stores data_risk and leaves data_x/data_y as-is", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  result <- do.call(tool, retro_data_risk())
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_equal(state$data_x, data_x)
  expect_equal(state$data_y, data_y)
  expect_s3_class(state$data_risk, "tbl_df")
  expect_false("label" %in% names(state$data_risk))
  expect_setequal(state$data_risk$patients, c(100, 90, 80, 70))
  # series is character with values matching the legend.
  expect_true(is.character(state$data_risk$series))
  expect_setequal(
    unique(state$data_risk$series),
    state$data_legend$series
  )
})

test_that("retro_tool_data() reconstructs survival data from a risk table", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  do.call(tool, retro_data_risk())
  expect_s3_class(state$data_survival, "tbl_df")
  expect_equal(names(state$data_survival), c("series", "time", "status"))
  expect_true(nrow(state$data_survival) > 0L)
})

test_that("retro_tool_data() requires a risk table", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  expect_error(tool(), 'argument "risk_patients" is missing')
})

test_that("retro_tool_data() takes risk arguments before events arguments", {
  # The schema's property order steers the order the model reads and thinks
  # in, and the risk table is the required input, so it comes first. This
  # also pins the removal of the old risk_events argument.
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_data(state)
  expect_equal(
    names(tool@arguments@properties),
    c("risk_patients", "risk_series", "risk_x", "events_total", "events_series")
  )
})

test_that("retro_tool_data() leaves data_events NULL when no total is supplied", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  do.call(tool, retro_data_risk())
  expect_null(state$data_events)
})

test_that("retro_tool_data() stores and returns a supplied total events count", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  result <- do.call(
    tool,
    c(
      retro_data_risk(),
      list(events_total = c(40, 30), events_series = c("Placebo", "Drug"))
    )
  )
  expect_s3_class(result, "ellmer::ContentToolResult")
  expect_s3_class(state$data_events, "tbl_df")
  expect_named(state$data_events, c("series", "events"))
  # Ordered by first appearance in events_series: Placebo before Drug.
  expect_equal(state$data_events$series, c("Placebo", "Drug"))
  expect_equal(state$data_events$events, c(40, 30))
})

test_that("retro_tool_data() accepts a total events count for only one arm", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  do.call(
    tool,
    c(retro_data_risk(), list(events_total = 40, events_series = "Placebo"))
  )
  expect_equal(nrow(state$data_events), 1L)
  expect_equal(state$data_events$series, "Placebo")
})

test_that("retro_tool_data() rejects a repeated arm in the total events count", {
  state <- retro_data_state()
  on.exit(unlink(state$image_clean))
  tool <- retro_tool_data(state)
  expect_error(
    do.call(
      tool,
      c(
        retro_data_risk(),
        list(events_total = c(5, 40), events_series = c("Placebo", "Placebo"))
      )
    ),
    "at most one entry per series"
  )
})

test_that("retro_tool_data() has no increasing argument", {
  # increasing is inferred automatically from the scaled data
  # (retro_scale_increasing()), not supplied by the model, so the tool
  # takes no increasing argument.
  state <- new.env(parent = emptyenv())
  tool <- retro_tool_data(state)
  expect_false("increasing" %in% names(tool@arguments@properties))
})
