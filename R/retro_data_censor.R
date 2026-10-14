#' @title Detect censoring tick marks in a distilled Kaplan-Meier image
#' @keywords internal
#' @noRd
#' @description Label which pixels of each curve's traced path sit under a
#'   censoring tick mark. Returns the path data from [retro_data_path()]
#'   augmented with one logical `censored` column - the same rows, in the
#'   same order, with the same columns, plus that one.
#' @details Most Kaplan-Meier figures draw a short vertical tick mark
#'   centered on the curve at every time a patient was censored. That tick
#'   is drawn in the series' own color, so it is part of the series' ink,
#'   but it is not part of the curve, so the thin medial-axis trace
#'   [retro_data_path()] returns runs straight past it. The difference
#'   between the two - ink the series owns that the trace does not account
#'   for - is what identifies a tick.
#'
#'   The measurement is per pixel column. For one series at one column,
#'   [retro_data_censor_column()] reports the ink's longest contiguous
#'   vertical run less the trace's own: an excess height. An ordinary
#'   column's excess height is on the order of the line width, since the
#'   trace is a one-pixel skeleton through a band several pixels thick. A
#'   tick column's is much larger, since the tick extends well past the line
#'   in both directions while the trace runs thinly through it. Splitting the
#'   excess heights into a low group and a high group therefore separates
#'   curve ink from tick ink, and that is what [retro_data_censor_color()]
#'   does, deferring to [retro_censor_centers()] for whether they form two
#'   groups at all (a figure that draws no ticks has only one).
#'
#'   Occlusion is not accounted for, and cannot be. Where another series and
#'   its own censoring ticks overplot a target series and its ticks, the
#'   target's ticks have no visible ink of their own color left in the
#'   image, so nothing can detect them. Tick detection works with visible
#'   ink only.
#' @return The tibble [retro_data_path()] returned - one row per curve
#'   pixel, columns `x`, `y`, `color`, `line_width`, `width`, and `height`,
#'   in the same row order - with one column added:
#'   - `censored`: logical, `TRUE` for a pixel in a column where a censoring
#'     tick mark was detected.
#'
#'   The return value is path data, not a new kind of object. Every step
#'   downstream that documents its input as coming from
#'   [retro_data_path()], [retro_data_scale()] in particular, describes this
#'   just as accurately.
#' @param input Character scalar, path to the distilled image file (the
#'   `clean` element of [retro_do_distill()]'s return value) - the same
#'   image [retro_data_path()] traced.
#' @param path Data frame of traced path pixels, as returned by
#'   [retro_data_path()].
#' @examples
#'   # A flat red line four pixels thick with a tall tick mark at column 10.
#'   pixels <- matrix("#ffffff", nrow = 30L, ncol = 20L)
#'   pixels[14L:17L, 2L:19L] <- "#dc3030"
#'   pixels[6L:25L, 10L] <- "#dc3030"
#'   input <- tempfile(fileext = ".png")
#'   magick::image_write(magick::image_read(pixels), input)
#'   path <- retroglyph:::retro_data_path(input)
#'   censoring <- retroglyph:::retro_data_censor(input, path)
#'   print(sort(unique(censoring$x[censoring$censored])))
retro_data_censor <- function(input, path) {
  stopifnot(
    "input must be a single string" = is.character(input) &&
      length(input) == 1L,
    "input must exist" = file.exists(input),
    "path must have columns x, y, and color" = all(
      c("x", "y", "color") %in% names(path)
    )
  )
  image <- magick::image_read(input)
  # Read the pixels through the same helper retro_data_path() used, so the
  # colors here compare equal to the ones it recorded in path$color.
  pixels <- retro_color_pixels(image)
  lapply(unique(path$color), function(color) {
    positions <- which(pixels$pixel_matrix == color, arr.ind = TRUE)
    ink <- data.frame(
      x = as.integer(positions[, "col"]),
      y = as.integer(positions[, "row"])
    )
    retro_data_censor_color(ink, path[path$color == color, ])
  }) |>
    dplyr::bind_rows()
}

#' @title Detect one series' censoring tick marks from its ink
#' @keywords internal
#' @noRd
#' @description Measure one excess height per pixel column of a series'
#'   traced path (see [retro_data_censor_column()]), split the excess heights
#'   into curve ink and tick ink, and label the path pixels accordingly.
#' @details The split is a univariate k-means on the excess heights, with the
#'   number of clusters coming from [retro_censor_centers()]: one cluster
#'   means the figure draws no censoring ticks and nothing is labeled, two
#'   means the higher-centered cluster is tick ink. Every path pixel in a
#'   tick column is labeled, since the excess height is a property of the
#'   column rather than of one pixel.
#'
#'   The clustering uses `Ckmeans.1d.dp` rather than `stats::kmeans()`.
#'   The excess heights are univariate, and `Ckmeans.1d.dp` solves univariate
#'   k-means exactly, by dynamic programming, so the split is both optimal
#'   and deterministic. `stats::kmeans()` is a heuristic started from a
#'   random seeding, so it can label the same figure differently from one
#'   run to the next - not something a digitization tool can afford, since
#'   the censoring times it reports would not reproduce.
#' @return `series` with one column added:
#'   - `censored`: logical, `TRUE` for a pixel in a column where a censoring
#'     tick mark was detected.
#' @param ink Data frame with integer columns `x` and `y`, the coordinates
#'   of every pixel of this series' color in the distilled image.
#' @param series Data frame of this series' traced path pixels - the rows of
#'   [retro_data_path()]'s return value for one `color`.
#' @examples
#'   # A path along row 15 from columns 1-20, with ink extending far above
#'   # and below the path at column 10 only: a tick mark.
#'   series <- data.frame(x = 1L:20L, y = rep(15L, 20L))
#'   ink <- rbind(
#'     data.frame(x = rep(1L:20L, each = 3L), y = rep(14L:16L, times = 20L)),
#'     data.frame(x = 10L, y = 5L:25L)
#'   )
#'   result <- retroglyph:::retro_data_censor_color(ink, series)
#'   print(result$x[result$censored])
retro_data_censor_color <- function(ink, series) {
  ink_by_column <- split(ink$y, ink$x)
  path_by_column <- split(series$y, series$x)
  columns <- as.integer(names(path_by_column))
  excess_heights <- vapply(
    names(path_by_column),
    function(column) {
      retro_data_censor_column(
        ink_y = ink_by_column[[column]],
        path_y = path_by_column[[column]]
      )
    },
    numeric(1L)
  )
  centers <- retro_censor_centers(excess_heights)
  if (centers < 2L || length(unique(excess_heights)) < 2L) {
    series$censored <- rep(FALSE, nrow(series))
    return(series)
  }
  clustering <- Ckmeans.1d.dp::Ckmeans.1d.dp(excess_heights, k = 2L)
  tick_cluster <- which.max(clustering$centers)
  tick_columns <- columns[clustering$cluster == tick_cluster]
  series$censored <- series$x %in% tick_columns
  series
}

#' @title Measure one pixel column's excess ink height
#' @keywords internal
#' @noRd
#' @description Report how much taller a series' ink is than its traced path
#'   in one pixel column: the excess height that distinguishes a censoring
#'   tick mark's ink from the curve's own.
#' @details The measurement is the ink's longest contiguous vertical run,
#'   less the path's own longest run. Runs rather than outright ranges
#'   because a column's ink need not be one piece: a legend swatch, an axis
#'   annotation, or another series' tick in the same color can leave a
#'   detached speck elsewhere in the column, and a range taken end to end
#'   would reach across the gap between them and read as a tall tick. A tick
#'   mark is a single unbroken run of ink, so measuring the longest run keeps
#'   the real thing and discards the coincidence.
#'
#'   Subtracting the path's run is what keeps a vertical step drop from
#'   reading like a tick: where the skeleton traverses the drop it runs
#'   nearly as far as the ink does, so the two cancel and the column nets out
#'   near zero. A tick is different in kind - the path runs thinly through
#'   it, so the tick's full height survives the subtraction.
#'
#'   Because the measurement is a run rather than a one-sided distance, a
#'   centered tick contributes both its overhangs, above the curve and
#'   below. That doubles the separation from an ordinary column, which yields
#'   only the line width. The cost is that a tick clipped by the panel edge,
#'   or one drawn asymmetrically, keeps only the overhang it has and so
#'   scores lower than a full tick - still well above line width, but closer
#'   to the low group. A tick cut in two by overplotting is reduced the same
#'   way, to its longer surviving piece.
#'
#'   The result is clamped at `0`, which matters for occluded columns. The
#'   path is reconstructed through occlusion, so a column can hold a skeleton
#'   running many rows above just a stray pixel or two of this series' own
#'   color. There the path out-runs the ink, and without the clamp the
#'   difference would be large and negative - an unsigned distance would
#'   report it as a tall tick. Clamping keeps such a column in the low group,
#'   where the absence of visible evidence belongs.
#'
#'   A column with no ink of this color returns `0` for the same reason. This
#'   is where the occlusion assumption documented in [retro_data_censor()]
#'   lands in the code: such a column is either fully overplotted by another
#'   series or a reconstructed path pixel bridging a gap, and either way
#'   there is no visible ink to evidence a tick.
#'
#'   One limitation is unaddressed. A step drop is flagged at its *outer*
#'   columns rather than its interior: the skeleton places its vertical spine
#'   inside the drop, so at the drop's edges the path is thin while the ink
#'   still runs the whole length of the drop, which is indistinguishable from
#'   a very tall tick. The subtraction helps only where the skeleton actually
#'   traverses the drop.
#' @return Numeric scalar, the excess height in pixels. `0` when the column
#'   has no ink of this series' color, or when the path's longest run is at
#'   least as long as the ink's.
#' @param ink_y Integer vector of `y` coordinates of this series' ink in
#'   this column, or `NULL` if the column has none.
#' @param path_y Integer vector of `y` coordinates of the traced path in
#'   this column.
#' @examples
#'   # Ink runs rows 5-25 (21 rows), the path rows 14-16 (3 rows): 18 pixels
#'   # of ink the path does not account for.
#'   retroglyph:::retro_data_censor_column(ink_y = 5L:25L, path_y = 14L:16L)
#'   # A detached speck of the same color elsewhere in the column is ignored:
#'   # only the 4-row run around the path counts.
#'   retroglyph:::retro_data_censor_column(
#'     ink_y = c(2L:5L, 14L:17L),
#'     path_y = 15L
#'   )
#'   # A step drop: the path runs nearly the whole length of the ink, so the
#'   # two cancel.
#'   retroglyph:::retro_data_censor_column(ink_y = 14L:40L, path_y = 16L:38L)
#'   # A column with no visible ink of this color.
#'   retroglyph:::retro_data_censor_column(ink_y = NULL, path_y = 14L:16L)
retro_data_censor_column <- function(ink_y, path_y) {
  if (length(ink_y) == 0L) {
    # No visible ink of this color in this column: occluded by another
    # series, or a reconstructed path pixel in a gap. Nothing to measure.
    return(0)
  }
  # Longest contiguous run, not outright range: a gap between adjacent
  # sorted rows starts a new run, so the cumulative count of gaps labels
  # each row with the run it belongs to, and the longest run's extent is
  # its row count less one.
  run <- function(y) {
    y <- sort(unique(y))
    max(tabulate(cumsum(c(1L, diff(y) != 1L)))) - 1L
  }
  max(0, run(ink_y) - run(path_y))
}

#' @title Count the groups a series' excess heights fall into
#' @keywords internal
#' @noRd
#' @description Report whether a series' per-column excess heights form one
#'   group or two: one when the figure draws no censoring tick marks and
#'   every column yields just line width, two when tick ink stands apart
#'   from curve ink.
#' @details This is a stub. It always reports `2`, so a figure with no
#'   censoring tick marks at all currently has its tallest columns
#'   labeled as ticks anyway - the k-means in
#'   [retro_data_censor_color()] will always find a split to make, even in
#'   noise. Detecting the number of groups is the part of this that needs
#'   real work, and it lives in its own function so it can be replaced
#'   outright without touching the measurement or labeling around it.
#' @return Integer scalar, `1L` or `2L`.
#' @param excess_heights Numeric vector of per-column excess heights from
#'   [retro_data_censor_column()], one per pixel column of one series.
#' @examples
#'   retroglyph:::retro_censor_centers(c(1, 1, 2, 1, 9, 10))
retro_censor_centers <- function(excess_heights) {
  # TODO: detect whether the excess heights form one group (a figure with no
  # censoring tick marks) or two (curve ink vs. tick ink).
  2L
}
