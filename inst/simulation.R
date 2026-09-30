# Simulated oncology-style Kaplan-Meier plot with a near-superimposed
# treatment arm that lets the placebo curve peek out in only a couple of places.

library(survival)
library(ggplot2)

set.seed(42)

n_per_arm <- 350
followup <- 36 # months of follow-up on the x axis
surv_at_end <- 0.30 # target placebo survival at end of follow-up
breaks <- seq(0, followup, by = 6)

placebo_col <- "#1B3A4B"
treat_col <- "#E28743"

# Sized for projection on a 16:9 slide rather than for a documentation page.
base_size <- 22

# ---- Placebo arm: exponential event times, administrative censoring only ----
# Rate chosen so S(followup) = surv_at_end.
lambda <- -log(surv_at_end) / followup

placebo_event <- rexp(n_per_arm, rate = lambda)
treat_event <- placebo_event

# # Hand-placed windows where treatment and placebo jitter randomly.
index <- placebo_event > 15
treat_event[index] <- pmax(
  placebo_event[index] + rnorm(sum(index), sd = 0.5),
  0.01
)

# # Hand-placed windows where treatment events depart from control events,
# # which is what makes the placebo curve visibly emerge.
index <- placebo_event > 4 & placebo_event < 8.5 # just past mid-study
treat_event[index] <- treat_event[index] + runif(sum(index), 0.25, 0.5)

index <- placebo_event > 18 & placebo_event < 20 # just past mid-study
treat_event[index] <- treat_event[index] + runif(sum(index), 0.5, 1)

index <- placebo_event > 24 & placebo_event < 30 # just past mid-study
treat_event[index] <- treat_event[index] - runif(sum(index), 0.5, 1)

index <- placebo_event > 30 # tail divergence
treat_event[index] <- treat_event[index] + runif(sum(index), 2.0, 4.0)

dat <- data.frame(
  time = pmin(c(placebo_event, treat_event), followup),
  status = as.integer(c(placebo_event, treat_event) <= followup),
  arm = factor(
    rep(c("Placebo", "Treatment"), each = n_per_arm),
    levels = c("Placebo", "Treatment")
  )
)

fit <- survfit(Surv(time, status) ~ arm, data = dat)

# ---- Step coordinates, evaluated on a shared time grid ---------------------
# The shared grid means both arms have a value at every event time, so the
# curves are directly comparable and overplot cleanly.
grid <- sort(unique(c(0, fit$time, followup)))
s <- summary(fit, times = grid, extend = TRUE)
steps <- data.frame(
  time = s$time,
  surv = s$surv,
  arm = factor(sub("arm=", "", s$strata), levels = c("Placebo", "Treatment"))
)

# ---- Numbers at risk ------------------------------------------------------
r <- summary(fit, times = breaks, extend = TRUE)
risk <- data.frame(
  time = r$time,
  n_risk = r$n.risk,
  arm = factor(sub("arm=", "", r$strata), levels = c("Treatment", "Placebo"))
)

# ---- Manual axis + risk-table geometry -------------------------------------
# Both axes are drawn by hand (segments + text, not theme axis elements) so
# that the risk table can live in the same panel, below y = 0, without
# dragging the y-axis title away from its axis line the way a patchwork
# gutter would. All the constants below are in the same data units as the
# curves (x in months, y in survival probability).
y_tick_len <- 0.7 # length of y-axis tick marks, in x units
x_tick_len <- 0.025 # length of x-axis tick marks, in y units

y_label_x <- -y_tick_len - 0.4 # x position of "0/25/50/75/100" labels
y_title_x <- y_label_x - 2.6 # x position of the rotated y-axis title

x_label_y <- -x_tick_len - 0.02 # y position of the "0/6/.../36" labels
x_title_y <- x_label_y - 0.07 # y position of the "Months" title

# The risk-table title and its row labels share one left-aligned anchor, far
# enough left of x = 0 that "Treatment"/"Placebo" (the widest labels, set at
# the same size as everything else) never run into the counts at time = 0.
risk_label_x <- -7.2
risk_title_y <- x_title_y - 0.14
risk_row_gap <- 0.1
risk_row_y <- c(
  Treatment = risk_title_y - risk_row_gap,
  Placebo = risk_title_y - 2 * risk_row_gap
)
risk$y <- risk_row_y[as.character(risk$arm)]

panel_bottom <- min(risk_row_y) - 0.4 * risk_row_gap

# ---- Curves, axes, and risk table -- all one ggplot ------------------------
# Placebo is drawn first and slightly thinner so treatment sits on top of it.
plot <- ggplot(mapping = aes(x = time, y = surv)) +
  geom_step(
    data = subset(steps, arm == "Placebo"),
    colour = placebo_col,
    linewidth = 1.5
  ) +
  geom_step(
    data = subset(steps, arm == "Treatment"),
    colour = treat_col,
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = followup - 0.5,
    y = 0.46,
    label = "Treatment",
    colour = treat_col,
    hjust = 1,
    fontface = "bold",
    size = 6.5
  ) +
  annotate(
    "text",
    x = followup - 0.5,
    y = 0.29,
    label = "Placebo",
    colour = placebo_col,
    hjust = 1,
    fontface = "bold",
    size = 6.5
  ) +
  # y axis: line, ticks, labels, rotated title
  annotate("segment", x = 0, xend = 0, y = 0, yend = 1, linewidth = 0.7) +
  annotate(
    "segment",
    x = -y_tick_len,
    xend = 0,
    y = seq(0, 1, 0.25),
    yend = seq(0, 1, 0.25),
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = y_label_x,
    y = seq(0, 1, 0.25),
    label = seq(0, 100, 25),
    hjust = 1,
    size = base_size / .pt
  ) +
  annotate(
    "text",
    x = y_title_x,
    y = 0.5,
    label = "Survival percentage",
    angle = 90,
    size = base_size / .pt
  ) +
  # x axis: line, ticks, labels, title
  annotate(
    "segment",
    x = 0,
    xend = followup,
    y = 0,
    yend = 0,
    linewidth = 0.7
  ) +
  annotate(
    "segment",
    x = breaks,
    xend = breaks,
    y = 0,
    yend = -x_tick_len,
    linewidth = 0.7
  ) +
  annotate(
    "text",
    x = breaks,
    y = x_label_y,
    label = breaks,
    vjust = 1,
    size = base_size / .pt
  ) +
  annotate(
    "text",
    x = followup / 2,
    y = x_title_y,
    label = "Months since randomization",
    vjust = 1,
    size = base_size / .pt
  ) +
  # risk table: title, row labels, counts -- sharing the x-axis time scale,
  # all at the same font size as the x-axis tick labels
  annotate(
    "text",
    x = risk_label_x,
    y = risk_title_y,
    label = "Number of patients at risk",
    hjust = 0,
    size = base_size / .pt
  ) +
  annotate(
    "text",
    x = risk_label_x,
    y = risk_row_y,
    label = names(risk_row_y),
    hjust = 0,
    size = base_size / .pt
  ) +
  geom_text(
    data = risk,
    aes(x = time, y = y, label = n_risk),
    inherit.aes = FALSE,
    size = base_size / .pt,
    colour = "black"
  ) +
  coord_cartesian(
    xlim = c(0, followup),
    ylim = c(panel_bottom, 1),
    clip = "off"
  ) +
  scale_x_continuous(breaks = breaks, expand = expansion(mult = 0.02)) +
  scale_y_continuous(breaks = seq(0, 1, 0.25), expand = c(0, 0)) +
  labs(title = "Simulated Kaplan-Meier image") +
  theme_classic(base_size = base_size) +
  theme(
    panel.grid.major.y = element_line(colour = "grey88", linewidth = 0.4),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(face = "plain", size = base_size + 1),
    # Left margin has to be wide enough to hold the hand-drawn y-axis title,
    # its tick labels, and the risk-table title/row labels, all of which sit
    # at negative x and rely on clip = "off" to escape the panel.
    plot.margin = margin(4, 14, 16, 150)
  )

ggsave("simulation.png", plot, width = 12, height = 7, dpi = 100)
