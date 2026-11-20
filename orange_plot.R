# orange_plot.R
#
# Question: Which of the five orange trees in the Orange dataset grew fastest?
#
# Data: Orange (built into R): trunk circumference (mm) of 5 orange trees,
# measured 7 times between 118 and 1582 days of age. Only package used besides
# base R: tidyverse.
#
# Arguments (all three are required, in this order):
#   1. palette    One of: classic, sunset, ocean. Sets the colour scheme.
#   2. base_size  A number from 8 to 24. Base text size of the plot, in points.
#   3. output     Output file name; must end in .png (the folder must exist).
#
# Command that produced the committed image (run from the repository root):
#   Rscript orange_plot.R ocean 12 orange_infographic.png
#
# The analysis is deterministic (no random numbers), so every run gives the
# same numbers. Fonts/rendering may differ slightly between machines.

suppressPackageStartupMessages(library(tidyverse))

# ---- 1. Read and validate command-line arguments ---------------------------

palettes <- list(
  classic = list(highlight = "#f28500", muted = "#b8b8b8", bg = "#ffffff", ink = "#222222", grid = "#e6e6e6"),
  sunset  = list(highlight = "#e8590c", muted = "#cbb8a6", bg = "#fff6ec", ink = "#2b1b17", grid = "#f0dfcc"),
  ocean   = list(highlight = "#f08a00", muted = "#9db4bd", bg = "#f1f7f9", ink = "#14213d", grid = "#d5e3e8")
)

usage <- paste0(
  "Usage: Rscript orange_plot.R <palette> <base_size> <output.png>\n",
  "  palette    one of: ", paste(names(palettes), collapse = ", "), "\n",
  "  base_size  number between 8 and 24 (text size in points)\n",
  "  output     file name ending in .png\n",
  "Example: Rscript orange_plot.R ocean 12 orange_infographic.png"
)

fail <- function(msg) {
  cat("Error: ", msg, "\n\n", usage, "\n", sep = "", file = stderr())
  quit(save = "no", status = 1)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) != 3) {
  fail(sprintf("expected 3 arguments, got %d.", length(args)))
}

palette_name <- args[1]
if (!palette_name %in% names(palettes)) {
  fail(sprintf("unknown palette '%s'.", palette_name))
}

base_size <- suppressWarnings(as.numeric(args[2]))
if (is.na(base_size) || base_size < 8 || base_size > 24) {
  fail(sprintf("base_size must be a number between 8 and 24, got '%s'.", args[2]))
}

out_file <- args[3]
if (!grepl("\\.png$", out_file, ignore.case = TRUE)) {
  fail(sprintf("output must end in .png, got '%s'.", out_file))
}
if (!dir.exists(dirname(out_file))) {
  fail(sprintf("output folder '%s' does not exist.", dirname(out_file)))
}

pal <- palettes[[palette_name]]

# ---- 2. Prepare data -------------------------------------------------------

trees <- as_tibble(as.data.frame(Orange)) |>
  mutate(
    Tree = as.character(Tree),
    years = age / 365.25
  )

# ---- 3. Analysis -----------------------------------------------------------

# Average growth rate per tree: slope of circumference against age, in mm per year
growth <- trees |>
  group_by(Tree) |>
  summarise(
    rate  = unname(coef(lm(circumference ~ years))[2]),
    final = circumference[which.max(years)],
    .groups = "drop"
  ) |>
  arrange(desc(rate))

fastest <- growth$Tree[1]
slowest <- growth$Tree[nrow(growth)]
rate_fast <- growth$rate[1]
rate_slow <- growth$rate[nrow(growth)]
span_years <- max(trees$years)

summary_line <- paste0(
  paste(
    sprintf("Tree %s: final circumference %d mm, average growth %.1f mm/year",
            growth$Tree, as.integer(growth$final), growth$rate),
    collapse = "\n"
  ),
  "\n"
)

# End-of-line labels, nudged apart so they never overlap
gap <- 0.07 * diff(range(trees$circumference))
ends <- trees |>
  group_by(Tree) |>
  filter(years == max(years)) |>
  ungroup() |>
  left_join(growth, by = "Tree") |>
  arrange(circumference) |>
  mutate(
    label = sprintf("Tree %s  (%.0f mm/yr)", Tree, rate),
    is_fastest = Tree == fastest
  )
label_y <- ends$circumference
for (i in seq_along(label_y)[-1]) {
  if (label_y[i] - label_y[i - 1] < gap) label_y[i] <- label_y[i - 1] + gap
}
ends$label_y <- label_y

trees <- trees |> mutate(is_fastest = Tree == fastest)

# ---- 4. Build the infographic ----------------------------------------------

headline <- sprintf("Tree %s won the orange growth race", fastest)

subtitle <- sprintf(
  paste0(
    "Five orange trees measured over %.1f years. Tree %s added %.0f mm of trunk circumference\n",
    "per year on average, against %.0f mm per year for the slowest, Tree %s."
  ),
  span_years, fastest, rate_fast, rate_slow, slowest
)

caption <- sprintf(
  paste0(
    "Data: Orange, 5 trees, 7 measurements each. Growth rate = slope of a straight line fitted to each tree.\n",
    "Reproduce: Rscript orange_plot.R %s %s %s"
  ),
  palette_name, format(base_size), basename(out_file)
)

p <- ggplot(trees, aes(x = years, y = circumference, group = Tree)) +
  geom_line(data = filter(trees, !is_fastest), colour = pal$muted, linewidth = 1.1) +
  geom_point(data = filter(trees, !is_fastest), colour = pal$muted, size = 2.4) +
  geom_line(data = filter(trees, is_fastest), colour = pal$highlight, linewidth = 2) +
  geom_point(data = filter(trees, is_fastest), colour = pal$highlight, size = 3.6) +
  geom_text(
    data = ends,
    aes(x = years + 0.1, y = label_y, label = label, colour = is_fastest, fontface = ifelse(is_fastest, "bold", "plain")),
    hjust = 0, size = base_size * 0.3
  ) +
  scale_colour_manual(values = c("TRUE" = pal$highlight, "FALSE" = pal$ink), guide = "none") +
  scale_x_continuous(
    breaks = 0:4,
    limits = c(0, span_years + 1.5),
    expand = expansion(mult = c(0.01, 0))
  ) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = headline,
    subtitle = subtitle,
    x = "Tree age (years)",
    y = "Trunk circumference (mm)",
    caption = caption
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    plot.background   = element_rect(fill = pal$bg, colour = NA),
    panel.background  = element_rect(fill = pal$bg, colour = NA),
    panel.grid.major  = element_line(colour = pal$grid),
    panel.grid.minor  = element_blank(),
    text              = element_text(colour = pal$ink),
    axis.text         = element_text(colour = pal$ink),
    plot.title        = element_text(face = "bold", size = base_size * 1.8, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = base_size * 1.0, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption      = element_text(size = base_size * 0.75, hjust = 0, colour = "grey35", margin = margin(t = 12)),
    plot.margin       = margin(20, 24, 16, 24)
  )

# ---- 5. Save ---------------------------------------------------------------

ggsave(out_file, plot = p, width = 10, height = 7, dpi = 150, bg = pal$bg)

cat(sprintf("Wrote %s\n", out_file))
cat(summary_line)