# orange_trees.R
#
# Question: After about four years, which of the five orange trees has the
# thickest trunk, and which grew fastest?
#
# Data: Orange (built into R): trunk circumference (mm) of 5 orange trees,
# measured 7 times between 118 and 1582 days of age. The infographic shows the
# last measurement as five drawn trees, ranked from thickest to thinnest trunk.
# Only package used besides base R: tidyverse.
#
# Arguments (all three are required, in this order):
#   1. palette    One of: classic, sunset, ocean. Sets the colour scheme.
#   2. base_size  A number from 8 to 24. Base text size of the plot, in points.
#   3. output     Output file name; must end in .png (the folder must exist).
#
# Command that produced the committed image (run from the repository root):
#   Rscript orange_trees.R classic 12 orange_trees.png
#
# The analysis is deterministic (no random numbers), so every run gives the
# same numbers. Fonts/rendering may differ slightly between machines.

suppressPackageStartupMessages(library(tidyverse))

# ---- 1. Read and validate command-line arguments ---------------------------

palettes <- list(
  classic = list(fruit = "#f28500", rind = "#b85c00", leaf = "#8fbf6a", leaf_dark = "#2f6b2f", bark = "#7a4b2a", bg = "#fffaf0", ink = "#3b2f1e", grid = "#f0e6d2"),
  sunset  = list(fruit = "#e8590c", rind = "#a63d00", leaf = "#9bb86a", leaf_dark = "#3b5d2a", bark = "#6b3e26", bg = "#fff3e0", ink = "#2b1b17", grid = "#f0dfcc"),
  ocean   = list(fruit = "#f08a00", rind = "#b06000", leaf = "#7fb8a4", leaf_dark = "#1f5f55", bark = "#6b4a3a", bg = "#f1f7f9", ink = "#14213d", grid = "#d5e3e8")
)

usage <- paste0(
  "Usage: Rscript orange_trees.R <palette> <base_size> <output.png>\n",
  "  palette    one of: ", paste(names(palettes), collapse = ", "), "\n",
  "  base_size  number between 8 and 24 (text size in points)\n",
  "  output     file name ending in .png\n",
  "Example: Rscript orange_trees.R classic 12 orange_trees.png"
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
  summarise(rate = unname(coef(lm(circumference ~ years))[2]), .groups = "drop")

# Last measurement of each tree, ranked from thickest to thinnest trunk
finals <- trees |>
  group_by(Tree) |>
  filter(years == max(years)) |>
  ungroup() |>
  left_join(growth, by = "Tree") |>
  arrange(desc(circumference)) |>
  mutate(pos = row_number(), is_top = pos == 1)

n_trees    <- nrow(finals)
span_years <- max(trees$years)
thickest   <- finals$Tree[1]
thinnest   <- finals$Tree[n_trees]
fastest    <- growth$Tree[which.max(growth$rate)]
rate_fast  <- max(growth$rate)

summary_line <- paste0(
  paste(
    sprintf("Tree %s: circumference %d mm after %.1f years, average growth %.1f mm/year",
            finals$Tree, as.integer(finals$circumference), span_years, finals$rate),
    collapse = "\n"
  ),
  "\n"
)

# ---- 4. Draw the trees -----------------------------------------------------

rx <- 0.40                              # crown half-width (x units)
ry <- 0.12 * max(finals$circumference)  # crown half-height (y units)
finals <- finals |> mutate(crown_y = circumference + 0.6 * ry)

# Crown: an ellipse sitting on top of each trunk
crown <- crossing(finals, theta = seq(0, 2 * pi, length.out = 80)) |>
  mutate(x = pos + rx * cos(theta), y = crown_y + ry * sin(theta))

# Oranges: fixed positions inside each crown (relative to the crown's size)
orange_spots <- tibble(
  ox = c(-0.50, 0.00, 0.50, -0.25, 0.30, 0.55),
  oy = c( 0.10, 0.55, 0.00, -0.35, -0.40, 0.45)
)
fruit <- crossing(finals, orange_spots) |>
  mutate(x = pos + ox * rx, y = crown_y + oy * ry)

top_of_plot <- max(finals$crown_y + ry) * 1.03

headline <- sprintf("Tree %s grew the thickest trunk", thickest)

subtitle <- sprintf(
  paste0(
    "After %.1f years Tree %s measures %d mm around the trunk and Tree %s only %d mm.\n",
    "Fastest average growth: Tree %s, at %.0f mm per year."
  ),
  span_years, thickest, as.integer(finals$circumference[1]),
  thinnest, as.integer(finals$circumference[n_trees]),
  fastest, rate_fast
)

caption <- sprintf(
  paste0(
    "Data: Orange, 5 trees. Trunk height in the drawing = trunk circumference (not tree height); read it at the top of each trunk.\n",
    "Trees are ranked by circumference at the last measurement. Crowns and oranges are decoration.\n",
    "Reproduce: Rscript orange_trees.R %s %s %s"
  ),
  palette_name, format(base_size), basename(out_file)
)

p <- ggplot() +
  # trunks
  geom_rect(
    data = finals,
    aes(xmin = pos - 0.08, xmax = pos + 0.08, ymin = 0, ymax = circumference),
    fill = pal$bark
  ) +
  # crowns (the thickest tree gets the darker crown)
  geom_polygon(
    data = crown,
    aes(x = x, y = y, group = Tree, fill = is_top),
    colour = pal$leaf_dark, linewidth = 0.6
  ) +
  # oranges
  geom_point(
    data = fruit, aes(x = x, y = y),
    shape = 21, fill = pal$fruit, colour = pal$rind, size = base_size * 0.38, stroke = 0.8
  ) +
  # ground
  geom_segment(
    aes(x = 0.4, xend = n_trees + 0.6, y = 0, yend = 0),
    colour = pal$bark, linewidth = 2
  ) +
  scale_fill_manual(values = c("TRUE" = pal$leaf_dark, "FALSE" = pal$leaf), guide = "none") +
  scale_x_continuous(
    breaks = finals$pos,
    labels = sprintf("Tree %s\n%d mm", finals$Tree, as.integer(finals$circumference)),
    limits = c(0.4, n_trees + 0.6),
    expand = expansion(0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 250, 50),
    limits = c(-8, top_of_plot),
    expand = expansion(0)
  ) +
  labs(
    title = headline,
    subtitle = subtitle,
    x = NULL,
    y = "Trunk circumference (mm)",
    caption = caption
  ) +
  theme_minimal(base_size = base_size) +
  theme(
    plot.background   = element_rect(fill = pal$bg, colour = NA),
    panel.background  = element_rect(fill = pal$bg, colour = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = pal$grid),
    panel.grid.minor  = element_blank(),
    text              = element_text(colour = pal$ink),
    axis.text         = element_text(colour = pal$ink),
    axis.text.x       = element_text(face = "bold", size = base_size * 1.05, lineheight = 1.1),
    plot.title        = element_text(face = "bold", size = base_size * 1.8, colour = pal$rind, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = base_size * 1.0, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption      = element_text(size = base_size * 0.75, hjust = 0, colour = "grey35", margin = margin(t = 12)),
    plot.margin       = margin(20, 24, 16, 24)
  )

# ---- 5. Save ---------------------------------------------------------------

ggsave(out_file, plot = p, width = 10, height = 8, dpi = 150, bg = pal$bg)

cat(sprintf("Wrote %s\n", out_file))
cat(summary_line)

# try for instance: Rscript orange_trees.R classic 12 orange_trees.png