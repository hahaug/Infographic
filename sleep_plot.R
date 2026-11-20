# sleep_plot.R
#
# Question: Does drug 2 give patients more extra sleep than drug 1?
#
# Data: sleep (built into R): extra hours of sleep for 10 patients, each of
# whom received both drugs. Only package used besides base R: tidyverse.
#
# Arguments (all three are required, in this order):
#   1. palette    One of: classic, sunset, ocean. Sets the colour scheme.
#   2. base_size  A number from 8 to 24. Base text size of the plot, in points.
#   3. output     Output file name; must end in .png (the folder must exist).
#
# Command that produced the committed image (run from the repository root):
#   Rscript sleep_plot.R ocean 12 sleep_infographic.png
#
# The analysis is deterministic (no random numbers), so every run gives the
# same numbers. Fonts/rendering may differ slightly between machines.

suppressPackageStartupMessages(library(tidyverse))

# ---- 1. Read and validate command-line arguments ---------------------------

palettes <- list(
  classic = list(better = "#1b6ca8", worse = "#d1495b", bg = "#ffffff", ink = "#222222", grid = "#e6e6e6"),
  sunset  = list(better = "#e07a1f", worse = "#6a2c70", bg = "#fff6ec", ink = "#2b1b17", grid = "#f0dfcc"),
  ocean   = list(better = "#0b7a75", worse = "#e4572e", bg = "#f1f7f9", ink = "#14213d", grid = "#d5e3e8")
)

usage <- paste0(
  "Usage: Rscript sleep_plot.R <palette> <base_size> <output.png>\n",
  "  palette    one of: ", paste(names(palettes), collapse = ", "), "\n",
  "  base_size  number between 8 and 24 (text size in points)\n",
  "  output     file name ending in .png\n",
  "Example: Rscript sleep_plot.R ocean 12 sleep_infographic.png"
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

# One row per patient, one column per drug
wide <- sleep |>
  mutate(ID = as.character(ID)) |>
  pivot_wider(names_from = group, values_from = extra, names_prefix = "drug_") |>
  mutate(
    diff = drug_2 - drug_1,
    outcome = case_when(
      diff > 0 ~ "Slept longer on drug 2",
      diff < 0 ~ "Slept less on drug 2",
      TRUE     ~ "No difference"
    )
  )

# One row per patient and drug, for plotting
long <- sleep |>
  mutate(ID = as.character(ID), x = as.numeric(group)) |>
  left_join(select(wide, ID, outcome), by = "ID")

means <- long |>
  group_by(x) |>
  summarise(extra = mean(extra), .groups = "drop") |>
  mutate(label = sprintf("%.2f h", extra), hjust = c(1.3, -0.3))

# ---- 3. Analysis -----------------------------------------------------------

n_total  <- nrow(wide)
n_better <- sum(wide$diff > 0)
n_tie    <- sum(wide$diff == 0)

test <- t.test(wide$drug_2, wide$drug_1, paired = TRUE)
mean_diff <- unname(test$estimate)
ci        <- test$conf.int
p_text    <- if (test$p.value < 0.001) "p < 0.001" else sprintf("p = %.3f", test$p.value)

summary_line <- sprintf(
  "Mean extra sleep: drug 1 = %.3f h, drug 2 = %.3f h | paired difference = %.3f h | 95%% CI [%.3f, %.3f] | t = %.3f, df = %d, p = %.5f\n",
  means$extra[1], means$extra[2], mean_diff, ci[1], ci[2],
  unname(test$statistic), as.integer(test$parameter), test$p.value
)

# ---- 4. Build the infographic ----------------------------------------------

headline <- sprintf("%d of %d Patients Slept Longer on Drug 2", n_better, n_total)

subtitle <- sprintf(
  paste0(
    "Average extra sleep: %.2f h on drug 1 vs %.2f h on drug 2.\n",
    "Mean paired difference: %.2f h (95%% CI %.2f to %.2f, paired t-test, %s).%s"
  ),
  means$extra[1], means$extra[2], mean_diff, ci[1], ci[2], p_text,
  if (n_tie > 0) sprintf("\n%d patient showed no difference.", n_tie) else ""
)

caption <- sprintf(
  "Data: sleep, %d patients who each received both drugs. Each thin line is one patient; the thick line is the group mean.\nReproduce: Rscript sleep_plot.R %s %s %s",
  n_total, palette_name, format(base_size), basename(out_file)
)

p <- ggplot(long, aes(x = x, y = extra)) +
  geom_hline(yintercept = 0, colour = "grey55", linetype = "dashed") +
  geom_line(aes(group = ID, colour = outcome), linewidth = 0.9, alpha = 0.8) +
  geom_point(aes(colour = outcome), size = 2.8, alpha = 0.9) +
  geom_line(data = means, aes(group = 1), colour = pal$ink, linewidth = 2, inherit.aes = TRUE) +
  geom_point(data = means, colour = pal$ink, size = 5, inherit.aes = TRUE) +
  geom_text(data = means, aes(label = label, hjust = hjust), colour = pal$ink,
            fontface = "bold", size = base_size * 0.3, inherit.aes = TRUE) +
  scale_x_continuous(breaks = c(1, 2), labels = c("Drug 1", "Drug 2"), limits = c(0.6, 2.4)) +
  scale_colour_manual(
    values = c(
      "Slept longer on drug 2" = pal$better,
      "Slept less on drug 2"   = pal$worse,
      "No difference"          = "grey50"
    ),
    name = NULL
  ) +
  labs(
    title = headline,
    subtitle = subtitle,
    x = NULL,
    y = "Extra hours of sleep",
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
    axis.text.x       = element_text(face = "bold", size = base_size * 1.2),
    plot.title        = element_text(face = "bold", size = base_size * 1.9, margin = margin(b = 6)),
    plot.subtitle     = element_text(size = base_size * 1.05, lineheight = 1.15, margin = margin(b = 14)),
    plot.caption      = element_text(size = base_size * 0.75, hjust = 0, colour = "grey35", margin = margin(t = 12)),
    legend.position   = "top",
    legend.justification = "left",
    plot.margin       = margin(20, 24, 16, 24)
  )

# ---- 5. Save ---------------------------------------------------------------

ggsave(out_file, plot = p, width = 8, height = 8, dpi = 150, bg = pal$bg)

cat(sprintf("Wrote %s\n", out_file))
cat(summary_line)