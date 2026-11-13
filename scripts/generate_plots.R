#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(scales)
  library(ggrepel)
  library(ggtext)
  library(ggnewscale)
  library(patchwork)
  library(broom)
  library(ragg)
  library(svglite)
})

dir.create("plots", showWarnings = FALSE, recursive = TRUE)

boe <- read_csv("data/boe_base_rate.csv", show_col_types = FALSE) |>
  mutate(date = as.Date(date)) |>
  arrange(date)

forecasts <- read_csv("data/boe_forecasts.csv", show_col_types = FALSE) |>
  mutate(
    forecast_date = as.Date(forecast_date),
    target_date = as.Date(target_date),
    recorded_at = as.Date(recorded_at)
  ) |>
  arrange(bank, forecast_date, target_date)

PALETTE <- c("ING" = "#D95F02", "Scotiabank" = "#1B4F72")
SHAPES  <- c("ING" = 16, "Scotiabank" = 17)
INK     <- "#111827"
MUTED   <- "#4B5563"

theme_sci <- function(base_size = 10.5) {
  theme_bw(base_size = base_size) +
    theme(
      plot.title = element_text(
        face = "bold", size = base_size + 1.8, color = INK, margin = margin(b = 3)
      ),
      plot.subtitle = element_textbox_simple(
        size = base_size - 0.6, color = "#374151", margin = margin(b = 6),
        lineheight = 1.25
      ),
      plot.caption = element_textbox_simple(
        size = base_size - 2.1, color = MUTED, hjust = 0, family = "mono",
        margin = margin(t = 8), lineheight = 1.25
      ),
      panel.border = element_rect(color = "#374151", linewidth = 0.65),
      panel.grid.major = element_line(color = "#E5E7EB", linewidth = 0.35),
      panel.grid.minor = element_line(color = "#F3F4F6", linewidth = 0.2),
      strip.background = element_rect(
        fill = "#F3F4F6", color = "#374151", linewidth = 0.65
      ),
      strip.text = element_text(face = "bold", size = base_size - 0.3, color = INK),
      axis.title = element_text(face = "bold", size = base_size - 0.2, color = INK),
      axis.text = element_text(color = "#1F2937", size = base_size - 1.0),
      axis.ticks = element_line(color = "#374151", linewidth = 0.4),
      legend.position = "top",
      legend.justification = "left",
      legend.box = "vertical",
      legend.box.just = "left",
      legend.spacing.y = unit(2, "pt"),
      legend.title = element_text(face = "bold", size = base_size - 1.0, color = INK),
      legend.text = element_text(size = base_size - 1.1, color = "#1F2937"),
      legend.key.height = unit(12, "pt"),
      legend.key.width = unit(18, "pt"),
      legend.margin = margin(1, 4, 1, 0),
      legend.box.margin = margin(0, 0, 2, 0),
      plot.margin = margin(10, 14, 10, 10)
    )
}

save_both <- function(plot_obj, stem, width = 11.0, height = 6.5) {
  ggsave(
    file.path("plots", paste0(stem, ".svg")),
    plot = plot_obj, device = svglite::svglite,
    width = width, height = height, bg = "#FFFFFF"
  )
  ggsave(
    file.path("plots", paste0(stem, ".png")),
    plot = plot_obj, device = ragg::agg_png,
    width = width, height = height, dpi = 300, bg = "#FFFFFF"
  )
}

actual_as_of <- function(query_dates, boe_df) {
  idx <- findInterval(query_dates, boe_df$date)
  ifelse(idx > 0, boe_df$rate[idx], NA_real_)
}

slice_boe_window <- function(boe_df, start_date) {
  in_range <- boe_df |> filter(date >= start_date)
  start_rate <- actual_as_of(start_date, boe_df)
  needs_anchor <- !is.na(start_rate) &&
    (nrow(in_range) == 0 || min(in_range$date) > start_date)
  if (needs_anchor) {
    in_range <- bind_rows(
      tibble(date = start_date, rate = start_rate, event_type = "anchor", source = "anchor"),
      in_range
    )
  }
  in_range
}

latest_actual_date <- max(boe$date)
latest_actual_rate <- tail(boe$rate, 1)

latest_vintages <- forecasts |>
  group_by(bank) |>
  filter(forecast_date == max(forecast_date)) |>
  ungroup()

# ===========================================================================
# PLOT 1: Term Structure + Inter-Vintage Uncertainty Ribbon & Spacious ggrepel
#         with Complete Graphical Legend of All Symbols, Lines & Ribbons
# ===========================================================================
boe_start <- as.Date("2023-06-01")
boe_win <- slice_boe_window(boe, boe_start)
mpc_changes <- boe |>
  filter(event_type == "change", date >= boe_start) |>
  mutate(
    prev_rate = lag(rate, default = actual_as_of(boe_start, boe)),
    delta_bps = round((rate - prev_rate) * 100),
    call_lbl = sprintf(
      "%s: %.2f%% (%+d bp)",
      format(date, "%d %b %y"), rate, delta_bps
    )
  )

vintage_stats <- forecasts |>
  filter(target_date >= as.Date("2026-12-31")) |>
  group_by(target_date) |>
  summarise(
    n_obs = n(),
    mean_r = mean(forecast_rate),
    sd_r = sd(forecast_rate),
    se_r = sd_r / sqrt(n_obs),
    ci_lo = ifelse(n_obs >= 2, mean_r - qt(0.975, df = n_obs - 1) * se_r, mean_r),
    ci_hi = ifelse(n_obs >= 2, mean_r + qt(0.975, df = n_obs - 1) * se_r, mean_r),
    q25 = quantile(forecast_rate, 0.25),
    q75 = quantile(forecast_rate, 0.75),
    min_r = min(forecast_rate),
    max_r = max(forecast_rate),
    .groups = "drop"
  )

# Push callouts cleanly into the open upper-right / lower-left whitespace via directional nudges + stronger repulsion
unified_p1_labels <- bind_rows(
  mpc_changes |>
    mutate(
      # Alternate / spread MPC callouts into upper-right & lower-left whitespace
      nx = case_when(
        date <= as.Date("2023-09-01") ~ 110,
        date == as.Date("2026-11-05") ~ -195,
        row_number() %% 2 == 0 ~ 135,
        TRUE ~ -145
      ),
      ny = case_when(
        date == as.Date("2023-06-22") ~ -0.32,
        date == as.Date("2023-08-03") ~ 0.30,
        date == as.Date("2025-12-18") ~ -0.34,
        date == as.Date("2026-11-05") ~ 0.36,
        row_number() %% 2 == 0 ~ 0.26,
        TRUE ~ -0.26
      )
    ) |>
    transmute(x = date, y = rate, nx = nx, ny = ny, lbl = call_lbl, grp = "MPC"),
  latest_vintages |>
    mutate(
      nx = case_when(
        bank == "ING" & horizon_label == "4Q26F" ~ -135,
        bank == "Scotiabank" & horizon_label == "2026(F)" ~ -140,
        bank == "ING" & horizon_label == "1Q27F" ~ -70,
        bank == "ING" & horizon_label == "2Q27F" ~ 0,
        bank == "ING" & horizon_label == "3Q27F" ~ 85,
        bank == "Scotiabank" & horizon_label == "2027(F)" ~ 155,
        bank == "ING" & horizon_label == "4Q27F" ~ 145,
        bank == "ING" & horizon_label == "4Q28F" ~ -110,
        bank == "Scotiabank" & horizon_label == "2028(F)" ~ -150,
        TRUE ~ 60
      ),
      ny = case_when(
        bank == "ING" & horizon_label == "4Q26F" ~ -0.30,
        bank == "Scotiabank" & horizon_label == "2026(F)" ~ 0.56,
        bank == "ING" & horizon_label == "1Q27F" ~ 0.48,
        bank == "ING" & horizon_label == "2Q27F" ~ 0.68,
        bank == "ING" & horizon_label == "3Q27F" ~ 0.52,
        bank == "Scotiabank" & horizon_label == "2027(F)" ~ 0.32,
        bank == "ING" & horizon_label == "4Q27F" ~ 0.14,
        bank == "ING" & horizon_label == "4Q28F" ~ 0.42,
        bank == "Scotiabank" & horizon_label == "2028(F)" ~ -0.28,
        TRUE ~ 0.30
      )
    ) |>
    transmute(
      x = target_date,
      y = forecast_rate,
      nx = nx,
      ny = ny,
      lbl = sprintf("%s %s: %.2f%%", ifelse(bank == "ING", "ING", "BNS"), horizon_label, forecast_rate),
      grp = bank
    )
)

label_colors_p1 <- c(
  "MPC" = "#1F2937",
  "ING" = unname(PALETTE["ING"]),
  "Scotiabank" = unname(PALETTE["Scotiabank"])
)

ing_vd <- max(latest_vintages$forecast_date[latest_vintages$bank == "ING"])
bns_vd <- max(latest_vintages$forecast_date[latest_vintages$bank == "Scotiabank"])

p1 <- ggplot() +
  # 1. Historical cross-vintage dispersion ribbons (Fill scale)
  geom_ribbon(
    data = vintage_stats,
    aes(x = target_date, ymin = min_r, ymax = max_r, fill = "Cross-vintage [min, max]"),
    alpha = 0.16
  ) +
  geom_ribbon(
    data = vintage_stats,
    aes(x = target_date, ymin = q25, ymax = q75, fill = "Cross-vintage IQR (25–75%)"),
    alpha = 0.30
  ) +
  scale_fill_manual(
    name = "Historical Dispersion:",
    values = c(
      "Cross-vintage [min, max]" = "#9CA3AF",
      "Cross-vintage IQR (25–75%)" = "#4B5563"
    ),
    guide = guide_legend(order = 2)
  ) +
  # 2. Reference / Realized / Ensemble lines (Color + Linetype scale #1)
  geom_line(
    data = vintage_stats,
    aes(x = target_date, y = mean_r, color = "Cross-vintage Mean \u00b1 95% t-CI", linetype = "Cross-vintage Mean \u00b1 95% t-CI"),
    linewidth = 0.7
  ) +
  geom_errorbar(
    data = vintage_stats,
    aes(x = target_date, ymin = ci_lo, ymax = ci_hi, color = "Cross-vintage Mean \u00b1 95% t-CI"),
    width = 24, linewidth = 0.48, alpha = 0.9, show.legend = FALSE
  ) +
  geom_vline(
    aes(xintercept = latest_actual_date, color = "T0 Boundary (Latest Verified)", linetype = "T0 Boundary (Latest Verified)"),
    linewidth = 0.6
  ) +
  geom_step(
    data = boe_win,
    aes(x = date, y = rate, color = "Realized BoE Bank Rate (MPC Decisions)", linetype = "Realized BoE Bank Rate (MPC Decisions)"),
    direction = "hv", linewidth = 0.95
  ) +
  geom_point(
    data = mpc_changes,
    aes(x = date, y = rate),
    color = INK, fill = "#FFFFFF", shape = 21, size = 2.4, stroke = 0.95, show.legend = FALSE
  ) +
  scale_color_manual(
    name = "Benchmark Series:",
    values = c(
      "Realized BoE Bank Rate (MPC Decisions)" = INK,
      "Cross-vintage Mean \u00b1 95% t-CI" = "#4B5563",
      "T0 Boundary (Latest Verified)" = "#6B7280"
    ),
    guide = guide_legend(
      order = 1,
      override.aes = list(
        shape = c(21, NA, NA),
        fill = c("#FFFFFF", NA, NA),
        linewidth = c(0.95, 0.75, 0.65)
      )
    )
  ) +
  scale_linetype_manual(
    name = "Benchmark Series:",
    values = c(
      "Realized BoE Bank Rate (MPC Decisions)" = "solid",
      "Cross-vintage Mean \u00b1 95% t-CI" = "22",
      "T0 Boundary (Latest Verified)" = "dashed"
    ),
    guide = guide_legend(order = 1)
  ) +
  # 3. Forecaster active curves (ggnewscale Color + Shape scale #2)
  ggnewscale::new_scale_color() +
  geom_line(
    data = latest_vintages,
    aes(x = target_date, y = forecast_rate, color = bank),
    linewidth = 1.0
  ) +
  geom_point(
    data = latest_vintages,
    aes(x = target_date, y = forecast_rate, color = bank, shape = bank),
    size = 2.8
  ) +
  scale_color_manual(
    name = "Active Forecast Curve:",
    values = PALETTE,
    labels = c(
      "ING" = sprintf("ING (as of %s)", ing_vd),
      "Scotiabank" = sprintf("Scotiabank (as of %s)", bns_vd)
    ),
    guide = guide_legend(order = 3)
  ) +
  scale_shape_manual(
    name = "Active Forecast Curve:",
    values = SHAPES,
    labels = c(
      "ING" = sprintf("ING (as of %s)", ing_vd),
      "Scotiabank" = sprintf("Scotiabank (as of %s)", bns_vd)
    ),
    guide = guide_legend(order = 3)
  ) +
  geom_label(
    data = tibble(
      x = latest_actual_date, y = 5.62,
      lbl = sprintf("T0 = %s (BoE = %.2f%%)", latest_actual_date, latest_actual_rate)
    ),
    aes(x = x, y = y, label = lbl),
    size = 2.6, fontface = "bold", family = "mono",
    fill = "#F9FAFB", color = INK, linewidth = 0.3
  ) +
  geom_label_repel(
    data = unified_p1_labels,
    aes(x = x, y = y, label = lbl),
    nudge_x = unified_p1_labels$nx,
    nudge_y = unified_p1_labels$ny,
    color = label_colors_p1[unified_p1_labels$grp],
    size = 2.35, family = "mono", fontface = "bold",
    fill = alpha("#FFFFFF", 0.92), label.padding = unit(0.16, "lines"),
    linewidth = 0.24, box.padding = 0.55, point.padding = 0.35,
    force = 3.5, force_pull = 0.35,
    min.segment.length = 0, segment.size = 0.38, segment.color = "#6B7280",
    seed = 202, max.overlaps = 40, show.legend = FALSE
  ) +
  scale_y_continuous(
    breaks = seq(2.75, 5.75, by = 0.25),
    limits = c(2.65, 5.78),
    labels = label_number(accuracy = 0.01, suffix = "%")
  ) +
  scale_x_date(
    date_breaks = "6 months",
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.05, 0.07))
  ) +
  labs(
    title = "UK Official Bank Rate: Realized Step Function, Latest Term Structures & Prior-Vintage Dispersion",
    subtitle = "Every MPC decision since Jun 2023 and every active forecast node is labeled with leader lines; forward ensemble bands summarize all prior vintages.",
    x = "Calendar Date (MPC Effective Date / Forecast Horizon End-of-Period)",
    y = "Bank Rate (% p.a.)",
    caption = sprintf(
      "Data: BoE IADB IUDBEDR (through %s) & recorded forecast vintages (N=%d obs across %d vintages).",
      latest_actual_date, nrow(forecasts), n_distinct(paste(forecasts$bank, forecasts$forecast_date))
    )
  ) +
  theme_sci()

save_both(p1, "01-latest-forecasts", width = 11.4, height = 6.8)

# ===========================================================================
# PLOT 2: Redesigned Layer-Separated Horizon Distribution & Dispersion Profile
#         (Zero Symbol Collision + Complete Graphical Legend in Both Panels)
# ===========================================================================
horizon_counts <- forecasts |>
  count(target_date, name = "n_tot") |>
  filter(n_tot >= 5) |>
  arrange(target_date)

dist_df <- forecasts |>
  inner_join(horizon_counts, by = "target_date") |>
  mutate(
    q_tag = paste0(year(target_date), "-Q", quarter(target_date)),
    realized = ifelse(
      target_date <= latest_actual_date,
      actual_as_of(target_date, boe),
      NA_real_
    )
  )

q_levels <- dist_df |>
  distinct(target_date, q_tag) |>
  arrange(target_date) |>
  pull(q_tag)

dist_df <- dist_df |>
  mutate(
    q_tag = factor(q_tag, levels = q_levels),
    x_num = as.numeric(q_tag)
  )

horizon_summary <- dist_df |>
  group_by(q_tag, x_num, target_date) |>
  summarise(
    n = n(),
    mean_r = mean(forecast_rate),
    sd_r = sd(forecast_rate),
    iqr_r = IQR(forecast_rate),
    ci_lo = mean_r - qt(0.975, df = n - 1) * (sd_r / sqrt(n)),
    ci_hi = mean_r + qt(0.975, df = n - 1) * (sd_r / sqrt(n)),
    realized = first(realized),
    stat_lbl = sprintf("n=%d\n\u03bc=%.2f\n\u03c3=%.2f", n, mean_r, sd_r),
    .groups = "drop"
  )

latest_dist_pts <- latest_vintages |>
  inner_join(horizon_counts, by = "target_date") |>
  mutate(
    q_tag = factor(paste0(year(target_date), "-Q", quarter(target_date)), levels = q_levels),
    x_num = as.numeric(q_tag) + ifelse(bank == "ING", 0.24, 0.33),
    bank_latest = paste0("Latest Active Call: ", bank),
    pt_lbl = sprintf("%s: %.2f%%", ifelse(bank == "ING", "ING", "BNS"), forecast_rate)
  )

# Horizontal sub-lanes within each quarter column x_num prevent symbol overlap:
#   x_num - 0.17 : Boxplot (median & IQR) + light historical vintage points
#   x_num + 0.14 : Gold diamond + error bar (Sample Mean ± 95% t-CI)
#   x_num - 0.34 to + 0.34 : Green horizontal segment + star at x_num (Realized BoE Rate)
#   x_num + 0.24 / +0.33 : Latest active ING / Scotiabank call (connected line + bold symbol + ggrepel)
p2_top <- ggplot() +
  # Vertical column separators between quarters for crisp reading
  geom_vline(
    xintercept = seq(1.5, length(q_levels) - 0.5, by = 1),
    color = "#E5E7EB", linewidth = 0.35, linetype = "dotted"
  ) +
  # 1. Boxplot in left sub-lane (Fill legend)
  geom_boxplot(
    data = dist_df,
    aes(x = x_num - 0.17, y = forecast_rate, group = q_tag, fill = "Vintage Boxplot (Median & IQR)"),
    width = 0.28, outlier.shape = NA, color = "#374151", linewidth = 0.48, alpha = 0.9
  ) +
  scale_fill_manual(
    name = "Distribution:",
    values = c("Vintage Boxplot (Median & IQR)" = "#E5E7EB"),
    guide = guide_legend(order = 1)
  ) +
  # 2. Historical vintage observations (Color + Shape scale #1)
  geom_point(
    data = dist_df,
    aes(
      x = x_num - 0.17 + ifelse(bank == "ING", -0.05, 0.05),
      y = forecast_rate,
      color = bank,
      shape = bank
    ),
    alpha = 0.42, size = 1.65
  ) +
  scale_color_manual(
    name = "Historical Vintages:",
    values = PALETTE,
    guide = guide_legend(order = 2, override.aes = list(alpha = 0.85, size = 2.4))
  ) +
  scale_shape_manual(
    name = "Historical Vintages:",
    values = SHAPES,
    guide = guide_legend(order = 2)
  ) +
  # 3. Summary Estimators & Realized Benchmark (ggnewscale #2: Gold Diamond + CI & Green Realized)
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale("shape") +
  geom_errorbar(
    data = horizon_summary,
    aes(
      x = x_num + 0.15, ymin = ci_lo, ymax = ci_hi,
      color = "Sample Mean \u00b1 95% t-CI"
    ),
    width = 0.14, linewidth = 0.65
  ) +
  geom_point(
    data = horizon_summary,
    aes(
      x = x_num + 0.15, y = mean_r,
      color = "Sample Mean \u00b1 95% t-CI",
      fill = "Sample Mean \u00b1 95% t-CI",
      shape = "Sample Mean \u00b1 95% t-CI"
    ),
    size = 2.55, stroke = 0.75
  ) +
  geom_segment(
    data = filter(horizon_summary, !is.na(realized)),
    aes(
      x = x_num - 0.38, xend = x_num + 0.38,
      y = realized, yend = realized,
      color = "Realized BoE Quarter-End Rate"
    ),
    linewidth = 1.05
  ) +
  geom_point(
    data = filter(horizon_summary, !is.na(realized)),
    aes(
      x = x_num, y = realized,
      color = "Realized BoE Quarter-End Rate",
      fill = "Realized BoE Quarter-End Rate",
      shape = "Realized BoE Quarter-End Rate"
    ),
    size = 2.5, stroke = 0.9
  ) +
  scale_color_manual(
    name = "Moment & Benchmark:",
    values = c(
      "Sample Mean \u00b1 95% t-CI" = "#111827",
      "Realized BoE Quarter-End Rate" = "#059669"
    ),
    guide = guide_legend(order = 3)
  ) +
  scale_fill_manual(
    name = "Moment & Benchmark:",
    values = c(
      "Sample Mean \u00b1 95% t-CI" = "#FBBF24",
      "Realized BoE Quarter-End Rate" = "#059669"
    ),
    guide = guide_legend(order = 3)
  ) +
  scale_shape_manual(
    name = "Moment & Benchmark:",
    values = c(
      "Sample Mean \u00b1 95% t-CI" = 23,
      "Realized BoE Quarter-End Rate" = 8
    ),
    guide = guide_legend(order = 3)
  ) +
  # 4. Latest active curve nodes highlighted on forward quarters + ggrepel labels
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale("shape") +
  geom_point(
    data = latest_dist_pts,
    aes(x = x_num, y = forecast_rate, color = bank_latest, shape = bank_latest),
    size = 2.8
  ) +
  scale_color_manual(
    name = "Active Curve Nodes:",
    values = c(
      "Latest Active Call: ING" = unname(PALETTE["ING"]),
      "Latest Active Call: Scotiabank" = unname(PALETTE["Scotiabank"])
    ),
    labels = c("Latest Active Call: ING" = "Latest ING", "Latest Active Call: Scotiabank" = "Latest Scotiabank"),
    guide = guide_legend(order = 4)
  ) +
  scale_shape_manual(
    name = "Active Curve Nodes:",
    values = c("Latest Active Call: ING" = 16, "Latest Active Call: Scotiabank" = 17),
    labels = c("Latest Active Call: ING" = "Latest ING", "Latest Active Call: Scotiabank" = "Latest Scotiabank"),
    guide = guide_legend(order = 4)
  ) +
  geom_label_repel(
    data = latest_dist_pts,
    aes(x = x_num, y = forecast_rate, label = pt_lbl),
    color = ifelse(latest_dist_pts$bank == "ING", PALETTE["ING"], PALETTE["Scotiabank"]),
    nudge_y = ifelse(latest_dist_pts$bank == "ING", 0.36, -0.34),
    size = 2.2, family = "mono", fontface = "bold",
    fill = alpha("#FFFFFF", 0.92), label.padding = unit(0.13, "lines"),
    linewidth = 0.22, box.padding = 0.38, point.padding = 0.25,
    force = 2.5, segment.size = 0.32, segment.color = "#6B7280", min.segment.length = 0,
    seed = 303, max.overlaps = 30, show.legend = FALSE
  ) +
  geom_text(
    data = horizon_summary,
    aes(x = x_num, y = 5.86, label = stat_lbl),
    size = 2.25, family = "mono", color = "#374151", lineheight = 0.92
  ) +
  scale_x_continuous(
    breaks = seq_along(q_levels),
    labels = q_levels,
    limits = c(0.45, length(q_levels) + 0.55),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(2.5, 5.5, by = 0.25),
    limits = c(2.52, 6.12),
    labels = label_number(accuracy = 0.01, suffix = "%")
  ) +
  labs(
    title = "Cross-Vintage Forecast Distribution & Moments by Target Quarter (Horizons with n \u2265 5)",
    subtitle = "Each quarter column separates the historical boxplot/vintages (left lane) from the sample mean \u00b1 95% t-CI (right lane) to prevent symbol overlap.",
    x = NULL,
    y = "Projected / Realized Rate (%)"
  ) +
  theme_sci() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Bottom sub-panel with explicit graphical legend for Grey Bars (SD) & Red Diamonds + Dotted Line (IQR)
p2_bot <- ggplot(horizon_summary, aes(x = x_num)) +
  geom_vline(
    xintercept = seq(1.5, length(q_levels) - 0.5, by = 1),
    color = "#E5E7EB", linewidth = 0.35, linetype = "dotted"
  ) +
  geom_col(
    aes(y = sd_r, fill = "Standard Deviation (\u03c3, pp)"),
    width = 0.44, alpha = 0.82
  ) +
  geom_line(
    aes(y = iqr_r, color = "Interquartile Range (IQR, pp)", linetype = "Interquartile Range (IQR, pp)"),
    linewidth = 0.65
  ) +
  geom_point(
    aes(y = iqr_r, color = "Interquartile Range (IQR, pp)", shape = "Interquartile Range (IQR, pp)"),
    size = 2.8
  ) +
  geom_text(
    aes(
      y = pmax(sd_r, iqr_r) + 0.055,
      label = sprintf("\u03c3=%.2f", sd_r)
    ),
    size = 2.2, family = "mono", color = INK
  ) +
  scale_fill_manual(
    name = "Dispersion Metrics:",
    values = c("Standard Deviation (\u03c3, pp)" = "#6B7280"),
    guide = guide_legend(order = 1)
  ) +
  scale_color_manual(
    name = NULL,
    values = c("Interquartile Range (IQR, pp)" = "#B91C1C"),
    guide = guide_legend(order = 2)
  ) +
  scale_shape_manual(
    name = NULL,
    values = c("Interquartile Range (IQR, pp)" = 18),
    guide = guide_legend(order = 2)
  ) +
  scale_linetype_manual(
    name = NULL,
    values = c("Interquartile Range (IQR, pp)" = "dotted"),
    guide = guide_legend(order = 2)
  ) +
  scale_x_continuous(
    breaks = seq_along(q_levels),
    labels = q_levels,
    limits = c(0.45, length(q_levels) + 0.55),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 0.8, by = 0.2),
    limits = c(0, max(c(horizon_summary$sd_r, horizon_summary$iqr_r), na.rm = TRUE) + 0.16),
    labels = label_number(accuracy = 0.01, suffix = " pp")
  ) +
  labs(
    x = "Target Horizon Quarter-End",
    y = "Dispersion (pp)",
    caption = "Top column headers report sample size (n), cross-vintage mean (\u03bc), and standard deviation (\u03c3) per target quarter."
  ) +
  theme_sci() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

p2 <- p2_top / p2_bot + plot_layout(heights = c(3.1, 1.35))
save_both(p2, "02-forecast-vintages", width = 11.8, height = 8.0)

# ===========================================================================
# PLOT 3: Fixed Year-End Revision Dynamics + Explicit Graphical Legend for
#         Observations, OLS Dashed Trend, 95% CI Ribbon, and Green Realized Line
# ===========================================================================
year_end_targets <- as.Date(c("2024-12-31", "2025-12-31", "2026-12-31", "2027-12-31"))

rev_df <- forecasts |>
  filter(target_date %in% year_end_targets) |>
  mutate(
    actual_end = ifelse(
      target_date <= latest_actual_date,
      actual_as_of(target_date, boe),
      NA_real_
    ),
    panel_id = ifelse(
      !is.na(actual_end),
      sprintf("Target: %s  [Realized BoE = %.2f%%]", target_date, actual_end),
      sprintf("Target: %s  [Pending Realization]", target_date)
    )
  )

panel_levels <- rev_df |>
  distinct(target_date, panel_id) |>
  arrange(target_date) |>
  pull(panel_id)

rev_df <- rev_df |>
  mutate(panel_id = factor(panel_id, levels = panel_levels))

ols_stats <- rev_df |>
  group_by(panel_id, target_date, bank) |>
  filter(n() >= 3) |>
  reframe({
    t_months <- as.numeric(forecast_date - min(forecast_date)) / 30.4375
    fit <- lm(forecast_rate ~ t_months)
    s <- summary(fit)
    slope_bp_mo <- coef(fit)[2] * 100
    pval <- coef(s)[2, 4]
    r2 <- s$r.squared
    tibble(
      eq_txt = sprintf(
        "%s: \u03b2=%+.1f bp/mo (R\u00b2=%.2f, p=%.2g, n=%d)",
        ifelse(first(bank) == "ING", "ING", "BNS"), slope_bp_mo, r2, pval, length(t_months)
      )
    )
  })

panel_eq_box <- ols_stats |>
  group_by(panel_id, target_date) |>
  summarise(box_lbl = paste(eq_txt, collapse = "\n"), .groups = "drop") |>
  mutate(
    x_pos = min(rev_df$forecast_date) + days(5),
    y_pos = 5.92
  )

rev_callouts <- rev_df |>
  group_by(panel_id, bank) |>
  mutate(
    is_key = row_number() == 1 |
      row_number() == n() |
      (forecast_rate == max(forecast_rate) & !duplicated(forecast_rate)) |
      (forecast_rate == min(forecast_rate) & !duplicated(forecast_rate))
  ) |>
  filter(is_key) |>
  ungroup() |>
  mutate(
    call_lbl = sprintf(
      "%s %.2f%% (%s)",
      ifelse(bank == "ING", "ING", "BNS"),
      forecast_rate,
      format(forecast_date, "%y-%m")
    )
  )

realized_hline <- rev_df |>
  distinct(panel_id, target_date, actual_end) |>
  filter(!is.na(actual_end))

p3 <- ggplot() +
  # 1. Forecaster color/fill/shape layer (ING vs Scotiabank)
  geom_smooth(
    data = rev_df,
    aes(
      x = forecast_date, y = forecast_rate,
      color = bank, fill = bank,
      linetype = "Linear OLS Drift Trend (\u00b1 95% CI Ribbon)"
    ),
    method = "lm", formula = y ~ x, se = TRUE,
    alpha = 0.16, linewidth = 0.7
  ) +
  geom_line(
    data = rev_df,
    aes(
      x = forecast_date, y = forecast_rate,
      color = bank,
      linetype = "Published Forecast Trajectory"
    ),
    linewidth = 0.8, alpha = 0.9
  ) +
  geom_point(
    data = rev_df,
    aes(x = forecast_date, y = forecast_rate, color = bank, shape = bank),
    size = 2.2
  ) +
  scale_color_manual(
    name = "Forecaster:",
    values = PALETTE,
    guide = guide_legend(order = 1)
  ) +
  scale_fill_manual(
    name = "Forecaster:",
    values = PALETTE,
    guide = guide_legend(order = 1)
  ) +
  scale_shape_manual(
    name = "Forecaster:",
    values = SHAPES,
    guide = guide_legend(order = 1)
  ) +
  scale_linetype_manual(
    name = "Model & Path Geometry:",
    values = c(
      "Linear OLS Drift Trend (\u00b1 95% CI Ribbon)" = "22",
      "Published Forecast Trajectory" = "solid"
    ),
    guide = guide_legend(
      order = 2,
      override.aes = list(
        color = "#374151",
        fill = c("#9CA3AF", NA),
        alpha = c(0.38, 1),
        shape = c(NA, 16)
      )
    )
  ) +
  # 2. Separate graphical legend entry for the Green Realized Year-End Benchmark Line
  ggnewscale::new_scale_color() +
  geom_hline(
    data = realized_hline,
    aes(yintercept = actual_end, color = "Realized Year-End BoE Bank Rate"),
    linetype = "solid", linewidth = 0.95
  ) +
  scale_color_manual(
    name = "Benchmark:",
    values = c("Realized Year-End BoE Bank Rate" = "#059669"),
    guide = guide_legend(order = 3, override.aes = list(linewidth = 1.2))
  ) +
  # 3. OLS equations & spacious ggrepel callouts
  geom_label(
    data = panel_eq_box,
    aes(x = x_pos, y = y_pos, label = box_lbl),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1, size = 2.25, family = "mono",
    color = INK, fill = alpha("#F9FAFB", 0.93), linewidth = 0.25
  ) +
  geom_label_repel(
    data = rev_callouts,
    aes(x = forecast_date, y = forecast_rate, label = call_lbl),
    color = ifelse(rev_callouts$bank == "ING", PALETTE["ING"], PALETTE["Scotiabank"]),
    size = 2.15, family = "mono", fontface = "bold",
    fill = alpha("#FFFFFF", 0.90), label.padding = unit(0.13, "lines"),
    linewidth = 0.2, box.padding = 0.38, point.padding = 0.25,
    force = 2.2, min.segment.length = 0, segment.size = 0.32, segment.color = "#6B7280",
    seed = 404, max.overlaps = 24, show.legend = FALSE
  ) +
  facet_wrap(~ panel_id, ncol = 2) +
  scale_y_continuous(
    breaks = seq(2.5, 5.5, by = 0.5),
    limits = c(2.55, 6.00),
    labels = label_number(accuracy = 0.01, suffix = "%")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%Y-%m") +
  labs(
    title = "Fixed-Target Forecast Revision Paths & OLS Drift Regressions (2024–2027 Year-Ends)",
    subtitle = "Each panel tracks revisions to a fixed Dec 31 horizon as new data arrived, with per-bank linear OLS slope (\u03b2 in bp/mo), R\u00b2, and two-sided p-value.",
    x = "Forecast Publication Date (Vintage)",
    y = "Projected Year-End Bank Rate (%)",
    caption = "Positive OLS slope \u03b2 > 0 across all 8 bank-target pairs indicates systematic upward (hawkish) revision as horizons approached."
  ) +
  theme_sci()

save_both(p3, "03-horizon-revisions", width = 11.6, height = 8.0)

# ===========================================================================
# PLOT 4: Redesigned Around Clear Stratified Statistical Comparison:
#   (A) Error Distribution by Lead-Time Stratum (Boxplots + Pooled & Per-Bank
#       Means ± 95% t-CI + Explicit Graphical Legend for Gold Diamonds, Bars, Zero Line)
#   (B) Unconditional & Horizon-Adjusted Bias / RMSE Summary by Forecaster
#       (Mean Bias ± 95% t-CI & Continuous OLS Decay Curve with Graphical Legend)
# ===========================================================================
eval_df <- forecasts |>
  filter(target_date <= latest_actual_date, target_date >= forecast_date) |>
  mutate(
    actual_rate = actual_as_of(target_date, boe),
    error_pp = forecast_rate - actual_rate,
    lead_months = as.numeric(target_date - forecast_date) / 30.4375,
    lead_bin = cut(
      lead_months,
      breaks = c(0, 3, 6, 12, 18, Inf),
      labels = c("0–3m", "3–6m", "6–12m", "12–18m", ">18m"),
      include.lowest = TRUE, right = TRUE
    ),
    x_bin = as.numeric(lead_bin)
  ) |>
  filter(!is.na(error_pp))

err_reg_summary <- eval_df |>
  group_by(bank) |>
  reframe({
    fit <- lm(error_pp ~ lead_months)
    s <- summary(fit)
    ttest <- t.test(error_pp, mu = 0)
    tibble(
      txt = sprintf(
        "**%s** (n=%d): Bias = %+.2f pp [95%% CI: %+.2f, %+.2f], \u03c3 = %.2f, MAE = %.2f, RMSE = %.2f | OLS: e = %+.2f %+.3f\u00b7h (R\u00b2 = %.2f, p = %.2g)",
        first(bank), n(), mean(error_pp), ttest$conf.int[1], ttest$conf.int[2],
        sd(error_pp), mean(abs(error_pp)), sqrt(mean(error_pp^2)),
        coef(fit)[1], coef(fit)[2], s$r.squared, coef(s)[2, 4]
      )
    )
  })

bin_levels <- levels(eval_df$lead_bin)

bin_stats <- eval_df |>
  group_by(lead_bin, x_bin) |>
  summarise(
    n = n(),
    mean_e = mean(error_pp),
    sd_e = sd(error_pp),
    mae_e = mean(abs(error_pp)),
    pval = ifelse(n >= 2 && sd_e > 0, t.test(error_pp, mu = 0)$p.value, NA_real_),
    ci_lo = ifelse(n >= 2, mean_e - qt(0.975, df = n - 1) * (sd_e / sqrt(n)), mean_e),
    ci_hi = ifelse(n >= 2, mean_e + qt(0.975, df = n - 1) * (sd_e / sqrt(n)), mean_e),
    lbl = sprintf(
      "n=%d\n\u03bc=%+.2f\n\u03c3=%.2f\np=%s",
      n, mean_e, sd_e,
      ifelse(is.na(pval), "NA", ifelse(pval < 0.001, "<.001", sprintf("%.2f", pval)))
    ),
    mean_tag = sprintf("%+.2f pp", mean_e),
    .groups = "drop"
  )

# Panel A (Primary, Wider): Stratified Boxplot + Separated Sub-Lane for Gold Diamond Mean ± 95% t-CI
p4_left <- ggplot() +
  geom_vline(
    xintercept = seq(1.5, length(bin_levels) - 0.5, by = 1),
    color = "#E5E7EB", linewidth = 0.35, linetype = "dotted"
  ) +
  # Zero-error reference line in legend
  geom_hline(
    aes(yintercept = 0, linetype = "Zero-Error Benchmark (e = 0)"),
    color = INK, linewidth = 0.8
  ) +
  scale_linetype_manual(
    name = "Benchmark:",
    values = c("Zero-Error Benchmark (e = 0)" = "solid"),
    guide = guide_legend(order = 1)
  ) +
  # Boxplot in left sub-lane
  geom_boxplot(
    data = eval_df,
    aes(x = x_bin - 0.16, y = error_pp, group = lead_bin, fill = "Stratum Boxplot (Median & IQR)"),
    width = 0.30, outlier.shape = NA, color = "#374151", linewidth = 0.5, alpha = 0.9
  ) +
  scale_fill_manual(
    name = "Distribution:",
    values = c("Stratum Boxplot (Median & IQR)" = "#E5E7EB"),
    guide = guide_legend(order = 2)
  ) +
  # Individual realized forecast errors by bank
  geom_point(
    data = eval_df,
    aes(
      x = x_bin - 0.16 + ifelse(bank == "ING", -0.06, 0.06),
      y = error_pp, color = bank, shape = bank
    ),
    alpha = 0.58, size = 1.95
  ) +
  scale_color_manual(
    name = "Forecaster:",
    values = PALETTE,
    guide = guide_legend(order = 3, override.aes = list(alpha = 0.9, size = 2.5))
  ) +
  scale_shape_manual(
    name = "Forecaster:",
    values = SHAPES,
    guide = guide_legend(order = 3)
  ) +
  # Gold Diamond + 95% t-CI in right sub-lane WITH EXPLICIT GRAPHICAL LEGEND!
  ggnewscale::new_scale_color() +
  ggnewscale::new_scale_fill() +
  ggnewscale::new_scale("shape") +
  geom_line(
    data = bin_stats,
    aes(x = x_bin + 0.18, y = mean_e, color = "Stratum Mean \u00b1 95% t-CI"),
    linetype = "dotted", linewidth = 0.6, show.legend = FALSE
  ) +
  geom_errorbar(
    data = bin_stats,
    aes(x = x_bin + 0.18, ymin = ci_lo, ymax = ci_hi, color = "Stratum Mean \u00b1 95% t-CI"),
    width = 0.16, linewidth = 0.72
  ) +
  geom_point(
    data = bin_stats,
    aes(
      x = x_bin + 0.18, y = mean_e,
      color = "Stratum Mean \u00b1 95% t-CI",
      fill = "Stratum Mean \u00b1 95% t-CI",
      shape = "Stratum Mean \u00b1 95% t-CI"
    ),
    size = 3.0, stroke = 0.8
  ) +
  geom_label_repel(
    data = bin_stats,
    aes(x = x_bin + 0.18, y = mean_e, label = mean_tag),
    nudge_x = 0.24, direction = "y",
    size = 2.25, family = "mono", fontface = "bold",
    color = "#111827", fill = alpha("#FEF3C7", 0.92),
    label.padding = unit(0.13, "lines"), linewidth = 0.22,
    min.segment.length = 0, segment.size = 0.3, segment.color = "#6B7280",
    seed = 606
  ) +
  scale_color_manual(
    name = "Moment Estimator:",
    values = c("Stratum Mean \u00b1 95% t-CI" = "#111827"),
    guide = guide_legend(order = 4)
  ) +
  scale_fill_manual(
    name = "Moment Estimator:",
    values = c("Stratum Mean \u00b1 95% t-CI" = "#FBBF24"),
    guide = guide_legend(order = 4)
  ) +
  scale_shape_manual(
    name = "Moment Estimator:",
    values = c("Stratum Mean \u00b1 95% t-CI" = 23),
    guide = guide_legend(order = 4)
  ) +
  geom_text(
    data = bin_stats,
    aes(x = x_bin, y = 0.68, label = lbl),
    size = 2.2, family = "mono", color = "#374151", lineheight = 0.90
  ) +
  scale_x_continuous(
    breaks = seq_along(bin_levels),
    labels = bin_levels,
    limits = c(0.45, length(bin_levels) + 0.62),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(-1.0, 0.75, by = 0.25),
    limits = c(-0.85, 0.84),
    labels = label_number(accuracy = 0.01, suffix = " pp", style_positive = "plus")
  ) +
  labs(
    title = "(A) Forecast Error Distribution & t-Test by Lead-Time Stratum",
    x = "Forecast Lead-Time Stratum (Months Ahead)",
    y = "Forecast Error e = Forecast \u2212 Realized (pp)"
  ) +
  theme_sci(base_size = 10.0) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

# Panel B (Companion): Continuous OLS Error Regression ± 95% CI Ribbon + Spacious Outlier Callouts
err_outliers <- eval_df |>
  mutate(abs_err = abs(error_pp)) |>
  arrange(desc(abs_err), desc(lead_months)) |>
  group_by(bank, sign(error_pp)) |>
  slice_head(n = 2) |>
  ungroup() |>
  filter(abs_err >= 0.25) |>
  mutate(
    out_lbl = sprintf(
      "%s %s\u2192%s (%+.2f)",
      ifelse(bank == "ING", "ING", "BNS"),
      format(forecast_date, "%y-%m"),
      horizon_label,
      error_pp
    )
  )

p4_right <- ggplot(eval_df, aes(x = lead_months, y = error_pp)) +
  geom_hline(yintercept = 0, color = INK, linewidth = 0.8, linetype = "solid") +
  geom_smooth(
    aes(color = bank, fill = bank, linetype = "OLS Regression \u00b1 95% CI"),
    method = "lm", formula = y ~ x, se = TRUE,
    alpha = 0.18, linewidth = 0.85
  ) +
  geom_point(
    aes(color = bank, shape = bank),
    alpha = 0.55, size = 1.9, show.legend = FALSE
  ) +
  geom_label_repel(
    data = err_outliers,
    aes(label = out_lbl, color = bank),
    nudge_y = ifelse(err_outliers$error_pp > 0, 0.14, -0.14),
    size = 2.15, family = "mono", fontface = "bold",
    fill = alpha("#FFFFFF", 0.90), label.padding = unit(0.13, "lines"),
    linewidth = 0.2, box.padding = 0.42, point.padding = 0.25,
    force = 2.5, min.segment.length = 0, segment.size = 0.32, segment.color = "#6B7280",
    seed = 505, max.overlaps = 20, show.legend = FALSE
  ) +
  scale_color_manual(name = "OLS Fit by Bank:", values = PALETTE, guide = guide_legend(order = 1)) +
  scale_fill_manual(name = "OLS Fit by Bank:", values = PALETTE, guide = guide_legend(order = 1)) +
  scale_shape_manual(values = SHAPES, guide = "none") +
  scale_linetype_manual(
    name = NULL,
    values = c("OLS Regression \u00b1 95% CI" = "solid"),
    guide = guide_legend(order = 2, override.aes = list(color = "#374151", fill = "#9CA3AF", alpha = 0.35))
  ) +
  scale_y_continuous(
    breaks = seq(-1.0, 0.75, by = 0.25),
    limits = c(-1.02, 0.84),
    labels = label_number(accuracy = 0.01, suffix = " pp", style_positive = "plus")
  ) +
  scale_x_continuous(breaks = seq(0, 30, by = 6)) +
  labs(
    title = "(B) Continuous Lead-Horizon OLS Decay (\u00b1 95% CI)",
    x = "Exact Lead Horizon h (Months)",
    y = NULL
  ) +
  theme_sci(base_size = 10.0)

p4 <- (p4_left | p4_right) +
  plot_layout(widths = c(1.25, 1.0)) +
  plot_annotation(
    title = "Econometric Evaluation of Realized BoE Bank Rate Forecast Errors (N = 151 Realized Pairs)",
    subtitle = paste(err_reg_summary$txt, collapse = "<br/>"),
    caption = paste0(
      "e = Forecast - Realized Bank Rate (pp); e < 0 indicates dovish bias (over-predicting rate cuts). ",
      "Top of each column in (A) reports sample size n, mean \u03bc, SD \u03c3, and two-sided H0: \u03bc = 0 Student-t p-value."
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13.0, color = INK, margin = margin(b = 4)),
      plot.subtitle = element_textbox_simple(size = 9.0, family = "mono", color = "#1F2937", lineheight = 1.3, margin = margin(b = 6)),
      plot.caption = element_textbox_simple(size = 8.4, family = "mono", color = MUTED, hjust = 0, lineheight = 1.25, margin = margin(t = 6))
    )
  )

save_both(p4, "04-forecast-error", width = 12.2, height = 6.8)
