# flower_plot.R
# Capsule-style flower plot (grouped petals) with adaptive radius.
# Dependencies: ggplot2, dplyr, purrr  (optional: pals for "glasbey")
# SPDX-License-Identifier: MIT
# thiago22yow

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(purrr)
})

# ---------------- Palettes (scalable) ----------------
.okabe_ito <- c("#E69F00","#56B4E9","#009E73","#F0E442",
                "#0072B2","#D55E00","#CC79A7","#999999")

.make_hcl <- function(n, L, C, H_start = 15){
  H <- seq(H_start, H_start + 360, length.out = n + 1)[1:n]
  grDevices::hcl(h = H %% 360, c = C, l = L)
}

.get_palette <- function(mode, n, custom = NULL){
  mode <- match.arg(mode, c("pastel_rainbow","dark_pastel","soft_pastel","okabe_ito","glasbey","custom"))
  if (mode == "glasbey") {
    if (requireNamespace("pals", quietly = TRUE)) return(pals::glasbey(n))
    return(.make_hcl(n, L = 75, C = 55, H_start = 5))  # vivid HCL fallback
  }
  if (mode == "okabe_ito") {
    if (n <= length(.okabe_ito)) return(.okabe_ito[seq_len(n)])
    extra <- .make_hcl(n - length(.okabe_ito), L = 75, C = 45, H_start = 0)
    return(c(.okabe_ito, extra))
  }
  if (mode == "pastel_rainbow") return(.make_hcl(n, L = 90, C = 35, H_start = 10))
  if (mode == "dark_pastel")    return(.make_hcl(n, L = 70, C = 40, H_start = 10))
  if (mode == "soft_pastel")    return(.make_hcl(n, L = 95, C = 25, H_start = 10))
  if (mode == "custom") {
    if (is.null(custom)) stop("Provide 'palette' when palette_mode='custom'.")
    if (n > length(custom)) return(grDevices::colorRampPalette(custom)(n))
    return(custom[seq_len(n)])
  }
  stop("Unknown palette_mode.")
}

# ------------- Normalize CSV -> canonical columns -------------
# Expected columns: label, shared, singletons, optional group, order
.normalize_flower_df <- function(df, cols = list(
  label = "label",
  shared = "shared",
  singletons = "singletons",
  group = NULL,
  order = NULL
)){
  must <- c("label","shared","singletons")
  for (m in must) {
    colname <- cols[[m]]
    if (is.null(colname) || !colname %in% names(df))
      stop(sprintf("Required column '%s' not found.", m))
  }
  out <- tibble::tibble(
    label = df[[cols$label]],
    shared = as.numeric(df[[cols$shared]]),
    singletons = as.numeric(df[[cols$singletons]]),
    group = if (!is.null(cols$group) && cols$group %in% names(df)) df[[cols$group]] else NA
  )
  out$order <- if (!is.null(cols$order) && cols$order %in% names(df)) df[[cols$order]] else seq_len(nrow(out))
  out
}

# ------------------------- Main function -------------------------
#' Capsule-style flower plot for pangenome-like summaries.
#'
#' @param data data.frame with columns: label, shared, singletons; optional: group, order.
#' @param core_value Numeric, value shown at the center (e.g., core gene count).
#' @param center_label Character. If "", the center label is omitted.
#' @param ring_radius Base ring radius. If auto_radius=TRUE, it is adapted.
#' @param inner_radius Inner circle radius. If NA, uses inner_ratio * ring_radius.
#' @param petal_len Petal length (tip to tip across the center line).
#' @param petal_width Petal width (diameter).
#' @param embed_frac Fraction of the petal that goes inside the ring (0..1).
#' @param cap_ratio Ellipse tip ratio (>1 makes tips more elongated).
#' @param angle_offset Global rotation in radians.
#' @param auto_radius Logical. If TRUE, adapts ring radius to the number of petals.
#' @param spacing_factor Target arc distance relative to petal_width.
#' @param radius_limits Numeric length-2, min/max clamp for adaptive radius.
#' @param inner_ratio Used when inner_radius is NA.
#' @param auto_label_offset Logical, increase label offset a bit when needed.
#' @param group_order Optional character vector to force group order around the ring.
#' @param value_pos_in,value_pos_out Relative positions for shared/singleton values.
#' @param label_offset Distance for labels beyond the petal tip.
#' @param rotate_labels,rotate_values Logical toggles.
#' @param ring_* Center ring style; inner_* inner-circle style.
#' @param palette_mode One of "soft_pastel","pastel_rainbow","dark_pastel","okabe_ito","glasbey","custom".
#' @param palette Custom palette vector if palette_mode="custom".
#' @param legend_title Legend title or NULL to hide.
#' @param petal_alpha Petal alpha; inner half appears darker over the ring.
#' @param center_* Center text styling. Use center_label="" to omit label.
#' @return A ggplot object.
flower_plot_capsule <- function(
    data,
    core_value,
    center_label    = "", # type some text to indicate central number
    # geometry
    ring_radius     = 2.4,
    inner_radius    = NA,      # if NA, uses inner_ratio * ring_radius
    petal_len       = 1.6,
    petal_width     = 0.70,
    embed_frac      = 0.5,
    cap_ratio       = 1.8,
    angle_offset    = 0,
    # ADAPTIVE RADIUS
    auto_radius     = TRUE,
    spacing_factor  = 1.18,           # target arc spacing ≈ spacing_factor * petal_width
    radius_limits   = c(1.6, 5.0),    # clamp for ring radius
    inner_ratio     = 0.375,          # inner_radius = inner_ratio * ring_radius when NA
    auto_label_offset = TRUE,
    # grouping
    group_order     = NULL,
    # text placement
    value_pos_in    = 0.60,
    value_pos_out   = 0.40,
    label_offset    = 0.70,
    rotate_labels   = TRUE,
    rotate_values   = FALSE,
    # center style
    ring_fill       = "grey90",
    ring_border     = "grey85",
    ring_border_w   = 0.6,
    inner_fill      = "grey80",
    inner_border    = NA,
    inner_border_w  = 0,
    # palette (default = soft_pastel)
    palette_mode    = c("soft_pastel","pastel_rainbow","dark_pastel","okabe_ito","glasbey","custom"),
    palette         = NULL,
    legend_title    = NULL,
    # petals
    petal_alpha     = 0.70,
    # center text
    center_value_y  = 0.00,
    center_value_size = 6,
    center_value_face = "bold",
    center_value_colour = "black",
    center_font = NULL,
    # titles / export
    title = NULL, subtitle = NULL, caption = NULL,
    export_path = NULL, width = 7, height = 7, dpi = 300
){
  stopifnot(all(c("label","shared","singletons") %in% names(data)))
  if (!"order" %in% names(data))  data$order <- seq_len(nrow(data))
  if (!"group" %in% names(data))  data$group <- NA_character_
  palette_mode <- match.arg(palette_mode)

  # ---- grouped ordering
  has_group <- !all(is.na(data$group))
  if (has_group) {
    if (!is.null(group_order)) {
      g_levels <- unique(c(group_order, setdiff(unique(data$group), group_order)))
    } else {
      g_levels <- unique(data$group)
    }
    data$group <- factor(data$group, levels = g_levels, ordered = TRUE)
    data <- dplyr::arrange(data, group, order)
  } else {
    data <- dplyr::arrange(data, order)
  }

  # ---- angles
  k <- nrow(data)
  angles <- seq(0, 2*pi, length.out = k + 1)[-(k + 1)] + angle_offset
  data$theta <- angles

  # ---- adaptive radius
  if (auto_radius) {
    target_spacing <- spacing_factor * petal_width
    ring_radius <- max(radius_limits[1],
                       min(radius_limits[2], (k * target_spacing) / (2*pi)))
  }
  if (is.na(inner_radius)) inner_radius <- inner_ratio * ring_radius
  if (auto_label_offset)   label_offset <- max(label_offset, 0.55 * petal_width)

  # ---- colors
  fill_key <- if (has_group) data$group else data$label
  cols <- setNames(.get_palette(palette_mode, length(unique(fill_key)), palette),
                   unique(fill_key))

  # helpers
  circle_df <- function(r, n=360){
    t <- seq(0, 2*pi, length.out = n)
    data.frame(x = r*cos(t), y = r*sin(t))
  }
  point_on_axis <- function(theta, r){
    cbind(x = -r*sin(theta), y = r*cos(theta))
  }

  # ---- capsule geometry (single polygon per petal)
  embed_frac <- max(0, min(1, embed_frac))
  L_total <- max(petal_len, petal_width + 1e-6)
  ax <- petal_width/2
  if (ring_radius <= ax) stop("ring_radius must be > petal_width/2.")
  cap_ratio_max <- (L_total/2 - 1e-6) / ax
  cap_ratio <- max(1, min(cap_ratio, cap_ratio_max))
  ay <- ax * cap_ratio

  L_in  <- L_total * embed_frac
  L_out <- L_total * (1 - embed_frac)
  R_inner_extreme <- ring_radius - L_in
  R_outer_extreme <- ring_radius + L_out

  capsule_df <- function(theta){
    y_bot_c <- R_inner_extreme + ay
    y_top_c <- R_outer_extreme - ay
    n_arc <- 140
    t1 <- seq(pi, 0, length.out = n_arc);  xt1 <- ax*cos(t1); yt1 <- y_top_c + ay*sin(t1)
    xrd <- ax; yrd <- seq(y_top_c, y_bot_c, length.out = 18)
    t2 <- seq(0, pi, length.out = n_arc);  xt2 <- ax*cos(t2); yt2 <- y_bot_c - ay*sin(t2)
    xle <- -ax; yle <- seq(y_bot_c, y_top_c, length.out = 18)
    xL <- c(xt1, rep(xrd,length(yrd)), xt2, rep(xle,length(yle)))
    yL <- c(yt1, yrd,                  yt2, yle)
    data.frame(
      x = xL*cos(theta) - yL*sin(theta),
      y = xL*sin(theta) + yL*cos(theta)
    )
  }

  petals_df <- purrr::pmap_dfr(
    list(data$theta, fill_key, data$label),
    function(th, fk, lab){
      poly <- capsule_df(th)
      poly$fill_key <- fk
      poly$group_id <- lab
      poly
    }
  )

  # ---- text placement
  val_in_r  <- R_inner_extreme + (ring_radius - R_inner_extreme) * value_pos_in
  val_out_r <- ring_radius     + (R_outer_extreme - ring_radius) * value_pos_out
  lab_r     <- R_outer_extreme + label_offset

  values_in_xy  <- point_on_axis(data$theta, val_in_r)
  values_out_xy <- point_on_axis(data$theta, val_out_r)
  labels_xy     <- point_on_axis(data$theta, lab_r)

  df_val_in  <- data.frame(value = data$shared,     vx = values_in_xy[,1],  vy = values_in_xy[,2],  theta = data$theta)
  df_val_out <- data.frame(value = data$singletons, vx = values_out_xy[,1], vy = values_out_xy[,2], theta = data$theta)
  df_lab     <- data.frame(label = data$label,      lx = labels_xy[,1],     ly = labels_xy[,2],     theta = data$theta)

  angle_tangent <- (data$theta + pi/2) * 180/pi
  df_lab$angle  <- if (rotate_labels) ifelse(cos(data$theta)<0, angle_tangent+180, angle_tangent) else 0
  df_lab$hjust  <- if (rotate_labels) 0.5 else ifelse(cos(data$theta)>=0, 0, 1)
  if (rotate_values){
    df_val_in$angle  <- (data$theta*180/pi)
    df_val_out$angle <- (data$theta*180/pi)
  } else {
    df_val_in$angle <- df_val_out$angle <- 0
  }

  lim <- max(R_outer_extreme + label_offset + 0.9, ring_radius + 0.5)
  center_font_use <- if (is.null(center_font)) "" else center_font

  # ---- draw base
  p <- ggplot() +
    geom_polygon(data = circle_df(ring_radius), aes(x = x, y = y),
                 fill = ring_fill, color = ring_border, linewidth = ring_border_w) +
    geom_polygon(data = petals_df,
                 aes(x = x, y = y, group = group_id, fill = fill_key),
                 color = NA, alpha = petal_alpha) +
    scale_fill_manual(values = cols, name = legend_title) +
    geom_polygon(data = circle_df(inner_radius), aes(x = x, y = y),
                 fill = inner_fill, color = inner_border, linewidth = inner_border_w) +
    geom_text(data = df_val_in,
              aes(x = vx, y = vy, label = as.character(value)),
              size = 3, fontface = "plain", colour = "grey20") +
    geom_text(data = df_val_out,
              aes(x = vx, y = vy, label = as.character(value)),
              size = 3, fontface = "plain", colour = "grey20") +
    geom_text(data = df_lab,
              aes(x = lx, y = ly, label = label, angle = angle, hjust = hjust),
              size = 3.1, fontface = "plain", colour = "grey25") +
    coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim), expand = FALSE) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 11),
      plot.caption = element_text(hjust = 0, size = 9, colour = "grey30"),
      legend.position = if (is.null(legend_title)) "none" else "right",
      plot.margin = margin(10, 10, 10, 10)
    )

  # center texts (added afterward)
  p <- p +
    geom_text(data = data.frame(x=0, y=center_value_y, lab=as.character(core_value)),
              aes(x=x, y=y, label=lab),
              size = center_value_size, fontface = center_value_face,
              colour = center_value_colour, family = center_font_use,
              inherit.aes = FALSE)

  if (!is.null(center_label) && nzchar(center_label)) {
    p <- p +
      geom_text(data = data.frame(x=0, y=center_label_y, lab=center_label),
                aes(x=x, y=y, label=lab),
                size = center_label_size, fontface = center_label_face,
                colour = "grey30", family = center_font_use,
                inherit.aes = FALSE)
  }

  if (!is.null(title))    p <- p + ggtitle(title)
  if (!is.null(subtitle)) p <- p + labs(subtitle = subtitle)
  if (!is.null(caption))  p <- p + labs(caption = caption)
  if (!is.null(export_path)) ggsave(export_path, plot = p, width = width, height = height, dpi = dpi)
  p
}

# ---------------- Convenience: read CSV and plot ----------------
#' Read a CSV (label/shared/singletons[/group][/order]) and draw the plot.
#' Additional arguments (...) are passed to flower_plot_capsule().
flower_plot_capsule_from_csv <- function(
    csv_path,
    core_value,
    cols = list(label="label", shared="shared", singletons="singletons", group="group", order="order"),
    ...
){
  df <- utils::read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  df_norm <- .normalize_flower_df(df, cols = cols)
  do.call(flower_plot_capsule, c(list(data = df_norm, core_value = core_value), list(...)))
}

# --- Example ---
# install.packages(c("ggplot2","dplyr","purrr"))
# source("flower_plot_capsule.R")
# p <- flower_plot_capsule_from_csv("flower_20_genomes.csv",
#   core_value=42, palette_mode="soft_pastel",
#   group_order=c("group_1","group_2","group_3","group_4","group_5"),
#   legend_title="Group")
# print(p)
