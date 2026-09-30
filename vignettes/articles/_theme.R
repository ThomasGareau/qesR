# The chart system of the qesR example articles (website only).
#
# Sourced by _setup.R, which every example page sources. Nothing here is
# exported or part of the package: vignettes/articles is .Rbuildignore'd,
# and the file name starts with "_", so pkgdown does not take it for an
# article. It needs ggplot2 and ragg (Config/Needs/website).
#
# Every chart is drawn twice, from the light and from the dark tokens below,
# and qz_figure() writes both PNGs; the CSS of pkgdown/extra.css shows the
# one that matches the page's theme. Dark mode is selected, not flipped: the
# dark steps were chosen and validated against the dark page surface.
#
# Palette validation. Every palette below was checked with the validator of
# the dataviz method (validate_palette.js, which is not in this repository,
# so no CI step runs it), on the page surfaces: Bootstrap's #ffffff (light)
# and #212529 (dark). Re-run these commands whenever a hex changes, and
# paste the new output here. The runs: the full party palette in legend
# order (PLQ, PQ, ADQ, QS, CAQ, PCQ, PVQ, ON), adjacent pairs, for lines and
# stacks; the six parties of the stacks (the grey of "Other" is the
# de-emphasis colour, not a categorical slot); all pairs of the four
# parties of the one-panel connected scatter (PLQ, PQ, QS, CAQ; the PCQ is
# grey there) and of the CROP lines (PLQ, PQ, ADQ, QS); the two language
# groups; the ordinal blue ramps (5 and 3 steps). Final run, 2026-09-29,
# every run exits 0:
#
# $ node validate_palette.js #b3202f,#1f55b0,#22876b,#e3740f,#1d97b0,#6f3f84,#3f6f22,#b88a3c --mode light --surface #ffffff --pairs adjacent
#   [PASS] Lightness band         all 8 inside L 0.43-0.77
#   [PASS] Chroma floor           all 8 >= 0.1
#   [PASS] CVD separation         worst adjacent #e3740f<->#22876b dE 9.7 (protan) . tritan 8.9
#   [PASS] Normal-vision floor    worst adjacent #b88a3c<->#3f6f22 dE 20.6 (normal)
#   [PASS] Contrast vs surface    all 8 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #cc323e,#1e78e1,#40a690,#d37812,#08a2af,#835cbe,#2c7f44,#af8e2a --mode dark --surface #212529 --pairs adjacent
#   [PASS] Lightness band         all 8 inside L 0.48-0.67
#   [PASS] Chroma floor           all 8 >= 0.1
#   [PASS] CVD separation         worst adjacent #af8e2a<->#2c7f44 dE 9.6 (protan) . tritan 7.0
#   [PASS] Normal-vision floor    worst adjacent #af8e2a<->#2c7f44 dE 17.6 (normal)
#   [PASS] Contrast vs surface    all 8 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #b3202f,#1f55b0,#22876b,#e3740f,#1d97b0,#6f3f84 --mode light --surface #ffffff --pairs adjacent
#   [PASS] Lightness band         all 6 inside L 0.43-0.77
#   [PASS] Chroma floor           all 6 >= 0.1
#   [PASS] CVD separation         worst adjacent #e3740f<->#22876b dE 9.7 (protan) . tritan 8.9
#   [PASS] Normal-vision floor    worst adjacent #22876b<->#1f55b0 dE 20.7 (normal)
#   [PASS] Contrast vs surface    all 6 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #cc323e,#1e78e1,#40a690,#d37812,#08a2af,#835cbe --mode dark --surface #212529 --pairs adjacent
#   [PASS] Lightness band         all 6 inside L 0.48-0.67
#   [PASS] Chroma floor           all 6 >= 0.1
#   [PASS] CVD separation         worst adjacent #835cbe<->#08a2af dE 12.1 (deutan) . tritan 7.0
#   [PASS] Normal-vision floor    worst adjacent #40a690<->#1e78e1 dE 20.5 (normal)
#   [PASS] Contrast vs surface    all 6 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #b3202f,#1f55b0,#e3740f,#1d97b0 --mode light --surface #ffffff --pairs all
#   [PASS] Lightness band         all 4 inside L 0.43-0.77
#   [PASS] Chroma floor           all 4 >= 0.1
#   [PASS] CVD separation         worst all-pairs #1d97b0<->#e3740f dE 18.2 (protan) . tritan 15.8
#   [PASS] Normal-vision floor    worst all-pairs #1d97b0<->#1f55b0 dE 19.1 (normal)
#   [PASS] Contrast vs surface    all 4 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #cc323e,#1e78e1,#d37812,#08a2af --mode dark --surface #212529 --pairs all
#   [PASS] Lightness band         all 4 inside L 0.48-0.67
#   [PASS] Chroma floor           all 4 >= 0.1
#   [PASS] CVD separation         worst all-pairs #d37812<->#cc323e dE 10.9 (deutan) . tritan 7.1
#   [PASS] Normal-vision floor    worst all-pairs #d37812<->#cc323e dE 15.3 (normal)
#   [PASS] Contrast vs surface    all 4 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #b3202f,#1f55b0,#22876b,#e3740f --mode light --surface #ffffff --pairs all
#   [PASS] Lightness band         all 4 inside L 0.43-0.77
#   [PASS] Chroma floor           all 4 >= 0.1
#   [PASS] CVD separation         worst all-pairs #22876b<->#b3202f dE 8.5 (deutan) . tritan 8.9
#   [PASS] Normal-vision floor    worst all-pairs #e3740f<->#b3202f dE 20.0 (normal)
#   [PASS] Contrast vs surface    all 4 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #cc323e,#1e78e1,#40a690,#d37812 --mode dark --surface #212529 --pairs all
#   [PASS] Lightness band         all 4 inside L 0.48-0.67
#   [PASS] Chroma floor           all 4 >= 0.1
#   [PASS] CVD separation         worst all-pairs #d37812<->#cc323e dE 10.9 (deutan) . tritan 7.0
#   [PASS] Normal-vision floor    worst all-pairs #d37812<->#cc323e dE 15.3 (normal)
#   [PASS] Contrast vs surface    all 4 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #2a78d6,#eb6834 --mode light --surface #ffffff --pairs all
#   [PASS] Lightness band         all 2 inside L 0.43-0.77
#   [PASS] Chroma floor           all 2 >= 0.1
#   [PASS] CVD separation         worst all-pairs #eb6834<->#2a78d6 dE 24.7 (protan) . tritan 32.7
#   [PASS] Normal-vision floor    worst all-pairs #eb6834<->#2a78d6 dE 33.6 (normal)
#   [PASS] Contrast vs surface    all 2 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #3987e5,#d95926 --mode dark --surface #212529 --pairs all
#   [PASS] Lightness band         all 2 inside L 0.48-0.67
#   [PASS] Chroma floor           all 2 >= 0.1
#   [PASS] CVD separation         worst all-pairs #d95926<->#3987e5 dE 26.8 (protan) . tritan 32.4
#   [PASS] Normal-vision floor    worst all-pairs #d95926<->#3987e5 dE 31.8 (normal)
#   [PASS] Contrast vs surface    all 2 >= 3:1
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #86b6ef,#5598e7,#2a78d6,#1c5cab,#0d366b --ordinal --mode light --surface #ffffff
#   [PASS] Lightness monotone     steps read light->dark
#   [PASS] Adjacent ΔL            all gaps >= 0.06
#   [PASS] Light-end contrast     #86b6ef at 2.11:1 vs surface
#   [PASS] Single hue             hue spread 4°
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #cde2fb,#9ec5f4,#6da7ec,#3987e5,#256abf --ordinal --mode dark --surface #212529
#   [PASS] Lightness monotone     steps read light->dark
#   [PASS] Adjacent ΔL            all gaps >= 0.06
#   [PASS] Light-end contrast     #256abf at 2.86:1 vs surface
#   [PASS] Single hue             hue spread 3°
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #86b6ef,#2a78d6,#104281 --ordinal --mode light --surface #ffffff
#   [PASS] Lightness monotone     steps read light->dark
#   [PASS] Adjacent ΔL            all gaps >= 0.06
#   [PASS] Light-end contrast     #86b6ef at 2.11:1 vs surface
#   [PASS] Single hue             hue spread 3°
#   -> ALL CHECKS PASS
#   (exit 0)
# $ node validate_palette.js #b7d3f6,#6da7ec,#256abf --ordinal --mode dark --surface #212529
#   [PASS] Lightness monotone     steps read light->dark
#   [PASS] Adjacent ΔL            all gaps >= 0.06
#   [PASS] Light-end contrast     #256abf at 2.86:1 vs surface
#   [PASS] Single hue             hue spread 2°
#   -> ALL CHECKS PASS
#   (exit 0)
#
# The ordinal ramps run light -> dark in both themes (the youngest or
# least is the lightest step, in light and in dark), so a group keeps its
# place when the reader switches themes. Adjacent OKLab Delta E (x100):
# light ord5 10.1 / 9.9 / 9.8 / 14.7, ord3 20.0 / 19.5; dark ord5 10.0 /
# 10.3 / 10.4 / 9.5, ord3 15.3 / 19.2 (target >= 8). No benchmark line is
# drawn in a ramp colour's lightness range: the official results are ink.
#
# Rules: a colour follows the party, never its rank, and never changes when
# a chart drops a party; in charts PVQ and ON fold into "Other" (grey) unless
# the chart is about them; the ADQ and the CAQ are never merged (they share
# the teal family, each with its own label). Where five or more parties would
# share one panel as dots, the pages use emphasis small multiples (one party
# in its hue per panel, the others grey) instead of more hues. The ordinal
# blue ramp (cohorts, age bands, interest) never shares a figure with the
# party palette. Text is always ink, never a series colour.

# ---- tokens -----------------------------------------------------------------

qz_tokens <- list(
  light = list(
    surface = "#ffffff", ink = "#1b1b1a", ink2 = "#52514e", muted = "#8a8984",
    grid = "#e8e7e1", axis = "#c3c2b7", wash = "#f0efec",
    party = c(PLQ = "#b3202f", PQ = "#1f55b0", ADQ = "#22876b", QS = "#e3740f",
              CAQ = "#1d97b0", PCQ = "#6f3f84", PVQ = "#3f6f22", ON = "#b88a3c",
              other = "#8a8984"),
    # ordinal blue, listed light -> dark in both modes (palette.md steps
    # 250 -> 700 here). The direction is fixed, never flipped between the
    # themes: the first step is the youngest / least, the last the oldest /
    # most, in light and in dark alike.
    ord5 = c("#86b6ef", "#5598e7", "#2a78d6", "#1c5cab", "#0d366b"),
    ord3 = c("#86b6ef", "#2a78d6", "#104281"),
    # sequential (heatmaps): steps 100 -> 700
    seq = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b"),
    yes = "#2a78d6", no = "#e34948",
    # two groups that are not parties (francophones, the others): slots 1 and 2
    # of the default categorical theme; never in a figure with party colours
    grp = c(fr = "#2a78d6", other = "#eb6834")
  ),
  dark = list(
    surface = "#212529", ink = "#f1f0ea", ink2 = "#c3c2b7", muted = "#9b9a93",
    grid = "#343a40", axis = "#495057", wash = "#383835",
    party = c(PLQ = "#cc323e", PQ = "#1e78e1", ADQ = "#40a690", QS = "#d37812",
              CAQ = "#08a2af", PCQ = "#835cbe", PVQ = "#2c7f44", ON = "#af8e2a",
              other = "#9b9a93"),
    # light -> dark as in the light theme (steps 100/150 -> 500), so a
    # group keeps its relative place when the reader switches themes
    ord5 = c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf"),
    ord3 = c("#b7d3f6", "#6da7ec", "#256abf"),
    seq = c("#0d366b", "#104281", "#184f95", "#256abf", "#3987e5", "#6da7ec", "#9ec5f4"),
    yes = "#3987e5", no = "#e66767",
    grp = c(fr = "#3987e5", other = "#d95926")
  )
)

# the party order of every legend and every stack (the validated order)
qz_parties <- c("PLQ", "PQ", "ADQ", "QS", "CAQ", "PCQ", "other")

qz_party_label <- function(p) {
  lab <- c(PLQ = "PLQ", PQ = "PQ", ADQ = "ADQ", QS = "QS", CAQ = "CAQ", PCQ = "PCQ",
           PVQ = tr("PV/PVQ", "PV/PVQ"), ON = "ON", other = tr("Other", "Autres"))
  unname(lab[as.character(p)])
}

# the harmonized party levels as the charts' party codes: PVQ, ON and
# "Other party" fold into "other"; no_party is not a party
qz_party_code <- function(x) {
  x <- as.character(x)
  out <- ifelse(x %in% c("PLQ", "PQ", "ADQ", "QS", "CAQ", "PCQ"), x,
                ifelse(x %in% c("PVQ", "ON", "Other party"), "other", NA_character_))
  out
}

# first installed of a metric-compatible family, so that label geometry is
# the same on macOS (Arial) and on the Ubuntu CI (Liberation Sans)
qz_font <- local({
  font <- NULL
  function() {
    if (is.null(font)) {
      have <- unique(systemfonts::system_fonts()$family)
      font <<- c(intersect(c("Arial", "Liberation Sans", "Helvetica"), have), "sans")[1]
    }
    font
  }
})

# ---- theme ------------------------------------------------------------------

theme_qes <- function(mode, base_size = 11, grid = c("y", "x", "none", "xy")) {
  grid <- match.arg(grid)
  k <- qz_tokens[[mode]]
  hair <- ggplot2::element_line(colour = k$grid, linewidth = 0.35)
  ggplot2::theme_minimal(base_size = base_size, base_family = qz_font()) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = k$ink),
      plot.background = ggplot2::element_rect(fill = k$surface, colour = NA),
      panel.background = ggplot2::element_rect(fill = k$surface, colour = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = if (grid %in% c("x", "xy")) hair else ggplot2::element_blank(),
      panel.grid.major.y = if (grid %in% c("y", "xy")) hair else ggplot2::element_blank(),
      axis.line.x = if (grid == "y") ggplot2::element_line(colour = k$axis, linewidth = 0.35) else ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = k$ink2, size = base_size * 0.85),
      axis.title = ggplot2::element_text(colour = k$ink2, size = base_size * 0.85),
      axis.title.y = ggplot2::element_text(angle = 90, margin = ggplot2::margin(r = 6)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 6)),
      strip.text = ggplot2::element_text(colour = k$ink, face = "bold", hjust = 0,
                                         size = base_size * 0.95, margin = ggplot2::margin(b = 4)),
      legend.position = "top", legend.justification = "left",
      # several legends (series, then the weighting key) stack in rows
      legend.box = "vertical", legend.box.just = "left", legend.spacing.y = ggplot2::unit(2, "pt"),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(colour = k$ink2, size = base_size * 0.85),
      legend.key.width = ggplot2::unit(16, "pt"), legend.key.height = ggplot2::unit(12, "pt"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.box.spacing = ggplot2::unit(4, "pt"),
      legend.spacing.x = ggplot2::unit(4, "pt"),
      panel.spacing = ggplot2::unit(14, "pt"),
      plot.margin = ggplot2::margin(6, 10, 6, 6),
      plot.caption = ggplot2::element_text(colour = k$ink2, hjust = 0, size = base_size * 0.8),
      plot.caption.position = "plot"
    )
}

scale_party <- function(mode, aesthetics = c("colour", "fill"), guide = "none", ...) {
  v <- qz_tokens[[mode]]$party
  ggplot2::scale_discrete_manual(aesthetics = aesthetics, values = v, breaks = qz_parties,
                                 labels = qz_party_label(qz_parties), guide = guide, ...)
}

# ---- marks ------------------------------------------------------------------
# ggplot units at 192 dpi, shown at half size: linewidth 0.7 is about 2 CSS px,
# point size 2.9 about 8 px, stroke 0.8 a 2 px surface ring.

qz_line <- function(...) ggplot2::geom_line(linewidth = 0.7, lineend = "round", linejoin = "round", ...)
qz_ci <- function(...) ggplot2::geom_linerange(linewidth = 0.45, alpha = 0.6, ...)
qz_cih <- function(...) ggplot2::geom_linerange(linewidth = 0.45, alpha = 0.6, orientation = "y", ...)
# a confidence band: a 12% wash in light mode, 24% in dark mode, where a
# thin wash of a mid-lightness hue barely shows on the dark surface
qz_band <- function(..., mode = "light") {
  ggplot2::geom_ribbon(alpha = if (identical(mode, "dark")) 0.24 else 0.12, colour = NA,
                       show.legend = FALSE, ...)
}

# Dots: filled with the series colour and ringed with the surface where the
# estimate is weighted; hollow (surface fill, series-colour ring) where the
# study's weight is still under review, so the estimate is unweighted. `d`
# must have a logical column `weighted`. Pass the colour as a mapping
# (`colour_aes`, the name of a column) or as one fixed colour (`fixed`).
#
# Legends: the filled layer feeds the fill legend (so a series key is a
# filled dot, the normal mark); the hollow layer feeds no series legend,
# only the "weighting" key of scale_weight_key(), mapped through `shape`.
# qz_dots() adds that scale itself (a later qz_dots() in the same plot
# replaces it, so draw the benchmark dots first): `key` shows the hollow
# key (by default when `d` has an unweighted row), `both` also keys the
# filled dot.
qz_dots <- function(mode, d, mapping, colour_aes = NULL, fixed = NULL, size = 2.9,
                    shape = 21, position = "identity", key = NULL, both = FALSE, wrap = FALSE) {
  if (is.null(key)) key <- any(!d$weighted %in% TRUE)
  k <- qz_tokens[[mode]]
  w <- d[d$weighted %in% TRUE, , drop = FALSE]
  u <- d[!d$weighted %in% TRUE, , drop = FALSE]
  # the filled dots have a fixed shape (so the series legend keys them as
  # filled dots); the hollow ones map it, for the weighting key
  u_map <- ggplot2::aes(shape = "unweighted")
  w_map <- ggplot2::aes(shape = "weighted")
  if (!is.null(colour_aes)) {
    filled <- ggplot2::geom_point(data = w, mapping = utils::modifyList(mapping, ggplot2::aes(fill = .data[[colour_aes]])),
                                  shape = shape, size = size, stroke = 0.8, colour = k$surface, position = position,
                                  inherit.aes = FALSE)
    hollow <- ggplot2::geom_point(data = u, mapping = utils::modifyList(utils::modifyList(mapping, ggplot2::aes(colour = .data[[colour_aes]])), u_map),
                                  size = size * 0.92, stroke = 1.1, fill = k$surface, position = position,
                                  inherit.aes = FALSE, show.legend = c(colour = FALSE, shape = TRUE))
  } else {
    filled <- if (both) {
      ggplot2::geom_point(data = w, mapping = utils::modifyList(mapping, w_map), size = size, stroke = 0.8,
                          colour = k$surface, fill = fixed, position = position, inherit.aes = FALSE)
    } else {
      ggplot2::geom_point(data = w, mapping = mapping, shape = shape, size = size, stroke = 0.8,
                          colour = k$surface, fill = fixed, position = position, inherit.aes = FALSE)
    }
    hollow <- ggplot2::geom_point(data = u, mapping = utils::modifyList(mapping, u_map), size = size * 0.92, stroke = 1.1,
                                  colour = fixed, fill = k$surface, position = position, inherit.aes = FALSE,
                                  show.legend = c(shape = TRUE))
  }
  list(filled, hollow, scale_weight_key(mode, both = both, shape = shape, show = key || both, wrap = wrap))
}

# The key of the filled / hollow encoding, drawn in neutral ink so that it
# never reads as a series. `both = FALSE` (the default) keys only the
# hollow dot: the filled dot is the normal mark, keyed by the series
# legend. The shapes are those of qz_dots() (21, a ringed circle).
scale_weight_key <- function(mode, both = FALSE, shape = 21, order = 99, show = TRUE, wrap = FALSE) {
  k <- qz_tokens[[mode]]
  if (!show) {
    return(ggplot2::scale_shape_manual(values = c(weighted = shape, unweighted = shape), guide = "none"))
  }
  lab <- c(weighted = tr("filled: weighted", "plein : pondéré"),
           unweighted = tr("hollow: unweighted (weight under review)", "creux : non pondéré (pondération en révision)"))
  if (wrap) lab <- sub(" (", "\n(", lab, fixed = TRUE)
  br <- if (both) c("weighted", "unweighted") else "unweighted"
  ggplot2::scale_shape_manual(
    values = c(weighted = shape, unweighted = shape), limits = c("weighted", "unweighted"),
    breaks = br, labels = lab[br],
    guide = ggplot2::guide_legend(order = order, override.aes = list(
      fill = c(weighted = k$ink2, unweighted = k$surface)[br],
      colour = c(weighted = k$surface, unweighted = k$ink2)[br],
      stroke = c(weighted = 0.8, unweighted = 1.1)[br], size = 2.8, alpha = 1, linetype = 0)))
}

qz_tile <- function(mode, ...) ggplot2::geom_tile(colour = qz_tokens[[mode]]$surface, linewidth = 0.7, ...)

# White or ink text for a label set inside a coloured fill: whichever has
# the higher contrast with the fill (WCAG relative luminance).
qz_label_ink <- function(fill, mode) {
  lum <- function(col) {
    rgb <- grDevices::col2rgb(col) / 255
    lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
    0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  }
  l <- lum(fill)
  ink <- "#1b1b1a"
  white <- (1.05) / (l + 0.05)
  dark <- (l + 0.05) / (lum(ink) + 0.05)
  ifelse(dark > white, ink, "#ffffff")
}

# End labels: one label per series at its last x, spread apart vertically
# (min_gap in y units) with a muted leader line from the line end; always in
# ink, never in the series colour.
qz_end_labels <- function(d, x, y, label, group, mode, min_gap, nudge_x, size = 3.3,
                          at = c("last", "first")) {
  at <- match.arg(at)
  k <- qz_tokens[[mode]]
  pick <- if (at == "last") which.max else which.min
  ends <- do.call(rbind, lapply(split(d, d[[group]], drop = TRUE), function(g) g[pick(g[[x]]), , drop = FALSE]))
  ends <- ends[order(ends[[y]]), , drop = FALSE]
  pos <- ends[[y]]
  if (length(pos) > 1L) {
    for (i in seq_along(pos)[-1]) pos[i] <- max(pos[i], pos[i - 1] + min_gap)
    # centre the spread block on the data, so labels do not all drift up
    shift <- mean(pos - ends[[y]])
    pos <- pos - min(shift, max(pos - ends[[y]]))
    for (i in seq_along(pos)[-1]) pos[i] <- max(pos[i], pos[i - 1] + min_gap)
  }
  ends$.ly <- pos
  sgn <- if (at == "last") 1 else -1
  ends$.x0 <- ends[[x]] + sgn * nudge_x * 0.15
  ends$.x1 <- ends[[x]] + sgn * nudge_x * 0.6
  ends$.xt <- ends[[x]] + sgn * nudge_x * 0.7
  list(
    ggplot2::geom_segment(data = ends, ggplot2::aes(x = .x0, xend = .x1, y = .data[[y]], yend = .ly),
                          colour = k$muted, linewidth = 0.3, inherit.aes = FALSE),
    ggplot2::geom_text(data = ends, ggplot2::aes(x = .xt, y = .ly, label = .data[[label]]),
                       hjust = if (at == "last") 0 else 1, size = size, colour = k$ink,
                       family = qz_font(), inherit.aes = FALSE)
  )
}

# Elections on a real year scale. From 2007 on, x is the year; 1998 sits
# at 2003, across a visible axis break (qz_xbreak()), so the nine years
# from 1998 to 2007 do not take a third of the width. Lines never cross the
# break: group them with qz_era(). On the full-width axis 2008 is labelled
# '08, next to 2007; `short` labels ('98, '07, '12, '18, '22) are for small
# multiples, where 2008 and 2014 keep their tick without a label.
qz_elections <- c("1998", "2007", "2008", "2012", "2014", "2018", "2022")
qz_ex <- function(year) {
  y <- as.numeric(as.character(year))
  ifelse(y < 2005, y + 5, y)
}
qz_era <- function(year) ifelse(as.numeric(as.character(year)) < 2005, "before", "after")
scale_x_election <- function(short = FALSE, add = c(0.7, 0.7), years = qz_elections) {
  years <- as.character(years)
  lab <- if (short) {
    ifelse(years %in% c("2008", "2014"), "", paste0("\u2019", substr(years, 3, 4)))
  } else {
    ifelse(years == "2008" & "2007" %in% years, "\u201908", years)
  }
  ggplot2::scale_x_continuous(breaks = qz_ex(years), labels = lab,
                              limits = range(qz_ex(years)) + c(-add[1], add[2]),
                              expand = ggplot2::expansion(0))
}

# The axis break between 1998 and 2007: a gap in the baseline with two
# slanted strokes, drawn in every panel. Use it with theme(axis.line.x =
# element_blank()) (qz_xbreak() draws the baseline itself) and
# coord_cartesian(clip = "off").
qz_xbreak <- function(mode, at = 2005, ymin = -Inf) {
  k <- qz_tokens[[mode]]
  slash <- grid::segmentsGrob(x0 = grid::unit(0.5, "npc") + grid::unit(c(-4.5, -0.5), "pt"),
                              x1 = grid::unit(0.5, "npc") + grid::unit(c(0.5, 4.5), "pt"),
                              y0 = grid::unit(-4, "pt"), y1 = grid::unit(4, "pt"),
                              gp = grid::gpar(col = k$ink2, lwd = 1.3, lineend = "round"))
  list(
    ggplot2::annotate("segment", x = -Inf, xend = at - 0.45, y = ymin, yend = ymin, colour = k$axis, linewidth = 0.35),
    ggplot2::annotate("segment", x = at + 0.45, xend = Inf, y = ymin, yend = ymin, colour = k$axis, linewidth = 0.35),
    ggplot2::annotation_custom(slash, xmin = at - 1, xmax = at + 1, ymin = ymin, ymax = ymin)
  )
}

qz_pct_axis <- function(x) paste0(qz_num(x, 0), tr("%", " %"))

# ---- the figure emitter ------------------------------------------------------

qz_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\"", "&quot;", x, fixed = TRUE)
}

# The only way a page emits a chart. `plot_fun(mode)` returns a ggplot; it
# is drawn in the light and the dark mode as 2x PNGs in knitr's figure
# folder. The title is an HTML <figcaption> (crisp, searchable, translated):
# `title` states the finding, `subtitle` says what the chart shows;
# `alt` is the image's alt text, `note` the source, weights and intervals
# line, and `table` (a data frame, already formatted) the collapsible table
# view with the exact values behind the figure.
qz_figure <- function(plot_fun, id, title, alt, table, note = NULL, subtitle = NULL, width = 7, height = 4.2) {
  dir <- knitr::opts_current$get("fig.path")
  if (is.null(dir) || !nzchar(dir)) dir <- "figure/"
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  # the page refers to the images relative to itself: "<page>_files/figure-html/"
  rel <- file.path(basename(dirname(file.path(dir, "x"))), "")
  parent <- basename(dirname(dirname(file.path(dir, "x"))))
  if (nzchar(parent) && parent != ".") rel <- file.path(parent, rel)
  src <- character()
  for (mode in c("light", "dark")) {
    file <- paste0(id, "-", mode, ".png")
    ragg::agg_png(file.path(dir, file), width = width, height = height, units = "in", res = 192,
                  background = qz_tokens[[mode]]$surface)
    print(plot_fun(mode))
    grDevices::dev.off()
    src[mode] <- paste0(rel, file)
  }
  px <- round(width * 96)
  tab <- suppressWarnings(knitr::kable(table, format = "html", row.names = FALSE, escape = TRUE,
                                       table.attr = "class=\"table qesr-tv\""))
  html <- paste0(
    "<figure class=\"qesr-fig\" id=\"fig-", id, "\">",
    "<figcaption><span class=\"qesr-title\">", qz_esc(title), "</span>",
    if (!is.null(subtitle)) paste0("<span class=\"qesr-sub\">", qz_esc(subtitle), "</span>") else "",
    "</figcaption>",
    "<img class=\"qesr-img qesr-light\" src=\"", src["light"], "\" alt=\"", qz_esc(alt),
    "\" width=\"", px, "\" height=\"", round(height * 96), "\" loading=\"lazy\">",
    "<img class=\"qesr-img qesr-dark\" src=\"", src["dark"], "\" alt=\"", qz_esc(alt),
    "\" width=\"", px, "\" height=\"", round(height * 96), "\" loading=\"lazy\">",
    if (!is.null(note)) paste0("<p class=\"qesr-note\">", qz_esc(note), "</p>") else "",
    "<details class=\"qesr-table\"><summary>", tr("Table view", "Vue en tableau"), "</summary>",
    paste(tab, collapse = "\n"),
    "</details></figure>"
  )
  knitr::asis_output(paste0("\n\n```{=html}\n", html, "\n```\n\n"))
}
