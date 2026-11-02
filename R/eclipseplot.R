#' Create a plot for a risk of bias assessment with the ROBUST-RCT tool
#'
#' @description
#' An independent visual representation of risk-of-bias assessments of randomized controlled trials <doi:10.31222/osf.io/58yda_v1>, based on the ROBUST-RCT framework <doi:10.1136/bmj-2024-081199>.
#' It can display either the standard two-step assessment or a simplified judgment-only version.
#'
#'
#' @param robust_data A data frame containing the data from the risk-of-bias assessment with the ROBUST-RCT tool.
#' @param standard Logical. If \code{TRUE} (default), displays the two-step eclipseplot. If \code{FALSE}, displays only the step 2 (judgment).
#' @param optionals Logical. If \code{TRUE}, includes the optional items (columns \code{opt1}, \code{opt2}, ...; any number).
#' @param title Character. Custom title for the eclipse plot.
#' @param plot Character. Parts to display: \code{"full"} (default), \code{"eclipses"}, or \code{"legend"}.
#' @param design Character. Orientation of the assembly: \code{"horizontal"} (default) or \code{"vertical"}.
#' @param proportions Numeric vector of length 2 (or a single number between 0 and 1). Relative widths (horizontal) or heights (vertical) for plot and legend. By default they are computed from the number of studies.
#' @param opt_prefix Character. Text for optional items in the plot. Default is "o".
#' @param opt_labels Names of the optional items shown in the legend, for this plot only. Either a vector in the order of the optional items (\code{c("Selective reporting", "Baseline imbalance")}), or a named vector naming the column or the plot label (\code{c(opt3 = "Selective reporting")} or \code{c(o1 = "Selective reporting")}). Items not named keep the default. Default \code{NULL}: "Optional item 1", "Optional item 2", ...
#'
#' @return A \code{ggplot} object (specifically a \code{ggdraw} object from the \code{cowplot} package)
#' representing the visualization of the risk of bias. The output can be further customized
#' using standard \code{ggplot2} functions or saved via \code{save_eclipseplot}.
#' The recommended size in inches is stored in \code{attr(x, "eclipse_size")}
#' and is used by \code{save_eclipseplot} when no size is given.
#'
#' @details
#' The input data frame must include the required columns for the ROBUST-RCT domains.
#' The following columns are expected:
#' \itemize{
#'   \item \strong{pmid:} PubMed ID or unique identifier.
#'   \item \strong{study:} Study name (e.g., "Author, 2026").
#'   \item \strong{Core Items (1-6):} Columns named 'itemX_step1' and 'itemX_step2' (where X is 1 to 6). As standard in the ROBUST-RCT tool, item 6 does not have a multiple-choice step 1 evaluation, so it is automatically marked as "Not applicable".
#'   \item \strong{Optional Items:} If optionals = TRUE, columns named 'optX' (X = 1, 2, 3, ...). Legacy formats ('optX_step1' and 'optX_step2') are supported, but only Step 2 will be plotted. In the plot, the optional items are numbered in sequence (o1, o2, ...) in the order of their column numbers, whatever those numbers are: a file with only 'opt3' and 'opt7' shows o1 and o2. Name them with \code{opt_labels}.
#' }
#'
#' \strong{Allowed values for Step 1 (Evaluation):} "Definitely Yes", "Probably Yes", "Probably No", "Definitely No", "Not applicable".
#'
#' \strong{Allowed values for Step 2 (Judgment):} "Definitely Low", "Probably Low", "Probably High", "Definitely High", "Not applicable".
#'
#' Values are matched without regard to case, extra spaces or hyphens, and the
#' abbreviations listed in \code{\link{eclipsedf}} are accepted. Empty cells and
#' unrecognised values are drawn as \strong{"Missing"} (white with a dashed
#' outline) and reported in a warning; they are never shown as "Not applicable".
#' Missing core columns are also drawn as "Missing". Studies are placed in the
#' order of the rows, so two studies with the same label are both shown.
#' Use \code{\link{check_eclipse_data}} to list all problems before plotting.
#'
#' @section Dataset Structure:
#' Your CSV should look like this (Top 2 rows):
#' \preformatted{
#' "pmid","study","item1_step1","item1_step2","item2_step1"..."item6_step2"
#' "40001","Albert, 2024","Definitely Yes","Definitely Low",..."Probably Low"
#' }
#'
#'
#' \strong{Color Mapping:}
#' \itemize{
#'   \item Definitely Low / Definitely Yes: Blue (#4C72B0)
#'   \item Probably Low / Probably Yes: Light Blue (#c3d0e4)
#'   \item Probably High / Probably No: Light Red (#f5c7c7)
#'   \item Definitely High / Definitely No: Red (#E15759)
#'   \item Not applicable: Grey (#D3D3D3)
#'   \item Missing / unrecognised: White with a dashed grey outline
#' }
#'
#'
#' @examples
#' # 1. Standard plot (Core items, Steps 1 & 2)
#' data(sample_brief)
#' eclipseplot_plot <- eclipseplot(sample_brief)
#' print(eclipseplot_plot)
#'
#' \donttest{
#' # 2. Judgment only (Core items, Step 2)
#' eclipseplot(sample_long, standard = FALSE)
#'
#' # 3. Plot with optional items (Core and optional items, Steps 1 & 2)
#' eclipseplot(sample_long, optionals = TRUE,
#'             opt_labels = c("Selective reporting", "Baseline imbalance", "Other bias"))
#'
#' # 3b. Naming only one optional item (by column or plot label); the others keep the default
#' eclipseplot(sample_long, optionals = TRUE, opt_labels = c(opt2 = "Baseline imbalance"))
#'
#' # 4. Judgment only with optional items (Core and optional items, Step 2)
#' eclipseplot(sample_long, standard = FALSE, optionals = TRUE)
#'
#' # 5. Vertical full-page plot
#' eclipseplot(sample_long, design = "vertical")
#'
#' # 6. Plotting only the dots
#' eclipseplot(sample_brief, plot = "eclipses")
#'
#' # 7. Plotting only the legend
#' eclipseplot(sample_brief, plot = "legend")
#' }
#'
#' @author Pedro Rodrigues Vidor \email{pedro.vidor@@ufrgs.br}
#' @author Yohan Casiraghi (Contributor)
#' @author Sofia Simoni Rossi Fermo (Contributor)
#' @author Adolfo Moraes de Souza (Contributor)
#' @author Maicon Falavigna (Contributor)
#' @author Patricia Klarmann Ziegelmann (Contributor)
#'
#'
#' @export
#' @importFrom ggplot2 ggplot aes scale_fill_manual scale_colour_manual scale_linetype_manual scale_x_continuous scale_y_continuous coord_fixed theme_minimal theme element_blank element_text element_rect margin ggsave
#' @importFrom dplyr filter bind_rows arrange
#' @importFrom ggforce geom_circle
#' @importFrom cowplot ggdraw draw_text draw_plot draw_grob plot_grid
#' @importFrom tidyr pivot_longer
#' @importFrom readxl read_excel


eclipseplot <- function(robust_data, standard = TRUE, optionals = FALSE,
                        title = NULL, plot = "full", design = "horizontal",
                        proportions = NULL, opt_prefix = "o", opt_labels = NULL) {

  # Null global variables to start
  study <- full_item <- item <- step <- value <- judgment_group <- x_coord <- y_coord <- is_optional <- row_id <- NULL

  # [0.9.8 M4] Validate 'plot' and 'design' (case-insensitive). Before, "Horizontal"
  # silently fell into the vertical branch and an unknown 'plot' returned the full plot.
  plot   <- match.arg(tolower(plot), c("full", "eclipses", "legend"))
  design <- match.arg(tolower(design), c("horizontal", "vertical"))
  if (!is.data.frame(robust_data)) stop("'robust_data' must be a data frame.", call. = FALSE)
  robust_data <- as.data.frame(robust_data, stringsAsFactors = FALSE)

  # [0.9.8 C2/C5/I2/I6] Check the data first. Errors stop with a clear message;
  # empty/unrecognised values, missing columns and duplicated labels give a warning.
  colnames(robust_data) <- .normalise_names(colnames(robust_data))
  issues <- check_eclipse_data(robust_data, standard = standard, optionals = optionals)
  if (any(issues$severity == "error")) {
    stop(paste(issues$message[issues$severity == "error"], collapse = "\n"), call. = FALSE)
  }
  if (any(issues$severity == "warning")) {
    warning(paste(c("Problems found in the data:",
                    .summarise_issues(issues[issues$severity == "warning", ])), collapse = "\n  "),
            call. = FALSE)
  }
  if (any(issues$severity == "info" & grepl("^Item [12]", issues$message))) {
    message("Items 1 and 2: ", sum(grepl("^Item [12]", issues$message)),
            " Step 2 rating(s) are not the equivalent of Step 1. See check_eclipse_data().")
  }

  # --- 1. Parametric Definitions ---
  r_value <- 0.25
  y_separation <- 3 * r_value
  judgment_levels_ordered <- c("Definitely Low", "Probably Low", "Probably High", "Definitely High", "Not applicable")

  color_palette_ordered <- c(
    "Definitely Low"  = "#4C72B0",
    "Probably Low"    = "#c3d0e4",
    "Probably High"   = "#f5c7c7",
    "Definitely High" = "#E15759",
    "Not applicable"  = "#D3D3D3",
    "Missing"         = "#FFFFFF"   # [0.9.8 C2] new category, distinct from "Not applicable"
  )
  missing_outline <- "#6b6b6b"

  # --- 2. Data Cleaning ---
  # [0.9.8 C4] Studies are positioned by row, not by label: duplicated labels no longer merge.
  n_studies <- nrow(robust_data)
  robust_data$row_id <- seq_len(n_studies)
  robust_data$study  <- .study_labels(robust_data)
  study_labels <- .wrap_labels(robust_data$study)

  # [0.9.8 I6] Missing core columns are added as empty, so they are drawn as "Missing"
  # instead of disappearing from the axis.
  needed <- if (standard) .core_cols else grep("_step2$", .core_cols, value = TRUE)
  for (col in setdiff(needed, colnames(robust_data))) robust_data[[col]] <- NA_character_

  # --- 3. Internal Transformation Logic ---
  # [0.9.8 C2] map_judg() replaced by .judgment_group() (R/input.R): case-insensitive,
  # accepts abbreviations, and empty/unrecognised values become "Missing" (not "Not applicable").
  core_present <- intersect(.core_cols, colnames(robust_data))
  long_core <- robust_data[, c("row_id", "study", core_present), drop = FALSE]
  long_core[core_present] <- lapply(long_core[core_present], as.character)
  long_core <- long_core |>
    tidyr::pivot_longer(cols = dplyr::all_of(core_present), names_to = "full_item", values_to = "value")
  long_core$item <- sub("^item([1-6])_step[12]$", "\\1", long_core$full_item)
  long_core$step <- as.numeric(sub("^item[1-6]_step([12])$", "\\1", long_core$full_item))
  long_core$is_optional <- FALSE

  item6_step1 <- data.frame(row_id = robust_data$row_id, study = robust_data$study,
                            item = "6", step = 1, value = "Not applicable",
                            is_optional = FALSE, stringsAsFactors = FALSE)

  long_final <- dplyr::bind_rows(long_core, item6_step1)

  opt_items <- character(0)
  if (optionals) {
    # [0.9.8 I7] Any number of optional items (opt1 ... opt10, opt11, ...); before only opt1-opt9.
    opt_cols <- .opt_columns(robust_data)

    if (length(opt_cols) == 0) {
      warning("optionals = TRUE, but no optional columns (opt1, opt2, ...) were found.", call. = FALSE)
    } else {
      # [0.9.9] Optional items are numbered in sequence (o1, o2, ...) in column order,
      # whatever the column numbers are: a reviewer who uses only opt3 sees "o1", not "o3".
      opt_num <- seq_along(opt_cols)
      opt_items <- paste0(opt_prefix, opt_num)
      long_opts <- robust_data[, c("row_id", "study", opt_cols), drop = FALSE]
      long_opts[opt_cols] <- lapply(long_opts[opt_cols], as.character)
      long_opts <- long_opts |>
        tidyr::pivot_longer(cols = dplyr::all_of(opt_cols), names_to = "full_item", values_to = "value")
      long_opts$item <- opt_items[match(long_opts$full_item, opt_cols)]
      long_opts$step <- 2          # Treated as judgment because there is no steps for optional items
      long_opts$is_optional <- TRUE
      long_final <- dplyr::bind_rows(long_final, long_opts)
    }
  }

  if (!standard) {
    long_final <- dplyr::filter(long_final, step == 2)
  }

  # [0.9.8 C5] Fixed, ordered item levels used for BOTH positions and axis labels.
  # Before, positions were computed before arrange(step) and labels after it, so a
  # missing item1_step1 produced the axis 2,3,4,5,6,1 with every column mislabelled.
  item_levels <- c(as.character(1:6), opt_items)

  plot_df <- long_final
  plot_df$judgment_group <- .judgment_group(plot_df$value)
  plot_df$y_coord <- (n_studies - plot_df$row_id + 1) * y_separation
  plot_df$x_coord <- match(plot_df$item, item_levels)
  plot_df$is_missing <- plot_df$judgment_group == "Missing"

  # --- 5. Main Plot Object ---

  # Garante que o Step 2 seja desenhado por cima do Step 1 (Efeito eclipse perfeito)
  plot_df <- plot_df |> dplyr::arrange(step)

  g_main <- ggplot2::ggplot(plot_df, ggplot2::aes(fill = judgment_group))

  # [0.9.8 C2] "Missing" circles get a dashed outline; the others keep their original look.
  circle_layer <- function(data, x_shift, outline_others, lw) {
    if (nrow(data) == 0) return(NULL)
    list(
      ggforce::geom_circle(
        data = data[!data$is_missing, , drop = FALSE],
        ggplot2::aes(x0 = x_coord + x_shift(step), y0 = y_coord, r = r_value),
        color = outline_others, linewidth = lw),
      ggforce::geom_circle(
        data = data[data$is_missing, , drop = FALSE],
        ggplot2::aes(x0 = x_coord + x_shift(step), y0 = y_coord, r = r_value),
        color = missing_outline, linewidth = 0.35, linetype = "22")
    )
  }

  if (standard) {
    # Camada 1: Itens principais (meia-luas)
    core_df <- plot_df[!plot_df$is_optional, , drop = FALSE]
    # Draw step by step so that step 2 always covers step 1
    for (s in c(1, 2)) {
      g_main <- g_main + circle_layer(core_df[core_df$step == s, , drop = FALSE],
                                      function(step) ifelse(step == 1, -(r_value/2), (r_value/2)),
                                      NA, 0)
    }
    # Camada 2: Itens opcionais (círculo inteiro, centralizado, com borda)
    if (optionals) {
      g_main <- g_main + circle_layer(plot_df[plot_df$is_optional, , drop = FALSE],
                                      function(step) 0, "white", 0.2)
    }
  } else {
    # Modo judgment-only: Tudo é centralizado
    g_main <- g_main + circle_layer(plot_df, function(step) 0, "white", 0.2)
  }

  g_main <- g_main +
    ggplot2::scale_fill_manual(values = color_palette_ordered, drop = FALSE) +
    ggplot2::scale_x_continuous(name = NULL, breaks = seq_along(item_levels), labels = item_levels,
                                limits = c(0.5, length(item_levels) + 0.5), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "", breaks = (n_studies:1) * y_separation, labels = study_labels,
                                limits = c(y_separation - 0.5, n_studies * y_separation + 0.5), expand = c(0, 0)) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text = ggplot2::element_text(face = "bold"),
                   legend.position = "none") # Hides the standard R legend

  # --- 6. Legend Construction ---
  # [0.9.8 M3] The legend is now built with absolute text units (grid), so the bullets stay
  # next to their labels at any output size. Before, fixed npc coordinates
  # (draw_text(x = 0.05, y = 0.85 ...)) spread the legend over the whole canvas.
  # [0.9.8 I7] Optional items are listed in the legend with their names.
  # [0.9.8 C2] "Missing" appears in the legend when the data contain missing values.

  has_missing <- any(plot_df$is_missing)
  opt_text <- character(0)
  if (length(opt_items) > 0) {
    opt_names <- .resolve_opt_labels(opt_labels, opt_cols, opt_items)
    opt_text <- paste0(opt_items, ": ", opt_names)
  }

  core_items_lines <- c("1: Random sequence generation", "2: Allocation concealment", "3: Blinding of participants", "4: Blinding of providers", "5: Blinding of assessors", "6: Outcome data missing")

  swatches <- lapply(judgment_levels_ordered, function(l) {
    # [0.9.9] Back to the original text "Not applicable" (the 0.9.8 note "(incl. item 6, step 1)"
    # made the legend too wide)
    list(type = "swatch", fill = color_palette_ordered[[l]], col = NA, lty = 1, text = l)
  })
  if (has_missing) {
    swatches[[length(swatches) + 1]] <- list(type = "swatch", fill = "#FFFFFF", col = missing_outline,
                                             lty = 2, text = "Missing / not recognised")
  }

  block_steps <- if (standard) list(
    list(type = "title", text = "Two Steps for Assessing Risk of Bias"),
    list(type = "text", text = "Left: Evaluation (Step 1)"),
    list(type = "text", text = "Right: Judgment (Step 2)")) else list()
  block_items <- c(list(list(type = "title", text = "Core Items")),
                   lapply(core_items_lines, function(t) list(type = "text", text = t)))
  if (length(opt_text) > 0) {
    block_items <- c(block_items, list(list(type = "gap")),
                     list(list(type = "title", text = "Optional Items")),
                     lapply(opt_text, function(t) list(type = "text", text = t)))
  }
  block_colours <- c(list(list(type = "title", text = "Ratings")), swatches)

  # Classic vertical legend for horizontal design
  classic_entries <- c(block_steps, if (standard) list(list(type = "gap")), block_items,
                       list(list(type = "gap")), block_colours)
  g_legend_classic <- cowplot::ggdraw() + cowplot::draw_grob(.legend_grob(classic_entries, size = 9))

  # Modular blocks for vertical design (horizontal footer)
  g_leg_a <- cowplot::ggdraw() + cowplot::draw_grob(.legend_grob(block_steps, size = 8))
  g_leg_b <- cowplot::ggdraw() + cowplot::draw_grob(.legend_grob(block_items, size = 8))
  g_leg_c <- cowplot::ggdraw() + cowplot::draw_grob(.legend_grob(block_colours, size = 8))

  # --- 7. Final Assembly ---
  # [0.9.8 I3/I4] Recommended output size computed from the number of studies and items.
  dims <- .eclipse_dims(n_studies, length(item_levels), study_labels, design,
                        classic_lines = .legend_lines(classic_entries),
                        footer_lines = max(.legend_lines(block_steps), .legend_lines(block_items),
                                           .legend_lines(block_colours)))

  # The following line corrects a small issue where parts of the graphic were transparent
  bg_white <- ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", color = NA))

  set_size <- function(p, size) { attr(p, "eclipse_size") <- size; p }

  if (plot == "eclipses") return(set_size(g_main, dims$main))
  if (plot == "legend") {
    if (design == "vertical") {
      # If the whole plot is in vertical, returns the legend in 3 columns
      # (which makes the legend more horizontal, ideal for being used with a vertical figure)
      legend_row <- cowplot::plot_grid(g_leg_a, g_leg_b, g_leg_c, ncol = 3, rel_widths = c(0.3, 0.35, 0.35))
      return(set_size(legend_row + bg_white, dims$legend))
    } else {
      # If horizontal, returns the classic version (3 rows of content, as standard)
      return(set_size(g_legend_classic + bg_white, dims$legend))
    }
  }

  if (is.null(title)) {
    title <- ifelse(standard, "Risk of Bias (ROBUST-RCT)", "Risk of Bias (ROBUST-RCT) - Judgment Only")
  }

  plot_title <- cowplot::ggdraw() + cowplot::draw_text(title, x = 0.5, y = 0.5, size = 14, fontface = "bold")

  # [0.9.8] A single number is accepted as the share of the plot (e.g. 0.75)
  if (!is.null(proportions)) {
    if (length(proportions) == 1) proportions <- c(proportions, 1 - proportions)
    if (length(proportions) != 2 || any(proportions <= 0)) stop("'proportions' must be two positive numbers.", call. = FALSE)
  }

  if (design == "horizontal") {
    if (is.null(proportions)) proportions <- dims$proportions
    final_panel <- cowplot::plot_grid(
      g_main + ggplot2::theme(plot.margin = ggplot2::margin(t = 10, r = 0, b = 10, l = 10)),
      g_legend_classic,
      ncol = 2, rel_widths = proportions, align = 'h', axis = 't'
    )
  } else {
    if (is.null(proportions)) proportions <- dims$proportions
    legend_row <- cowplot::plot_grid(g_leg_a, g_leg_b, g_leg_c, ncol = 3, rel_widths = c(0.3, 0.35, 0.35))
    final_panel <- cowplot::plot_grid(g_main, legend_row, ncol = 1, rel_heights = proportions)
  }

  # [0.9.8 I4] Title height in absolute size (0.6 in) instead of 8% of the figure,
  # which wasted space on tall figures.
  title_share <- dims$title_in / dims$full[["height"]]
  final_plot <- cowplot::plot_grid(plot_title, final_panel, ncol = 1, rel_heights = c(title_share, 1 - title_share))
  return(set_size(final_plot + bg_white, dims$full))
}


# ---------------------------------------------------------------------------
# Internal helpers for eclipseplot() (new in 0.9.8)
# ---------------------------------------------------------------------------

# Lower-case the standard column names so "Item1_Step1" or "PMID" are accepted
.normalise_names <- function(nm) {
  low <- tolower(trimws(nm))
  std <- low %in% c("pmid", "study", .core_cols, "item6_step1") | grepl(.opt_pattern, low)
  nm[std] <- low[std]
  nm
}

# Long study labels are wrapped on two lines instead of shrinking the plot
.wrap_labels <- function(x, width = 40) {
  vapply(x, function(s) {
    if (nchar(s) <= width) return(s)
    w <- strwrap(s, width = width)
    if (length(w) > 2) w <- c(w[1], paste0(substr(paste(w[-1], collapse = " "), 1, width - 3), "..."))
    paste(w, collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
}

.resolve_opt_labels <- function(opt_labels, opt_cols, opt_items) {
  # [0.9.9] Default "Optional item 1", "Optional item 2", ... (sequential, like the plot labels).
  # opt_labels overrides the default for this plot only: by position, or by name, where the
  # name can be the column (opt3) or the plot label (o1).
  k <- length(opt_cols)
  default <- paste("Optional item", seq_len(k))
  out <- default
  if (is.null(opt_labels) || length(opt_labels) == 0) return(out)
  nms <- names(opt_labels)
  opt_labels <- as.character(opt_labels)
  if (!is.null(nms) && all(nzchar(nms))) {
    key <- tolower(trimws(nms))
    pos <- match(key, tolower(opt_cols))
    pos[is.na(pos)] <- match(key[is.na(pos)], tolower(opt_items))
    if (any(is.na(pos))) {
      warning("opt_labels: no optional item called ", paste0("'", nms[is.na(pos)], "'", collapse = ", "),
              ". Use the column name (", paste(opt_cols, collapse = ", "), ") or the plot label (",
              paste(opt_items, collapse = ", "), ").", call. = FALSE)
    }
    out[pos[!is.na(pos)]] <- opt_labels[!is.na(pos)]
  } else {
    if (length(opt_labels) > k) {
      warning("opt_labels has ", length(opt_labels), " names but the data have ", k,
              " optional item(s); the extra names were ignored.", call. = FALSE)
    }
    n <- min(length(opt_labels), k)
    if (n > 0) out[seq_len(n)] <- opt_labels[seq_len(n)]
  }
  blank <- is.na(out) | trimws(out) == ""
  out[blank] <- default[blank]
  out
}

# Number of text lines of a legend block (titles count 1.15, gaps 0.6)
.legend_lines <- function(entries) {
  if (length(entries) == 0) return(0)
  sum(vapply(entries, function(e) switch(e$type, title = 1.6, gap = 0.6, 1.25), numeric(1))) + 1
}

# Legend as a grid grob with absolute units (points), anchored at the top-left corner.
# Points are used (not "lines") because "lines" depends on each grob's own font size,
# which made bold titles drift away from the other entries.
.legend_grob <- function(entries, size = 9) {
  if (length(entries) == 0) return(grid::nullGrob())
  line_pt <- size * 1.2
  pt <- function(n_lines) grid::unit(n_lines * line_pt, "points")
  grobs <- list()
  y <- 1   # in legend lines, from the top
  x0 <- grid::unit(0.05, "npc")
  for (e in entries) {
    if (e$type == "gap") { y <- y + 0.6; next }
    step <- if (e$type == "title") 1.6 else 1.25
    yy <- grid::unit(1, "npc") - pt(y)
    if (e$type == "title") {
      grobs[[length(grobs) + 1]] <- grid::textGrob(e$text, x = x0, y = yy, hjust = 0, vjust = 0.5,
                                                   gp = grid::gpar(fontsize = size + 1, fontface = "bold"))
    } else if (e$type == "text") {
      grobs[[length(grobs) + 1]] <- grid::textGrob(e$text, x = x0, y = yy, hjust = 0, vjust = 0.5,
                                                   gp = grid::gpar(fontsize = size))
    } else if (e$type == "swatch") {
      grobs[[length(grobs) + 1]] <- grid::circleGrob(
        x = x0 + pt(0.45), y = yy, r = pt(0.42),
        gp = grid::gpar(fill = e$fill, col = if (is.na(e$col)) NA else e$col, lty = e$lty, lwd = 0.8))
      grobs[[length(grobs) + 1]] <- grid::textGrob(e$text, x = x0 + pt(1.4), y = yy,
                                                   hjust = 0, vjust = 0.5, gp = grid::gpar(fontsize = size))
    }
    y <- y + step
  }
  grid::gTree(children = do.call(grid::gList, grobs))
}

# Recommended size (inches) for the figure, proportional to the number of studies
.eclipse_dims <- function(n_studies, n_items, labels, design, classic_lines, footer_lines) {
  per_study <- 0.42                       # vertical space per study
  per_item  <- 0.56                       # horizontal space per item column
  lab_chars <- max(nchar(unlist(strsplit(labels, "\n"))), 4)
  label_w   <- min(lab_chars * 0.075 + 0.3, 3.6)
  main_w <- label_w + n_items * per_item + 0.4
  main_h <- n_studies * per_study + 0.7
  title_in <- 0.6
  line_in <- 9 / 72 * 1.2                 # one legend line at 9 pt
  if (design == "horizontal") {
    legend_w <- 3.4
    legend_h <- classic_lines * line_in + 0.3
    width  <- max(main_w + legend_w, 9)
    height <- title_in + max(main_h, legend_h)
    proportions <- c(width - legend_w, legend_w) / width
    legend_size <- c(width = legend_w + 0.4, height = legend_h + 0.2)
  } else {
    legend_h <- footer_lines * (8 / 72 * 1.2) + 0.3
    width  <- max(main_w, 8.27)            # A4 width: portrait page
    height <- title_in + main_h + legend_h
    proportions <- c(main_h, legend_h) / (main_h + legend_h)
    legend_size <- c(width = width, height = legend_h + 0.2)
  }
  list(full = c(width = round(width, 2), height = round(height, 2)),
       main = c(width = round(main_w, 2), height = round(main_h, 2)),
       legend = round(legend_size, 2), proportions = proportions, title_in = title_in)
}






#' Get path to example files (.csv)
#'
#' @param name Name of the file: \code{"brief"}, \code{"long"} or \code{"messy"}.
#' @return A character string containing the absolute file path to the requested
#' sample CSV file within the package installation directory.
#' @examples
#' get_eclipse_data("brief")
#' @export
get_eclipse_data <- function(name = c("brief", "long", "messy")) {
  # [0.9.8 new] match.arg: "messy" is now available and a typo gives an error
  # (before, any name other than "brief" silently returned sample_long.csv).
  name <- match.arg(name)
  file_name <- paste0(c(brief = "sample_brief", long = "sample_long", messy = "messy")[[name]], ".csv")
  path <- system.file("extdata", file_name, package = "eclipseplot")

  if (path == "") {
    stop("File not found. Make sure the package is installed correctly.")
  }

  return(path)
}


#' Export the eclipseplot to a file
#'
#' @param plot_object The ggplot object returned by eclipseplot().
#' @param filename Character. The name of the file to save. Must include extension (e.g., "plot.png", "plot.pdf", "plot.tiff").
#' @param width Width in inches. Default \code{NULL} uses the size recommended by \code{eclipseplot()} (it grows with the number of studies), or 14 if unknown.
#' @param height Height in inches. Default \code{NULL} uses the recommended size, or 9.8 if unknown.
#' @param dpi Resolution (Default is 300, suitable for journals).
#'
#' @return No return value, called for side effects (saves a file to the specified location).
#'
#' @examples
#' \donttest{
#'   # Using a temporary directory
#'   data(sample_brief)
#'   p <- eclipseplot(sample_brief)
#'   tmp_file <- file.path(tempdir(), "test_plot.png")
#'   save_eclipseplot(p, filename = tmp_file)
#' }
#' @export
save_eclipseplot <- function(plot_object, filename, width = NULL, height = NULL, dpi = 300) {
  # [0.9.8 M4] dpi default 72 -> 300. [0.9.8 I3/I4] size taken from the plot when not given.
  size <- attr(plot_object, "eclipse_size")
  if (is.null(width))  width  <- if (!is.null(size)) size[["width"]]  else 14
  if (is.null(height)) height <- if (!is.null(size)) size[["height"]] else 9.8

  args <- list(filename = filename, plot = plot_object, width = width, height = height,
               dpi = dpi, bg = "white", limitsize = FALSE)
  # [0.9.8] PDF through cairo so that accented study names are embedded correctly
  if (tolower(tools::file_ext(filename)) == "pdf" && capabilities("cairo")) args$device <- grDevices::cairo_pdf
  do.call(ggplot2::ggsave, args)
  message(paste0("Plot saved to: ", filename))
  invisible(filename)
}









#' Convert ROBUST Standard Spreadsheet to eclipseplot format
#'
#' @description
#' This function facilitates the conversion of data extracted using the standard
#' ROBUST spreadsheet into the format required by 'eclipseplot'. It automatically
#' identifies header locations, maps Step 1 and Step 2 columns for each domain,
#' and filters out common spreadsheet artifacts (e.g., merged cells or labels like
#' "Intervention Group"). Ratings are standardised with \code{\link{eclipsedf}}.
#'
#' @param path Path to the '.xlsx' file.
#' @param sheet Sheet name or index (defaults to 1).
#'
#' @return A standardized data frame formatted for 'eclipseplot' with specific
#' column names (itemX_stepY) and categorical levels for bias assessment.
#'
#' @seealso \code{\link{read_eclipse}}, which reads both this spreadsheet and plain tables.
#' @export
#' @importFrom utils tail head
from_xlsx <- function(path, sheet = 1) {

  # 1. Load data as text
  # [0.9.8] col_types = "text" and quiet name repair (avoids "New names:" messages)
  raw_data <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                                  col_types = "text", .name_repair = "minimal"))

  if (nrow(raw_data) == 0) stop("The spreadsheet is empty.", call. = FALSE)

  # 2. Locate the row containing "Step 1" or "Step 2"
  # We convert to character and remove NAs to check the row content
  header_row_idx <- which(apply(raw_data, 1, function(x) {
    any(grepl("Step 1|Step 2", as.character(x), ignore.case = TRUE))
  }))[1]

  if (is.na(header_row_idx)) {
    stop("Could not find ROBUST-RCT headers (Step 1/Step 2) in the spreadsheet. ",
         "Use the standard ROBUST spreadsheet, or a table with the columns pmid, study, item1_step1 ... item6_step2 ",
         "(read it with read_eclipse()).", call. = FALSE)
  }
  # [0.9.8 new] Header in the last row: (n+1):n used to read the header row again as data
  if (header_row_idx >= nrow(raw_data)) {
    stop("The spreadsheet has the Step 1/Step 2 headers but no data rows below them.", call. = FALSE)
  }

  # 3. Extract headers and data body
  headers <- as.character(raw_data[header_row_idx, ])
  data_body <- raw_data[(header_row_idx + 1):nrow(raw_data), ]
  colnames(data_body) <- headers

  # 4. Map columns dynamically with fallbacks
  step1_ind <- which(grepl("Step 1", headers, ignore.case = TRUE))
  step2_ind <- which(grepl("Step 2", headers, ignore.case = TRUE))

  # Check if we found enough columns to proceed
  # [0.9.8] check moved before any use of the indices
  if (length(step1_ind) < 5 || length(step2_ind) < 6) {
    stop("The spreadsheet does not contain enough 'Step 1' or 'Step 2' columns for the 6 standard items ",
         "(found ", length(step1_ind), " Step 1 and ", length(step2_ind), " Step 2).", call. = FALSE)
  }

  # Improved Study Index Search: look for 'Study', 'Article', 'Author', or just use column 1
  study_idx <- which(grepl("Study|Article|Author", headers, ignore.case = TRUE))[1]
  if (is.na(study_idx)) study_idx <- 1
  pmid_idx <- which(grepl("PMID|PubMed", headers, ignore.case = TRUE))[1]

  # [0.9.8 new] Item 6 Step 2 is the 6th Step 2 column. Before, it was the LAST Step 2
  # column, which picked an optional item when the sheet had optional items after item 6.
  item6_idx <- step2_ind[6]
  if (length(step2_ind) > 6) {
    warning("The spreadsheet has ", length(step2_ind), " 'Step 2' columns. The 6th was used as item 6; ",
            "the others were ignored (optional items are not read from this spreadsheet).", call. = FALSE)
  }

  clean_df <- data.frame(
    pmid = if (is.na(pmid_idx)) NA_character_ else as.character(data_body[[pmid_idx]]),
    study = data_body[[study_idx]],

    item1_step1 = data_body[[step1_ind[1]]],
    item1_step2 = data_body[[step2_ind[1]]],

    item2_step1 = data_body[[step1_ind[2]]],
    item2_step2 = data_body[[step2_ind[2]]],

    item3_step1 = data_body[[step1_ind[3]]],
    item3_step2 = data_body[[step2_ind[3]]],

    item4_step1 = data_body[[step1_ind[4]]],
    item4_step2 = data_body[[step2_ind[4]]],

    item5_step1 = data_body[[step1_ind[5]]],
    item5_step2 = data_body[[step2_ind[5]]],

    item6_step2 = data_body[[item6_idx]],
    stringsAsFactors = FALSE
  )

  # 5. Filter out noise
  noise_labels <- c("Intervention", "Control", "Overall", "Study", "Article", "Author", NA)
  clean_df <- clean_df[!clean_df$study %in% noise_labels, ]
  clean_df <- clean_df[!is.na(clean_df$study), ]
  rownames(clean_df) <- NULL

  if (nrow(clean_df) == 0) stop("No studies were found below the Step 1/Step 2 headers.", call. = FALSE)

  # [0.9.8 new] Ratings standardised (same rules as eclipsedf and eclipseplot)
  for (col in .core_cols) clean_df[[col]] <- .std_values(clean_df[[col]], step = sub(".*step", "", col))

  return(clean_df)
}





#' Data Cleaning Helper for Eclipse Plot
#'
#' @description
#' Standardizes both column names and cell values to ensure compatibility with
#' the 'eclipseplot' engine. It uses fuzzy matching via regular expressions to
#' identify headers and a robust dictionary to map various shorthand judgments
#' into the four standard risk-of-bias levels.
#'
#' @param df A data frame to be cleaned.
#'
#' @return A standardized data frame with the official ROBUST-RCT wording
#' (Step 1: "Definitely Yes" ... "Definitely No"; Step 2 and optional items:
#' "Definitely Low" ... "Definitely High"; or "Not applicable") and column names
#' (pmid, study, itemX_stepY, optX) for use in the eclipse plot. Empty cells are
#' returned as \code{NA}; unrecognised values are kept unchanged and reported in
#' a warning, so they can be corrected.
#'
#' @details
#' \strong{Column Name Standardization:}
#' The function identifies columns regardless of case and replaces them with:
#' \itemize{
#'   \item \code{itemX_stepY}: Matches any string containing "item" followed by a number
#'   and "step" followed by a number (e.g., "Item 1, Step 2" becomes "item1_step2").
#'   \item \code{optX}: Matches "opt1", "Opt 2", "optional 3", "opt4_step2", ...
#'   \item \code{pmid}: Matches "pmid", "pm_id", "pm id", "article id", "study id", "trial id", or "id".
#'   \item \code{study}: Matches "study", "article", "label", "paper", "rct", "trial", "author", "reference" or "citation".
#' }
#' Identifier rules are applied before label rules, so "Article ID" becomes
#' \code{pmid} and "Article" becomes \code{study}. If two columns match the same
#' name, the first is used and the others are left unchanged. If there is no
#' study column, the first text column that is not a rating or an identifier is used.
#'
#' \strong{Value Standardization (Fuzzy Mapping):}
#' Judgment inputs are converted to lowercase, trimmed, and hyphens, underscores,
#' dots and slashes are read as spaces. The mapping logic is as follows:
#' \itemize{
#'   \item \strong{"Definitely Low"/"Definitely Yes"}: "definitely yes", "definitely low", "dy", "dl", "yes", or "y".
#'   \item \strong{"Probably Low"/"Probably Yes"}: "probably yes", "probably low", "py", or "pl".
#'   \item \strong{"Probably High"/"Probably No"}: "probably no", "probably high", "pn", or "ph".
#'   \item \strong{"Definitely High"/"Definitely No"}: "definitely no", "definitely high", "dn", "dh", "no", or "n".
#'   \item \strong{"Not applicable"}: "not applicable" or "n/a".
#'   \item Empty cells become \code{NA} (drawn as "Missing" by \code{eclipseplot}).
#'   \item Any other text is kept and reported (drawn as "Missing").
#' }
#'
#' @note
#' Step 1 columns use the Step 1 wording (Yes/No) and Step 2 and optional
#' columns use the Step 2 wording (Low/High). Both are drawn with the same colours.
#'
#' @examples
#' data(messy)
#' messy
#' organized <- eclipsedf(messy)
#' organized
#'
#' @export


eclipsedf <- function(df) {

  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)

  # 1. Standardize Values Function (Internal)
  # [0.9.8] Replaced by .std_values() (R/input.R), shared with eclipseplot(). Changes:
  #  - "probably-yes", "definitely_low", etc. are recognised (only "probably-no" was before);
  #  - empty cells -> NA and unrecognised text kept as is (before both became "Not applicable");
  #  - Step 1 columns keep the Step 1 wording (Yes/No).

  # 2. Rename Columns using Flexible Regex
  current_names <- colnames(df)
  new_names <- current_names
  taken <- character(0)

  for (i in seq_along(current_names)) {
    name_clean <- tolower(trimws(current_names[i]))
    target <- NA_character_

    # Identify Item X Step Y (Captures Item.1.Step.2, item 1, step 2, etc.)
    # The [^0-9]* means "anything that is not a number"
    if (grepl("item[^0-9]*([1-9])[^0-9]*step[^0-9]*([1-9])", name_clean)) {
      item_num <- sub(".*item[^0-9]*([1-9]).*", "\\1", name_clean)
      step_num <- sub(".*step[^0-9]*([1-9]).*", "\\1", name_clean)
      target <- paste0("item", item_num, "_step", step_num)
    }
    # [0.9.8 I7] Optional items: opt1, "Opt 2", "optional 3", "opt4_step2" -> optN
    else if (grepl("^opt(ional)?[^a-z0-9]*([0-9]+)([^0-9]*step[^0-9]*2)?$", name_clean)) {
      target <- paste0("opt", sub("^opt(ional)?[^a-z0-9]*([0-9]+).*$", "\\2", name_clean))
    }
    # [0.9.8 M4] Identify PMID BEFORE study, so "Article ID" becomes pmid (it became study)
    else if (grepl("pmid|pm[^a-z0-9]*id|(article|study|trial)[^a-z0-9]*id|^id$", name_clean)) {
      target <- "pmid"
    }
    # Identify Study (Study, Article, label, paper, rct)
    else if (grepl("study|article|label|paper|rct|trial|author|reference|citation", name_clean)) {
      target <- "study"
    }

    # [0.9.8 new] Never create two columns with the same name (before, "Study" and
    # "Study ID" both became "study" and the first one, possibly the ID, was kept).
    if (!is.na(target) && !(target %in% taken)) {
      new_names[i] <- target
      taken <- c(taken, target)
    }
  }
  colnames(df) <- new_names

  # 3. Apply Value Cleaning to Item columns
  # [0.9.8 new] Optional columns are also cleaned (before, "probably low" in opt1 was not).
  target_cols <- grep("^item[0-9]_step[0-9]$|^opt[0-9]+$", colnames(df), value = TRUE)
  unrecognised <- character(0)
  for (col in target_cols) {
    step <- if (grepl("_step1$", col)) 1 else 2
    original <- df[[col]]
    df[[col]] <- .std_values(original, step = step)
    bad <- !is.na(df[[col]]) & !.is_recognised(df[[col]])
    if (any(bad)) unrecognised <- c(unrecognised, paste0(col, " row ", which(bad), ": '", original[bad], "'"))
  }
  if (length(unrecognised) > 0) {
    warning("Values not recognised (kept unchanged; shown as 'Missing' in the plot):\n  ",
            paste(utils::head(unrecognised, 10), collapse = "\n  "),
            if (length(unrecognised) > 10) paste0("\n  ... and ", length(unrecognised) - 10, " more"),
            call. = FALSE)
  }

  # 4. Final Structure Fixes
  # [0.9.8 new] Before, the FIRST column was renamed to "study" even when it was a rating
  # column (e.g. item1_step1), which deleted that column. Now only a free text column is used.
  if (!"study" %in% colnames(df)) {
    candidates <- setdiff(colnames(df), c("pmid", target_cols))
    candidates <- candidates[!grepl("^item|^opt", candidates, ignore.case = TRUE)]
    candidates <- candidates[vapply(df[candidates], function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(candidates) > 0) {
      colnames(df)[colnames(df) == candidates[1]] <- "study"
    }
  }
  if (!"pmid" %in% colnames(df)) {
    df$pmid <- NA_character_    # [0.9.8] NA instead of 0 (0 looked like a real identifier)
  }

  # 5. Reorder to EclipsePlot Standard
  expected_order <- c("pmid", "study", .core_cols)

  opt_cols <- .opt_columns(df)
  if(length(opt_cols) > 0) {
    expected_order <- c(expected_order, opt_cols)
  }

  existing_cols <- expected_order[expected_order %in% colnames(df)]
  df_final <- df[, existing_cols, drop = FALSE]
  rownames(df_final) <- NULL
  # [0.9.8] Keep the original names so that error messages show the user's columns
  attr(df_final, "original_names") <- current_names

  return(df_final)
}
