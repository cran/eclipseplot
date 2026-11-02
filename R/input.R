# Input helpers: value normalisation, file reading and data checks.
# New in 0.9.8. These functions are shared by eclipseplot(), eclipsedf(),
# from_xlsx() and the Shiny app, so the package and the app always apply
# the same rules.

# ---------------------------------------------------------------------------
# Vocabulary
# ---------------------------------------------------------------------------

.step1_levels <- c("Definitely Yes", "Probably Yes", "Probably No", "Definitely No")
.step2_levels <- c("Definitely Low", "Probably Low", "Probably High", "Definitely High")
.na_label <- "Not applicable"

# Lower-case aliases -> position 1..4 (1 = Definitely Low/Yes ... 4 = Definitely High/No)
.value_aliases <- list(
  c("definitely yes", "definitely low", "dy", "dl", "yes", "y"),
  c("probably yes", "probably low", "py", "pl"),
  c("probably no", "probably high", "pn", "ph"),
  c("definitely no", "definitely high", "dn", "dh", "no", "n")
)
.na_aliases <- c("not applicable", "n a", "not app", "na applicable")

.clean_token <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- gsub("[-_./]+", " ", x)      # "probably-yes", "n/a", "definitely_low"
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

#' Standardise rating values (internal)
#'
#' Recognised values are converted to the official ROBUST-RCT wording for the
#' given step; blank cells become \code{NA}; unrecognised text is returned
#' unchanged so that it can be reported to the user.
#' @noRd
.std_values <- function(x, step = 2) {
  x <- as.character(x)
  out <- x
  tok <- .clean_token(x)
  target <- if (identical(as.numeric(step), 1)) .step1_levels else .step2_levels
  blank <- is.na(x) | tok == ""
  for (i in seq_along(.value_aliases)) {
    out[!blank & tok %in% .value_aliases[[i]]] <- target[i]
  }
  out[!blank & tok %in% .na_aliases] <- .na_label
  out[blank] <- NA_character_
  out
}

# Plot category: 4 risk levels, "Not applicable" or "Missing"
.judgment_levels <- c("Definitely Low", "Probably Low", "Probably High",
                      "Definitely High", "Not applicable", "Missing")

.judgment_group <- function(x) {
  x <- .std_values(x, step = 2)            # step-1 wording maps to the same colours
  out <- rep("Missing", length(x))
  hit <- match(x, c(.step2_levels, .na_label))
  out[!is.na(hit)] <- c(.step2_levels, .na_label)[hit[!is.na(hit)]]
  factor(out, levels = .judgment_levels)
}

.is_recognised <- function(x) {
  !is.na(x) & .std_values(x, 2) %in% c(.step2_levels, .na_label)
}

# ---------------------------------------------------------------------------
# Column helpers
# ---------------------------------------------------------------------------

.core_cols <- c("item1_step1", "item1_step2", "item2_step1", "item2_step2",
                "item3_step1", "item3_step2", "item4_step1", "item4_step2",
                "item5_step1", "item5_step2", "item6_step2")

# Optional columns: opt1, opt2, ..., opt12 (any number); legacy optN_step2
.opt_pattern <- "^opt([0-9]+)(_step2)?$"

.opt_columns <- function(df) {
  nm <- colnames(df)
  hit <- grepl(.opt_pattern, nm, ignore.case = TRUE)
  cols <- nm[hit]
  num <- as.integer(sub(.opt_pattern, "\\1", cols, ignore.case = TRUE))
  # If both optN and optN_step2 exist keep optN
  keep <- !duplicated(num)
  cols <- cols[keep]; num <- num[keep]
  cols[order(num)]
}

.study_labels <- function(df) {
  n <- nrow(df)
  lab <- if ("study" %in% colnames(df)) as.character(df$study) else rep(NA_character_, n)
  if ("pmid" %in% colnames(df)) {
    pm <- as.character(df$pmid)
    use_pm <- (is.na(lab) | trimws(lab) == "") & !is.na(pm) & trimws(pm) != ""
    lab[use_pm] <- paste0("PMID: ", pm[use_pm])
  }
  empty <- is.na(lab) | trimws(lab) == ""
  lab[empty] <- paste0("Row ", seq_len(n)[empty])
  lab
}

# ---------------------------------------------------------------------------
# Exported: check_eclipse_data()
# ---------------------------------------------------------------------------

#' Check a data set before plotting
#'
#' Lists every problem that would make the Eclipse plot misleading: missing
#' columns, empty or unrecognised ratings, duplicated study labels, optional
#' columns that cannot be read, and (as information only) items 1 and 2 whose
#' Step 2 rating is not the equivalent of the Step 1 rating.
#'
#' Ratings are compared without regard to case, extra spaces or hyphens, and the
#' abbreviations accepted by \code{\link{eclipsedf}} (e.g. "py", "DL") are valid.
#'
#' @param robust_data A data frame in the eclipseplot format.
#' @param standard Logical. If \code{TRUE}, Step 1 columns are required.
#' @param optionals Logical. If \code{TRUE}, optional columns are checked.
#'
#' @return A data frame with one row per problem and the columns
#'   \code{severity} ("error", "warning" or "info"), \code{row}, \code{study},
#'   \code{column}, \code{value} and \code{message}. It has zero rows when no
#'   problem is found. Rows are numbered as in the input (row 1 = first study).
#'
#' @examples
#' check_eclipse_data(sample_brief)
#' messy_values <- sample_brief
#' messy_values$item1_step1[1] <- "definately yes"
#' messy_values$item2_step2[2] <- NA
#' check_eclipse_data(messy_values)
#' @export
check_eclipse_data <- function(robust_data, standard = TRUE, optionals = TRUE) {
  issues <- list()
  add <- function(severity, message, row = NA_integer_, study = NA_character_,
                  column = NA_character_, value = NA_character_) {
    issues[[length(issues) + 1]] <<- data.frame(
      severity = severity, row = as.integer(row), study = as.character(study),
      column = as.character(column), value = as.character(value),
      message = message, stringsAsFactors = FALSE)
  }
  finish <- function() {
    if (length(issues) == 0) {
      return(data.frame(severity = character(0), row = integer(0), study = character(0),
                        column = character(0), value = character(0), message = character(0),
                        stringsAsFactors = FALSE))
    }
    out <- do.call(rbind, issues)
    out[order(match(out$severity, c("error", "warning", "info"))), , drop = FALSE]
  }

  if (!is.data.frame(robust_data)) {
    add("error", "The input must be a data frame.")
    return(finish())
  }
  if (nrow(robust_data) == 0 || ncol(robust_data) == 0) {
    add("error", "The data set has no rows. Check that the file is not empty and that the first row contains the column names.")
    return(finish())
  }

  nm <- colnames(robust_data)
  required <- if (standard) .core_cols else grep("_step2$", .core_cols, value = TRUE)
  present <- intersect(required, nm)

  if (length(present) == 0) {
    orig <- attr(robust_data, "original_names")
    if (!is.null(orig)) nm <- orig
    add("error", paste0(
      "No ROBUST-RCT item columns were found. Expected columns such as ",
      "item1_step1, item1_step2 ... item6_step2. Columns found: ",
      paste(utils::head(nm, 8), collapse = ", "),
      if (length(nm) > 8) ", ..." else "",
      if (length(nm) == 1 && grepl(";", nm)) ". The file seems to use ';' as separator." else "."))
    return(finish())
  }

  missing_cols <- setdiff(required, nm)
  if (length(missing_cols) > 0) {
    add("warning", paste0("Missing column(s): ", paste(missing_cols, collapse = ", "),
                          ". These cells are shown as 'Missing'."),
        column = paste(missing_cols, collapse = ", "))
  }
  if (!"study" %in% nm && !"pmid" %in% nm) {
    add("warning", "No 'study' or 'pmid' column. Studies are labelled 'Row 1', 'Row 2', ...")
  } else if (!"study" %in% nm) {
    add("info", "No 'study' column. Studies are labelled with the 'pmid' column.")
  }

  labels <- .study_labels(robust_data)

  # Duplicated study labels
  dup <- unique(labels[duplicated(labels)])
  for (d in dup) {
    rows <- which(labels == d)
    add("warning", paste0("Study label '", d, "' appears in rows ", paste(rows, collapse = ", "),
                          ". Each row is plotted separately; consider '", d, "a', '", d, "b'."),
        row = rows[1], study = d, column = "study", value = d)
  }

  # Ratings
  rating_cols <- present
  opt_cols <- character(0)
  if (optionals) {
    opt_cols <- .opt_columns(robust_data)
    rating_cols <- c(rating_cols, opt_cols)
    odd_opt <- nm[grepl("^opt", nm, ignore.case = TRUE) &
                    !grepl(.opt_pattern, nm, ignore.case = TRUE) &
                    !grepl("^opt[0-9]+_step1$", nm, ignore.case = TRUE)]
    if (length(odd_opt) > 0) {
      add("warning", paste0("Column(s) not read as optional items: ", paste(odd_opt, collapse = ", "),
                            ". Name optional items opt1, opt2, opt3, ..."),
          column = paste(odd_opt, collapse = ", "))
    }
  }

  for (col in rating_cols) {
    v <- as.character(robust_data[[col]])
    blank <- is.na(v) | trimws(v) == ""
    bad <- !blank & !.is_recognised(v)
    for (r in which(blank)) {
      add("warning", "Empty cell, shown as 'Missing'.", row = r, study = labels[r], column = col, value = "")
    }
    for (r in which(bad)) {
      add("warning", paste0("Value '", v[r], "' not recognised, shown as 'Missing'."),
          row = r, study = labels[r], column = col, value = v[r])
    }
  }

  # Items 1 and 2: Step 2 should be the equivalent of Step 1 (ROBUST-RCT rule)
  if (standard) {
    for (it in 1:2) {
      s1 <- paste0("item", it, "_step1"); s2 <- paste0("item", it, "_step2")
      if (all(c(s1, s2) %in% nm)) {
        g1 <- .judgment_group(robust_data[[s1]]); g2 <- .judgment_group(robust_data[[s2]])
        incons <- which(g1 != g2 & !g1 %in% "Missing" & !g2 %in% "Missing")
        for (r in incons) {
          add("info", paste0("Item ", it, ": Step 2 ('", robust_data[[s2]][r],
                             "') is not the equivalent of Step 1 ('", robust_data[[s1]][r],
                             "'). For items 1 and 2, ROBUST-RCT has no separate judgment."),
              row = r, study = labels[r], column = s2, value = as.character(robust_data[[s2]][r]))
        }
      }
    }
  }
  finish()
}

# Summarise issues for warnings/messages (internal)
.summarise_issues <- function(iss, max_lines = 8) {
  if (nrow(iss) == 0) return(character(0))
  txt <- ifelse(is.na(iss$row), iss$message,
                paste0("row ", iss$row, " (", iss$study, "), ", iss$column, ": ", iss$message))
  # Collapse many identical-column messages
  if (length(txt) > max_lines) {
    txt <- c(utils::head(txt, max_lines), paste0("... and ", length(txt) - max_lines,
                                                 " more. Use check_eclipse_data() to see all."))
  }
  txt
}

# ---------------------------------------------------------------------------
# Exported: read_eclipse()
# ---------------------------------------------------------------------------

#' Read an assessment file (.csv, .tsv, .txt, .xlsx or .xls)
#'
#' Reads a file in the eclipseplot format or the standard ROBUST spreadsheet.
#' For text files, the encoding (UTF-8 or Windows-1252/Latin-1) and the
#' separator (comma, semicolon or tab) are detected automatically, so CSV files
#' saved by Excel in any language can be used directly. For Excel files, the
#' standard ROBUST spreadsheet is detected and converted with
#' \code{\link{from_xlsx}}; any other sheet is read with its first row as
#' column names.
#'
#' @param path Path to the file.
#' @param clean Logical. If \code{TRUE} (default), the result is passed to
#'   \code{\link{eclipsedf}} to standardise column names and ratings.
#' @param sheet Sheet name or index for Excel files (default 1).
#'
#' @return A data frame. All columns are read as text.
#' @examples
#' df <- read_eclipse(get_eclipse_data("brief"))
#' head(df, 2)
#' @export
read_eclipse <- function(path, clean = TRUE, sheet = 1) {
  if (!file.exists(path)) stop("File not found: ", path, call. = FALSE)
  if (file.info(path)$size == 0) stop("The file is empty.", call. = FALSE)

  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    raw <- suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                                               col_types = "text", .name_repair = "minimal"))
    if (nrow(raw) == 0) stop("The spreadsheet is empty.", call. = FALSE)
    is_robust <- any(vapply(seq_len(min(nrow(raw), 30)), function(i) {
      any(grepl("Step 1|Step 2", as.character(unlist(raw[i, ])), ignore.case = TRUE))
    }, logical(1)))
    if (is_robust) {
      df <- from_xlsx(path, sheet = sheet)
    } else {
      df <- .table_from_rows(raw)
    }
  } else if (ext %in% c("csv", "tsv", "txt", "")) {
    df <- .read_text_table(path)
  } else {
    stop("Unsupported file type '.", ext, "'. Use .csv or .xlsx.", call. = FALSE)
  }

  if (nrow(df) == 0) stop("The file has column names but no data rows.", call. = FALSE)
  if (clean) df <- eclipsedf(df)
  df
}

# First non-empty row as header (internal)
.table_from_rows <- function(raw) {
  raw <- as.data.frame(raw, stringsAsFactors = FALSE)
  nonempty <- which(apply(raw, 1, function(x) any(!is.na(x) & trimws(x) != "")))
  if (length(nonempty) == 0) stop("The spreadsheet is empty.", call. = FALSE)
  h <- nonempty[1]
  header <- as.character(unlist(raw[h, ]))
  header[is.na(header) | header == ""] <- paste0("V", which(is.na(header) | header == ""))
  body <- raw[-seq_len(h), , drop = FALSE]
  colnames(body) <- header
  keep <- apply(body, 1, function(x) any(!is.na(x) & trimws(x) != ""))
  body <- body[keep, , drop = FALSE]
  rownames(body) <- NULL
  body
}

.read_text_table <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  # Strip UTF-8 byte order mark
  if (length(bytes) >= 3 && all(bytes[1:3] == as.raw(c(0xEF, 0xBB, 0xBF)))) bytes <- bytes[-(1:3)]
  if (length(bytes) == 0) stop("The file is empty.", call. = FALSE)
  txt <- rawToChar(bytes)
  if (!validUTF8(txt)) {
    conv <- iconv(txt, from = "CP1252", to = "UTF-8")
    if (is.na(conv)) conv <- iconv(txt, from = "latin1", to = "UTF-8")
    txt <- conv
  }
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\r\n|\n|\r")[[1]]
  lines <- lines[trimws(lines) != ""]
  if (length(lines) == 0) stop("The file is empty.", call. = FALSE)

  # Separator: the one that splits the header into the most fields
  header <- gsub('"[^"]*"', "", lines[1])
  counts <- c("," = lengths(regmatches(header, gregexpr(",", header))),
              ";" = lengths(regmatches(header, gregexpr(";", header))),
              "\t" = lengths(regmatches(header, gregexpr("\t", header))))
  sep <- if (all(counts == 0)) "," else names(counts)[which.max(counts)]

  df <- utils::read.table(text = paste(lines, collapse = "\n"), sep = sep, header = TRUE,
                          quote = "\"", check.names = FALSE, colClasses = "character",
                          na.strings = c("", "NA"), strip.white = TRUE, comment.char = "",
                          fill = TRUE, encoding = "UTF-8")
  # Remove fully empty rows/unnamed empty columns (Excel often adds them)
  empty_col <- (colnames(df) == "" | grepl("^V[0-9]+$", colnames(df))) &
    vapply(df, function(x) all(is.na(x) | trimws(x) == ""), logical(1))
  df <- df[, !empty_col, drop = FALSE]
  keep <- apply(df, 1, function(x) any(!is.na(x) & trimws(x) != ""))
  df <- df[keep, , drop = FALSE]
  rownames(df) <- NULL
  attr(df, "eclipse_sep") <- sep
  df
}
