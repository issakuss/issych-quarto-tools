needs::needs(ini, kableExtra, english)


flatten_abbr <- function(abbr, sections_exclude = NULL) {
  if (!is.null(sections_exclude)){
    for (section in sections_exclude) {
      abbr[section] <- NULL
    }
  }
  abbr <- unlist(abbr)
  names(abbr) <- sub("^[^.]+\\.", "", names(abbr))

  return(abbr)
}


fullform_columns <- function(df, columns = NULL, abbr) {
  if (is.null(columns)) {
    return(df)
  }
  
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- sapply(df[[col]], function(x) {
        # if (is.null(abbr[x])) x else abbr[x]
        # if (!is.null(abbr[x])) abbr[x] else x
        if (x == "") "-" else if (is.null(abbr[x])) x else abbr[x]
      })
    }
  }
  return(df)
}


fullform_colnames <- function(df, abbr) {
  colnames(df) <- sapply(colnames(df), function(col) {
    if (!is.na(abbr[col])) abbr[col] else col})
  return(df)
}


fullform <- function(table,
                     cols_abbr = NULL,
                     path_ini_abbr = "abbr.ini") {
  abbr <- flatten_abbr(read.ini(path_ini_abbr),
                       sections_exclude = c("short"))
  table <- fullform_columns(table, cols_abbr, abbr)
  table <- fullform_colnames(table, abbr)

  return(table)
}


set_sdgt <- function(table,
                     nr = NULL,
                     rate = NULL,
                     p = NULL,
                     int = NULL,
                     replace_na_to = "-") {
  for (col in rate) {
      table[[col]] <- sapply(table[[col]],
                             function(x) {nr$rate(x)})
  }
  for (col in p) {
      table[[col]] <- sapply(table[[col]],
                             function(x) {nr$p(x, wo_eq=TRUE)})
  }
  for (col in int) {
      table[[col]] <- sapply(table[[col]],
                             function(x) {as.character(as.integer(x))})
  }
  table[sapply(table, is.numeric)] <- lapply(
    table[sapply(table, is.numeric)], function(x) sapply(x, nr$gen))

  table[is.na(table)] <- replace_na_to
  return(table)
}


NumRenderer <- function(sdgt_general,
                        sdgt_rate,
                        sdgt_percent,
                        sdgt_pval,
                        min_pval,
                        abbr_head_zero_of_rate) {
  obj <- list(
    sdgt_general = as.numeric(sdgt_general),
    sdgt_rate = as.numeric(sdgt_rate),
    sdgt_percent = as.numeric(sdgt_percent),
    sdgt_pval = as.numeric(sdgt_pval),
    min_pval = as.numeric(min_pval),
    abbr_head_zero_of_rate = abbr_head_zero_of_rate
  )

  obj$rnd <- function(val, n_sdgt) {
    rounded <- round(as.numeric(val), n_sdgt)
    if(rounded == 0) {
      return(paste0("0.", strrep("0", n_sdgt)))
    }
    rounded <- format(round(as.numeric(val), n_sdgt))
    sdgt <- strsplit(rounded, "\\.")[[1]][2]
    n_short <- n_sdgt - nchar(sdgt)
    if (n_short > 0) {
      rounded <- paste0(rounded, strrep("0", n_short))
    }
    return(rounded)
  }

  obj$rate2str <- function(val) {
    if (obj$abbr_head_zero_of_rate) {
      return(gsub("0\\.", ".", as.character(val)))
    }
    return(val)
  }

  obj$rate <- function(val) {
    return(obj$rate2str(obj$rnd(val, obj$sdgt_rate)))
  }

  obj$perc <- function(val) {
    return(obj$rnd(as.numeric(val) * 100, obj$sdgt_percent))
  }

  obj$gen <- function(val) {
    if (is.na(val)) {
      return(val)
    }
    return(obj$rnd(val, obj$sdgt_general))
  }

  obj$p <- function(val, wo_eq=FALSE) {
    if(is.na(val)) {
      return(val)
    }
    if (as.numeric(val) < obj$min_pval) {
      return(paste("< ", obj$rate2str(obj$min_pval)))
    }
    if (wo_eq) {
      return (obj$rate2str(obj$rnd(val, obj$sdgt_pval)))
    }
    return(paste("= ",obj$rate2str(obj$rnd(val, obj$sdgt_pval))))
  }

  obj$wd <- function(val,
                     max_val_tobe_word = 10,
                     add_article = FALSE,
                     is_head = FALSE) {
    capitalize_first <- function(word) {
      paste0(toupper(substring(word, 1, 1)), substring(word, 2))
    }
    val <- as.numeric(val)
    if (val > max_val_tobe_word) {
      return(val)
    }
    word <- as.character(ifelse(add_article, indefinite, as.english)(val))
    if (!is_head) {
      return(word)
    }
    return(capitalize_first(word))
  }

  return(obj)

}
