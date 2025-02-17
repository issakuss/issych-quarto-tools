needs::needs(ini, kableExtra, english)


fullform_columns <- function(df, columns = NULL, abbr) {
  # columnsに指定された列に含まれる略語をフルフォームに変換
  if (is.null(columns)) {
    return(df)
  }
  
  for (col in columns) {
    if (col %in% names(df)) {
      df[[col]] <- sapply(df[[col]], function(x) {
        if (x == "") "-" else if (is.null(abbr[x])) x else abbr[x]
      })
    }
  }
  return(df)
}


fullform_colnames <- function(df, abbr) {
  # 列名をフルフォームに変換
  colnames(df) <- sapply(colnames(df), function(col) {
    if (!is.na(abbr[col])) abbr[col] else col})
  return(df)
}


flatten_abbr <- function(abbr, sections_exclude = NULL) {
  # Abbreviation の情報をまとめた辞書について、セクションを除いてフラットにする
  if (!is.null(sections_exclude)){
    for (section in sections_exclude) {
      abbr[section] <- NULL
    }
  }
  abbr <- unlist(abbr)
  names(abbr) <- sub("^[^.]+\\.", "", names(abbr))

  return(abbr)
}


fullform <- function(table,
                     cols_abbr = NULL,
                     path_ini_abbr = "abbr.ini") {
  # path_ini_abbr で指定された ini ファイルを読み込み、列名の省略形をフルフォームに変換
  # cols_abbr で指定された列に含まれる略語をフルフォームに変換
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
  # 表に含まれる数値を指定された桁数で丸める
  # nr: NumRenderer オブジェクト
  # rate: 比率を表す列を指定
  # p: p値を表す列を指定
  # int: 整数を表す列を指定
  # replace_na_to: NA を置き換える文字列
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
  # 数値を指定された桁数で丸めるためのクラス
  # sdgt_rate: 比率を丸めるときの桁数
  # sdgt_percent: パーセントを丸めるときの桁数
  # sdgt_pval: p値をまるめるときの桁数
  # sdgt_general: 上記以外の数値をまるめるときの桁数
  # min_pval: p値がこの値より小さい場合、"p < {min_pval}" に変換
  # abbr_head_zero_of_rate: 比率の先頭の0を省略する（"p = .001" といった表記にする）かどうか
  obj <- list(
    sdgt_general = as.numeric(sdgt_general),
    sdgt_rate = as.numeric(sdgt_rate),
    sdgt_percent = as.numeric(sdgt_percent),
    sdgt_pval = as.numeric(sdgt_pval),
    min_pval = as.numeric(min_pval),
    abbr_head_zero_of_rate = abbr_head_zero_of_rate
  )

  obj$rnd <- function(val, n_sdgt) {
    # 数値を丸める
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
    # 比率を文字列に変換
    if (obj$abbr_head_zero_of_rate) {
      return(gsub("0\\.", ".", as.character(val)))
    }
    return(val)
  }

  obj$rate <- function(val) {
    # 比率を丸める
    return(obj$rate2str(obj$rnd(val, obj$sdgt_rate)))
  }

  obj$perc <- function(val) {
    # パーセントを丸める
    return(obj$rnd(as.numeric(val) * 100, obj$sdgt_percent))
  }

  obj$gen <- function(val) {
    # 数値一般を丸める
    if (is.na(val)) {
      return(val)
    }
    return(obj$rnd(val, obj$sdgt_general))
  }

  obj$p <- function(val, wo_eq=FALSE) {
    # p値を丸める
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
    # 数値を英単語に変換（e.g., 11 → eleven）
    # max_val_tobe_word: この値より大きい場合はそのまま返す
    # add_article: 冠詞を付けるかどうか
    # is_head: 先頭を大文字にするかどうか
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
