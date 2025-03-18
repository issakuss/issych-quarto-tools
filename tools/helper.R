needs::needs(ini, kableExtra, english)


ready_to_embed_fig <- function(dir_result, embeded_fig_exts) {
  # dir_resultにある、拡張子がembeded_fig_extsのファイルをtex-source-filesにコピーする
  # PDF生成に必要となるファイルを一箇所にまとめるために必要な処理
  if (!dir.exists("tex-source-files")) {
    dir.create("tex-source-files")
  }
  fig_files <- list.files(dir_result, pattern = paste0("\\", embeded_fig_exts, "$"), full.names = TRUE)
  file.copy(fig_files, "tex-source-files/")
}


fullform_cols <- function(df, cols = NULL, abbr) {
  # columnsに指定された列に含まれる略語をフルフォームに変換
  if (is.null(cols)) {
    return(df)
  }
  
  for (col in cols) {
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


fullform_rownames <- function(df, abbr) {
  # 行名をフルフォームに変換
  rownames(df) <- sapply(rownames(df), function(row) {
    if (!is.na(abbr[row])) abbr[row] else row})
  return(df)
}


flatten_list <- function(abbr, sections_exclude = NULL) {
  # Abbreviation の情報をまとめた辞書について、セクションを潰してフラットにする
  if (!is.null(sections_exclude)){
    for (section in sections_exclude) {
      abbr[section] <- NULL
    }
  }
  abbr <- unlist(abbr)
  names(abbr) <- sub("^[^.]+\\.", "", names(abbr))

  return(abbr)
}

pack_rows_bycol <- function(kable_input, df, col, abbr = NULL) {
  # dfに指定した行を用いてpackする
  # abbrを指定すると、abbrを用いてフルフォームに変換する
  group_values <- df[[col]]
  if (!is.null(abbr)) {
    group_values <- unname(abbr[group_values])
  }
  index <- table(group_values)
  kable_input <- pack_rows(kable_input, index = as.list(index))
  return(kable_input)
}

set_sdgt <- function(table,
                     nr = NULL,
                     ngto = NULL,
                     p = NULL,
                     int = NULL,
                     replace_na_to = "-") {
  # 表に含まれる数値を指定された桁数で丸める
  # nr: NumRenderer オブジェクト
  # ngto: 比率、相関係数など、絶対値が1を超えない値（p値は別）を表す列を指定（Not Greater than One）
  # p: p値を表す列を指定
  # int: 整数を表す列を指定
  # replace_na_to: NA を置き換える文字列
  for (col in ngto) {
      table[[col]] <- sapply(table[[col]],
                             function(x) {nr$ngto(x)})
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
                        sdgt_not_greater_than_one,
                        sdgt_percent,
                        sdgt_pval,
                        min_pval,
                        abbr_ngto_head) {
  # 数値を指定された桁数で丸めるためのクラス
  # sdgt_not_greater_than_one: 比率、相関係数など、絶対値が1を超え得ない値（p値は別） を
  # sdgt_percent: パーセントを丸めるときの桁数
  # sdgt_pval: p値をまるめるときの桁数
  # sdgt_general: 上記以外の数値をまるめるときの桁数
  # min_pval: p値がこの値より小さい場合、"p < {min_pval}" に変換
  # abbr_ngto_head: 1を超え得ない値の先頭の0を省略する（"p = .001" といった表記にする）かどうか
  obj <- list(
    sdgt_general = as.numeric(sdgt_general),
    sdgt_not_greater_than_one = as.numeric(sdgt_not_greater_than_one),
    sdgt_percent = as.numeric(sdgt_percent),
    sdgt_pval = as.numeric(sdgt_pval),
    min_pval = as.numeric(min_pval),
    abbr_ngto_head = abbr_ngto_head
  )

  obj$rnd <- function(val, n_sdgt) {
    # 数値を丸める
    if (as.numeric(val) == 1) {
      return(paste0("1.", strrep("0", n_sdgt)))
    }
    as.numeric(val)
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
    if (obj$abbr_ngto_head) {
      return(gsub("0\\.", ".", as.character(val)))
    }
    return(val)
  }

  obj$ngto <- function(val) {
    # 1を超え得ない値を丸める
    return(obj$rate2str(obj$rnd(val, obj$sdgt_not_greater_than_one)))
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
