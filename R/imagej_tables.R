##' Collect CSV files
##'
##' Provide the path to a directory containing csv files that will be
##' read. Optionally, add a dataframe object containing
##' annotations. The dataframe containing annotations must, as its
##' first column, contain a list of strings that will match to the
##' file names of the imported files.
##' @title Collect CSVs
##' @param path the directory from which files will be read
##' @param pattern a regular expression that matches the files of
##'   interest
##' @param annotations a dataframe with its first collumn to be matched to filenames
##' @return a list of annotated dataframes.
##' @author Jones
##' @export
collect_csv_files <- function(path = ".", pattern = "*.csv",
                              annotations = NULL) {
  files <- list.files(path, pattern)
  a <- list()
  if (is.null(annotations)) {
    for (i in files) {
      a[[i]] <- structure(read.csv(i)
                          , reporter = FALSE
                          , strong = FALSE
                          , error = FALSE
                          , note = "")
    }
  } else {
    for (i in files) {
      for (j in annotations[,1]) {
        if (grepl(j, i)) {
          reporter_note <- annotations[annotations[,1] %in% j,][[2]]
          strong_note   <- annotations[annotations[,1] %in% j,][[3]]
          error_note    <- annotations[annotations[,1] %in% j,][[4]]
          note_note     <- annotations[annotations[,1] %in% j,][[5]]
        }
      }
      a[[i]] <- structure(read.csv(i)
                          , reporter = reporter_note
                          , strong = strong_note
                          , error = error_note
                          , note = note_note)
    }
  }
  a
}

##' Grep Collected
##'
##' A helper function to pattern-match against the names of the
##' dataframes.
##' @title Grep names of dataframes
##' @param data dataframes
##' @param pattern a regular expression that matches the names of
##'   dataframes in a list of data frames
##' @return a list of dataframes that match the specified criteria
##' @author Jones
##' @export
grep_collected <- function(data, pattern) {
  data_names <- names(data)
  data[grep(pattern, data_names)]
}
##' Write List of Names
##'
##' Write a list of names of the files in a particular folder.
##' @title Create a CSV containing a list of the file names
##' @param folder the path to a directory
##' @param id the string appended to describe the document
##' @return a string describing the result
##' @author Jones
##' @export
write_list_of_names <- function(folder = ".", id = "-names") {
  if_else(folder == ".", folder1 <- "", folder1 <- folder)
  utils::write.table(list.files(path = folder),
              paste0(folder1, id, ".csv"))
  paste0(folder1, id, " has been created.")
}
##' Add Labels as Columns
##'
##' This function adds labels as columns to each dataframe in a list
##' of dataframes. This may be useful when datasets can be combined
##' into one dataframe.
##' @title Add labels with values as columns
##' @param data a list of dataframes
##' @param labels strings that identify the datasets
##' @return the data with the labels and their values appended as
##'   columns
##' @author Jones
##' @export
add_labels_as_columns <- function(data, labels) {
  for (i in labels) {
    new_col <- c()
    new_col <- data.frame(rep(attr(data, i), nrow(data)))
    colnames(new_col) <- i
    data %>% add_column(new_col) -> data
  }
  data
}

#' Label Lists
#'
#' @param b the dataframe list to be labeled
#' @param labels the labels
#'
#' @return the dataframe labeled
#' @author Jones
#' @export
label_lists <- function(b, labels) {
  for (i in 1:length(b)) {
    names(b[[i]]) <- labels
  }
  b
}


##' Annotate and Combine
##'
##' Annotate and combine all of the dataframes in your list according
##' your rules. If a list of combinations is provided, you need not
##' provide the labels field. If no list of combintions is provided
##' then each label is assumed to become a group.
##' @title Annotate and combine dataframes
##' @param data a list of dataframes
##' @param labels a vector of strings that identify the datasets
##' @param combos a list of vectors of named logical values that
##'   identify the sets of labels that should ultimately be included
##'   or excluded
##' @return a collection of tables to be analyzed
##' @author Jones
##' @export
annotate_and_combine <- function(data, labels = NULL, combos = NULL) {
  ## Data must be dataframes in a list, and annotations must be path
  ## to a CSV.
  if (is.null(labels) && is.null(combos)) {
    stop("need labels or combos")
  }
  ## A helper function for the purrr::reduce command later
  my_bind_rows <<- function(a, b) bind_rows(a, b, .id = "image")
  ## A helper function to get the list of data frames included in each
  ## group
  data_set_inclusion_and_bool_map <- function(data, getters, labels) {
    ## Define helper function to add names to the list of labels
    label_lists <- function(b, labels) {
      for (i in 1:length(b)) {
        names(b[[i]]) <- labels
      }
      b
    }
    ## Get the data-set inclusion
    data %>%
      purrr::map(function(d) purrr::map(getters, function(e) d %>% e)) %>%
      label_lists(labels) -> data_set_inclusion
    data %>%
      purrr::map(function(d) purrr::map_lgl(getters, function(e) d %>% e)) -> bool_purrr::map
    list(`data set inclusion` = data_set_inclusion, `bool map` = bool_purrr::map)
  }
  if (! is.null(labels)) {
    ## Get the data-set inclusion
    for (i in names(data)) {
      for (j in labels) {
        if (grepl(j, i)) {
          attr(data[[i]], j) <- TRUE
        }
        else {
          attr(data[[i]], j) <- FALSE
        }
      }
    }
    data1 <<- data
  } else {
    ## Get the data-set inclusion
    labels <- combos %>% purrr::reduce(bind_rows) %>% names
    for (i in names(data)) {
      for (j in labels) {
        if (grepl(j, i)) {
          attr(data[[i]], j) <- TRUE
        }
        else {
          attr(data[[i]], j) <- FALSE
        }
      }
    }
    data1 <<- data
  }
  if (is.null(combos)) {
    ## data <- purrr::map(.x, ~ add_labels_as_columns(.x, labels))
    labels_get <<- purrr::map(labels, ~ paste0(.x, "_get")) # labels for info about the datasets
    purrr::map(labels, attr_getter) %>% walk2(labels_get, ~ assign(.y, .x)) ->> getters
    data_set_inclusion_and_bool_map(data, getters, labels)[[1]] ->> dsi
    purrr::map(labels, function(label) purrr::keep(data, function(table) attr(table, label))) ->> df_sets
    purrr::map(df_sets, function(set) purrr::map(set, function(table) fill(bind_cols(table, data.frame(attributes(table)[-c(1,2,3)]))))) ->> df_sets_annotated
    purrr::map(df_sets, function(x) purrr::reduce(x, my_bind_rows)) -> out
    names(out) <- labels
  } else {
    combos %>% purrr::reduce(bind_rows) %>% # generating dataframe
      # automatically allows us to get
      # unique rows and names
      names -> combos_unique # assign names
    labels <- combos_unique
    ## combos_unique <- unique(flatten(combos))
    combos_get <<- purrr::map(combos_unique, ~ paste0(.x, "_get")) # combos for info about the datasets
    purrr::map(combos_unique, attr_getter) %>%
      walk2(combos_get, ~ assign(.y, .x)) ->> getters # function to get the value of the specified attribute
    data_set_inclusion_and_bool_map(data, getters, labels)[[1]] ->> dsi
    purrr::map(combos, function(combo) purrr::keep(data, function(table) all(purrr::map_lgl(names(combo), function(name) (attr(table, name) == combo[[name]]))))) ->> df_sets
    purrr::map(df_sets, function(set) purrr::map(set, function(table) fill(bind_cols(table, data.frame(attributes(table)[-c(1,2,3)]))))) ->> df_sets_annotated
    purrr::map(df_sets_annotated, function(x) purrr::reduce(x, my_bind_rows)) -> out
    my_str_flatten <- function(x) str_flatten(x, "-")
    names(out) <- purrr::map_chr(purrr::map(combos, names), my_str_flatten)
  }
  out
}

#' A function to plot
#'
#' @param data the data that will be input
#' @param x string denoting the x parameter (categorical)
#' @param y string denoting the y parameter
#' @param x_lab string denoting the x label
#' @param y_lab string denoting the y label
#' @param file_name string denoting how the output should be identified
#'
#' @return a plot
#' @author Jones
#' @export
fun_plot <- function(data,
                     x = "reporter",
                     y = "Circ.1",
                     x_lab = "Reporter",
                     y_lab = "Circularity (index)",
                     file_name = "plot") {
  x_quo <- rlang::quo(`!!`(sym(x)))
  y_quo <- rlang::quo(`!!`(sym(y)))
  capitalize <- function(string) {
    string <- tolower(string)
    substr(string, 1, 1) <- toupper(substr(string, 1, 1))
    string
  }

  ggplot2::ggplot(data, aes(`!!`(x_quo), `!!`(y_quo)),
         labels = capitalize) +
    ggplot2::labs(title = paste(x, "vs", y)) +
    ggplot2::xlab(x_lab) +
    ggplot2::ylab(y_lab) +
    ggplot2::geom_violin() +
    ggplot2::stat_summary() +
    ggplot2::geom_signif(comparisons = list(c("FALSE", "TRUE")),
                map_signif_level = function(p) sprintf("p = %.2g", p)) +
    ggplot2::scale_x_discrete(limits = c("FALSE","TRUE"),
                     labels = capitalize) +
    ggpubr::theme_pubr()  -> p
  ggplot2::ggsave(paste0(file_name, ".pdf"), p)
  ggplot2::ggsave(paste0(file_name, ".png"), p)
  p
}

#' Merge Folder
#'
#' Merge all of the tables in a particular folder into one dataframe, write it, and return it.
#' @param path
#'
#' @return one dataframe
#' @author Jones
#' @export
merge_folder <- function(path, name = "combined") {
  files <- list.files(path, ".csv", full.names = TRUE)
  a <- list()
  for (i in files) {
    a[[i]] <- read.csv(i)
  }
  # names(a) <- files
  out <- purrr::reduce(a, dplyr::bind_rows)
  utils::write.csv(out, paste0(name, ".csv"))
  out
}
