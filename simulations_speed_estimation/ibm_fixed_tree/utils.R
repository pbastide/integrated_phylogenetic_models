################################################################################
## Utility xml functions
################################################################################
#' @title Create xml lines of data
#'
#' @description
#' Create a line with continuous data
#'
#' @param data a p x ntaxa matrix of data, with species as colnames
#' @param trait the name of the trait
#'
#' @return A line of text
##
get_all_data_lines <- function(data, trait, date = FALSE, date_sep = "_", offset = 0){
  fun <- function(x){
    get_data_line(x[2:length(x)], x[1], trait, date, date_sep, offset)
  }
  data <- unname(rbind(colnames(data), data))
  data_lines <- apply(data, 2, fun)
  return(data_lines)
}

#' @title Create xml line of data
#'
#' @description
#' Create a line with continuous data
#'
#' @param data a vector of continuous data
#' @param species the name of the species
#' @param trait the name of the trait
#'
#' @return A line of text
##
get_data_line <- function(data, species, trait, date = FALSE, date_sep = "_", offset = 0.0){
  data_line <- paste0("\t\t<taxon id=\"", species, "\">")
  if (date) {
    dd <- strsplit(species, "_")[[1]]
    dd <- as.numeric(dd[length(dd)]) + offset
    data_line <- paste0(data_line, "  <date value=\"", dd, "\" direction=\"forwards\" units=\"years\"/> ")
  }
  data_line <- paste0(data_line, " <attr name=\"", trait, "\"> ")
  data_line <- paste0(data_line, paste(data, collapse=" "))
  data_line <- paste0(data_line, " </attr> </taxon>")
  return(data_line)
}

insert_below <- function(xml_file, block, linePos) {
  return(c(xml_file[1:linePos],
           block,
           xml_file[(linePos + 1):length(xml_file)]))
}
