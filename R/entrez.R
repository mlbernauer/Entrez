library(RCurl)
library(XML)

#' MEDLINE Parser
#' @param file_name Path to MEDLINE file or output from entrez_fetcher()
#' @keywords pubed, medline, NCBI
#' @export
#' @examples
#' # If using MEDLINE file on local machine
#' results <- medline_parser("/home/user/Downloads/pubmed_results.txt")
#'
#' # If using output from efetch()
#' search_results <- efetch("\"university of new mexico\"[AD] AND \"pharmacy\"[AD]")
#' results <- medline_parser(search_results)
medline_parser = function(file_name){
  if(file.exists(file_name)){
    lines <- readLines(file_name)
  }
  else {
    lines <- strsplit(file_name, "\n")[[1]]
  }
  medline_records <- list()
  key <- 0
  record <- 0
  for(line in lines){
    header <- sub(" {1,20}", "", substring(line, 1, 4))
    value <- sub("^.{6}", "", line)
    if(header == "" & value == ""){
      next
    }
    else if(header == "PMID"){
      record = record + 1
      medline_records[[record]] <- list()
      medline_records[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medline_records[[record]][key] <- paste(medline_records[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medline_records[[record]][key][[1]])){
        medline_records[[record]][key] <- value
      }
      else { 
        medline_records[[record]][key] <- paste(medline_records[[record]][key], value, sep=";")
      }
    }
  }
return(medline_records)
}

#' Function for retrieving PubMed search results
#' This function is for retrieving structured text from NCBI databases
#' @param query_string The query to used to retrieve results.
#' @param db Specifies what NCBI database to run th query against.
#' @param type Specifies the format results should be returned in i.e. medline
#' @export
#' @examples
#' efetch("pharmacy[AD]", "pubmed", "medline")
efetch = function(query_string, db, type){
  base <- "http://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
  query <- gsub(" ", "+", query_string)
  query <- paste("esearch.fcgi?db=", db, "&term=", query, "&usehistory=y", sep="")
  url <- paste(base, query, sep="")
  url_result <- getURL(url)

  xml_data <- xmlToList(xmlParse(url_result))
  web <- xml_data["WebEnv"][[1]]
  key <- xml_data["QueryKey"][[1]]

  fetch <- paste(base, "efetch.fcgi?db=", db, "&query_key=", key,
                "&WebEnv=", web, "&rettype=", type, "&retmode=text", sep="")
  data <- getURL(fetch)
  return(data)
}