library(RCurl)
library(XML)
library(tm)

#' MEDLINE Parser
#' @param file_name Path to MEDLINE file or output from entrez_fetcher()
#' @keywords pubed, medline, NCBI
#' @export
#' @examples
#' # If using MEDLINE file on local machine
#' results <- medlineParser("/home/user/Downloads/pubmed_results.txt")
#'
#' # If using output from efetch()
#' search_results <- efetch("\"university of new mexico\"[AD] AND \"pharmacy\"[AD]")
#' results <- medlineParser(search_results)
medlineParser = function(file_name){
  if(file.exists(file_name)){
    lines <- readLines(file_name)
  }
  else {
    lines <- strsplit(file_name, "\n")[[1]]
  }
  medlineRecords <- list()
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
      medlineRecords[[record]] <- list()
      medlineRecords[[record]][header] <- value
    }
    else if(header == "" & value != ""){
      medlineRecords[[record]][key] <- paste(medlineRecords[[record]][key], value)
    }
    else{
      key <- header
      if(is.null(medlineRecords[[record]][key][[1]])){
        medlineRecords[[record]][key] <- value
      }
      else { 
        medlineRecords[[record]][key] <- paste(medlineRecords[[record]][key], value, sep=";")
      }
    }
  }
return(medlineRecords)
}

#' Function for retrieving PubMed search results
#' This function is for retrieving structured text from NCBI databases
#' @param query_string The query to used to retrieve results.
#' @param db Specifies what NCBI database to run th query against.
#' @param type Specifies the format results should be returned in i.e. medline
#' @export
#' @examples
#' efetch("pharmacy[AD]", "pubmed", "medline")
efetch = function(query_string, db="pubmed", type="medline"){
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

#' Function for extracting certain fields from medline records.
#' This function take medline records and a field argument and returns a list of fields.
#' @param key The field to be extracted from medline records.
#' @param recs The list of medline records returned from medlineParser.
medlineFields <- function(recs, key, sep=FALSE){
  fields <- unlist(lapply(recs, function(x) x[key][[1]]))
  if(sep != FALSE){
    fields <- lapply(fields, function(x) strsplit(x, sep)[[1]])
  }
  return(fields)
}

#' Returns a document-term matrix using field specified by user
medlineDocumentTermMatrix <- function(recs, key, label="AU"){
  text <- extractFields(recs, key)
  corp <- VCorpus(VectorSource(text))
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, content_transformer(tolower))
  corp <- tm_map(corp, removeWords, stopwords("english"))
  dtm <- as.matrix(DocumentTermMatrix(corp))
  return(dtm)
}


