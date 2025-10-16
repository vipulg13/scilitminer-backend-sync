library(tidyverse)
library(data.table)
library(jsonlite)
library(mongolite)
library(logger)
library(reticulate)
library(plumber)
library(future)
library(promises)
library(httr)

#install.packages("rdflib)

options(digits = 22)

# Set logger path
log_appender(appender_file(Sys.getenv("LOG_PATH")))
log_layout(layout_glue)
log_threshold(TRACE)

# Function to source project folders
sourceFolder <- function(dirPath) {
  if (!dir.exists(dirPath)) {
    logger::log_info(paste0("Directory '", dirPath, "' does not exist. Skipping...\n"))
  } else {
    files <- list.files(dirPath)
    files <- files[grepl(".R$", files) | grepl(".r$", files)]
    len <- length(files)
    if (len > 0) {
      log_info(paste0("Sourcing R files in folder '", dirPath, "'...\n"))
      for (i in 1:len) {
        curFile <- file.path(dirPath, files[i])
        source(curFile)
      }
    }
  }
}

# Function to source python folders
sourcePyFolder <- function(dirPath) {
  reticulate::use_condaenv(Sys.getenv("PY_ENV"), required = TRUE)
  if (!dir.exists(dirPath)) {
    logger::log_info(paste0("Directory '", dirPath, "' does not exist. Skipping...\n"))
  } else {
    files <- list.files(dirPath)
    files <- files[grepl(".PY$", files) | grepl(".py$", files)]
    len <- length(files)
    if (len > 0) {
      logger::log_info(paste0("Sourcing Python files in folder '", dirPath, "'...\n"))
      for (i in 1:len) {
        curFile <- file.path(dirPath, files[i])
        reticulate::source_python(curFile, envir = globalenv())
      }
    }
  }
}


# Source all folders
sourceFolder("source/dao")
sourceFolder("source/embed")

# Source python folder
sourcePyFolder("source/python")

future::plan(multicore, workers = future::availableCores()[[1]])

# REST Endpoint call up
pr("rest_controller.R") %>% pr_run(host = "0.0.0.0", port = 9009)

