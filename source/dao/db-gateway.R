#db-gateway.R
#
#This file contains functions to retrieve or
#push data into database
#
#

#Get the user id from database
getUserID <- function(username) {
  jsonQry <- '{}'
  jsonFld <- '{"password":0, "_id":1}'
  jsonQry <- paste0("{\"username\":\"", username, "\"}")
  conn <- mongo(collection = "user", url = mongo_db("sys"))
  
  tryCatch({
    userID <- conn$find(query = jsonQry, fields = jsonFld)
  }, finally = {
    rm(conn)
  })
  
  return(userID)

}

#Get jobs from database
getJobs <- function(job_type, user_id = NA, job_status = NA, full_object = FALSE) {
  jsonQry <- '{}'
  jsonFld <- '{"user_id":0, "_id":1}'
  
  if (job_type == "MINING" && !full_object)  
    jsonFld <- '{"user_id":0, "_id":1, "mining_opts":0}'
  
  if (is.na(user_id) && is.na(job_status)){
    jsonQry <- paste0("{\"job_type\":\"", job_type,  "\"}")
  } else if (is.na(user_id) && !is.na(job_status)) {
    jsonQry <- paste0("{\"job_type\":\"", job_type, "\", \"job_status\":", job_status, "}")
  } else if (is.na(job_status)) {
    jsonQry <- paste0("{\"job_type\":\"", job_type, "\", \"user_id\":\"", user_id, "\"}")
  } else {
    jsonQry <- paste0("{\"job_type\":\"", job_type, "\", \"user_id\":\"", user_id, "\", \"job_status\":", job_status, "}")
  }
  conn <- mongo(collection = "job", url = mongo_db("app"))
  
  tryCatch({
    dt <- conn$find(query = jsonQry, fields = jsonFld)
    dt[is.na(dt)] <- ""
  }, finally = {
    rm(conn)
  })
  #setcolorder(dt, c("collection_name", "collection_desc"))
  return(dt)
}

#Create a new job entry in database
createNewJob <- function(jobObj) {
  conn <- mongo(collection = "job", url = mongo_db("app"))
  tryCatch({
   conn$insert(jobObj)
  }, finally = {
    rm(conn)
  })
  return()
}

#Update the job status in database
updateJobStatus <- function(job_id, job_status, job_status_description) {
  jsonQry <- paste0("{\"_id\":\"", job_id, "\"}")
  if (job_status == 3)
    jsonFld <- paste0("{\"$set\":{\"job_status\":", job_status, ",\"job_status_description\":\"", job_status_description, "\",\"job_completion_date\":\"", Sys.time(), "\"}}")
  else if (job_status == 4)
    jsonFld <- paste0("{\"$set\":{\"job_status\":", 1, ",\"job_status_description\":\"", job_status_description, "\",\"job_completion_date\":", NA, ",\"job_rerun_date\":\"", Sys.time(), "\"}}")
  else
    jsonFld <- paste0("{\"$set\":{\"job_status\":", job_status, ",\"job_status_description\":\"", job_status_description, "\"}}")
  conn <- mongo(collection = "job", url = mongo_db("app"))

  tryCatch({
    conn$update(query = jsonQry, update = jsonFld)
  }, finally = {
    rm(conn)
  })
  return()
}

#Get search data from database for a specific task type
getSearchData <- function(coll_name = NULL, dois = NULL) {
  jsonQry <- "{}"
  jsonFld <- "{}"
  
  if (!is.null(coll_name) && !is.null(dois)) {
    idVec <- paste(coll_name, dois, sep = "_")
    idVec <- paste(shQuote(idVec, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"_id\": {\"$in\": [", idVec, "]}}")
  } else if (!is.null(coll_name)) {
    jsonQry <- paste0("{\"coll_name\":\"", coll_name, "\"}")
  } else if (!is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"doi\": {\"$in\": [", dois, "]}}")
  }

  conn <- mongo(collection = "search", url = mongo_db("app"))
  tryCatch({
    dt <- data.table(conn$find(query = jsonQry, fields = jsonFld))
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  if (nrow(dt) == 0)
    log_info("No data found corresponding to the provided search type and dois")
  else {
    dt[, "_id" := NULL]
  }
  return(dt)
}

#Upload search results to database for a specific task type
uploadSearchData <- function(dt) {
  if (nrow(dt) > 0) {
    conn <- mongo(collection = "search", url = mongo_db("app"))
    tryCatch({
      if (!("source_1" %in% conn$index()[["name"]])) 
        conn$index(add = "doi")
      conn$insert(dt)
      log_info("The search object has been successfully uploaded")
    }, warning = function(w) {
      message(paste("Warning while data uploading:", w, sep = "\n")) 
    }, finally = {
      rm(conn)
    })
  } else {
    log_info("The search object is empty and therefore cannot be uploaded")
  }
}

#Get collect data from database for a specific task type
getCollectData <- function(coll_name = NULL, dois = NULL) {
  jsonQry <- "{}"
  jsonFld <- "{}"
  
  if (!is.null(coll_name) && !is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"coll_name\":\"", coll_name, "\", \"_id\": {\"$in\": [", dois, "]}}")
  } else if (!is.null(coll_name)) {
      jsonQry <- paste0("{\"coll_name\":\"", coll_name, "\"}")
  } else if (!is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"_id\": {\"$in\": [", dois, "]}}")
  }
  
  conn <- mongo(collection = "collect", url = mongo_db("app"))
  tryCatch({
    dt <- data.table(conn$find(query = jsonQry, fields = jsonFld))
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  if (nrow(dt) == 0)
    log_info("No data found corresponding to the provided collect type and dois")
  else
    setnames(dt, "_id", "doi")
  return(dt)
}

#Upload collect results to database for a specific task type
uploadCollectData <- function(dt) {
  if (nrow(dt) > 0){
    if ("doi" %in% names(dt))
      setnames(dt, "doi", "_id")
    conn <- mongo(collection = "collect", url = mongo_db("app"))
    tryCatch({
      conn$insert(dt)
      log_info("The metadata corresponding to the collected data has been successfully uploaded to the database")
    }, warning = function(w) {
      message(paste("Warning while data uploading:", w, sep = "\n")) 
    }, finally = {
      rm(conn)
    })
  } else {
    log_info("The metadata corresponding to the collected data is empty and therefore cannot be uploaded")
  }
}

#Get IR collection from database
getIRData <- function(dois = NULL, all_fields = TRUE) {
  jsonQry <- "{}"
  jsonFld <- "{}"
  
  if (!all_fields)
    jsonFld <- '{"_id":1}'  #jsonFld <- paste0("{\"", i, "\"",":1}")
    
  if (!is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"_id\": {\"$in\": [", dois, "]}}")
  }
  conn <- mongo(collection = "index", url = mongo_db("app"))
  tryCatch({
    lst <- conn$find(query = jsonQry, fields = jsonFld) %>%
            apply(1, function(x) list(x)) %>%
            unlist(recursive = F, use.names = F)
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  if (length(lst) == 0)
    log_info("No IR data found")
  if (!all_fields)
    lst <- unlist(lst)
  return(lst)
}

#Upload IR collection results to database for a specific job id
uploadIRData <- function(vecDocs) {
  if (length(vecDocs) > 0) {
    #part1 upload the indexing data in global.index collection
    conn <- mongo(collection = "index", url = mongo_db("app"))
    tryCatch({
      conn$insert(vecDocs)
      rm(conn)
      log_info("The index-ready document has been uploaded to app.index collection")
    }, error = function(e) {
      log_error("Following error occurred while uploading index-ready document to the database: {e}")
    }, finally = {
      rm(conn)
    })
  } else {
    log_info("The index-ready document is empty and therefore cannot be uploaded")
  }
}


#Upload IR Sub-Collection results to database
uploadIRResult <- function(irResultObj) {
  conn <- mongo(collection = "subcoll", url = mongo_db("app"))
  resp <- tryCatch({
    conn$insert(irResultObj)
    TRUE
    #log_info("The search results have been successfully uploaded.\n")
  }, error = function(e) {
    log_error("Following error occurred while uploading IR results to the database: {e}")
    stop(e)
  }, warning = function(w) {
    log_info(paste("Warning while while uploading IR results to the database: {w}")) 
  }, finally = {
    rm(conn)
  })
  return(resp)
}

#Get domain information
getDomainInfo <- function() {
  jsonQry <- '{}'
  jsonFld <- '{"_id":0, "domain_name":1}'
  conn <- mongo(collection = "domain", url = mongo_db("app"))
  tryCatch({
    lst <- conn$find(query = jsonQry, field = jsonFld)
  }, error = function(e) {
    log_error("Following error occurred while retrieving domain info from the database: {e}")
  }, finally = {
    rm(conn)
  })
  
  if (length(lst) == 0) {
    log_info("No domain info data found")
    return()
  }

  return(lst$domain_name)
}

#Create domain information
createDomainInfo <- function(domain_name) {
  lst <- list("_id"=gsub(" ", "_", domain_name), "domain_name"=domain_name)
  doc <- toJSON(lst)
  conn <- mongo(collection = "domain", url = mongo_db("app"))
  tryCatch({
    conn$insert(doc)
    log_info("A domain {domain_name} info has been successfully created.\n")
  }, error = function(e) {
    log_error("Following error occurred while uploading domain info to the database: {e}")
  }, finally = {
    rm(conn)
  })
}

#Delete domain information
deleteDomainInfo <- function(domain_name) {
  jsonQry <- paste0("{\"domain_name\":", "\"", domain_name, "\"}")
  conn <- mongo(collection = "domain", url = mongo_db("app"))
  tryCatch({
    conn$remove(jsonQry)
    log_info("A domain {domain_name} has been successfully deleted\n")
  }, error = function(e) {
    log_error("Following error occurred while deleting domain info to the database: {e}")
  }, finally = {
    rm(conn)
  })
}

#Get IR sub collection
getIRSubColl <- function(sub_coll = NULL, dois = FALSE) {
  jsonQry <- '{}'
  jsonFld <- '{"_id":1, "dois":0}'
  if (!is.null(sub_coll))
    jsonQry <- paste0("{\"_id\":\"", sub_coll, "\"}")
  if (!is.null(sub_coll) && dois)
    jsonFld <- '{"_id":0, "dois":1}'
  conn <- mongo(collection = "subcoll", url = mongo_db("app"))
  tryCatch({
    if(!is.null(sub_coll)) {
      lst <- conn$iterate(query = jsonQry, field = jsonFld)
      lst <- lst$one()
    } else {
      lst <- conn$find(query = jsonQry, field = jsonFld)
    }
    log_info("The IR sub-collection names have been successfully retrieved\n")
  }, error = function(e) {
    log_error("Following error occurred while retrieving IR sub-collection names from the database: {e}")
  }, finally = {
    rm(conn)
  })
  
  return(lst)
}

#Upload API Keys to database
uploadAPIKeys <- function(apiKeyObj) {
  lst <- fromJSON(apiKeyObj)
  jsonWhere <- paste0("{\"user_id\":", "\"", lst$user_id, "\"}")
  jsonSet <- paste0("{\"$set\":{\"elsevier_api_key\":", "\"", lst$elsevier_api_key, "\",", 
                    "\"springer_api_key\":", "\"", lst$springer_api_key, "\",", 
                    "\"wiley_api_key\":", "\"", lst$wiley_api_key, "\",",
                    "\"azure_openai_api_key\":", "\"", lst$azure_openai_api_key, "\",",
                    "\"azure_openai_api_endpoint\":", "\"", lst$azure_openai_api_endpoint, "\",",
                    "\"azure_openai_api_version\":", "\"", lst$azure_openai_api_version, "\",",
                    "\"blablador_api_key\":", "\"", lst$blablador_api_key, "\"}}")
  conn <- mongo(collection = "api_key", url = mongo_db("app"))
  tryCatch({
    conn$update(jsonWhere, jsonSet, upsert = TRUE)
    log_info("The API Keys have been successfully uploaded.\n")
  }, error = function(e) {
    log_error("Following error occurred while uploading API Keys to the database: {e}")
  }, finally = {
    rm(conn)
  })
}

#Get api key from database
getAPIKeys <- function(user_id) {
  jsonQry <- paste0("{\"user_id\":\"", user_id,  "\"}")
  jsonFld <- '{"user_id":0, "_id":0}'
  conn <- mongo(collection = "api_key", url = mongo_db("app"))
  tryCatch({
    dt <- conn$find(query = jsonQry, fields = jsonFld)
  }, finally = {
    rm(conn)
  })
  return(dt)
}

#Get IR eval collection from database
getIRData_eval <- function() {
  conn <- mongo(collection = "index", url = mongo_db("app"))
  tryCatch({
    lst <- conn$aggregate('[{"$sample": { "size": 250 }}]', pagesize = 10000, iterate = TRUE)
#    %>%  
#    apply(1, function(x) list(x)) %>%
#      unlist(recursive = F, use.names = F) %>%
#      apply(1, function(x) list(x)) %>%
#      unlist(recursive = F, use.names = F)
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  return(lst)
}

#Upload IR collection results to database for a specific job id
uploadIRData_eval <- function(vecDocs) {
  if (length(vecDocs) > 0) {
    #part1 upload the indexing data in global.index collection
    conn <- mongo(collection = "index_eval_250", url = mongo_db("app"))
    tryCatch({
      conn$insert(vecDocs)
      log_info("The index-ready document has been uploaded to app.index collection")
    }, error = function(e) {
      log_error("Following error occurred while uploading index-ready document to the database: {e}")
    }, finally = {
      rm(conn)
    })
  } else {
    log_info("The index-ready document is empty and therefore cannot be uploaded")
  }
}

# get synonyms
getSynonyms_old <- function(domain_name) {
  jsonQry <- paste0("{\"_id\":\"",  gsub(" ", "_", domain_name),  "\"}")
  jsonFld <- '{"_id":0, "synonym_rules": 1}'
  conn <- mongo(collection = "synonym", url = mongo_db("app"))
  tryCatch({
    dt <- conn$find(query = jsonQry, fields = jsonFld)
  }, finally = {
    rm(conn)
  })
  if (is.null(dt$synonym_rules))
    return("")
  
  synRules <- gsub("<br>", "\n", dt$synonym_rules)
  return(synRules)
  # if (is.null(domain_name))
  #   return(404)
  # domain_name <- gsub("_", " ", domain_name)
  # file_path <- file.path(Sys.getenv("SYNONYM_BASE_PATH"), domain_name, "synonyms.txt")
  # if(file.exists(file_path)) {
  #   synObj <- readtext(file_path)
  #   return(synObj$text)
  # } else
  #   return("")
}

# update synonyms
saveSynonyms_old <- function(synObj) {
  lst <- fromJSON(synObj)
  jsonWhere <- paste0("{\"_id\":", "\"",  gsub(" ", "_", lst$domain_name), "\"}")
  jsonSet <- paste0("{\"$set\":{\"synonym_rules\":", "\"", gsub("\n", "<br>", lst$synonym_rules), "\",", 
                    "\"domain_name\":", "\"", lst$domain_name, "\" }}")
  conn <- mongo(collection = "synonym", url = mongo_db("app"))
  tryCatch({
    conn$update(jsonWhere, jsonSet, upsert = TRUE)
    log_info("The synonym rules have been successfully uploaded.\n")
  }, error = function(e) {
    log_error("Following error occurred while uploading synonym rules to the database: {e}")
  }, finally = {
    rm(conn)
  })
}

#Get synonym domain names
getSynonymDomainNames <- function() {
  jsonQry <- '{}'
  jsonFld <- '{"_id":0, "domain_name":1}'
  conn <- mongo(collection = "synonym_test", url = mongo_db("app"))
  tryCatch({
    dt <- conn$find(query = jsonQry, field = jsonFld)
  }, error = function(e) {
    log_error("Following error occurred while retrieving domain name from the database: {e}")
  }, finally = {
    rm(conn)
  })
  
  if (is.null(dt$domain_name)) {
    log_info("No domain info data found")
    return()
  }
  
  return(dt$domain_name)
}

#Get doi_norm
getDOINorm <- function(doi) {
  jsonQry <- paste0("{\"_id\":\"", doi, "\"}")
  jsonFld <- '{"_id":0, "doi_norm":1}'
  conn <- mongo(collection = "collect", url = mongo_db("app"))
  tryCatch({
    out <- conn$find(query = jsonQry, field = jsonFld)
  }, error = function(e) {
    log_error("Following error occurred while retrieving doi norm from the database: {e}")
  }, finally = {
    rm(conn)
  })
  if (length(out) == 0) {
    log_info("No doi norm found")
    doi_norm <- character()
  } else {
    doi_norm <-out$doi_norm
  }
  return(doi_norm)
}

#Get doi_norm
getDOIsNorm <- function(dois) {
  if (!is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"_id\": {\"$in\": [", dois, "]}}")
  }
  jsonFld <- '{"_id":0, "doi_norm":1}'
  conn <- mongo(collection = "collect", url = mongo_db("app"))
  tryCatch({
    out <- conn$find(query = jsonQry, field = jsonFld)
  }, error = function(e) {
    log_error("Following error occurred while retrieving doi norm from the database: {e}")
  }, finally = {
    rm(conn)
  })
  if (length(out) == 0) {
    log_info("No doi norm found")
    doi_norm <- character()
  } else {
    doi_norm <- out$doi_norm
  }
  return(doi_norm)
}

#Get doi_norm
getDOIs <- function(dois_norm) {
  if (is.null(dois_norm) || length(dois_norm) == 0) return(character())
  
  # Prepare query for input dois_norm
  dois_quoted <- paste(shQuote(dois_norm, type = "cmd"), collapse = ", ")
  jsonQry <- paste0('{"doi_norm": {"$in": [', dois_quoted, ']}}')
  
  # Fetch both _id and doi_norm from MongoDB for mapping
  jsonFld <- '{"_id":1, "doi_norm":1}'
  
  conn <- mongo(collection = "collect", url = mongo_db("app"))
  out <- tryCatch({
    conn$find(query = jsonQry, fields = jsonFld)
  }, error = function(e) {
    log_error("Error retrieving doi_norm: {e}")
    data.frame()
  }, finally = {
    rm(conn)
  })
  
  if (nrow(out) == 0) {
    log_info("No doi norm found")
    return(character())
  }
  
  # Create named vector: names = doi_norm, values = _id
  matched <- setNames(as.character(out$`_id`), out$doi_norm)
  
  # Return _id values in order of input dois_norm; missing ones will be NA
  result <- matched[dois_norm]
  
  return(result)
}



getSynQuery <- function(domain_name = NULL) {
  if (is.null(domain_name)) {
    query <- 
      '[
          {
            "$project": {
              "_id": 0,
              "synonyms_set": 1
            }
          }
       ]'
  }else {
    # get domain
    query <- sprintf('
      [
        {
          "$match": {
            "synonyms_set.id": {
              "$regex": "^%s"
            }
          }
        },
        {
          "$project": {
            "synonyms_set": {
              "$filter": {
                "input": "$synonyms_set",
                "as": "item",
                "cond": {
                  "$regexMatch": {
                    "input": "$$item.id",
                    "regex": "^%s"
                  }
                }
              }
            }
          }
        },
        {
          "$unwind": "$synonyms_set"
        },
        {
          "$group": {
            "_id": null,
            "synonyms": {
              "$push": "$synonyms_set.synonyms"
            }
          }
        },
        {
          "$project": {
            "_id": 0,
            "synonyms": {
              "$reduce": {
                "input": "$synonyms",
                "initialValue": "",
                "in": {
                  "$concat": [
                    "$$value",
                    {
                      "$cond": [
                        {
                          "$eq": [
                            "$$value",
                            ""
                          ]
                        },
                        "",
                        "\\n"
                      ]
                    },
                    "$$this"
                  ]
                }
              }
            }
          }
        }
      ]
    ', domain_name, domain_name)
  }
  return(query)
}

# get synonyms
getSynonyms <- function(domain_name = NULL) {
  if (is.null(domain_name))
    return("")
  query <- getSynQuery(domain_name)
  conn <- mongo(collection = "synonym", url = mongo_db("app"))
  tryCatch({
    dt <- conn$aggregate(query)
  }, finally = {
    rm(conn)
  })
  if (is.null(dt$synonyms))
    return("")
  return(dt$synonyms)
}

# get synonyms
getAllSynonyms <- function() {
  query <- getSynQuery()
  conn <- mongo(collection = "synonym", url = mongo_db("app"))
  tryCatch({
    dt <- conn$aggregate(query)
  }, finally = {
    rm(conn)
  })
  if (!length(dt))
    return("")
  return(toJSON(dt, auto_unbox = T))
}

getLastSynNoQuery <- function(domain_name) {
  query <- sprintf('[
    {
      "$match": {
        "synonyms_set.id": {
          "$regex": "^%s"
        }
      }
    },
    {
      "$project": {
        "_id": 0,
        "synonyms_set": {
          "$filter": {
            "input": "$synonyms_set",
            "as": "item",
            "cond": {
              "$regexMatch": {
                "input": "$$item.id",
                "regex": "^%s"
              }
            }
          }
        }
      }
    },
    {
      "$project": {
        "last_id": {
          "$arrayElemAt": ["$synonyms_set.id", -1]
        }
      }
    },
    {
      "$project": {
        "last_number": {
          "$toInt": {
            "$arrayElemAt": [
              { "$split": ["$last_id", "-"] },
              1
            ]
          }
        }
      }
    }
  ]', domain_name, domain_name)
  return(query)
}

# get last number from the database
getLastSynNo <- function(domain_name) {
  query <- getLastSynNoQuery(domain_name)
  conn <- mongo(collection = "synonym_test", url = mongo_db("app"))
  tryCatch({
    dt <- conn$aggregate(query)
  }, finally = {
    rm(conn)
  })
  if (!length(dt))
    return(0)
  return(dt$last_number)
}

# save synonyms
saveSynonyms <- function(synObj) {
  lst <- fromJSON(synObj)
  domain_name <- gsub(" ", "_", lst$domain_name)
  last_index <- getLastSynNo(domain_name)
  synonym_rules_list <- strsplit(lst$synonym_rules, "\n")[[1]]
  conn <- mongo(collection = "synonym_test", url = mongo_db("app"))
  configObj <- read_json(Sys.getenv("ES_CONFIG_PATH"))
  
  tryCatch({
    results <- sapply(seq_along(synonym_rules_list), function(i) {
      synonym <- list(
        id = sprintf("%s-%d", domain_name, last_index + i),
        synonyms = synonym_rules_list[[i]]
      )
      
      update_query <- sprintf(
        '{"$push": {"synonyms_set": %s}}',
        toJSON(synonym, auto_unbox = TRUE)
      )
      conn$update(
        query = '{}',
        update = update_query
      )
      
      resp <- putSynonymRule(synrule_id = synonym$id, synrule = synonym$synonyms, configObj = configObj)
      if (!resp) {
        log_info(paste0("error while uploading synonym rules with id ", synonym$id, " to elasticsearch"))
      }
    })
    
    if (all(results)) {
      log_info("The synonym rules have been successfully uploaded into the database.\n")
    } else if (any(results)) {
      log_info("Some synonym rules have been successfully uploaded into the database.\n")
    } else {
      log_info("The synonym rules couldn't be uploaded into the database.\n")
    }
  }, error = function(e) {
    log_error("Following error occurred while uploading synonym rules: {e}")
  }, finally = {
    rm(conn)
  })
}


# Get fulltext

#Get IR collection from database
getFulltext <- function(dois = NULL) {
  jsonQry <- "{}"
  jsonFld <- '{"_id":0, "body":1}'
  
  if (!is.null(dois)) {
    dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
    jsonQry <- paste0("{\"_id\": {\"$in\": [", dois, "]}}")
  }
  conn <- mongo(collection = "index", url = mongo_db("app"))
  tryCatch({
    lst <- conn$find(query = jsonQry, fields = jsonFld) %>%
      apply(1, function(x) list(x)) %>%
      unlist(recursive = F, use.names = F)
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  if (length(lst) == 0)
    log_info("No IR data found")
  return(unlist(lst))
}

#Get IR collection from database
getODEDataset <- function(sub_coll = NULL) {
  if (is.null(sub_coll))
    return(NULL)
  
  jsonQry <- paste0("{\"_id\":\"", sub_coll, "\"}")
  jsonFld <- '{"dois.doi": 1, "_id": 0}'

  conn <- mongo(collection = "subcoll", url = mongo_db("app"))
  tryCatch({
    dois <- conn$find(query = jsonQry, fields = jsonFld)
    dois <- unlist(dois$dois, use.names = F)
    rm(conn)
    
    if (!is.null(dois)) {
      dois <- paste(shQuote(dois, type="cmd"), collapse=", ")
      jsonQry <- paste0("{\"_id\": {\"$in\": [", dois, "]}}")
    }
    jsonFld <- '{"_id":1, "title":1}'
    conn <- mongo(collection = "collect", url = mongo_db("app"))
    dt <- conn$find(query = jsonQry, fields = jsonFld)
  }, warning = function(w) {
    message(paste("Warning while data fetching:", w, sep = "\n")) 
  }, finally = {
    rm(conn)
  })
  if (nrow(dt) == 0)
    log_info("No data found")
  setnames(dt, "_id", "doi")
  return(dt)
}


# function to get relevant sentences against the user query
getRelevantSentences <- function(query, ir_subcoll, sentence_model, sentence_per_passage, rel_passages, sentence_similarity_threshold, adj_sentences, lstSentenceModel) {
  documents <- getSentenceEmbeddings(subcoll_name=ir_subcoll, sentence_model=sentence_model, sentence_per_passage=sentence_per_passage)
  if (!nrow(documents))
    return(NULL)
  sentence_model <- get(sentence_model, lstSentenceModel)
  relevant_sentences <- extract_relevant_sentences(documents=documents, 
                                                   query=query, 
                                                   sentence_model=sentence_model,
                                                   sentence_per_passage=as.double(sentence_per_passage),
                                                   top_k=as.double(rel_passages),
                                                   similarity_threshold=sentence_similarity_threshold,
                                                   adjacent=as.double(adj_sentences),
                                                   num_workers=Sys.getenv("NUM_OF_WORKERS"))
  return(relevant_sentences)
}

#Get data extraction schema names
getDESchemaNames <- function() {
  jsonQry <- '{}'
  jsonFld <- '{"_id":0, "schema_names":1}'
  conn <- mongo(collection = "de_schema_names", url = mongo_db("app"))
  tryCatch({
    dt <- conn$find(query = jsonQry, field = jsonFld)
  }, error = function(e) {
    log_error("Following error occurred while retrieving data extraction schema names from the database: {e}")
  }, finally = {
    rm(conn)
  })
  
  if (is.null(dt$schema_names)) {
    log_info("No data extraction schema names found")
    return()
  }
  
  return(dt$schema_names[[1]])
}

#Save data extraction schema name
saveDESchemaName <- function(de_schema_name) {
  if (missing(de_schema_name) || de_schema_name == "") {
    log_error("Invalid schema name provided for saving.")
    return(FALSE)
  }
  
  conn <- mongo(collection = "de_schema_names", url = mongo_db("app"))
  tryCatch({
    conn$update(
      query = '{}',
      update = sprintf('{"$addToSet": {"schema_names": "%s"}}', de_schema_name),
      upsert = TRUE
    )
    
    log_info("Successfully saved schema name: {de_schema_name}")
    return(TRUE)
  }, error = function(e) {
    log_error("Error saving data extraction schema name: {e}")
    return(FALSE)
  }, finally = {
    rm(conn)
  })
}

