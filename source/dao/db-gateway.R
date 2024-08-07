#db-gateway.R
#
#This file contains functions to retrieve or
#push data into database
#
#

#Create a new user in database/ Logon
createNewUser <- function(username, password) {
  dt <- data.table(username = username, password = password)
  conn <- mongo(collection = "user", url = mongo_db("sys"))
  
  tryCatch({
    conn$insert(dt)
  }, finally = {
    rm(conn)
  })
  
  return()
}

#Credentials authentication/ Login
authenticateUser <- function(username, password) {
  jsonQry <- '{}'
  jsonFld <- '{"password":1}'
  jsonQry <- paste0("{\"username\":\"", username, "\"}")
  conn <- mongo(collection = "user", url = mongo_db("sys"))
  
  tryCatch({
    dt <- data.table(conn$find(query = jsonQry, fields = jsonFld))
    if (!(nrow(dt) && password_verify(dt$password, password))) {
      statusCode <- 401
      stop()
    }
    statusCode <- 200
  }, finally = {
    rm(conn)
    return(statusCode)
  })
}

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
  tryCatch({
    conn$insert(irResultObj)
    #log_info("The search results have been successfully uploaded.\n")
  }, error = function(e) {
    log_error("Following error occurred while uploading IR results to the database: {e}")
  }, finally = {
    rm(conn)
  })
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

# #Get domain information
# getDomainInfo <- function(mining_opts) {
#   jsonQry <- '{}'
#   if (mining_opts)
#     jsonFld <- '{"_id":1, "domain_name":1, "mining_opts":1}'
#   else
#     jsonFld <- '{"_id":1, "domain_name":1, "mining_opts":0}'
#   conn <- mongo(collection = "domain", url = mongo_db("app"))
#   tryCatch({
#     lst <- conn$find(query = jsonQry, field = jsonFld)
#     log_info("The domain info has been successfully retrieved\n")
#   }, error = function(e) {
#     log_error("Following error occurred while retrieving domain info from the database: {e}")
#   }, finally = {
#     rm(conn)
#   })
#   
#   if (length(lst) == 0) {
#     log_info("No domain info data found")
#   } else {
#     if (mining_opts)
#       names(lst) <- c("_id", "domain_name", "mining_opts")
#     else
#       names(lst) <- c("_id", "domain_name")
#   }
#   return(lst)
# }
# 
# #Create domain information
# CreateDomainInfo <- function(domain_name, mining_opts) {
#   lst <- list("_id"=gsub(" ", "_", domain_name), "domain_name"=domain_name, "mining_opts"=mining_opts)
#   doc <- toJSON(lst, auto_unbox = TRUE)
#   conn <- mongo(collection = "domain", url = mongo_db("app"))
#   tryCatch({
#     conn$insert(doc)
#     log_info("A domain {domain_name} info has been successfully created.\n")
#   }, error = function(e) {
#     log_error("Following error occurred while uploading domain info to the database: {e}")
#   }, finally = {
#     rm(conn)
#   })
# }


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
                    "\"wiley_api_key\":", "\"", lst$wiley_api_key, "\" }}")
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
  conn <- mongo(collection = "synonym_test", url = mongo_db("app"))
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







# 
# domain_name <- "materials science"
# mining_opts <- list("qty", "pltdgtz", "tbl", "matcomp", "miscner")


# Acronyms for mining options for materials science domain

# qty      - quantities from text
# pltdgtz  - plot digitization
# tbl      - table
# matcomp  - material composition
# miscner  - mics named entity recognition from text

# 
# 
# 
# 
# 
# 
# getUUID <- function(api) {
#   CONN <- connectDB()
#   qry <- paste0("SELECT [UUID] FROM ", TABLE_API,
#                 " WHERE ", "[API] = '", api, "'")
#   res <- dbSendQuery(CONN, qry)
#   dt <- dbFetch(res)
#   dbClearResult(res)
#   disconnectDB(CONN)
#   dt <- data.table(dt)
#   return(dt)
# }
# 
# getSearchUUIDs <- function() {
# 
# }
# 
# getRawData <- function() {
#   
# }
# 
# getKDData <- function() {
#   
# }
# 
# getAnalysisData <- function() {
#   
# }
# 
# getModel <- function() {
#   
# }
# 
# uploadRawData <- function(dt) {
#   if (nrow(dt) > 0){
#     CONN <- connectDB()
#     dbWriteTable(CONN, TABLE_RAWDATA, dt)
#     disconnectDB(CONN)
#   } else {
#     cat("Data object is empty and thefore nothing can be uploaded.")
#   }
# }
# 
# uploadKDData <- function() {
#   
# }
# 
# uploadAnalysisData <- function() {
#   
# }
# 
# uploadModel <- function() {
#   
# }
# 
