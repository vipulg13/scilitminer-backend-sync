# plumber.R

fastapi_v1_doc_embeddings <- paste0(Sys.getenv("LLM_SERVICE_ENDPOINT"), "embeddings")
fastapi_v1_tqa <- paste0(Sys.getenv("LLM_SERVICE_ENDPOINT"), "tqa")

lstSentenceModel <- list()
lstSentenceModel$`all-MiniLM-L6-v2` <- load_sentence_model(model_name="all-MiniLM-L6-v2", model_zoo_path = Sys.getenv("MODEL_ZOO_PATH"))
lstSentenceModel$`all-MiniLM-L12-v2` <- load_sentence_model(model_name="all-MiniLM-L12-v2", model_zoo_path = Sys.getenv("MODEL_ZOO_PATH")) 

#* @plumber
function(pr) {
  pr <- pr |>
    # Overwrite the default serializer to return more digits
    pr_set_serializer(serializer_json(digits = 22))
}


#* Get Jobs
#* @param job_type
#* @param user_id
#* @param job_status
#* @get /getJobs
function(job_type, user_id=NA, job_status=NA) {
  getJobs(job_type, user_id, job_status)
}

#* Create a collection generator job
#* @param jobObj JSON object with collection generator job parameters
#* @post /createJob
function(jobObj) {
  createNewJob(jobObj)
}

#* Get UserID
#* @param username
#* @get /userID/<username>
function(username) {
  getUserID(username)
}

#* Get Search Results
#* @param reqObj JSON object with search query parameters
#* @param search_service
#* @post /postIRSearch
function(reqObj, search_service) {
  configObj <- read_json(Sys.getenv("ES_CONFIG_PATH"))
  if (search_service == "semantic") {
    lstReq <- fromJSON(reqObj)
    qryObj <- lstReq$query
    configObj$similarity_threshold <- lstReq$similarity_threshold
    #print(lstReq$similarity_threshold)
    resp <- httr::GET(fastapi_v1_doc_embeddings, query = list(text = qryObj), encode = "json")
    if (resp$status_code != 200) {
      return(resp$status_code)
    }
    qryObj <- unlist(httr::content(resp))
    result <- searchEngine(qryObj, configObj, search_service) %>%
      fromJSON(simplifyVector = T)
  } else {
    result <- searchEngine(reqObj, configObj, search_service) %>%
      fromJSON(simplifyVector = T) 
  }
}

#* Save Search Results
#* @param irResultObj JSON object with search result
#* @post /saveIRResult
function(irResultObj, res) {
  tryCatch({
    result <- uploadIRResult(irResultObj)
    if (result) {
      res$status <- 200
    }
  }, error = function(e) {
    res$status <- 500
  })
}

#* Get Domain info
#* @get /getDomainInfo
function() {
  getDomainInfo()
}

#* Get IR sub collection info
#* @get /getIRSubCollInfo
function() {
  getIRSubColl()
}

#* Get Mining Results
#* @param mineResultObj JSON object with mining parameters
#* @get /getMineResult
function(mineResultObj) {
  # call the function
}

#* Save API Keys 
#* @param apiKeyObj JSON object with api keys
#* @post /postAPIKeys
function(apiKeyObj) {
  save_apiK <- uploadAPIKeys(apiKeyObj) #%>%
  #fromJSON(simplifyVector = T)
}

#* Get API Keys 
#* @param account_id user id
#* @get /getAPIKeys
function(account_id) {
  getAPIKeys(user_id=account_id)
}

#* Get data extraction schema names 
#* @get /getDESchemaNames
function() {
  getDESchemaNames()
}

#* Save data extraction schema name
#* @param de_schema_name data extraction schema names
#* @post /postDESchemaName
function(de_schema_name) {
  saveDESchemaName(de_schema_name=de_schema_name)
}


#* Save synonyms 
#* @param synObj synonym obj
#* @post /postSynonyms
function(synObj) {
  saveSyn <- saveSynonyms(synObj) #%>%
  #fromJSON(simplifyVector = T)
}

#* Get synonyms 
#* @param domain_name domain name
#* @get /getSynonyms
function(domain_name) {
  getSynonyms(domain_name)
}

#* Get synonym domain names
#* @get /getSynonymDomainNames
function() {
  getDomainInfo()
}

#* text reasoning 
#* @param reqObj text question answer object
#* @post /postTQARequest
function(reqObj) {
  lstRsp <- list()
  lstReq <- fromJSON(reqObj)
  print("Function: getRelevantSetences")
  print(Sys.time())
  lstRlvntSent <- getRelevantSentences(query = lstReq$query, 
                                       ir_subcoll = lstReq$ir_subcoll, 
                                       sentence_model = lstReq$sentence_model,
                                       sentence_per_passage = lstReq$sentence_per_passage,
                                       rel_passages = lstReq$rel_passages, 
                                       sentence_similarity_threshold = lstReq$sentence_similarity_threshold, 
                                       adj_sentences = lstReq$adj_sentences, 
                                       lstSentenceModel = lstSentenceModel)
  print(Sys.time())
  if (is.null(lstRlvntSent)) {
    lstRsp$answer <- "The given source is empty"
    return(lstRsp)
  }
  combined_sentences <- combined_sentences_dict(lstRlvntSent)
  
  # call llm service
  lstTQA <- list(mdl_name=lstReq$qa_model,
                 query=lstReq$query,
                 source=combined_sentences
            )
  print("Function: LLM API Call")
  print(Sys.time())
  resp <- httr::POST(fastapi_v1_tqa,
                     body = lstTQA,
                     encode = "json")
  print(Sys.time())
  if (!resp$status_code == 200) {
    return(resp$status_code)
  } else {
    print("Function: Result Processing")
    print(Sys.time())
    jsonResp <- httr::content(resp)
    answerObj <- fromJSON(jsonResp$response)
    if (is.null(answerObj$answer) || answerObj$answer == "null") {
      lstRsp$answer <- "The given source does not provide any information related to the user query."
    } else {
      answer <- gsub("source_", "", answerObj$answer)
      lstRsp$answer <- answer
      lstRsp$relevant_data <- data.table(
        sequence = seq_along(lstRlvntSent),
        doi = getDOIs(names(lstRlvntSent)),
        passages = unlist(lapply(lstRlvntSent, function(x) 
          if (length(x) > 0) paste(x, collapse = " ") 
          else NA_character_)
        )
      )
    }
  }
  gc()
  print(Sys.time())
  return(lstRsp)
}