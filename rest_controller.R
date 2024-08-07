# plumber.R


#* Logon Service
#* @param username
#* @param password
#* @post /logon
function(username, password) {
  hash <- password_store(password)
  createNewUser(username, hash)
}

#* Login Service
#* @param username
#* @param password
#* @post /login
function(username, password) {
  statusCode <- authenticateUser(username, password)
  return(statusCode)
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
#* @param qryObj JSON object with search query parameters
#* @post /postIRSearch
function(qryObj) {
  configObj <- read_json(Sys.getenv("ES_CONFIG_PATH"))
  result <- searchEngine(qryObj, configObj) %>%
    fromJSON(simplifyVector = T)
}

#* Save Search Results
#* @param irResultObj JSON object with search result
#* @post /saveIRResult
function(irResultObj) {
  save_result <- uploadIRResult(irResultObj) #%>%
    #fromJSON(simplifyVector = T)
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