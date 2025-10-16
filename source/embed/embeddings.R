
# update (temporary function)
updateDocWithEmbd <- function(coll_name, model_path) {
  device <- get_device()
  device <- "cpu"
  tryCatch({
    conn <- mongo(collection = coll_name, url = mongo_db("app"))
    dt <- data.table(conn$find(query = '{}', field = '{"_id":1, "body": 1}'))
    rm(conn)
    model <- load_model(model_path, device)
    tokenizer <- load_tokenizer(model_path)
    for (i in 1:NROW(dt)) {
      body <- dt[["body"]][i]
      if (nchar(body)) {
        embeds <- generate_embeddings(body, model, tokenizer, 8192, 4096, device)
        embeds <- toJSON(embeds, digits = NA, auto_unbox = TRUE)
        jsonWhere <- paste0("{\"_id\":", "\"",  dt[["_id"]][i], "\"}")
        jsonSet <- paste0("{\"$set\":{\"embeddings\":", embeds, "}}")
      } else {
        jsonWhere <- paste0("{\"_id\":", "\"",  dt[["_id"]][i], "\"}")
        jsonSet <- paste0("{\"$unset\":{\"embeddings\":", "\"\"", "}}")
      }
      conn <- mongo(collection = coll_name, url = mongo_db("app"))
      conn$update(jsonWhere, jsonSet, upsert = TRUE)
    }
  }, finally = {
    rm(conn)
    gc(full = TRUE)
    clear_gpu_memory()
  })
}


getQueryEmbeddings <- function(query, model_path) {
  tryCatch({
    device <- get_device()
    model <- load_model(model_path, device)
    tokenizer <- load_tokenizer(model_path)
    embeds <- generate_embeddings(query, model, tokenizer, 8192, 4096, device)
    embeds_vec <- unlist(embeds)
    #embeds_vec <- paste(embeds[[1]], collapse = ",")
  }, finally = {
    gc(full = TRUE)
    clear_gpu_memory()
  })
  return(embeds_vec)
}


createSentenceEmbeddings <- function(subcoll_name = "gTiAl_minCR", sentence_model_name = "all-MiniLM-L6-v2", max_passage_count = 5) {
  tryCatch({
    embed_coll_name <- paste(subcoll_name, sentence_model_name, sep = "_")
    conn <- mongo(collection = "subcoll", url = mongo_db("app"))
    qry <- paste0("{\"_id\":", "\"", subcoll_name, "\"}")
    dt <- data.table(conn$find(query = qry, field = '{}'))
    rm(conn)
    dois <- unlist(sapply(dt$dois, function(x) x$doi, simplify = FALSE))
    documents <- getFulltext(dois)
    dois_norm <- getDOIsNorm(dois)
    attr(documents, "names") <- dois_norm
    sentence_model <- load_sentence_model(sentence_model_name)
    coll_name <- paste0(subcoll_name, "_", sentence_model_name)
    for (i in seq_along(documents)) {
      doc <- documents[i]
      if (nchar(doc)) {
        lst <- generate_sentence_embeddings(doc, sentence_model, max_passage_count=5L)
        sentenceObj <- toJSON(lst$sentences, digits = NA, auto_unbox = TRUE)
        
        jsonWhere <- paste0("{\"_id\":", "\"", names(doc), "\"}")
        
        # Initialize `jsonSet` with the sentences part
        jsonSet <- paste0("{\"$set\":{\"sentences\":", sentenceObj)
        
        # Convert embeddings in the list to JSON and dynamically build `jsonSet`
        for (j in seq_len(max_passage_count)) {
          embedding_json <- toJSON(lst[[paste0("embeddings_", j)]], digits = NA, auto_unbox = TRUE)
          
          # Append the embedding JSON to `jsonSet`
          jsonSet <- paste0(jsonSet, ",\"embeddings_", j, "\":", embedding_json)
        }
        
        # Close the JSON structure
        jsonSet <- paste0(jsonSet, "}}")

      } else {
        jsonWhere <- paste0("{\"_id\":", "\"", names(doc), "\"}")
        emptyObj <- toJSON(list(), digits = NA, auto_unbox = TRUE)
        jsonSet <- paste0("{\"$set\":{\"", "sentences", "\":", emptyObj, paste0("\"embeddings_", 1:max_passage_count, "\":", emptyObj, collapse = ",") , "}}")
      }
      conn <- mongo(collection = coll_name, url = mongo_db("app"))
      conn$update(jsonWhere, jsonSet, upsert = TRUE)
      gc(full = TRUE)
    }
  }, finally = {
    rm(conn)
    gc(full = TRUE)
    #clear_gpu_memory()
  })
}


getSentenceEmbeddings <- function(subcoll_name = "gTiAl_minCR", sentence_model = "all-MiniLM-L6-v2", sentence_per_passage=3) {
  tryCatch({
    embeddings_key <- paste0("embeddings_", sentence_per_passage)
    jsonQry <- "{}"
    jsonFld <- paste0("{\"_id\":1, \"sentences\":1, \"", embeddings_key, "\":1}")
    conn <- mongo(collection = paste(subcoll_name, sentence_model, sep = "_"), url = mongo_db("app"))
    dt <- conn$find(query = jsonQry, fields = jsonFld)
  }, finally = {
    rm(conn)
    gc(full = TRUE)
  })
  return(dt)
}

# case when all relevant sentences from all documents are combined first to generate the summary
#query <- "How does microstructure influence creep?"
#documents <- getSentenceEmbeddings()
#sentence_model <- load_sentence_model()
#relevant_sentences <- extract_relevant_sentences(documents=documents, query=query, sentence_model=sentence_model, top_k=5L, similarity_threshold=0.3, adjacent=0L)
#combined_sentences <- combined_sentences(relevant_sentences)
#all_sentences <- paste(combined_sentences, collapse = " ")
#summarizer_model <- load_summarizer_model()
#summarizer_tokenizer <- load_summarizer_tokenizer()
#final_summary <- summarize_iteratively(sentences=all_sentences, summarizer_model=summarizer_model, summarizer_tokenizer=summarizer_tokenizer, chunk_size=768L, max_summary_length=1024L, stride=0.5)

# case when a summary is generated per document and then the summary is combined to generate a final summary
#query <- "How does microstructure influence creep?"
#documents <- getSentenceEmbeddings()
#sentence_model <- load_sentence_model()
#relevant_sentences <- extract_relevant_sentences(documents=documents, query=query, sentence_model=sentence_model, top_k=3L, similarity_threshold=0.3, adjacent = 1L)
#summarizer_model <- load_summarizer_model()
#summarizer_tokenizer <- load_summarizer_tokenizer()
#doc_5_relevant_sentences <- relevant_sentences[1:5]
#doc_5_relevant_sentences <- relevant_sentences
#summary <- list()
#for (i in seq_along(doc_5_relevant_sentences)) {
#  sentences <- paste(doc_5_relevant_sentences[[i]], collapse = " ")
#  summary[i] <- summarize_iteratively(sentences=sentences, summarizer_model=summarizer_model, summarizer_tokenizer=summarizer_tokenizer, chunk_size=768L, max_summary_length=1024L, stride=0.5)
#}
#combined_summary <- paste(unlist(summary), collapse = " ")
#final_summary <- summarize_iteratively(sentences=combined_summary, summarizer_model=summarizer_model, summarizer_tokenizer=summarizer_tokenizer, chunk_size=768L, max_summary_length=1024L, stride=0.5)

