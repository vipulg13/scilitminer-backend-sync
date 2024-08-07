#db-connection.R
#
#This file contains a function to connect to the database
#
#

mongo_db <- function(db_name = NULL) {
  if (is.null(db_name)) {
    conn <- paste0("mongodb://",
                   Sys.getenv("MONGO_USR"),
                   ":",
                   Sys.getenv("MONGO_PWD"),
                   "@",
                   Sys.getenv("MONGO_HOST"),
                   ":",
                   Sys.getenv("MONGO_PORT"),
                   "/",
                   "app",
                   "?authSource=admin")
  } else {
    conn <- paste0("mongodb://",
                   Sys.getenv("MONGO_USR"),
                   ":",
                   Sys.getenv("MONGO_PWD"),
                   "@",
                   #"localhost",
                   Sys.getenv("MONGO_HOST"),
                   ":",
                   Sys.getenv("MONGO_PORT"),
                   "/",
                   db_name,
                   "?authSource=admin")
  }
  return(conn)
}
