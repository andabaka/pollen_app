
library(config)

config <- config::get()


# Connect to MongoDB
mongo_conn <- mongo(
  collection = config$mongo_collection,
  db = config$mongo_db,
  url = config$mongo_url
)

# Define users
users <- data.frame(
  username = c("user1", "user2", "user3"),
  password = c("pass1", "pass2", "pass3")
)

# Check if users already exist and insert them if they don't
for (i in 1:nrow(users)) {
  query <- sprintf('{"username": "%s"}', users$username[i])
  existing_user <- mongo_conn$find(query)
  
  if (nrow(existing_user) == 0) {
    # If the user doesn't exist, insert them
    mongo_conn$insert(users[i, ])
  }
}