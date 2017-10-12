STATUS_GUEST <- 0
STATUS_USER <- 1
STATUS_ADMIN <- 2
STATUS_ERROR <- -1

# create table user
createTableUser <- function(db = DB_POOL) {
  tryCatch({
    # cat("in: ", "createTableUser", "\n")
    dbExecute(
      conn = db,
      "CREATE TABLE IF NOT EXISTS `User` (
      `id`        INTEGER PRIMARY KEY,
      `username`	VARCHAR(255) NOT NULL UNIQUE,
      `email`	    VARCHAR(255),
      `password`	VARCHAR(255) NOT NULL,
      `admin`	    INTEGER(1) DEFAULT 0
    );"
)
    #dbDisconnect(db)
  }, error = function(e) {
    print(e)
  })
}

##
# initialize user DB: connect and creatTable if not exists
#
initDB <- function(dbdir, dbfile) {
  # cat("in: ", "initdb", "\n")
  if (!dir.exists((dbdir))) {
    dir.create(dbdir)
  }

  pool <- dbPool(drv = RSQLite::SQLite(),
                 dbname = file.path(dbdir, dbfile))
  createTableUser(pool)
  return(pool)
}

# add user with email
addUser <- function(username = "",
                    email = "",
                    password = "") {
  # cat("in: ", "adduser", "\n")
  tryCatch({
    query <-
      paste0(
        "INSERT INTO User(username, email, password) VALUES ('",
        username,
        "', '" ,
        email,
        "', '" ,
        password,
        "')"
      )
    dbExecute(conn = db, query)
  }, error = function(e) {
    print(e)
  })
}

# createNewUserFromList
addUsersFromList <- function(db = DB_POOL) {
  addUser(db,
          username = values["username"],
          email = values["email"],
          password = values["password"])
}


# delete user
deleteUserByUsername <- function(username = "", db = DB_POOL) {
  tryCatch({
    dbExecute(conn = db,
              paste0("DELETE FROM User where username='", username, "'"))
  }, error = function(e) {
    print(e)
  })
}


# delete user by ID
deleteUserByID <- function(id, db = DB_POOL) {
  tryCatch({
    dbExecute(conn = db,
              paste0("DELETE FROM User where id= ", id, " ;"))

  }, error = function(e) {
    print(e)
  })
}


# change user information
updateUser <- function(username = "",
                       email = "",
                       password = "",
                       db = DB_POOL) {
  cat("in: ", "updateuser", "\n")
  tryCatch({
    q <-
      paste0(
        "UPDATE User SET `email` = '",
        email   ,
        "' , `password` = '",
        password,
        "'  WHERE `username` = '",
        username,
        "';"
      )
    dbExecute(conn = db, q)
  }, error = function(e) {
    print(e)
  })
}


# update user from list
updateUserFromListByID <- function(values, db = DB_POOL) {
  #cat("data to update*:  ", "email: ", values["email"], " password: ", values["password"], " username: ", values["username"],"\n")
  q <- paste0(
    "UPDATE User SET `email` = '",
    values["email"],
    "' , `password` = '",
    values["password"],
    "' , `username` = '",
    values["username"],
    "'  WHERE `id` = ",
    values["id"],
    ";"
  )
  dbExecute(conn = db, q)
}



# read user
getUser <- function(username = "", db = DB_POOL) {
  # cat("in: ", "readuser", "\n")
  query <-
    paste0("SELECT * FROM User WHERE `username`  = '", username, "'")
  dbGetQuery(conn = db, query)
}

# get userById
getUserById <- function(id, db = DB_POOL) {
  # cat("in: ", "geruserbyid", "\n")
  df <-
    dbGetQuery(db, paste0("select * from User where id = " , id))
  return(df)
}

# get user id from username
getUserId <- function(username, db = DB_POOL) {
  # cat("in: ", "getuserid", "\n")
  user.id <-
    dbGetQuery(db,
               paste0(
                 "select id, username, email from User where username = " ,
                 " '",
                 username,
                 "' ; "
               ))
  return(user.id)
}

# get all users
getUsers <- function(db = DB_POOL) {
  # cat("in: ", "getUsers", "\n")
  df <- dbGetQuery(db, 'select id, username, email from User')
  return(df)
}

userExists <- function(username = "", db = DB_POOL) {
  # cat("in: ", "userexists", "\n")
  user <- getUser(db, username = username)
  if (is.na(user$username[1]))
    return(FALSE)
  return(TRUE)
}

# check username/password match
credentialsMatch <- function(username = "",
                             password = '',
                             db = DB_POOL) {
  # cat("in: ", "credentialsmatch", "\n")
  a <- getUser(db, username = username)

  if (is.na(a$username[1])) {
    # User does not exist
    return(STATUS_GUEST)
  }

  # Check credentials
  if (username == a$username[1] & password == a$password[1]) {
    if (a$admin == 1) {
      # User is admin
      return(STATUS_ADMIN)
    } else {
      # Normal user
      return(STATUS_USER)
    }
  } else {
    # Wrong password
    return(STATUS_ERROR)
  }

}

##
## Create database pool
##
if (!exists("DB_DIR") || !exists("DB_FILE")) {
  stop(
    "Variables DB_DIR with path to the database repository and DB_FILE with database file name are required"
  )
}
DB_POOL <- initDB(DB_DIR, DB_FILE)
