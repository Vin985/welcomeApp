# install.packages("DBI")
# devtools::install_github("rstudio/pool")

rm(list = ls())

library(ecapputils)
library(shinyjs)
library(DBI)
library(pool)
library(DT)


# library(shinyStore)

ROOT_DIR <- "C:/dev/EC/welcomeApp"

DB_DIR <- file.path(ROOT_DIR, "db")
DB_FILE <- "User.sqlite"
createDBPool()

source(file.path(ROOT_DIR, "pagesRender.R"))
source(file.path(ROOT_DIR, "loginUI.R"), encoding = "UTF-8")
source(file.path(ROOT_DIR, "loginServer.R"), encoding = "UTF-8")
source(file.path(ROOT_DIR, "adminServer.R"), encoding = "UTF-8")


PAGE_ADMIN <- "admin"
PAGE_APP <- "app"
PAGE_LANG <- "lang"


LANG_DATA <- loadLanguages("lang.csv")

CHANGE_LANG_EVENT <- "EVENT_changeLang"
CHANGE_PAGE_EVENT <- "EVENT_changePage"



readAppConf(file.path(ROOT_DIR, ".."))

onStop(function(){
  poolClose(DB_POOL)
})

# APP_URL <- read.csv2("appconf.csv", stringsAsFactors = FALSE,
#                      encoding = "UTF-8")
