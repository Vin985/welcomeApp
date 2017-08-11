# install.packages("DBI")
# devtools::install_github("rstudio/pool")

library(ecapputils)
library(shinyjs)
library(DBI)
library(pool)


# library(shinyStore)

ROOT_DIR <- "C:/dev/EC/welcomeApp"

DB_DIR <- file.path(ROOT_DIR, "db")
DB_FILE <- "User.sqlite"

source("serverRender.R")
source("loginUI.R", encoding = "UTF-8")
source("loginServer.R", encoding = "UTF-8")
source("dbutils.R", encoding = "UTF-8")




LANG_DATA <- loadLanguages("lang.csv")

CHANGE_LANG_EVENT <- "EVENT_changeLang"



readAppConf(file.path(ROOT_DIR, ".."))

APP_URL <- read.csv2("appconf.csv", stringsAsFactors = FALSE,
                     encoding = "UTF-8")
