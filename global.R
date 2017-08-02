library(ecapputils)
library(shinyjs)
# library(shinyStore)

source("loginUI.R", encoding = "UTF-8")

LANG_DATA <- loadLanguages("lang.csv")

CHANGE_LANG_EVENT <- "EVENT_changeLang"

ROOT_DIR <- "C:/dev/EC/welcomeApp"
readAppConf(file.path(ROOT_DIR, ".."))

APP_URL <- read.csv2("appconf.csv", stringsAsFactors = FALSE,
                     encoding = "UTF-8")
