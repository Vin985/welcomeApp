library(ecapputils)
library(shinyStore)

LANG_DATA <- loadLanguages("lang.csv")

APP_URL <- read.csv2("appconf.csv", stringsAsFactors = FALSE,
                     encoding = "UTF-8")
