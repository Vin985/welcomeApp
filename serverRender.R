require(shiny)
require(shinyjs)
require(ecapputils)


selectLanguageRender <- function(input, output, session, lang) {
  renderUI({
    column(
      6,
      offset = 3,
      div(class = "masthead"),
      hr(),
      div(
        class = "jumbotron",
        h1("GeoAviRWeb"),
        p(class = "lead"),
        changeLanguageOutput(class = "btn btn-xlarge btn-success")
      ),
      hr(),
      div(
        class = "marketing-row",
        div(class = "col-sm-6 col-md-6",
            i18nText("geoavir.desc", lang = "fr")),
        div(class = "col-sm-6 col-md-6",
            i18nText("geoavir.desc", lang = "en"))
      ),
      div(class = "footer")

    )
  })
}

generateApplicationLinks <- function(lang) {
  if (!is.null(APP_URL)) {
    lapply(APP_URL$id, function(x, lang){
      actionButton(paste0(x,"app"), i18nText(x, lang))
    }, lang)
  }
}

selectApplicationRender <- function(input, output, session, lang) {
  renderUI({
    column(
      6,
      offset = 3,
      useShinyjs(),
      div(class = "masthead"),
      changeLanguageOutput(lang = lang, button = FALSE),
      hr(),
      div(class = "jumbotron",
          p(class = "lead"),
          h3(i18nText("select.app", lang)),
          generateApplicationLinks(lang)),
      div(class = "footer")

    )
  })
}


