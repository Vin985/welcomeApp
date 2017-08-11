library(shiny)


actionButtonStyled <- function(inputId, label, style = "" , class = "") {

  if ( class == "") {
    class <- "btn action-button btn-default"
  }

  tags$button(id=inputId,
              style = style,
              type="button",
              class=class,
              label)
}



passwordEncryptedInput <- function(inputId, label, value = "", width = NULL) {
  div(class = "form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type="password", class="form-control", value=value)
  )
}



uiLoginHeader <- function(){
  tagList(tags$head(
    tags$link(rel = "stylesheet", type = "text/css",href = "assets/css/style.css"),
    tags$script(type = "text/javascript", src = "assets/js/spark-md5.min.js"),
    tags$script(type = "text/javascript", src = "assets/js/passwdEncryptBinding.js"),
    tags$script(type = "text/javascript", src = "assets/js/message-handler.js")
  ))
}



uiTabPanelChangePassword <- function(){
  tabPanel("Modifier le mot de passe",
           fluidPage(
             mainPanel(
               div(class="modal in", id="login-modalChangePassword", tabindex="-1", role="dialog",  "aria-labelledby"="myModalLabel", "aria-hidden"="true", style="display: modal-dialog; margin-top:100px;",
                   div(class="modal-dialog",
                       div(class="loginmodal-container",
                           h1("Modifier le mot de passe"), br(),
                           #div(id="uiLogin",
                           div(
                             #textInput("userName1", "Nom utilisateur:", placeholder ="Username"),
                             textInput("email", "Courriel:", placeholder ="courriel"),
                             passwordEncryptedInput("passwdOld", "Ancien mot de passe:"),
                             passwordEncryptedInput("passwdNew", "Nouveau mot de passe:"),
                             br(),
                             actionButton("changePasswordbutton", "Modifier le mot de passe", class="btn btn-success action-button login loginmodal-submit"),
                             htmlOutput("loginErrorChangePassword")
                           )
                       )
                   )
               )
             )
           )
  )

}


loginModal <- function(userInfo) {
  modalDialog(div(class = "loginmodal-container", h1(geti18nValue("portal.login", userInfo$lang)), br(),
              div(
                textInput("userName", geti18nValue("username", userInfo$lang), placeholder = geti18nValue("username", userInfo$lang)),
                passwordEncryptedInput("passwd", geti18nValue("password", userInfo$lang)),
                br(),
                actionButton("loginAction", geti18nValue("login", userInfo$lang), class = "btn btn-primary action-button login loginmodal-submit"),
                htmlOutput("loginError")
              )),
              div(class = "login-help"), footer = NULL,
              easyClose = TRUE)
}

uiLoginModalDialog <- function(error=NULL){
  div(
    div(class = "modal in", id="login-modal", tabindex="-1", role="dialog",  "aria-labelledby"="myModalLabel", "aria-hidden"="true", style="display: modal-dialog;",
        div(class = "modal-dialog",
            div(class = "loginmodal-container",
                h1("Connexion à GeoAviRWeb"), br(),
                #div(id="uiLogin",
                div(
                  textInput("userName", "Nom utilisateur:", placeholder ="Username"),
                  #passwordInput("passwd", "Password:"),
                  passwordEncryptedInput("passwd", "Mot de passe:"),
                  br(),
                  actionButton("Login", "Se connecter", class="btn btn-primary action-button login loginmodal-submit"),
                  #actionButton("Login", "Se connecter"),

#                   if(!is.null(error))
#                     span(error, class ="text-danger"),

                  htmlOutput("loginError")

                ),
                div(class="login-help"#,
                    #a(href="#", "Register"), a(href="#", "Forgot Password")
                )))),
    div(class="modal-backdrop in")
  )
}


uiTabPanelLogout <- function(){
  tabPanel("Se déconnecter",
           fluidPage(
#              div(class="row",
#                  div(class="col-sm-offset-3 col-sm-4 text-center",
#                      div(class="btn-group", "data-toggle"="buttons",
#                          actionButtonStyled('logoutbutton', encode('Se déconnecter'), style = "width:300px;", class="btn btn-danger action-button btn-lg")
#                      )
#                  )
#              )
           )
  )
}


uiTabPanelAdmin <- function(){
  tabPanel("Gérer les utilisateurs",
           fluidPage( div("test here")
                      )
  )
}

get_ui=function(role){
  if(role=="TEST"){
    return(list_field_user)
  }else{
    return(list_field_admin)
  }
}
