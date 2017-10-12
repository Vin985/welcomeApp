


adminServer <- function(input, output, session, userInfo) {
  #==============================================================================================================================================
  ## CRUD reactive code
  #
  # input fields are treated as a group
  formData <- reactive({
    sapply(names(GetTableMetadata()$fields), function(x)
      input[[x]])
  })

  # Click "Submit" button -> save data
  observeEvent(input$submit, {
    if (input$id != "0") {
      # it is a selected account to be updated
      UpdateData(formData())
    } else {
      # create new user
      CreateData(formData())
      UpdateInputs(CreateDefaultRecord(), session)
    }
  }, priority = 1)


  # Press "New" button -> display empty record
  observeEvent(input$new, {
    UpdateInputs(CreateDefaultRecord(), session)
  })


  # Press "Delete" button -> delete from data
  observeEvent(input$delete, {
    DeleteData(formData())
    UpdateInputs(CreateDefaultRecord(), session)
  }, priority = 1)


  # Select row in table -> show details in inputs
  observeEvent(input$responses_rows_selected, {
    if (length(input$responses_rows_selected) > 0) {
      data <- ReadData()[input$responses_rows_selected,]
      UpdateInputs(data, session)
    }

  })


  # display table
  output$responses <- DT::renderDataTable({
    # require the presence of "responses" id
    req(isAdmin(userInfo$user))
    #update after submit is clicked
    input$submit
    #update after delete is clicked
    input$delete
    d <- ReadData()
  }, server = FALSE, selection = "single",
  colnames = unname(GetTableMetadataToDisplayInTable()$fields))
}

# output$uploadedFileContent <- DT::renderDataTable(DT::datatable(
#   reactiveDataVisualizeContent(),
#   options = list(
#     orderClasses = TRUE,
#     lengthMenu = c(5, 10, 20, 30, 50),
#     pageLength = 5
#   )
#


#==============================================================================================================================================
## CRUD helpers
# data is of type list: list(id = "", username = "", password = "", email = "")
CastData <- function(data) {
  datar <- data.frame(
    id = data["id"],
    username = data["username"],
    #password = data["password"],
    email = data["email"],
    stringsAsFactors = FALSE
  )
  return (datar)
}


CreateDefaultRecord <- function() {
  mydefault <-
    CastData(list(
      id = "0",
      username = "",
      password = "",
      email = ""
    ))
  return (mydefault)
}


UpdateInputs <- function(data, session) {
  updateTextInput(session, "id", value = unname(data["id"]))
  updateTextInput(session, "username", value = unname(data["username"]))
  updateTextInput(session, "email", value = unname(data["email"]))
}


#==============================================================================================================================================
## CRUD ops
#

CreateData <- function(data) {
  # save data to database
  createNewUserFromListe(data)

  # add the data to the datatable responses (DT::dataTableOutput)
  data <- CastData(data)

  if (exists("responses")) {
    responses <<- rbind(responses, data)
  } else {
    responses <<- data
  }
}


ReadData <- function() {
  getUsers()
}


UpdateData <- function(data) {
  # update the user data row in the DB
  updateUserFromListByID(data)

  responses <<- data
}


DeleteData <- function(data) {
  # delete from DB
  user <- getUSerById(data["id"])

  if (user["admin"] == 1) {
    output$crudErrors <- renderUI({
      span("Opération non autorisée. Un compte administrateur ne peut être supprimé.",
           class = "text-danger")
    })
  }
  else{
    output$crudErrors <- renderUI({

    })
    deleteUserByID(data["id"])

    # update the datatable responses (DT::dataTableOutput)
    if (exists("responses")) {
      responses <<-
        responses[row.names(responses) != unname(data["id"]),]
    } else {
      responses <<- data
    }
  }

}

GetTableMetadata <- function() {
  fields <- c(
    id = "Id",
    username = "username",
    password = "password",
    email = "email"
  )
  result <- list(fields = fields)
  return (result)
}

# password field is not displayed
GetTableMetadataToDisplayInTable <- function() {
  fields <- c(id = "Id",
              username = "username",
              #password = "password",
              email = "email")
  result <- list(fields = fields)
  return (result)
}
