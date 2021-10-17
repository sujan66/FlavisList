library(shiny)
library(shinydashboard)
library(dplyr)
library(rcdk) #package to work with smiles

load("main.RData")

header <- dashboardHeader(title = "Flavis List")

sidebar <- dashboardSidebar(
    radioButtons(
        inputId = "source",
        label = "Search by",
        choices = c(
            "Flavis Name" = "name",
            "CAS no." = "cas",
            "FEMA no." = "fema"
        )
    ),
    
    conditionalPanel(
        condition = "input.source == 'name'",
        selectizeInput('flavis', "Select Flavis Name", 
                       choices = NULL)
    ),
    
    conditionalPanel(
        condition = "input.source == 'cas'",
        selectizeInput('cas_no', "Select CAS number", 
                       choices = NULL)
    ),
    
    conditionalPanel(
        condition = "input.source == 'fema'",
        selectizeInput('fema_no', "Select FEMA number", 
                       choices = NULL)
    )
)

body <- dashboardBody(
    fluidRow(
        infoBoxOutput('flavisName'),
        infoBoxOutput('casNo'),
        infoBoxOutput('femaNo')
    ),
    fluidRow(
        infoBoxOutput('flNo'),
        infoBoxOutput('ceNo'),
        infoBoxOutput('jefcaNo')
    ),
    fluidRow(
        infoBoxOutput('euGroup'),
        infoBoxOutput('inRegister'),
        infoBoxOutput('status')
    ),
    fluidRow(
        infoBoxOutput('remarks', width = 8)
    ),
    fluidRow(
        uiOutput('structure')
    )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
    
    updateSelectizeInput(session, 'flavis', selected = "",
                         choices = unique(synonyms_list$synonym),
                         server = T)
    
    updateSelectizeInput(session, 'cas_no', selected = "",
                         choices = sort(unique(table_cleaned$cas_no)),
                         server = T)
    
    updateSelectizeInput(session, 'fema_no', selected = "",
                         choices = sort(unique(table_cleaned$fema_no)),
                         server = T)
    
    #reactive function for sub-setting data based on the search selection
    rval_data <- reactive({
        if (input$source == "name") {
            name_flavis <- subset(synonyms_list, synonym == input$flavis)$flavis_name
            if (length(name_flavis) > 0)
                data <- filter(table_cleaned, flavis_name == name_flavis)
            else 
                data <- data.frame()
            if (length(data$flavis_name) > 1)
                data <- data[1, ]
        } else if (input$source == "cas") {
            data <- filter(table_cleaned, cas_no == input$cas_no)
            if (length(data$cas_no) > 1)
                data <- data[1, ]
        } else if (input$source == "fema") {
            data <- filter(table_cleaned, fema_no == input$fema_no)
            if (length(data$fema_no) > 1)
                data <- data[1, ]
        }
        return(data)
    })
    
    #reactive function for returning the smile structure from the smile table
    rval_smile <- reactive({
        fname <- rval_data()$flavis_name
        if(length(fname) == 0)
            smile <- NA  
        else
            smile <- as.character(filter(smiles_table, flavis_name == fname)$smiles)
        return(smile)
    })
    
    status <- reactiveVal('false')
    
    observeEvent({
        list(input$flavis, input$cas_no, input$fema_no)
        }, {status('true')}
    )
    
    output$flavisName <- renderInfoBox({
        infoBox(
            title = "Flavis Name",
            value = rval_data()$flavis_name
        )
    })
    
    output$casNo <- renderInfoBox({
        infoBox(
            title = "CAS Number",
            value = rval_data()$cas_no
        )
    })
    
    output$femaNo <- renderInfoBox({
        infoBox(
            title = "FEMA Number",
            value = rval_data()$fema_no
        )
    })
    
    output$flNo <- renderInfoBox({
        infoBox(
            title = "FL Number",
            value = rval_data()$fl_no
        )
    })
    
    output$ceNo <- renderInfoBox({
        infoBox(
            title = "CE Number",
            value = rval_data()$ce_no
        )
    })
    
    output$jefcaNo <- renderInfoBox({
        infoBox(
            title = "JEFCA Number",
            value = rval_data()$jecfa_no
        )
    })
    
    output$euGroup <- renderInfoBox({
        infoBox(
            title = "Chemical Group (EU)",
            value = rval_data()$eu_chemical_group
        )
    })
    
    output$inRegister <- renderInfoBox({
        infoBox(
            title = "In Register?",
            value = rval_data()$in_register
        )
    })
    
    output$status <- renderInfoBox({
        infoBox(
            title = "Status",
            value = rval_data()$status
        )
    })
    
    output$remarks <- renderInfoBox({
        infoBox(
            title = "Remarks",
            value = rval_data()$remarks
        )
    })
    
    #deprecated(utility) function
    #output$structure <- renderImage({
    #   if (nrow(rval_data()) != 0) {
    #       fname <- rval_data()$flavis_name
    #       smile <- as.character(filter(smiles_table, flavis_name == fname)$smiles)
    #       mol <- parse.smiles(smile)
    #       todepict(mol[[1]], "mol.png")
    #       list(src = "mol.png")
    #   }
    #}, deleteFile = F)
    
    output$structure <- renderUI({
        smile <- rval_smile()
        if (!is.na(smile)) {
            mol <- parse.smiles(smile)
            todepict(mol[[1]], "mol.png")
            box(title = "Molecular Structure",
                imageOutput('mol'),
                width = 4,
                height = 200
            )
        } else {
            box(title = "Molecular Structure",
                footer = NA
            )
        }
    })
    
    #since rendering structure is different from rendering info boxes,
    #observeEvent is used to update the structure every time new input is given.
    #By not using the function, the updated png file will not be used and the older 
    #structure will be depicted for the wrong compound.
    observeEvent({list(input$flavis, input$cas_no, input$fema_no)},
        output$mol <- renderImage({
            list(src = "mol.png")},
            deleteFile = F
        )
    )
}

shinyApp(ui = ui, server = server)
