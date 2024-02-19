library(tidyverse)
library(shiny)
library(DT)
library(readxl)
library(fontawesome)

Protein_Digestibility_Data <- read_excel("Protein Digestibility Data .xlsx") %>%
    select(!Notes)


# shiny app
ui <- fluidPage(
    fluidPage(
        fluidRow(h1("Protein Digestibility Data"),
                 actionButton("gitbutton",
                              label = tags$h5("View data or docummentation on github  ", fa("github", fill = "black", height = "20px", vertical_align = "-0.35em"), onclick = "window.open('https://github.com/NutrientInstitute/protein-digestibility', '_blank')"),
                              style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;float:right;")),

        # Create a new Row in the UI for selectInputs
        fluidRow(style="background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px;",
            h3("Filters:"),
            column(2,
                   checkboxGroupInput("species",
                                      "Subject species:",
                                       choices = c(unique(as.character(Protein_Digestibility_Data$`Subject species`))),
                                      selected = c(unique(as.character(Protein_Digestibility_Data$`Subject species`))))
            ),
            column(2,
                   checkboxGroupInput("protein_AA",
                                      "Protein or AA:",
                                      choices = c(unique(as.character(Protein_Digestibility_Data$`Protein or AA`))),
                                      selected = c(unique(as.character(Protein_Digestibility_Data$`Protein or AA`))))
            ),
            column(2,
                   checkboxGroupInput("sample_type",
                                      "Sample type:",
                                      choices = c(unique(as.character(Protein_Digestibility_Data$`Sample type`))),
                                      selected = c(unique(as.character(Protein_Digestibility_Data$`Sample type`))))
            ),
            column(2,
                   checkboxGroupInput("calc",
                                      "Digestibility calculation:",
                                      choices = c(unique(as.character(Protein_Digestibility_Data$`Digestibility calculation`))),
                                      selected = c(unique(as.character(Protein_Digestibility_Data$`Digestibility calculation`))))
            )
        ),
        # Create a new row for the table.
        fluidRow(style = "padding-top: 20px;",
            DT::dataTableOutput("table")
    )))

server <- function(input, output) {
    output$table <- DT::renderDataTable({
        DT::datatable(Protein_Digestibility_Data %>%
            filter(`Subject species` %in% input$species) %>%
            filter(`Sample type` %in% input$sample_type) %>%
            filter(`Protein or AA` %in% input$protein_AA) %>%
            filter(`Digestibility calculation` %in% input$calc),
            extensions = 'Buttons',
            rownames = FALSE,
            options = list(
                dom = 'Bfrtip',
                # autoWidth = TRUE,
                pageLength = 25,
                lengthMenu = c(25, 50, 75, 100),
                buttons =
                    list( list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )))
    )})
}

shinyApp(ui, server)
