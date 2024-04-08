library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(shinyWidgets)

Protein_Digestibility_Data <- read_csv("https://raw.githubusercontent.com/NutrientInstitute/protein-digestibility/main/Protein%20Digestibility%20Data%20%20-%20full%20data.csv", col_types = cols(`Digestibility calculation` = col_character())) %>%
    select(!Notes)
fileName <- "Protein Digestibility Hub"

# shiny app
ui <- fluidPage(
    fluidPage(
        fluidRow(br()),
        fluidRow(column(5, h1("Protein Digestibility Hub")),
                 actionButton("gitbutton",
                              label = tags$h5("View data or docummentation on github  ", fa("github", fill = "black", height = "20px", vertical_align = "-0.35em"), onclick = "window.open('https://github.com/NutrientInstitute/protein-digestibility', '_blank')"),
                              style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px;float:right;")),
        fluidRow(br()),
        # Create a new Row in the UI for selectInputs
        fluidRow(style="background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                 column(2, h4("Filters:", style = "font-weight: bold;"))),
        fluidRow(style="background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                 column(2,
                        virtualSelectInput(
                            inputId = "species",
                            label = "Species:",
                            choices = c("human", "pig", "rat"),
                            selected = c(unique(as.character(Protein_Digestibility_Data$Species))),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                 ),
                 column(2,
                        virtualSelectInput(
                            inputId = "model",
                            label = "Model:",
                            choices = c("in vivo", "in vitro"),
                            selected = c(unique(as.character(Protein_Digestibility_Data$Model))),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                 ),
                 column(2,
                        virtualSelectInput(
                            inputId = "sample",
                            label = "Sample:",
                            choices = c("fecal", "ileal"),
                            selected = c(unique(as.character(Protein_Digestibility_Data$Sample))),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                 ),
                 column(3,
                        virtualSelectInput(
                            inputId = "measure",
                            label = "Measure:",
                            choices = c("apparent digestibility", "standardized digestibility", "true digestibility", "biological value", "metabolic activity"),
                            selected = c(unique(as.character(Protein_Digestibility_Data$Measure))),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                 ),
                 column(3,
                        virtualSelectInput(
                            inputId = "analyte",
                            label = "Analyte:",
                            choices = list(
                                "crude protein" = "crude protein",
                                "Essential amino acid" = c("histidine", "isoleucine", "leucine", "lysine", "reactive lysine", "methionine", "phenylalanine", "threonine", "tryptophan", "valine")
                            ),
                            selected = unique(as.character(Protein_Digestibility_Data$Analyte)),
                            showValueAsTags = TRUE,
                            multiple = TRUE,
                            width = '100%'
                        )
                 )),
        # Create a new row for the table.
        fluidRow(style = "padding-top: 20px;",
                 DT::dataTableOutput("table")
        )))

server <- function(input, output) {
    output$table <- DT::renderDataTable(server = FALSE, {
        DT::datatable(Protein_Digestibility_Data %>%
                          filter(`Species` %in% input$species) %>%
                          filter(`Model` %in% input$model) %>%
                          filter(`Sample` %in% input$sample) %>%
                          filter(`Analyte` %in% input$analyte) %>%
                          filter(`Measure` %in% input$measure),
                      extensions = 'Buttons',
                      class = "display cell-border compact",
                      rownames = FALSE,
                      options = list(
                          dom = 'Bfrtip',
                          pageLength = 25,
                          lengthMenu = c(25, 50, 75, 100),
                          columnDefs = list(list(targets = "_all", className = "dt-head-nowrap")),
                          buttons = list(
                              'copy', 'print', list(
                                  extend = 'collection',
                                  buttons = list(
                                      list(extend = "csv", filename = fileName),
                                      list(extend = "excel", filename = fileName),
                                      list(extend = "pdf", filename = fileName)),
                                  text = 'Download')
                          )
                      )
        )  %>%
            formatStyle(1:15, 'vertical-align'='top', 'overflow-wrap'= 'break-word') %>%
            formatStyle(14, 'overflow-wrap'= 'break-word', 'word-break' = 'break-word', width = '10%') %>%
            formatStyle(15, width = '10%')
    })
}

shinyApp(ui, server)
