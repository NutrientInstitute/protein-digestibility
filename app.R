library(tidyverse)
library(shiny)
library(DT)
library(readxl)
library(fontawesome)
library(readr)

Protein_Digestibility_Data <- read_csv("https://raw.githubusercontent.com/NutrientInstitute/protein-digestibility/main/Protein%20Digestibility%20Data%20%20-%20full%20data.csv") %>%
    select(!Notes)


# shiny app
ui <- fluidPage(
    fluidPage(
        fluidRow(h1("Protein Digestibility Hub"),
                 actionButton("gitbutton",
                              label = tags$h5("View data or docummentation on github  ", fa("github", fill = "black", height = "20px", vertical_align = "-0.35em"), onclick = "window.open('https://github.com/NutrientInstitute/protein-digestibility', '_blank')"),
                              style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;float:right;")),

        # Create a new Row in the UI for selectInputs
        fluidRow(style="background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px;",
            h3("Filters:"),
            column(2,
                   checkboxGroupInput("species",
                                      "Species:",
                                       # choices = c(unique(as.character(Protein_Digestibility_Data$`Subject species`))),
                                      choices = c("human", "pig", "rat"),
                                      # selected = c(unique(as.character(Protein_Digestibility_Data$`Subject species`)))
                                      # selected = NULL
                                      selected = c("human", "pig", "rat"))
            ),
            column(2,
                   checkboxGroupInput("protein_AA",
                                      "Protein or AA:",
                                      # choices = c(unique(as.character(Protein_Digestibility_Data$`Protein or AA`))),
                                      choices = c("crude protein", "histidine", "isoleucine", "leucine", "lysine", "reactive lysine", "methionine", "phenylalanine", "threonine", "tryptophan", "valine"),
                                      # selected = c(unique(as.character(Protein_Digestibility_Data$`Protein or AA`))))
                                      # selected = NULL
                                      selected = c("crude protein", "histidine", "isoleucine", "leucine", "lysine", "reactive lysine", "methionine", "phenylalanine", "threonine", "tryptophan", "valine"))
            ),
            column(2,
                   checkboxGroupInput("sample_type",
                                      "Sample type:",
                                      # choices = c(unique(as.character(Protein_Digestibility_Data$`Sample type`))),
                                      choices = c("fecal", "ileal", "in vitro"),
                                      # selected = c(unique(as.character(Protein_Digestibility_Data$`Sample type`))))
                                      # selected = NULL
                                      selected = c("fecal", "ileal", "in vitro"))
            ),
            column(2,
                   checkboxGroupInput("calc",
                                      "Digestibility calculation:",
                                      # choices = c(unique(as.character(Protein_Digestibility_Data$`Digestibility calculation`))),
                                      choices = c("apparent", "standardized", "true"),
                                      # selected = c(unique(as.character(Protein_Digestibility_Data$`Digestibility calculation`))))
                                      # selected = NULL
                                      selected = c("apparent", "standardized", "true"))
            )
        ),
        # Create a new row for the table.
        fluidRow(style = "padding-top: 20px;",
            DT::dataTableOutput("table")
    )))

server <- function(input, output) {
    output$table <- DT::renderDataTable(server = FALSE, {
        DT::datatable(Protein_Digestibility_Data %>%
            filter(`Species` %in% input$species) %>%
            filter(`Sample type` %in% input$sample_type) %>%
            filter(`Protein or AA` %in% input$protein_AA) %>%
            filter(`Digestibility calculation` %in% input$calc),
            extensions = 'Buttons',
            class = "display cell-border compact",
            rownames = FALSE,
            options = list(
                dom = 'Bfrtip',
                # autoWidth = TRUE,
                pageLength = 25,
                lengthMenu = c(25, 50, 75, 100),
                columnDefs = list(list(targets = "_all", className = "dt-head-nowrap")),
                buttons =
                    list( list(
                        extend = 'collection',
                        buttons = c('csv', 'excel', 'pdf'),
                        text = 'Download'
                    )))
    )  %>%
            formatStyle(1:14, 'vertical-align'='top', 'overflow-wrap'= 'break-word') %>%
            # formatStyle(2, width = '8%') %>%
            # formatStyle(4, width = '8%') %>%
            # formatStyle(10, width = '5%') %>%
            # formatStyle(12, width = '5%') %>%
            formatStyle(13, 'overflow-wrap'= 'break-word', display = "block", overflow = 'hidden', width = '200px') %>%
            formatStyle(14, width = '200px')
    })
}

shinyApp(ui, server)
