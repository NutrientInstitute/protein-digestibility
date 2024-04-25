library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)

Protein_Digestibility_Data <- read_csv("https://raw.githubusercontent.com/NutrientInstitute/protein-digestibility/main/Protein%20Digestibility%20Data%20%20-%20full%20data.csv") %>%
    # Protein_Digestibility_Data <- read_csv("Protein Digestibility Data  - full data.csv") %>%
    select(!Notes)

fileName <- "Protein Digestibility Hub"

# shiny app
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            .float-right-button {
                float: right;
                margin-right: 10px;
                margin-top: 5px;
                background-color: transparent;
                border: none;
            }
            .accordion-toggle {
                cursor: pointer;
                background-color: #dedede;
                border: 0.5px solid #bdbdbd;
                margin-top: 0px;
                font-weight: bold;
                padding-left: 20px;
                padding-bottom:50px;
            }
            .accordion-content {
                display: none;
                padding: 20px;
                background-color: #F6F6F6;
                border-left: 0.5px solid #bdbdbd;
                border-right: 0.5px solid #bdbdbd;
                border-bottom: 0.5px solid #bdbdbd;
            }
        "))
    ),
    fluidPage(
        fluidRow(br()),
        fluidRow(column(5, h1("Protein Digestibility Hub")),
                 actionButton("gitbutton",
                              label = tags$h5("View data or docummentation on github  ", fa("github", fill = "black", height = "20px", vertical_align = "-0.35em"), onclick = "window.open('https://github.com/NutrientInstitute/protein-digestibility', '_blank')"),
                              style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px;float:right;"),
                 actionButton("gitbutton",
                              label = tags$h5("Provide feedback", fa("comment", fill = "black", height = "20px", vertical_align = "-0.35em"), onclick = "window.open('https://www.nutrientinstitute.org/protein-digestibility-feedback', '_blank')"),
                              style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px; margin-right: 20px; float:right;")),
        fluidRow(br()),
        useShinyjs(),
        fluidRow(
            div(id = "info-toggle", class = "accordion-toggle",
                onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_click', Math.random(), {priority: 'event'}); }",
                column(8, p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")),
                column(4, uiOutput("toggleButton")))),
        fluidRow(
            hidden(
                div(id = "infoContent",
                    class = "accordion-content",
                    style="background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    p("The Protein Digestibility Hub Project serves as a dedicated repository for protein digestibility information, addressing a critical gap in accessible data. For more information see the Protein Digestibility Hub ", a(href = "https://github.com/NutrientInstitute/protein-digestibility", "github page."), " Please contact ", a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"), "to report problems or provide feedback."),
                    br(),
                    p("Currently, the Protein Digestibility Hub contains data from the following sources:"),
                    tags$ul(
                        tags$li(a(href = "https://www.ars.usda.gov/arsuserfiles/80400535/data/classics/usda%20handbook%2074.pdf", "USDA ENERGY VALUE OF FOODS (Agricultural Handbook No. 74, 1955)")),
                        tags$li( "AMINO-ACID CONTENT OF FOODS AND BIOLOGICAL DATA ON PROTEINS (FAO 1970)"),
                        tags$li(a(href = "https://www.fao.org/ag/humannutrition/36216-04a2f02ec02eafd4f457dd2c9851b4c45.pdf", "Report of a Sub-Committee of the 2011 FAO Consultation on 'Protein Quality Evaluation in Human Nutrition'"))
                    )))
        ),
        fluidRow(br()),
        # Create a new Row in the UI for selectInputs
        fluidRow(style="background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                 column(2, p("Filters", style = "font-weight: bold;font-size: 20px; padding-top:10px;"))),
        fluidRow(style="background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                 column(2,
                        virtualSelectInput(
                            inputId = "species",
                            label = "Species:",
                            choices = c("human","human (predicted from pig)", "pig", "rat"),
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
                                "Essential amino acid" = c("histidine", "isoleucine", "leucine", "lysine", "reactive lysine", "methionine", "phenylalanine", "threonine", "tryptophan", "valine"),
                                "Conditionally essential amino acid" = c("arginine", "cysteine", "glycine", "proline", "tyrosine"),
                                "Non-essential amino acid" = c("alanine", "aspartic acid", "glutamic acid",  "serine")
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

server <- function(input, output, session) {
    visibility <- reactiveVal(TRUE)

    output$toggleButton <- renderUI({
        if (visibility()) {
            actionButton("show_hide", label = icon("plus", style="color: #333; font-size: 24px; font-weight: bold;"), class = "float-right-button")
        } else {
            actionButton("show_hide", label = icon("minus", style="color: #333; font-size: 24px; font-weight: bold;"), class = "float-right-button")
        }
    })

    observeEvent(input$show_hide, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    })

    # Observe clicks on the info-toggle div
    observeEvent(input$info_toggle_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    output$table <- DT::renderDataTable(server = FALSE, {
        DT::datatable(Protein_Digestibility_Data %>%
                          filter(`Species` %in% input$species) %>%
                          filter(`Model` %in% input$model) %>%
                          filter(`Sample` %in% input$sample) %>%
                          filter(`Analyte` %in% input$analyte) %>%
                          filter(`Measure` %in% input$measure) %>%
                          mutate(across(where(is.character), ~ gsub("\n", "<br>", .))),
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
                      ),
                      escape = FALSE  # Allow HTML rendering
        )  %>%
            formatStyle(1:15, 'vertical-align'='top', 'overflow-wrap'= 'break-word') %>%
            formatStyle(14, 'overflow-wrap'= 'break-word', 'word-break' = 'break-word', width = '12%') %>%
            formatStyle(15, width = '12%')
    })
}

shinyApp(ui, server)
