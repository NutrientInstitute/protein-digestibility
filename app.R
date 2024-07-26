library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)
library(bslib)

# Protein_Digestibility_Data <- read_csv("https://raw.githubusercontent.com/NutrientInstitute/protein-digestibility/main/Protein%20Digestibility%20Data%20%20-%20full%20data.csv") %>%
 Protein_Digestibility_Data <-
     read_csv("Protein Digestibility Data  - full data.csv") %>%
    select(!Notes)
EAA_composition <- read_csv("EAA_composition.csv") %>%
    drop_na(Protein) %>%
    pivot_longer(histidine:valine, names_to = "AA", values_to = "value")
scoring_pattern <- read_csv("scoring_pattern.csv")
portion_sizes <- read_csv("portion_sizes.csv")


fileName <- "Protein Quality Hub"

# shiny app
ui <- fluidPage(
    tags$head(tags$style(
        HTML(
            "
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
            .nav-tabs {
                border-bottom: 1px solid black;
                margin-bottom: 10px;
                width:100%;
                padding: 0px;
            }
            .nav-tabs li {
                font-size: 20px;
                background-color: white;
                color: black;
            }
            .nav > li > a:hover, .nav > li > a:focus {
                outline: rgb(0, 0, 0) none 1px;
            }
            .nav-tabs li a {
                font-size: 25px;
                background-color:#F6F6F6;
                color: #808080;
                border-bottom: solid 1px #000;
            }
            .tab-content {
                width: 100%;
            }
            .tabbable {
                width: 100%;
            }
            .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
                border-color: rgb(0, 0, 0) rgb(0, 0, 0) transparent;
            }
        "
        )
    )),
    fluidPage(
        fluidRow(br()),
        withMathJax(),
        fluidRow(
            column(5, h1("Protein Quality Hub")),
            actionButton(
                "gitbutton",
                label = tags$h5(
                    "View data or docummentation on github  ",
                    fa(
                        "github",
                        fill = "black",
                        height = "20px",
                        vertical_align = "-0.35em"
                    ),
                    onclick = "window.open('https://github.com/NutrientInstitute/protein-digestibility', '_blank')"
                ),
                style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px;float:right;"
            ),
            actionButton(
                "gitbutton",
                label = tags$h5(
                    "Provide feedback",
                    fa(
                        "comment",
                        fill = "black",
                        height = "20px",
                        vertical_align = "-0.35em"
                    ),
                    onclick = "window.open('https://www.nutrientinstitute.org/protein-digestibility-feedback', '_blank')"
                ),
                style = "padding-top:0px;padding-bottom:0px;background-color:#F6F6F6;margin-bottom: 20px;margin-top: 20px; margin-right: 20px; float:right;"
            )
        ),
        fluidRow(br()),
        useShinyjs(),
        navset_tab(
            nav_panel(
                "Protein Quality Scoring",
                fluidRow(
                    div(
                        id = "info-toggle_2",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_2_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                        ),
                        column(4, uiOutput("toggleButton_2"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent_2",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p(
                            "In the",
                            tags$em(" Protein Quality Scoring"),
                            " tab of the ",
                            tags$em("Protein Digestibility Hub"),
                            "you will find protein quality scores generated using digestibility values from the ",
                            tags$em("Protein Digestibility Data"),
                            " tab. Digestibility values can be identified across tables using the variable ",
                            tags$em("NI_ID"),
                            ".",
                            br(),
                            "For more information on EAA recommendations and scoring patterns used in this app, consult the ",
                            a(href = "https://github.com/NutrientInstitute/protein-digestibility", "project GitHub page"),
                            ". Note that this tool is still in development, please contact",
                            a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
                            "to report problems or provide feedback."
                        ),
                        br(),

                        h4("EAA-9:", tags$sup("1")),
                        p(
                            "Intended application: EAA-9 scores are percentages, representing the ability of a food to meet daily essential amino acid (EAA) recommendations, by default the RDAs. In practice, the score can be used to compare protein quality between food sources, and as a dietary quality tool to track progress toward meeting EAA recommendations."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        # " Calculation is based on the minimum percentage of the RDA met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid.",
                        p(
                            "The EAA-9 calculation is based on the minimum percentage of the RDA (or personalized EAA recommendations) met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid adjusted for digestibility (if digestibility data is available). EAA RDAs are satisfied when the EAA-9 score for foods consumed in a day reaches 100%."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{EAA-9}=min(\\frac{\\text{His Present}}{\\text{His RDA}},\\frac{\\text{Ile Present}}{\\text{Ile RDA}},\\frac{\\text{Leu Present}}{\\text{Leu RDA}},\\frac{\\text{Lys Present}}{\\text{Lys RDA}},\\frac{\\text{Met Present}}{\\text{Met RDA}},\\frac{\\text{Phe Present}}{\\text{Phe RDA}},\\frac{\\text{Thr Present}}{\\text{Thr RDA}},\\frac{\\text{Trp Present}}{\\text{Trp RDA}},\\frac{\\text{Val Present}}{\\text{Val RDA}})\\times100\\times\\text{digestibility}\\)',
                        br(),
                        p(br()),
                        h4(
                            "Protein Digestibility Corrected Amino Acid Score (PDCAAS):",
                            tags$sup("2")
                        ),
                        p(
                            "Intended application: PDCAAS values range from 0 to 1 and represent the quality of 1 gram of protein from a food source compared to a reference gram of protein. In practice, the score can be used to compare the protein quality between food sources on a per gram basis."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        p(
                            "The PDCAAS calculation is based on the ratio of limiting amino acid in a gram of protein compared to the same amino acid in a reference protein capped at 1 (i.e. 100%), adjusted for digestibility. PDCASS was designed to be calculated using fecal digestibility values.*"
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{PDCAAS}=min(\\frac{\\text{mg of amino acid in 1 g test protein}}{\\text{mg of amino acid in reference pattern}},1)\\times\\text{digestibility}\\)',
                        br(),
                        p(br()),
                        h4(
                            "Digestible Indispensable Amino Acid Score (DIAAS):",
                            tags$sup("3")
                        ),
                        p(
                            "Intended application: DIAAS values represent the ratio between the quality of 1 gram of protein from a food source compared to a reference gram of protein. In practice, the score can be used to compare the protein quality between food sources on a per gram basis."
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        p(
                            "The DIAAS calculation is based on the ratio of limiting amino acid in a gram of protein compared to the same amino acid in a reference protein, adjusted for digestibility. Unlike PDCAAS, DIAAS is not truncated at 1 (i.e. 100%) and is  calculated using ileal digestibility.*"
                        ),
                        p(br(), style = "margin-bottom: -15px;"),
                        '\\(\\text{DIAAS}=100\\times\\frac{\\text{mg of digestible dietary indispensable amino acid in 1 g of the dietary protein}}{\\text{mg of the same dietary indispensable amino acid in 1g of the reference protein}}\\times\\text{digestibility}\\)',
                        p(br()),
                        p(
                            "*While specific digestibility samples (i.e. ileal, fecal) are preferred in the calculation of PDCAAS and DIAAS, protein quality scores are provided for all available digestibility values."
                        ),

                        br(),
                        tags$small(h5("References:"),
                                   tags$ol(tags$li(
                                       p(
                                           "Forester SM, Jennings-Dobbs EM, Sathar SA, Layman DK. Perspective: Developing a Nutrient-Based Framework for Protein Quality. J Nutr. 2023 Aug;153(8):2137-2146. doi: ",
                                           a(href = "10.1016/j.tjnut.2023.06.004", "https://doi.org/10.1016/j.tjnut.2023.06.004"),
                                           ". Epub 2023 Jun 8. PMID: 37301285."
                                       )
                                   ),
                                   tags$li(
                                       p(
                                           "Food and Agriculture Organization of the United Nations, World Health Organization & United Nations University. Protein and amino acid requirements in human nutrition : report of a joint FAO/WHO/UNU expert consultation [Internet]. World Health Organization; 2007 [cited 2022 Dec 1]. Available from: ",
                                           a(href = "https://apps.who.int/iris/handle/10665/43411", "https://apps.who.int/iris/handle/10665/43411"),
                                           "."
                                       )
                                   ),
                                   tags$li(
                                       p(
                                           "FAO. Dietary protein quality evaluation in human nutrition: report of an FAO Expert Consultation. Food and nutrition paper; 92. FAO: Rome [Internet]. FAO (Food and Agriculture Organization); 2013. Available from:",
                                           a(href = "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf", "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf"),
                                           "."
                                       )
                                   )))
                    )
                )),
                fluidRow(br()),
                # Create a new Row in the UI for selectInputs
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             4,
                             p("Choose scoring method(s)", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                        2,
                        checkboxGroupInput(
                            inputId = "score",
                            label = "Score",
                            choices = c("EAA-9", "PDCAAS", "DIAAS"),
                            selected = "EAA-9"
                        ),
                        radioButtons(
                            inputId = "show_calc",
                            label = "Score calculation",
                            choiceNames = c("Show score only", "Show calculation and score"),
                            choiceValues = c("score_only", "show_calc")
                        )
                    ),
                    column(
                        2,
                        conditionalPanel(
                            condition = "input.score.includes('EAA-9')",
                            p("EAA-9 Scoring options:", style = "font-size: 15px;font-style: italic;text-decoration:underline;"),
                            pickerInput(
                                inputId = "EAA_rec",
                                label = "EAA Recommendations",
                                choices = unique(
                                    c(
                                        unlist(
                                            scoring_pattern %>% filter(Unit == "mg/kg/d") %>% select(`Pattern Name`)
                                        ),
                                    "Choose custom recommendations"
                                    )
                                ),
                                selected = "RDA for Adults"
                            ),
                            conditionalPanel(
                                condition = "input.EAA_rec == 'Choose custom recommendations'",
                                fluidRow(
                                    column(4,
                                           numericInput(
                                               inputId = "his",
                                               label = "His (mg/kg/d)",
                                               value = "14",
                                               width = '90px'
                                           ),
                                           numericInput(
                                               inputId = "leu",
                                               label = "Leu (mg/kg/d)",
                                               value = "19",
                                               width = '90px'
                                           ),
                                           numericInput(
                                               inputId = "ile",
                                               label = "Ile (mg/kg/d)",
                                               value = "42",
                                               width = '90px'
                                           )
                                    ),
                                    column(4,
                                           numericInput(
                                               inputId = "lys",
                                               label = "Lys (mg/kg/d)",
                                               value = "38",
                                               width = '90px'
                                           ),
                                           numericInput(
                                               inputId = "met_cys",
                                               label = "Met+Cys (mg/kg/d)",
                                               value = "19",
                                               width = '90px'
                                           ),
                                            numericInput(
                                                inputId = "phe_tyr",
                                                label = "Phe+Tyr (mg/kg/d)",
                                                value = "33",
                                                width = '90px'
                                            )
                                           ),
                                    column(4,
                                        numericInput(
                                            inputId = "thr",
                                            label = "Thr (mg/kg/d)",
                                            value = "20",
                                            width = '90px'
                                        ),
                                        numericInput(
                                            inputId = "trp",
                                            label = "Trp (mg/kg/d)",
                                            value = "5",
                                            width = '90px'
                                        ),
                                        numericInput(
                                            inputId = "val",
                                            label = "Val (mg/kg/d)",
                                            value = "24",
                                            width = '90px'
                                        )
                                        )
                                )
                            ),
                            conditionalPanel(
                                condition = "input.EAA_rec!='Choose custom recommendations'",
                                pickerInput(
                                    inputId = "rec_age",
                                    label = "Age",
                                    choices = unique(scoring_pattern$Age)
                                )
                            ),
                            numericInput(
                                inputId = "weight",
                                label = "Weight (kg)",
                                value = "70"
                            ),
                            radioButtons(
                                inputId = "serving_size",
                                label = "Serving size of food",
                                choices = c("Use standard serving sizes", "Choose your own serving"),
                                selected = "Use standard serving sizes"
                            ),
                            conditionalPanel(
                                condition = "input.serving_size == 'Choose your own serving'",
                                numericInput(
                                    inputId = "serving_weight",
                                    label = "Serving size (g)",
                                    value = "100"
                                )
                            )
                        )
                    ),
                    column(
                        2,
                        conditionalPanel(
                            condition = "input.score.includes('PDCAAS')",
                            p("PDCAAS Scoring options:", style = "font-size: 15px;font-style: italic;text-decoration:underline;"),
                            pickerInput(
                                inputId = "EAA_rec_PDCAAS",
                                label = "EAA Recommendations",
                                choices = unique(unlist(
                                    scoring_pattern %>% filter(Unit == "mg/g protein") %>% select(`Pattern Name`)
                                )),
                                selected = "FAO Scoring Pattern"
                            ),
                            pickerInput(
                                inputId = "rec_age_PDCAAS",
                                label = "Age",
                                choices = unique(scoring_pattern$Age)
                            ),
                            br()
                        )
                    ),
                    column(
                        2,
                        conditionalPanel(
                            condition = "input.score.includes('DIAAS')",
                            p("DIAAS Scoring options:", style = "font-size: 15px;font-style: italic;text-decoration:underline;"),
                            pickerInput(
                                inputId = "EAA_rec_DIAAS",
                                label = "EAA Recommendations",
                                choices = unique(unlist(
                                    scoring_pattern %>% filter(Unit == "mg/g protein") %>% select(`Pattern Name`)
                                )),
                                selected = "FAO Scoring Pattern"
                            ),
                            pickerInput(
                                inputId = "rec_age_DIAAS",
                                label = "Age",
                                choices = unique(scoring_pattern$Age)
                            )
                        )
                    ),
                    column(
                        2,
                        p("Included Digestibility Values ", style = "font-size: 15px;font-style: italic;text-decoration:underline;"),

                        checkboxGroupInput(
                            inputId = "pq_measure",
                            label = "Measure",
                            choices = c(
                                "apparent digestibility",
                                "standardized digestibility",
                                "true digestibility"
                            ),
                            selected = c(
                                "apparent digestibility",
                                "standardized digestibility",
                                "true digestibility"
                            )
                        ),
                        checkboxGroupInput(
                            inputId = "pq_sample",
                            label = "Sample",
                            choices = c("fecal", "ileal"),
                            selected = c("fecal", "ileal")
                        ),
                        checkboxGroupInput(
                            inputId = "pq_species",
                            label = "Species",
                            choices = c("human", "human (predicted from pig)", "pig", "rat"),
                            selected = c("human", "human (predicted from pig)", "pig", "rat")
                        ),
                        checkboxGroupInput(
                            inputId = "pq_analyte",
                            label = "Analyte",
                            choices = c("crude protein", "individual amino acids"),
                            selected = c("crude protein", "individual amino acids")
                        )

                    )
                    ),
                fluidRow(br()),
                # Create a new row for the table.
                fluidRow(style = "padding-top: 20px;",
                         DT::dataTableOutput("table_2"))
            ),
            nav_panel(
                "Protein Digestibility Data",
                fluidRow(
                    div(
                        id = "info-toggle",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                        ),
                        column(4, uiOutput("toggleButton"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p(
                            "The Protein Digestibility Hub Project serves as a dedicated repository for protein digestibility information, addressing a critical gap in accessible data. For more information see the Protein Digestibility Hub ",
                            a(href = "https://github.com/NutrientInstitute/protein-digestibility", "github page."),
                            " Please contact ",
                            a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
                            "to report problems or provide feedback."
                        ),
                        br(),
                        p(
                            "Currently, the Protein Digestibility Hub contains data from the following sources:"
                        ),
                        tags$ul(
                            tags$li(
                                a(
                                    href = "https://www.ars.usda.gov/arsuserfiles/80400535/data/classics/usda%20handbook%2074.pdf",
                                    "USDA ENERGY VALUE OF FOODS (Agricultural Handbook No. 74, 1955)"
                                )
                            ),
                            tags$li(
                                "AMINO-ACID CONTENT OF FOODS AND BIOLOGICAL DATA ON PROTEINS (FAO 1970) ",
                                br(),
                                tags$small(
                                    tags$i(
                                        "(",
                                        tags$b("Note"),
                                        ": The original publication has been removed from the FAO website, but can still be accessed via the ",
                                        tags$a(href = "https://web.archive.org/web/20231125115519/https://www.fao.org/3/ac854t/AC854T00.htm", "Wayback Machine"),
                                        ")"
                                    )
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://www.fao.org/ag/humannutrition/36216-04a2f02ec02eafd4f457dd2c9851b4c45.pdf",
                                    "Report of a Sub-Committee of the 2011 FAO Consultation on 'Protein Quality Evaluation in Human Nutrition'"
                                )
                            ),
                            tags$li(
                                a(
                                    href = "https://doi.org/10.17226/13298",
                                    "Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)",
                                    tags$b("**")
                                )
                            )
                        ),
                        br(),
                        tags$small(
                            p(
                                tags$b("**"),
                                "Digestibility and protein data from ",
                                tags$i(
                                    "Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)"
                                ),
                                "  was collected from the following sources:"
                            ),
                            tags$ol(
                                tags$li(
                                    "AAFCO (Association of American Feed Control Officials). 2010. Official Publication. Oxford, IN: AAFCO."
                                ),
                                tags$li("AminoDat 4.0. 2010. Evonik Industries, Hanau, Germany."),
                                tags$li(
                                    "Cera, K. R., D. C. Mahan, and G. A. Reinhart. 1989. Apparent fat digestibilities and performance responses of postweaning swine fed diets supplemented with coconut oil, corn oil or tallow. Journal of Animal Science 67:2040-2047."
                                ),
                                tags$li(
                                    "CVB (Dutch PDV [Product Board Animal Feed]). 2008. CVB Feedstuff Database. Available online at http://www.pdv.nl/english/Voederwaardering/about_cvb/index.php. Accessed on June 9, 2011."
                                ),
                                tags$li(
                                    "NRC (National Research Council). 1998. Nutrient Requirements of Swine, 10th Rev. Ed. Washington, DC: National Academy Press."
                                ),
                                tags$li(
                                    "NRC. 2007. Nutrient Requirements of Horses, 6th Rev. Ed. Washington, DC: The National Academies Press."
                                ),
                                tags$li(
                                    "Powles, J., J. Wiseman, D. J. A. Cole, and S. Jagger. 1995. Prediction of the apparent digestible energy value of fats given to pigs. Animal Science 61:149-154."
                                ),
                                tags$li(
                                    "Sauvant, D., J. M. Perez, and G. Tran. 2004. Tables of Composition and Nutritional Value of Feed Materials: Pigs, Poultry, Sheep, Goats, Rabbits, Horses, Fish, INRA, Paris, France, ed. Wageningen, the Netherlands: Wageningen Academic."
                                ),
                                tags$li(
                                    "USDA (U.S. Department of Agriculture), Agricultural Research Service. 2010. USDA National Nutrient Database for Standard Reference, Release 23. Nutrient Data Laboratory Home Page. Available online at http://www.ars.usda.gov/ba/bhnrc/ndl. Accessed on August 10, 2011."
                                ),
                                tags$li(
                                    "van Milgen, J., J. Noblet, and S. Dubois. 2001. Energetic efficiency of starch, protein, and lipid utilization in growing pigs. Journal of Nutrition 131:1309-1318."
                                )
                            )
                        )
                    )
                )),
                fluidRow(br()),
                # Create a new Row in the UI for selectInputs
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Filters", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                        2,
                        virtualSelectInput(
                            inputId = "species",
                            label = "Species:",
                            choices = c("human", "human (predicted from pig)", "pig", "rat"),
                            selected = c(unique(
                                as.character(Protein_Digestibility_Data$Species)
                            )),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        virtualSelectInput(
                            inputId = "model",
                            label = "Model:",
                            choices = c("in vivo", "in vitro"),
                            selected = c(unique(
                                as.character(Protein_Digestibility_Data$Model)
                            )),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        virtualSelectInput(
                            inputId = "sample",
                            label = "Sample:",
                            choices = c("fecal", "ileal"),
                            selected = c(unique(
                                as.character(Protein_Digestibility_Data$Sample)
                            )),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        virtualSelectInput(
                            inputId = "measure",
                            label = "Measure:",
                            choices = c(
                                "apparent digestibility",
                                "standardized digestibility",
                                "true digestibility",
                                "biological value",
                                "metabolic activity"
                            ),
                            selected = c(unique(
                                as.character(Protein_Digestibility_Data$Measure)
                            )),
                            multiple = TRUE,
                            showValueAsTags = TRUE,
                            width = '100%'
                        )
                    ),
                    column(
                        4,
                        virtualSelectInput(
                            inputId = "analyte",
                            label = "Analyte:",
                            choices = list(
                                "crude protein" = "crude protein",
                                "Essential amino acid" = c(
                                    "histidine",
                                    "isoleucine",
                                    "leucine",
                                    "lysine",
                                    "reactive lysine",
                                    "methionine",
                                    "phenylalanine",
                                    "threonine",
                                    "tryptophan",
                                    "valine"
                                ),
                                "Conditionally essential amino acid" = c("arginine", "cysteine", "glycine", "proline", "tyrosine"),
                                "Non-essential amino acid" = c("alanine", "aspartic acid", "glutamic acid",  "serine")
                            ),
                            selected = unique(as.character(Protein_Digestibility_Data$Analyte)),
                            showValueAsTags = TRUE,
                            multiple = TRUE,
                            width = '100%'
                        )
                    )
                ),
                fluidRow(br()),
                # Create a new Row in the UI for search options
                fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                         column(
                             2,
                             p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                         )),
                fluidRow(
                    style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                    column(
                        2,
                        textInput(
                            inputId = "NI_ID_tab1",
                            label = "NI_ID:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "foodGrp",
                            label = "Food group:",
                            width = '100%'
                        )
                    ),
                    column(2,
                           textInput(
                               inputId = "food",
                               label = "Food:",
                               width = '100%'
                           )),
                    column(
                        2,
                        textInput(
                            inputId = "analysisMethod",
                            label = "Analysis method:",
                            width = '100%'
                        )
                    ),
                    column(
                        2,
                        textInput(
                            inputId = "source",
                            label = "Sources:",
                            width = '100%'
                        )
                    )
                ),
                fluidRow(br()),
                # Create a new row for the table.
                fluidRow(style = "padding-top: 20px;",
                         DT::dataTableOutput("table"))
            ),
            nav_panel(
                "AA Composition Data",
                fluidRow(
                    div(
                        id = "info-toggle_3",
                        class = "accordion-toggle",
                        onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_3_click', Math.random(), {priority: 'event'}); }",
                        column(
                            8,
                            p("Information", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                        ),
                        column(4, uiOutput("toggleButton_3"))
                    )
                ),
                fluidRow(hidden(
                    div(
                        id = "infoContent_3",
                        class = "accordion-content",
                        style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                        p("Here you will find the food composition data used to calculate protein quality scores. References to food composition data sources can be found below."),
                        br(),
                        p("References:"),
                        tags$ol(
                            tags$li("Haytowitz DB, Ahuja JKC, Wu X, Somanchi M, Nickle M, Nguyen QA, et al. USDA National Nutrient Database for Standard Reference, Legacy Release. In: FoodData Central.  Nutrient Data Laboratory, Beltsville Human Nutrition Research Center, ARS, USDA; 2019.  https://doi.org/10.15482/USDA.ADC/1529216. Accessed May 27, 2022.")
                        ),
                        br()
                    )
                )),
                fluidRow(br()),
                fluidRow(
                    div(style = "font-size: 1.2em;",
                        prettyToggle(
                            inputId = "show_NI_ID",
                            label_on = "Hide NI_ID",
                            icon_on = icon("square-minus"),
                            status_on = "default",
                            status_off = "default",
                            label_off = "Show NI_ID",
                            icon_off = icon("square-plus"),
                            bigger = TRUE,
                            shape = "curve"
                        )
                    )
                ),
                # Create a new Row in the UI for selectInputs
                # fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
                #          column(
                #              4,
                #              p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
                #          )),
                # fluidRow(
                #     style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
                #     column(
                #         2,
                #         textInput(inputId = "foodcomp_NI_ID",
                #                   label = "NI_ID")
                #     ),
                #     column(
                #         2,
                #         textInput(inputId = "foodcomp_food",
                #                   label = "Food")
                #         ),
                #     column(
                #         2,
                #         textInput(inputId = "foodcomp_FDC_ID",
                #                   label = "FDC_ID")
                #     )
                #     ),
                # fluidRow(br()),
                # Create a new row for the table.
                fluidRow(style = "padding-top: 20px;",
                         DT::dataTableOutput("table_3"))
            )
        )
    )
)

server <- function(input, output, session) {
    visibility <- reactiveVal(TRUE)

    output$toggleButton <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    output$toggleButton_2 <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide_2",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide_2",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    output$toggleButton_3 <- renderUI({
        if (visibility()) {
            actionButton(
                "show_hide_3",
                label = icon("plus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        } else {
            actionButton(
                "show_hide_3",
                label = icon("minus", style = "color: #333; font-size: 24px; font-weight: bold;"),
                class = "float-right-button"
            )
        }
    })

    observeEvent(input$show_hide, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    })

    observeEvent(input$show_hide_2, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_2", anim = TRUE)
    })

    observeEvent(input$show_hide_3, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_3", anim = TRUE)
    })


    # Observe clicks on the info-toggle div
    observeEvent(input$info_toggle_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    observeEvent(input$info_toggle_2_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_2", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase

    observeEvent(input$info_toggle_3_click, {
        visibility(!visibility())
        shinyjs::toggle(id = "infoContent_3", anim = TRUE)
    }, ignoreInit = TRUE)  # ignore initialization phase



    observeEvent(input$EAA_rec, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec)

        updatePickerInput(
            session = session,
            inputId = "rec_age",
            choices = unique(age_options$Age)
        )
    })

    observeEvent(input$EAA_rec_PDCAAS, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec_PDCAAS)

        updatePickerInput(
            session = session,
            inputId = "rec_age_PDCAAS",
            choices = rev(unique(age_options$Age))
        )
    })

    observeEvent(input$EAA_rec_DIAAS, {
        age_options <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec_DIAAS)

        updatePickerInput(
            session = session,
            inputId = "rec_age_DIAAS",
            choices = rev(unique(age_options$Age))
        )
    })



    # render table for protein digestibility data
    output$table <- DT::renderDataTable(server = TRUE, {
        DT::datatable(
            Protein_Digestibility_Data %>%
                mutate(n = as.character(n)) %>%
                mutate(SD = as.character(SD)) %>%
                mutate(`Protein (g)` = as.character(`Protein (g)`)) %>%
                replace_na(
                    list(
                        "Food group" = "not reported",
                        Food = "not reported",
                        "Protein (g)" = "not reported",
                        Diet = "not reported",
                        n = "not reported",
                        SD = "not reported",
                        "Analysis method(s)" = "not reported",
                        "Original Source(s)" = "not reported"
                    )
                ) %>%
                filter(`Species` %in% input$species) %>%
                filter(`Model` %in% input$model) %>%
                filter(`Sample` %in% input$sample) %>%
                filter(`Analyte` %in% input$analyte) %>%
                filter(`Measure` %in% input$measure) %>%
                filter(str_detect(
                    NI_ID, ifelse(input$NI_ID_tab1 == "", ".*", paste0("(?i)", input$NI_ID_tab1))
                )) %>%
                filter(str_detect(
                    `Food group`,
                    ifelse(
                        input$foodGrp == "",
                        "[aeiou]",
                        paste0("(?i)", input$foodGrp)
                    )
                )) %>%
                filter(str_detect(
                    Food, ifelse(input$food == "", "[aeiou]", paste0("(?i)", input$food))
                )) %>%
                filter(str_detect(
                    `Analysis method(s)`,
                    ifelse(
                        input$analysisMethod == "",
                        "[aeiou]",
                        paste0("(?i)", input$analysisMethod)
                    )
                )) %>%
                filter(
                    str_detect(
                        `Collected From`,
                        ifelse(
                            input$source == "",
                            "[aeiou]",
                            paste0("(?i)", input$source)
                        )
                    ) |
                        str_detect(
                            `Original Source(s)`,
                            ifelse(
                                input$source == "",
                                "[aeiou]",
                                paste0("(?i)", input$source)
                            )
                        )
                ) %>%
                mutate(across(
                    where(is.character), ~ gsub("\n", "<br>", .)
                )),
            extensions = c('Buttons', 'FixedHeader'),
            class = "display cell-border compact",
            rownames = FALSE,
            options = list(
                fixedHeader = TRUE,
                dom = 'Brtip',
                pageLength = 25,
                lengthMenu = c(25, 50, 75, 100),
                columnDefs = list(list(
                    targets = "_all", className = "dt-head-nowrap"
                )),
                buttons = list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = list(
                            list(extend = "csv", filename = fileName),
                            list(extend = "excel", filename = fileName),
                            list(extend = "pdf", filename = fileName)
                        ),
                        text = 'Download'
                    )
                )
            ),
            escape = FALSE  # Allow HTML rendering
        )  %>%
            formatStyle(1:16,
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle(
                14:15,
                'overflow-wrap' = 'break-word',
                'word-break' = 'break-word'
            )
    })

    # render table for protein quality scores
    output$table_2 <- DT::renderDataTable(server = TRUE, {
        PQ_df <- data.frame(NI_ID = character())

        if ("EAA-9" %in% input$score) {
            if (input$EAA_rec == "Choose custom recommendations") {
                scoring_pattern[199,] <- list("Choose custom recommendations", "histidine", ifelse(is.numeric(input$hist),input$hist, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[200,] <- list("Choose custom recommendations", "leucine", ifelse(is.numeric(input$leu),input$leu, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[201,] <- list("Choose custom recommendations", "isoleucine", ifelse(is.numeric(input$ile),input$ile, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[202,] <- list("Choose custom recommendations", "lysine", ifelse(is.numeric(input$lys),input$lys, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[203,] <- list("Choose custom recommendations", "methionine+cysteine", ifelse(is.numeric(input$met_cys),input$met_cys, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[204,] <- list("Choose custom recommendations", "phenylalanine+tyrosine", ifelse(is.numeric(input$phe_tyr),input$phe_tyr, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[205,] <- list("Choose custom recommendations", "threonine", ifelse(is.numeric(input$thr),input$thr, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[206,] <- list("Choose custom recommendations", "tryptophan", ifelse(is.numeric(input$trp),input$trp, 1), "mg/kg/d", "custom", NA, NA, NA)
                scoring_pattern[207,] <- list("Choose custom recommendations", "valine", ifelse(is.numeric(input$val),input$val, 1), "mg/kg/d", "custom", NA, NA, NA)
            }

            if (input$show_calc == "score_only") {
                    if (input$serving_size == "Use standard serving sizes") {

                        EAA_9 <- EAA_composition %>%
                            left_join(
                                portion_sizes %>%
                                    select(fdc_id, g_weight , portion) %>%
                                    rename("fdcId" = "fdc_id") %>%
                                    mutate(portion = paste0(portion, " (", g_weight, " g)"))
                            ) %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (g_weight / 100)) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) / 100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" &
                                            Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    } else{
                        EAA_9 <- EAA_composition %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (input$serving_weight / 100)) %>%
                            mutate(portion = paste0(input$serving_weight, " g")) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    }
                }
                if (input$show_calc == "show_calc") {
                    if (input$serving_size == "Use standard serving sizes") {
                        EAA_9 <- EAA_composition %>%
                            left_join(
                                portion_sizes %>%
                                    select(fdc_id, g_weight , portion) %>%
                                    rename("fdcId" = "fdc_id") %>%
                                    mutate(portion = paste0(portion, " (", g_weight, " g)"))
                                ) %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (g_weight / 100)) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                                ) %>%
                            mutate(calculation = paste0(round(value,2), "/", round(Amount * input$weight, 2))) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId, NI_ID, Protein, portion, value, AA, calculation)

                        temp <- EAA_9 %>%
                            select(fdcId, NI_ID, value, calculation) %>%
                            group_by(fdcId, NI_ID) %>%
                            summarise(
                                calculation = paste0(
                                    "min(",
                                    str_c(calculation, collapse = ", "),
                                    ") x 100 x digestibility = ",
                                    paste0(min(round(
                                        value, 2
                                    ))),
                                    " x 100 x digestibility"
                                )
                            ) %>%
                            ungroup()

                        EAA_9 <- EAA_9 %>%
                            select(!calculation) %>%
                            left_join(temp) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`,
                                   calculation) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            mutate(
                                calculation = str_replace_all(
                                    calculation,
                                    "digestibility",
                                    as.character(`Value (%)`)
                                )
                            ) %>%
                            mutate(calculation = paste0(calculation, " = ", `EAA-9`)) %>%
                            mutate(`EAA-9` = calculation) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            rename("EAA-9 (%)" = "EAA-9")

                        rm(temp)
                    } else{
                        EAA_9 <- EAA_composition %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (input$serving_weight /
                                                        100)) %>%
                            mutate(portion = paste0(input$serving_weight, " g")) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(calculation = paste0(value, "/", Amount *
                                                            input$weight)) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA,
                                   calculation)

                        temp <- EAA_9 %>%
                            select(fdcId, NI_ID, value, calculation) %>%
                            group_by(fdcId, NI_ID) %>%
                            summarise(
                                calculation = paste0(
                                    "min(",
                                    str_c(calculation, collapse = ", "),
                                    ") x 100 x digestibility = ",
                                    paste0(min(round(value, 2))),
                                    " x 100 x digestibility"
                                )
                            ) %>%
                            ungroup()

                        EAA_9 <- EAA_9 %>%
                            select(!calculation) %>%
                            left_join(temp) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`,
                                   calculation) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            mutate(
                                calculation = str_replace_all(
                                    calculation,
                                    "digestibility",
                                    as.character(`Value (%)`)
                                )
                            ) %>%
                            mutate(calculation = paste0(calculation, " = ", `EAA-9`)) %>%
                            mutate(`EAA-9` = calculation) %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    }
                } else{
                    if (input$serving_size == "Use standard serving sizes") {
                        EAA_9 <- EAA_composition %>%
                            left_join(
                                portion_sizes %>%
                                    select(fdc_id, g_weight , portion) %>%
                                    rename("fdcId" = "fdc_id") %>%
                                    mutate(
                                        portion = paste0(portion, " (", g_weight, " g)")
                                    )
                            ) %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (g_weight / 100)) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    } else{
                        EAA_9 <- EAA_composition %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (input$serving_weight /
                                                        100)) %>%
                            mutate(portion = paste0(input$serving_weight, " g")) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    }
                }
                if (input$show_calc == "show_calc") {
                    if (input$serving_size == "Use standard serving sizes") {
                        EAA_9 <- EAA_composition %>%
                            left_join(
                                portion_sizes %>%
                                    select(fdc_id, g_weight , portion) %>%
                                    rename("fdcId" = "fdc_id") %>%
                                    mutate(
                                        portion = paste0(portion, " (", g_weight, " g)")
                                    )
                            ) %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (g_weight / 100)) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(calculation = paste0(value, "/", Amount *
                                                            input$weight)) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA,
                                   calculation)

                        temp <- EAA_9 %>%
                            select(fdcId, NI_ID, value, calculation) %>%
                            group_by(fdcId, NI_ID) %>%
                            summarise(
                                calculation = paste0(
                                    "min(",
                                    str_c(calculation, collapse = ", "),
                                    ") x 100 x digestibility = ",
                                    paste0(min(round(
                                        value, 2
                                    ))),
                                    " x 100 x digestibility"
                                )
                            ) %>%
                            ungroup()

                        EAA_9 <- EAA_9 %>%
                            select(!calculation) %>%
                            left_join(temp) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`,
                                   calculation) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            mutate(
                                calculation = str_replace_all(
                                    calculation,
                                    "digestibility",
                                    as.character(`Value (%)`)
                                )
                            ) %>%
                            mutate(calculation = paste0(calculation, " = ", `EAA-9`)) %>%
                            mutate(`EAA-9` = calculation) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            rename("EAA-9 (%)" = "EAA-9")

                        rm(temp)
                    } else{
                        EAA_9 <- EAA_composition %>%
                            mutate(value = value * 1000) %>%
                            mutate(value = value * (input$serving_weight /
                                                        100)) %>%
                            mutate(portion = paste0(input$serving_weight, " g")) %>%
                            left_join(
                                scoring_pattern %>%
                                    rename("AA" = "Analyte") %>%
                                    filter(`Pattern Name` == input$EAA_rec) %>%
                                    filter(ifelse(is.character(input$rec_age), Age == input$rec_age, !is.na(Age))) %>%
                                    select(AA, Amount)
                            ) %>%
                            mutate(calculation = paste0(value, "/", Amount *
                                                            input$weight)) %>%
                            mutate(value = value / (Amount * input$weight)) %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   portion,
                                   value,
                                   AA,
                                   calculation)

                        temp <- EAA_9 %>%
                            select(fdcId, NI_ID, value, calculation) %>%
                            group_by(fdcId, NI_ID) %>%
                            summarise(
                                calculation = paste0(
                                    "min(",
                                    str_c(calculation, collapse = ", "),
                                    ") x 100 x digestibility = ",
                                    paste0(min(round(
                                        value, 2
                                    ))),
                                    " x 100 x digestibility"
                                )
                            ) %>%
                            ungroup()

                        EAA_9 <- EAA_9 %>%
                            select(!calculation) %>%
                            left_join(temp) %>%
                            group_by(fdcId, NI_ID, Protein, portion) %>%
                            mutate(`EAA-9` = min(value)) %>%
                            filter(value == `EAA-9`) %>%
                            ungroup() %>%
                            select(fdcId,
                                   NI_ID,
                                   Protein,
                                   AA,
                                   portion,
                                   `EAA-9`,
                                   calculation) %>%
                            rename("Limiting AA" = "AA") %>%
                            separate_longer_delim(NI_ID, delim = ";") %>%
                            left_join(Protein_Digestibility_Data) %>%
                            mutate(value_label = `Value (%)`) %>%
                            mutate(`Value (%)` = as.numeric(`Value (%)`) /
                                       100) %>%
                            mutate(`EAA-9` = `EAA-9` * `Value (%)`) %>%
                            filter(
                                Analyte == "crude protein" |
                                    `Limiting AA` == Analyte |
                                    (
                                        `Limiting AA` == "methionine+cysteine" &
                                            Analyte == "methionine"
                                    ) |
                                    (
                                        `Limiting AA` == "phenylalanine+tyrosine" &
                                            Analyte == "phenylalanine"
                                    ) | (
                                        `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                                    )
                            ) %>%
                            filter(Measure != "biological value") %>%
                            mutate(`EAA-9` = round(`EAA-9` * 100, 2)) %>%
                            mutate(
                                calculation = str_replace_all(
                                    calculation,
                                    "digestibility",
                                    as.character(`Value (%)`)
                                )
                            ) %>%
                            mutate(calculation = paste0(calculation, " = ", `EAA-9`)) %>%
                            mutate(`EAA-9` = calculation) %>%
                            select(
                                NI_ID,
                                Food,
                                Measure,
                                Sample,
                                Species,
                                Analyte,
                                value_label,
                                `Limiting AA`,
                                fdcId,
                                portion,
                                `EAA-9`
                            ) %>%
                            rename("Digestibility Measure" = "Measure") %>%
                            rename("Digestibility Sample" = "Sample") %>%
                            rename("Digestibility Species" = "Species") %>%
                            rename("Digestibility Analyte" = "Analyte") %>%
                            rename("Digestibility Value (%)" = "value_label") %>%
                            rename("serving size" = "portion") %>%
                            rename("EAA-9 (%)" = "EAA-9")
                    }
                }



            PQ_df <- full_join(PQ_df, EAA_9)

        }

        if (ifelse(length(input$score != 1), "PDCAAS" %in% input$score,  "PDCAAS" == input$score)) {
            if (input$show_calc == "score_only") {
                PDCAAS <- EAA_composition %>%
                    mutate(value = (value * 1000) / Protein) %>%
                    left_join(
                        scoring_pattern %>%
                            rename("AA" = "Analyte") %>%
                            filter(`Pattern Name` == input$EAA_rec_PDCAAS) %>%
                            filter(Age == input$rec_age_PDCAAS) %>%
                            select(AA, Amount)
                    ) %>%
                    mutate(value = value / Amount) %>%
                    select(fdcId, NI_ID, value, AA) %>%
                    group_by(fdcId, NI_ID) %>%
                    mutate(PDCAAS = min(value)) %>%
                    filter(value == PDCAAS) %>%
                    ungroup() %>%
                    mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
                    select(fdcId, NI_ID, AA, PDCAAS) %>%
                    rename("Limiting AA" = "AA") %>%
                    separate_longer_delim(NI_ID, delim = ";") %>%
                    left_join(Protein_Digestibility_Data) %>%
                    mutate(value_label = `Value (%)`) %>%
                    mutate(`Value (%)` = as.numeric(`Value (%)`) / 100) %>%
                    mutate(PDCAAS = PDCAAS * `Value (%)`) %>%
                    filter(
                        Analyte == "crude protein" |
                            `Limiting AA` == Analyte |
                            (
                                `Limiting AA` == "methionine+cysteine" &
                                    Analyte == "methionine"
                            ) |
                            (
                                `Limiting AA` == "phenylalanine+tyrosine" &
                                    Analyte == "phenylalanine"
                            ) | (
                                `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                            )
                    ) %>%
                    filter(Measure != "biological value") %>%
                    select(
                        NI_ID,
                        Food,
                        Measure,
                        Sample,
                        Species,
                        Analyte,
                        value_label,
                        `Limiting AA`,
                        fdcId,
                        PDCAAS
                    ) %>%
                    rename("Digestibility Measure" = "Measure") %>%
                    rename("Digestibility Sample" = "Sample") %>%
                    rename("Digestibility Species" = "Species") %>%
                    rename("Digestibility Analyte" = "Analyte") %>%
                    rename("Digestibility Value (%)" = "value_label") %>%
                    distinct() %>%
                    mutate(PDCAAS = round(PDCAAS, 4))
            }
            if (input$show_calc == "show_calc") {
                PDCAAS <- EAA_composition %>%
                    mutate(value = (value) / Protein) %>%
                    mutate(value = value * 1000) %>%
                    left_join(
                        scoring_pattern %>%
                            rename("AA" = "Analyte") %>%
                            filter(`Pattern Name` == input$EAA_rec_PDCAAS) %>%
                            filter(Age == input$rec_age_PDCAAS) %>%
                            select(AA, Amount)
                    ) %>%
                    mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                    mutate(value = value / Amount) %>%
                    select(fdcId, NI_ID, Protein, value, AA, calculation)

                temp_2 <- PDCAAS %>%
                    select(fdcId, NI_ID, value, calculation) %>%
                    group_by(fdcId, NI_ID) %>%
                    summarise(
                        calculation = paste0(
                            "min(",
                            str_c(calculation, collapse = ", "),
                            ", 1) x digestibility = ",
                            paste0(ifelse(
                                min(round(value, 2)) >= 1, 1, min(round(value, 2))
                            )),
                            " x digestibility"
                        )
                    ) %>%
                    ungroup()

                PDCAAS <- PDCAAS %>%
                    select(!calculation) %>%
                    select(fdcId, NI_ID, Protein, value, AA) %>%
                    group_by(fdcId, NI_ID, Protein) %>%
                    mutate(PDCAAS = min(value)) %>%
                    filter(value == PDCAAS) %>%
                    ungroup() %>%
                    left_join(temp_2) %>%
                    mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
                    select(fdcId, NI_ID, Protein, AA, PDCAAS, calculation) %>%
                    rename("Limiting AA" = "AA") %>%
                    separate_longer_delim(NI_ID, delim = ";") %>%
                    left_join(Protein_Digestibility_Data) %>%
                    mutate(value_label = `Value (%)`) %>%
                    mutate(`Value (%)` = as.numeric(`Value (%)`) / 100) %>%
                    mutate(PDCAAS = PDCAAS * `Value (%)`) %>%
                    filter(
                        Analyte == "crude protein" |
                            `Limiting AA` == Analyte |
                            (
                                `Limiting AA` == "methionine+cysteine" &
                                    Analyte == "methionine"
                            ) |
                            (
                                `Limiting AA` == "phenylalanine+tyrosine" &
                                    Analyte == "phenylalanine"
                            ) | (
                                `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                            )
                    ) %>%
                    filter(Measure != "biological value") %>%
                    mutate(PDCAAS = round(PDCAAS, 4)) %>%
                    mutate(calculation = str_replace_all(
                        calculation,
                        "digestibility",
                        as.character(`Value (%)`)
                    )) %>%
                    mutate(calculation = paste0(calculation, " = ", PDCAAS)) %>%
                    mutate(PDCAAS = calculation) %>%
                    select(
                        NI_ID,
                        Food,
                        Measure,
                        Sample,
                        Species,
                        Analyte,
                        value_label,
                        `Limiting AA`,
                        fdcId,
                        PDCAAS
                    ) %>%
                    rename("Digestibility Measure" = "Measure") %>%
                    rename("Digestibility Sample" = "Sample") %>%
                    rename("Digestibility Species" = "Species") %>%
                    rename("Digestibility Analyte" = "Analyte") %>%
                    rename("Digestibility Value (%)" = "value_label") %>%
                    distinct()
                rm(temp_2)
            }


            PQ_df <- full_join(PQ_df, PDCAAS)
        }
        if ("DIAAS" %in% input$score) {
            if (input$show_calc == "score_only") {
                DIAAS <- EAA_composition %>%
                    mutate(value = (value * 1000) / Protein) %>%
                    left_join(
                        scoring_pattern %>%
                            rename("AA" = "Analyte") %>%
                            filter(`Pattern Name` == input$EAA_rec_DIAAS) %>%
                            filter(Age == input$rec_age_DIAAS) %>%
                            select(AA, Amount)
                    ) %>%
                    mutate(value = value / Amount) %>%
                    select(fdcId, NI_ID, Protein, value, AA) %>%
                    group_by(fdcId, NI_ID, Protein) %>%
                    mutate(DIAAS = min(value)) %>%
                    filter(value == DIAAS) %>%
                    ungroup() %>%
                    select(fdcId, NI_ID, Protein, AA, DIAAS) %>%
                    rename("Limiting AA" = "AA") %>%
                    separate_longer_delim(NI_ID, delim = ";") %>%
                    left_join(Protein_Digestibility_Data) %>%
                    mutate(value_label = `Value (%)`) %>%
                    mutate(`Value (%)` = as.numeric(`Value (%)`) / 100) %>%
                    mutate(DIAAS = DIAAS * `Value (%)`) %>%
                    filter(
                        Analyte == "crude protein" |
                            `Limiting AA` == Analyte |
                            (
                                `Limiting AA` == "methionine+cysteine" &
                                    Analyte == "methionine"
                            ) |
                            (
                                `Limiting AA` == "phenylalanine+tyrosine" &
                                    Analyte == "phenylalanine"
                            ) | (
                                `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                            )
                    ) %>%
                    filter(Measure != "biological value") %>%
                    select(
                        NI_ID,
                        Food,
                        Measure,
                        Sample,
                        Species,
                        Analyte,
                        value_label,
                        `Limiting AA`,
                        fdcId,
                        DIAAS
                    ) %>%
                    rename("Digestibility Measure" = "Measure") %>%
                    rename("Digestibility Sample" = "Sample") %>%
                    rename("Digestibility Species" = "Species") %>%
                    rename("Digestibility Analyte" = "Analyte") %>%
                    rename("Digestibility Value (%)" = "value_label") %>%
                    mutate(DIAAS = round(DIAAS * 100, 2))
            }
            if (input$show_calc == "show_calc") {
                DIAAS <- EAA_composition %>%
                    mutate(value = (value * 1000) / Protein) %>%
                    left_join(
                        scoring_pattern %>%
                            rename("AA" = "Analyte") %>%
                            filter(`Pattern Name` == input$EAA_rec_DIAAS) %>%
                            filter(Age == input$rec_age_DIAAS) %>%
                            select(AA, Amount)
                    ) %>%
                    mutate(calculation = paste0(round(value, 2), "/", Amount)) %>%
                    mutate(value = value / Amount) %>%
                    select(fdcId, NI_ID, Protein, value, AA, calculation)

                temp_1 <- DIAAS %>%
                    select(fdcId, NI_ID, value, calculation) %>%
                    group_by(fdcId, NI_ID) %>%
                    summarise(
                        calculation = paste0(
                            "100 x min(",
                            str_c(calculation, collapse = ", "),
                            ") x digestibility = 100 x ",
                            paste0(min(round(
                                value, 2
                            ))),
                            " x digestibility"
                        )
                    ) %>%
                    ungroup()

                DIAAS <- DIAAS %>%
                    select(!calculation) %>%
                    left_join(temp_1) %>%
                    group_by(fdcId, NI_ID, Protein) %>%
                    select(fdcId, NI_ID, Protein, value, AA, calculation) %>%
                    group_by(fdcId, NI_ID, Protein) %>%
                    mutate(DIAAS = min(value)) %>%
                    filter(value == DIAAS) %>%
                    ungroup() %>%
                    select(fdcId, NI_ID, Protein, AA, DIAAS, calculation) %>%
                    rename("Limiting AA" = "AA") %>%
                    separate_longer_delim(NI_ID, delim = ";") %>%
                    left_join(Protein_Digestibility_Data) %>%
                    mutate(value_label = `Value (%)`) %>%
                    mutate(`Value (%)` = as.numeric(`Value (%)`) / 100) %>%
                    mutate(DIAAS = DIAAS * `Value (%)`) %>%
                    filter(
                        Analyte == "crude protein" |
                            `Limiting AA` == Analyte |
                            (
                                `Limiting AA` == "methionine+cysteine" &
                                    Analyte == "methionine"
                            ) |
                            (
                                `Limiting AA` == "phenylalanine+tyrosine" &
                                    Analyte == "phenylalanine"
                            ) | (
                                `Limiting AA` == "lysine" & Analyte == "reactive lysine"
                            )
                    ) %>%
                    filter(Measure != "biological value") %>%
                    mutate(DIAAS = round(DIAAS, 4) * 100) %>%
                    mutate(calculation = str_replace_all(
                        calculation,
                        "digestibility",
                        as.character(`Value (%)`)
                    )) %>%
                    mutate(calculation = paste0(calculation, " = ", DIAAS)) %>%
                    mutate(DIAAS = calculation) %>%
                    select(
                        NI_ID,
                        Food,
                        Measure,
                        Sample,
                        Species,
                        Analyte,
                        value_label,
                        `Limiting AA`,
                        fdcId,
                        DIAAS
                    ) %>%
                    rename("Digestibility Measure" = "Measure") %>%
                    rename("Digestibility Sample" = "Sample") %>%
                    rename("Digestibility Species" = "Species") %>%
                    rename("Digestibility Analyte" = "Analyte") %>%
                    rename("Digestibility Value (%)" = "value_label") %>%
                    distinct()
                rm(temp_1)
            }

            PQ_df <- full_join(PQ_df, DIAAS)
        }
        if("crude protein" %in% input$pq_analyte){
            if(!("individual amino acids" %in% input$pq_analyte)){
                PQ_df <- PQ_df %>%
                    filter(`Digestibility Analyte` == "crude protein")
            }
        }
        if("individual amino acids" %in% input$pq_analyte){
            if(!("crude protein" %in% input$pq_analyte)){
                PQ_df <- PQ_df %>%
                    filter(`Digestibility Analyte` != "crude protein")
            }
        }
        DT::datatable(
            PQ_df %>%
                # mutate(
                #     fdcId = paste0(
                #         "Haytowitz DB, Ahuja JKC, Wu X, Somanchi M, Nickle M, Nguyen QA, et al. USDA National Nutrient Database for Standard Reference, Legacy Release. In: FoodData Central.  Nutrient Data Laboratory, Beltsville Human Nutrition Research Center, ARS, USDA; 2019.  https://doi.org/10.15482/USDA.ADC/1529216. Accessed May 27, 2022. <br> fdcId: ",
                #         fdcId
                #     )
                # ) %>%
                arrange(NI_ID) %>%
                mutate("Food Composition Ref No" = 1) %>%
                filter(`Digestibility Sample` %in% input$pq_sample) %>%
                filter(`Digestibility Measure` %in% input$pq_measure) %>%
                filter(`Digestibility Species` %in% input$pq_species),
            class = "display cell-border compact",
            rownames = FALSE,
            extensions = c('FixedHeader', 'Buttons'),
            options = list(
                fixedHeader = TRUE,
                dom = 'Brftip',
                pageLength = 25,
                lengthMenu = c(15, 25, 50, 75, 100),
                columnDefs = list(list(
                    targets = c(1, 2, 9), className = "dt-head-nowrap"
                )),
                buttons = list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = list(
                            list(extend = "csv", filename = fileName),
                            list(extend = "excel", filename = fileName),
                            list(extend = "pdf", filename = fileName)
                        ),
                        text = 'Download'
                    )
                )
            ),
            escape = FALSE  # Allow HTML rendering
        )  %>%
            formatStyle(1:16,
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle(3:7, width = '6%') %>%
            formatStyle(9, width = '8%')
    })

    # table for food mappings
    output$table_3 <- DT::renderDataTable(server = TRUE, {
        fdcmp_df <- EAA_composition

        if(input$show_NI_ID == FALSE){
            fdcmp_df <- fdcmp_df %>%
                select(fdcId, description, Protein, AA, value) %>%
                mutate("FDC_ID" = fdcId) %>%
                select(FDC_ID, description, Protein, AA, value) %>%
                distinct() %>%
                mutate(AA = factor(AA,
                                   levels = c("histidine", "isoleucine", "leucine", "lysine", "methionine+cysteine", "phenylalanine+tyrosine", "threonine", "tryptophan", "valine"),
                                   labels = c("His (g/100g)", "Ile (g/100g)", "Leu (g/100g)", "Lys (g/100g)", "Met+Cys (g/100g)", "Phe+Tyr (g/100g)", "Thr (g/100g)", "Trp (g/100g)", "Val (g/100g)"))) %>%
                pivot_wider(names_from = "AA", values_from = "value") %>%
                rename("Protein (g/100g)" = "Protein") %>%
                rename("Food description" = "description") %>%
                mutate("Ref No" = 1) %>%
                distinct()
        }else{
            fdcmp_df <- EAA_composition %>%
                select(NI_ID, description, fdcId, Protein, AA, value) %>%
                mutate("FDC_ID" = fdcId) %>%
                mutate(NI_ID = str_replace_all(NI_ID, ";", "; ")) %>%
                select(NI_ID, FDC_ID, description, Protein, AA, value) %>%
                distinct() %>%
                mutate(AA = factor(AA,
                                   levels = c("histidine", "isoleucine", "leucine", "lysine", "methionine+cysteine", "phenylalanine+tyrosine", "threonine", "tryptophan", "valine"),
                                   labels = c("His (g/100g)", "Ile (g/100g)", "Leu (g/100g)", "Lys (g/100g)", "Met+Cys (g/100g)", "Phe+Tyr (g/100g)", "Thr (g/100g)", "Trp (g/100g)", "Val (g/100g)"))) %>%
                pivot_wider(names_from = "AA", values_from = "value") %>%
                rename("Protein (g/100g)" = "Protein") %>%
                rename("Food description" = "description") %>%
                mutate("Ref No" = 1) %>%
                distinct()
        }

        DT::datatable(
            fdcmp_df,
            extensions = c('Buttons', 'FixedHeader'),
            class = "display cell-border compact",
            rownames = FALSE,
            options = list(
                fixedHeader = TRUE,
                scrollCollapse = TRUE,
                dom = 'Brftip',
                pageLength = 25,
                lengthMenu = c(15, 25, 50, 75, 100),
                buttons = list(
                    'copy',
                    'print',
                    list(
                        extend = 'collection',
                        buttons = list(
                            list(extend = "csv", filename = fileName),
                            list(extend = "excel", filename = fileName),
                            list(extend = "pdf", filename = fileName)
                        ),
                        text = 'Download'
                    )
                )
            ),
            escape = FALSE  # Allow HTML rendering
        ) %>%
            formatStyle(1:16,
                        'vertical-align' = 'top',
                        'overflow-wrap' = 'break-word') %>%
            formatStyle(4:14, width = '5%')
    })
}

shinyApp(ui, server)
