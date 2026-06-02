library(tidyverse)
library(shiny)
library(DT)
library(readr)
library(shinyWidgets)
library(fontawesome)
library(shinyjs)
library(bslib)
library(openxlsx)

fuzzy_search_filter <- function(df, search_term) {
  if (search_term == "") return(df)

  clean_text <- function(x) {
    x <- tolower(x)
    x <- gsub("'s\\b", "", x)
    x <- gsub("[^a-z0-9\\s]", " ", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }

  search_tokens <- strsplit(clean_text(search_term), "\\s+")[[1]]

  # Combine all columns into one string per row
  df$.__combined__ <- apply(df, 1, function(row) clean_text(paste(row, collapse = " ")))

  # Filter rows where all search tokens appear (substring match)
  df_filtered <- df[apply(df, 1, function(row) {
    val <- clean_text(paste(row, collapse = " "))
    all(sapply(search_tokens, function(tok) grepl(tok, val, fixed = TRUE)))
  }), ]

  df_filtered$.__combined__ <- NULL  # Clean up
  return(df_filtered)
}



Protein_Correction <-
  read_csv("Correction Factors - full data.csv") %>%
  mutate(`Protein Form` = ifelse(`Protein Form` == "cystine", "cysteine", `Protein Form`)) %>%
  mutate(`Protein Form` = ifelse(`Protein Form` == "arganine", "arginine", `Protein Form`)) %>%
  select(!Notes) %>%
  select(!Diet) %>%
  select(!`Food group`) %>%
  select(!Model) %>%
  filter(Calculation != "biological value")
EAA_composition <- read_csv("EAA_composition.csv") %>%
  pivot_longer(histidine:valine, names_to = "AA", values_to = "value") %>%
  mutate(`food identifier` = str_trim(`food identifier`)) %>%
  mutate(fdcId = as.character(fdcId)) %>%
  filter(!(is.na(`food identifier`) & is.na(fdcId))) %>%
  replace_na(list(NI_ID = "Not Available", fdcId = "Not Applicable"))

scoring_pattern <- read_csv("scoring_pattern.csv")
portion_sizes <- read_csv("portion_sizes.csv")

col_meta <- read_csv("col_meta.csv")




# shiny app
ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
      /* Style default browser tooltips */
    [title] {
      position: relative;
    }

    [title]:hover::after {
      content: attr(title);
      position: absolute;
      white-space: pre-line;
      top: 100%;
      left: 0;
      z-index: 9999;

      background-color: rgba(0, 0, 0, 0.85);
      color: #fff;
      padding: 8px 10px;
      border-radius: 6px;
      font-size: 14px;
      font-weight: normal;
      min-width: 200px;
      max-width: 800px;
      line-height: 1.3;
      box-shadow: 0px 2px 8px rgba(0, 0, 0, 0.2);
    }

    [title]:hover::before {
      content: '';
      position: absolute;
      top: 95%;
      left: 10px;
      margin-left: -5px;
      border-width: 5px;
      border-style: solid;
      border-color: rgba(0, 0, 0, 0.85) transparent transparent transparent;
      z-index: 9999;
    }

      /* Enforce minimum font size using clamp() */
      table, th, td  {
        font-size: clamp(12px, 1em, 100px);
      }

      body, div, span, p, li, a, button, input, select, label {
        font-size: clamp(14px, 1em, 100px);
      }

      .my-wrapper {
        width: 100%;
        margin: 0 auto;
      }

      .my-inner1 {
        margin: 0 20px;
        display: inline-block;
        vertical-align: left;
      }

      .my-inner2 {
        margin: 0 50px;
        display: inline-block;
        vertical-align: top;
      }

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
        padding-bottom: 50px;
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
        width: 100%;
        padding: 0px;
        margin-left: -10px;
      }

      .nav-tabs li {
        font-size: 25px;
        background-color: white;
        color: black;
        padding: 0;
      }

      .nav > li > a:hover, .nav > li > a:focus {
        outline: rgb(0, 0, 0) none 1px;
      }

      .nav-tabs li a {
        font-size: 25px;
        background-color: #F6F6F6;
        color: #808080;
        border-bottom: solid 1px #000;
      }

      .nav-tabs li a:hover {
        background-color: #e0e0e0;
        color: #000;
        transition: background-color 0.3s ease, color 0.3s ease;
      }

      .nav-tabs > li:not(.active) > a {
        border: 1px solid black;
        border-radius: 4px 4px 0 0;
        color: #555;
        background-color: #f9f9f9;
      }

      .tab-content {
        width: 100%;
      }

      .tabbable {
        width: 100%;
      }



      input::placeholder {
      color: #555;
      font-weight: bold;
      font-style: italic;
      opacity: 1; /* Prevents grayed-out look in some browsers */
      font-size: 15px;
    }

      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        border-color: rgb(0, 0, 0) rgb(0, 0, 0) transparent;
        border-radius: 4px 4px 0 0;
      }

    /* Style default browser tooltips - only in table headers */
thead th[title] {
  position: relative;
}

thead th[title]:hover::after {
  content: attr(title);
  position: absolute;
  white-space: pre-line;
  top: 100%;
  left: 0;
  z-index: 9999;
  background-color: rgba(0, 0, 0, 0.85);
  color: #fff;
  padding: 8px 10px;
  border-radius: 6px;
  font-size: 14px;
  font-weight: normal;
  min-width: 200px;
  max-width: 800px;
  line-height: 1.3;
  box-shadow: 0px 2px 8px rgba(0, 0, 0, 0.2);
}

thead th[title]:hover::before {
  content: '';
  position: absolute;
  top: 95%;
  left: 10px;
  margin-left: -5px;
  border-width: 5px;
  border-style: solid;
  border-color: rgba(0, 0, 0, 0.85) transparent transparent transparent;
  z-index: 9999;
}

    "
    )
  ))
  ,
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
          onclick = "window.open('https://github.com/NutrientInstitute/protein-quality-hub', '_blank')"
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
    tags$script(HTML("
  $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function(e) {
    setTimeout(function() {
      $.fn.dataTable.tables({visible: true, api: true}).each(function() {
        var table = $(this).DataTable();

        // Destroy and recreate FixedHeader to reset
        if (table.fixedHeader) {
          table.fixedHeader.disable();
          table.fixedHeader.enable();
        }

        table.columns.adjust();
      });
      $(window).trigger('resize');
    }, 100);
  });

  // Handle table initialization and redraws
  $(document).on('init.dt draw.dt', function(e) {
    var table = $(e.target).DataTable();

    setTimeout(function() {
      if (table.fixedHeader) {
        table.fixedHeader.adjust();

        // Force header alignment
        var headerCells = $(table.table().header()).find('th');
        var fixedHeaderCells = $('.fixedHeader-floating th');

        headerCells.each(function(i) {
          var width = $(this).outerWidth();
          fixedHeaderCells.eq(i).css('width', width + 'px');
        });
      }
    }, 50);
  });

  // Handle window resize
  $(window).on('resize', function() {
    $.fn.dataTable.tables({visible: true, api: true}).each(function() {
      var table = $(this).DataTable();
      if (table.fixedHeader) {
        table.fixedHeader.adjust();
      }
    });
  });

  // Initial setup
  $(document).ready(function() {
    setTimeout(function() {
      $(window).trigger('resize');
    }, 500);
  });
")),
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
              p("Definitions and Calculations", style = "font-weight: bold;font-size: 20px; padding-top: 10px;")
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
              "The Protein Quality Hub is a resource and an invaluable tool for scientists, educators, students, and health professionals. As the first comprehensive database for protein quality scoring, it features the largest collection of amino acid profiles and digestibility data for a wide range of ingredients and common foods as well as from different species and different sampling sites. It also includes available data concerning the impact of food processing, such as reactive lysine, and compares calculations for apparent and true digestibility. The diverse data on digestibility are integrated into this hub under the naming scheme \"Correction Factors,\" consistent with FAO nomenclature \"correction for amino acid digestibility and availability\"."
            ),
            br(),
            p(
              "In the",
              tags$em(" Protein Quality Scoring"),
              " tab of the ",
              tags$em("Protein Quality Hub"),
              "you will find protein quality scores generated using digestibility values from the ",
              tags$em("Correction Factors"),
              " tab. Correction factors for bioavailability can be identified across tables using the variable ",
              tags$em("NI_ID"),
              ".",
              br(),
              "For more information on EAA recommendations and scoring patterns used in this app, consult the ",
              a(href = "https://github.com/NutrientInstitute/protein-quality-hub", "project GitHub page"),
              ". Note that this tool is still in development, please contact",
              a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
              "to report problems or provide feedback."
            ),
            br(),
            h4(
              "Protein Digestibility Corrected Amino Acid Score (PDCAAS):",
              tags$sup("1")
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
              tags$sup("2")
            ),
            p(
              "Intended application: DIAAS values represent the ratio between the quantity of digestible amino acids of 1 gram of protein from a food source compared to a reference gram of protein. In practice, the score can be used to compare the protein quality between food sources on a per gram basis."
            ),
            p(br(), style = "margin-bottom: -15px;"),
            p(
              "The DIAAS calculation is based on the ratio of limiting amino acid in a gram of protein compared to the same amino acid in a reference protein, adjusted for digestibility. Unlike PDCAAS, DIAAS is not truncated at 1 (i.e. 100%) and is calculated using ileal digestibility of individual amino acids. This means that the limiting amino acid is determined from both digestibility and amino acid content, rather than amino acid content alone*"
            ),
            p(br(), style = "margin-bottom: -15px;"),
            '\\(\\text{DIAAS}=\\frac{\\text{mg of digestible dietary indispensable amino acid in 1 g of the dietary protein}}{\\text{mg of the same dietary indispensable amino acid in 1g of the reference protein}}\\times100\\)',
            p(br()),
            h4("EAA-9:", tags$sup("3")),
            p(
              "Intended application: EAA-9 scores are percentages, representing the ability of a food to meet daily essential amino acid (EAA) recommendations, by default the RDAs. In practice, the score can be used to compare protein quality between food sources, and as a dietary quality tool to track progress toward meeting EAA recommendations."
            ),
            p(br(), style = "margin-bottom: -15px;"),
            # " Calculation is based on the minimum percentage of the RDA met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid.",
            p(
              "The EAA-9 calculation is based on the minimum percentage of the RDA (or personalized EAA recommendations) met per serving(s) of food, where the minimum is the lowest percentage met by a single amino acid corrected for bioavailability (if correction factor is available). EAA-9 can be calculated using correction factors for either crude protein or individual amino acids. EAA RDAs are satisfied when the EAA-9 score for foods consumed in a day reaches 100%."
            ),
            p(br(), style = "margin-bottom: -15px;"),
            '\\(\\text{EAA-9}=min(\\frac{\\text{His Present}}{\\text{His RDA}},\\frac{\\text{Ile Present}}{\\text{Ile RDA}},\\frac{\\text{Leu Present}}{\\text{Leu RDA}},\\frac{\\text{Lys Present}}{\\text{Lys RDA}},\\frac{\\text{Met Present}}{\\text{Met RDA}},\\frac{\\text{Phe Present}}{\\text{Phe RDA}},\\frac{\\text{Thr Present}}{\\text{Thr RDA}},\\frac{\\text{Trp Present}}{\\text{Trp RDA}},\\frac{\\text{Val Present}}{\\text{Val RDA}})\\times100\\times\\text{Correction Factor}\\)',
            br(),


            br(),
            tags$small(h5("References:"),
                       tags$ol(tags$li(
                         p(
                           "Food and Agriculture Organization of the United Nations, World Health Organization & United Nations University. Protein and amino acid requirements in human nutrition : report of a joint FAO/WHO/UNU expert consultation [Internet]. World Health Organization; 2007 [cited 2022 Dec 1]. Available from: ",
                           a(href = "https://apps.who.int/iris/handle/10665/43411", "https://apps.who.int/iris/handle/10665/43411"),
                           "."
                         )
                       ),
                       tags$li(
                         p(
                           "FAO. Dietary protein quality evaluation in human nutrition: report of an FAO Expert Consultation. Food and nutrition paper; 92. FAO: Rome [Internet]. FAO (Food and Agriculture Organization); 2013. Available from:",
                           a(
                             href = "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf",
                             "https://www.fao.org/ag/humannutrition/35978-02317b979a686a57aa4593304ffc17f06.pdf"
                           ),
                           "."
                         )
                       ),
                       tags$li(
                         p(
                           "Forester SM, Jennings-Dobbs EM, Sathar SA, Layman DK. Perspective: Developing a Nutrient-Based Framework for Protein Quality. J Nutr. 2023 Aug;153(8):2137-2146. doi: ",
                           a(href = "10.1016/j.tjnut.2023.06.004", "https://doi.org/10.1016/j.tjnut.2023.06.004"),
                           ". Epub 2023 Jun 8. PMID: 37301285."
                         )
                       ))),
            br(),
            p(style = "font-weight:strong; font-size:120%; ", "Column Definitions:"),
            HTML(
              "
<table style=\"width:100%; border-collapse: collapse; margin-top: 20px; margin-bottom: 30px;\">
  <thead>
    <tr>
      <th style=\"border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;\">Variable Name(s)</th>
      <th style=\"border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;\">Description</th>
    </tr>
  </thead>
  <tbody>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">NI_ID</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Unique Nutrient Institute (NI) identifier for each correction factor data point.</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Food</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Description of the food, as provided by the source of correction factor data</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor Species</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Target species of bioavailability analysis<br><span style=\"font-style: italic; color: #888; font-size: 90%;\">Defined as &ldquo;Species&rdquo; in the Correction Factors table</span></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor Sample Location</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The type  or location of sample collected for analysis (e.g. ileal, fecal, etc...)<br><span style=\"font-style: italic; color: #888; font-size: 90%;\">Defined as &ldquo;Sample Location&rdquo; in the Correction Factors table</span></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor Protein Form</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The protein or amino acid for which correction factor coefficient is provided<br><span style=\"font-style: italic; color: #888; font-size: 90%;\">Defined as &ldquo;Protein Form&rdquo; in the Correction Factors table</span></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor Calculation</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The name of the correction factor (i.e. apparent digestibility, metabolic availability, etc...)<br><span style=\"font-style: italic; color: #888; font-size: 90%;\">Defined as &ldquo;Calculation&rdquo; in the Correction Factors table</span></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor (%)</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Value of the associated measure, expressed as a percentage<br></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Limiting AA</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The limiting essential amino acid determined by the amino acid scoring pattern or recommendations</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">fdcId <span style=\"font-style: italic; font-size: 90%; color: #888;\">(if applicable)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">FoodData Central (FDC) identifier, used to map protein correction factors to food composition data from FoodData Central</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">serving size <span style=\"font-style: italic; font-size: 90%; color: #888;\">(if applicable)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">Serving size of food used to calculate EAA-9 score</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">EAA-9 (%) <span style=\"font-style: italic; font-size: 90%; color: #888;\">(if applicable)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">EAA-9 score calculated as documented in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a> README.md and &lsquo;Definitions and Calculations&rsquo; section above the Protein Quality Scoring table</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">PDCAAS <span style=\"font-style: italic; font-size: 90%; color: #888;\">(if applicable)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">PDCAAS calculated as documented in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a> README.md and &lsquo;Definitions and Calculations&rsquo; section above the Protein Quality Scoring table</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">DIAAS <span style=\"font-style: italic; font-size: 90%; color: #888;\">(if applicable)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">DIAAS calculated as documented in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a> README.md and &lsquo;Definitions and Calculations&rsquo; section above the Protein Quality Scoring table</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Food Composition Ref</td><td style=\"border: 1px solid #ccc; padding: 8px;\">A citation indicating where food composition data was collected - citations created using <a href='https://www.nutrientinstitute.org/cfdc' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">CDFC Citation Generator</a></td></tr>

    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor Ref</td><td style=\"border: 1px solid #ccc; padding: 8px;\">A citation or list of ordered citations indicating the original source(s) of the correction factor data (as cited in the source data was collected from).</td></tr>
  </tbody>
</table>
"
            )



          )
        )),
fluidRow(br()),
# Create a new Row in the UI for search options
fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
         column(
           2,
           p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
         )),
fluidRow(style = "background-color: #F6F6F6;padding-left: 10px;padding-bottom: 0px; padding-top: 0px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
         column(
           6,
           textInput(
             inputId = "search_tab1",
             label = "",
             placeholder = "Search for food (e.g. beans, chicken, cheese ...)",
             width = '100%'
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
  style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 20px; padding-top: 10px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
  fluidRow(
    column(
      3,
      checkboxGroupInput(
        inputId = "score",
        label = "Score(s)",
        choices = c("PDCAAS", "DIAAS", "EAA-9")
      ),
      conditionalPanel(  condition = "input.score != ''",
      radioButtons(
        inputId = "show_calc",
        label = "Score calculation",
        choiceNames = c("Show calculation and score", "Show score only"),
        choiceValues = c("show_calc", "score_only")
      ))
    ),
    column(
      3,
      conditionalPanel(
        condition = "input.score.includes('PDCAAS')",
        p("PDCAAS Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
        fluidRow(
          style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
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
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = "input.score.includes('DIAAS')",
        p("DIAAS Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
        fluidRow(
          style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
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
          ),
          br()
        )
      )
    ),
    column(
      3,
      conditionalPanel(
        condition = "input.score.includes('EAA-9')",
        p("EAA-9 Scoring options:", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -4px;"),
        fluidRow(
          style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 15px;",
          pickerInput(
            inputId = "EAA_rec",
            label = "EAA Recommendations",
            choices = unique(c(
              unlist(
                scoring_pattern %>% filter(Unit == "mg/kg/d") %>% select(`Pattern Name`)
              ),
              "Choose custom recommendations"
            )),
            selected = "RDA for Adults"
          ),
          conditionalPanel(condition = "input.EAA_rec == 'Choose custom recommendations'",
                           fluidRow(
                             column(
                               4,
                               numericInput(
                                 inputId = "his",
                                 label = "His (mg/kg/d)",
                                 value = "14",
                                 width = '90px'
                               ),
                               numericInput(
                                 inputId = "leu",
                                 label = "Leu (mg/kg/d)",
                                 value = "42",
                                 width = '90px'
                               ),
                               numericInput(
                                 inputId = "ile",
                                 label = "Ile (mg/kg/d)",
                                 value = "19",
                                 width = '90px'
                               )
                             ),
                             column(
                               4,
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
                             column(
                               4,
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
                           )),
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
          ),
          div(
            style = "background-color: #EDEDED; border-left: 4px solid #A6A09B;
           padding: 1px 8px; margin: 1px 0; border-radius: 4px;
           font-weight: 500; font-size: 14px; color: #212529; max-width:97%;",
           checkboxInput(
             inputId = "require_bioavail",
             label = "Only see EAA-9 scores for foods with correction factors",
             value = FALSE
           )
          )

          ,

          # uiOutput("eaa9_note"),


        )
      )
    )
  ),
  conditionalPanel(  condition = "input.score != ''",
  fluidRow(
    style = "margin-bottom: -5px;",
    p("Correction Factors", style = "font-size: 15px;font-weight: 500;color: #808080;margin-bottom: -2px;margin-top:5px;"),
    fluidRow(
      style = "border: 1px solid #bdbdbd;margin: auto;margin-right: 25px;padding: 7px; background-color: #F2F2F2;",
      column(
        2,
        checkboxGroupInput(
          inputId = "pq_species",
          label = "Species",
          choices = c("human", "human (predicted from swine)", "swine", "rat"),
          selected = c("human", "human (predicted from swine)", "swine", "rat")
        )
      ),
      column(
        2,
        uiOutput("pq_sample_ui")

      ),
      column(
        2,
        checkboxGroupInput(
          inputId = "pq_analyte",
          label = "Protein Form",
          choices = c("crude protein", "individual amino acids"),
          selected = c("crude protein", "individual amino acids")
        )
      ),
      column(
        2,
        checkboxGroupInput(
          inputId = "pq_measure",
          label = "Calculation",
          choices = c(
            "true digestibility",
            "apparent digestibility",
            "standardized digestibility"
          ),
          selected = c(
            "true digestibility",
            "apparent digestibility",
            "standardized digestibility"
          )
        )
      ),
      # uiOutput("default_correction_note"),
    )

  )
)),
fluidRow(br()),
# Create download button
fluidRow(
  style = "float: right;",
  dropdownButton(
    label = "Download",
    circle = FALSE,
    status = "secondary",
    icon = icon("download"),
    tags$div(
      downloadButton(
        "download_csv_PQ",
        "CSV",
        class = "btn btn-secondary",
        icon = icon("file-csv")
      ),
      downloadButton(
        "download_excel_PQ",
        "Excel",
        class = "btn btn-secondary",
        icon = icon("file-excel")
      )
    )
  )
),
# Create a new row for the table.
fluidRow(style = "padding-top: -20px;",
         DT::dataTableOutput("table_2"))
      ),
nav_panel(
  "Correction Factors",
  fluidRow(
    div(
      id = "info-toggle",
      class = "accordion-toggle",
      onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_click', Math.random(), {priority: 'event'}); }",
      column(
        8,
        p("Definitions and Calculations", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
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
        "The diverse data on the digestibility and bioavailability of protein and amino acids are integrated into this hub under the naming scheme \"Correction Factors,\" consistent with FAO nomenclature \"correction for amino acid digestibility and availability\"."
      ),
      p(
        "The Protein Quality Hub Project serves as a dedicated repository for information on protein bioavailability correction factors, addressing a critical gap in accessible data. For more information see the Protein Quality Hub ",
        a(href = "https://github.com/NutrientInstitute/protein-quality-hub", "github page."),
        " Please contact ",
        a(href = "https://www.nutrientinstitute.org/protein-digestibility-feedback", "Nutrient Institute"),
        "to report problems or provide feedback."
      ),
      br(),
      p(
        "Currently, the Protein Quality Hub contains correction factors from the following sources:"
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
            href = "https://doi.org/10.1016/j.tjnut.2025.08.004",
            "True Ileal Amino Acid Digestibility of Human Foods Classified According to Food Type as Determined in the Growing Pig (Hodgkinson et al. 2025)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.17226/13298",
            "Nutrient Requirements of Swine: Eleventh Revised Edition (NRC 2012)",
            tags$b("**")
          )
        ),
        tags$li(
          a(
            href = "https://books.google.com/books?id=ieEEPqffcxEC&lpg=PP1&pg=PP1#v=onepage&q&f=false ISBN: 92-5-103097-9",
            "Protein quality Evaluation: Report of Joint FAO/WHO Expert Consultation (FAO 1991)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/137.8.1874",
            "Application of the indicator amino acid oxidation technique for the determination of metabolic availability of sulfur amino acids from casein versus soy protein isolate in adult men (Humayun et al. 2007)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.3945/jn.112.166728",
            "Lysine from cooked white rice consumed by healthy young men is highly metabolically available when assessed using the indicator amino acid oxidation technique (Prolla et al. 2013)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxaa086",
            "Bioavailable Methionine Assessed Using the Indicator Amino Acid Oxidation Method Is Greater When Cooked Chickpeas and Steamed Rice Are Combined in Healthy Young Men (Rafii et al. 2020)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxaa227",
            "Bioavailable Lysine, Assessed in Healthy Young Men Using Indicator Amino Acid Oxidation, is Greater when Cooked Millet and Stewed Canadian Lentils are Combined (Fakiha et al. 2020)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxab410",
            "Bioavailable Lysine Assessed Using the Indicator Amino Acid Oxidation Method in Healthy Young Males is High when Sorghum is Cooked by a Moist Cooking Method (Paoletti et al. 2022)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxy039",
            "Metabolic Availability of the Limiting Amino Acids Lysine and Tryptophan in Cooked White African Cornmeal Assessed in Healthy Young Men Using the Indicator Amino Acid Oxidation Technique (Rafii et al. 2018)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxy039",
            "Lysine Bioavailability in School-Age Children Consuming Rice Is Reduced by Starch Retrogradation (Caballero et al. 2020)"
          )
        ),
        tags$li(
          a(
            href = "https://doi.org/10.1093/jn/nxy039",
            "Metabolic Availability of Methionine Assessed Using Indicator Amino Acid Oxidation Method, Is Greater when Cooked Lentils and Steamed Rice Are Combined in the Diet of Healthy Young Men (Rafii et al. 2022)"
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
      ),
      br(),
      p(style = "font-weight:strong; font-size:120%; ", "Column Definitions:"),
      HTML(
        "
<table style=\"width:100%; border-collapse: collapse; margin-top: 20px; margin-bottom: 30px;\">
  <thead>
    <tr>
      <th style=\"border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;\">Variable Name</th>
      <th style=\"border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;\">Description</th>
    </tr>
  </thead>
  <tbody>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">NI_ID</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Unique Nutrient Institute (NI) identifier for each correction factor data point.</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Food group <span style=\"font-style: italic; color: #888; font-size: 90%;\">(not in table - available in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a>)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">Food group as specified by the data source from which the correction factor data was collected</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Food</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Description of the food used in correction factor analysis</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor (%)</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Value of the associated correction factor measure, expressed as a percentage</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Correction Factor SD</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Standard deviation of the provided value</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Species</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Target species of correction factor analysis</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Sample Location</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The type  or location of sample collected for analysis (e.g. ileal, fecal, etc...)</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Calculation</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The name of the correction factor (i.e. apparent digestibility, metabolic availability, etc...)</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Protein Form</td><td style=\"border: 1px solid #ccc; padding: 8px;\">The protein or amino acid for which correction factor coefficient is provided</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Model<span style=\"font-style: italic; color: #888; font-size: 90%;\">(not in table - available in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a>)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">Experimental model (either in vivo or in vitro)</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Protein (g)</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Amount of protein (in grams) from the applicable food ingested (in the case of in vivo analysis) or analyzed (in the case of in vitro analysis)</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">n</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Number of subjects from which in vivo data was collected (if applicable)</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Analysis method(s)</td><td style=\"border: 1px solid #ccc; padding: 8px;\">Name of the analysis method(s), technique(s), or assay(s) used to measure bioavailability, as specified in the source the data was collected from</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Data Collection Source</td><td style=\"border: 1px solid #ccc; padding: 8px;\">A citation indicating where the data appearing in this table was collected from - citations created using <a href='https://www.nutrientinstitute.org/cfdc' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">CDFC Citation Generator</a></td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Original Data Source(s)</td><td style=\"border: 1px solid #ccc; padding: 8px;\">A citation or list of ordered citations indicating the original source(s) of the correction factor data (as cited in the source data was collected from).</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Notes <span style=\"font-style: italic; color: #888; font-size: 90%;\">(not in table - available in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a>)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">Any additional notes or comments applicable to the collected data that are provided in the source data has been collected from.</td></tr>
    <tr><td style=\"border: 1px solid #ccc; padding: 8px;\">Diet <span style=\"font-style: italic; color: #888; font-size: 90%;\">(not in table - available in <a href='https://github.com/NutrientInstitute/protein-quality-hub' target='_blank' style=\"color: #3b6ea5; text-decoration: underline;\">github</a>)</span></td><td style=\"border: 1px solid #ccc; padding: 8px;\">Description of the diet consumed by experimental subjects</td></tr>
  </tbody>
</table>
"
      )


    )
  )),
fluidRow(br()),
# Create a new Row in the UI for search options
fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
         column(
           2,
           p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
         )),
fluidRow(style = "background-color: #F6F6F6;padding-left: 20px;padding-bottom: 0px; padding-top: 0px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
         column(
           6,
           textInput(
             inputId = "search_tab2",
             label = "",
             placeholder = "Search for food (e.g. beans, chicken, cheese ...)",
             width = '100%'
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
  # column(
  #   2,
  #   virtualSelectInput(
  #     inputId = "model",
  #     label = "Model:",
  #     choices = c("in vivo", "in vitro"),
  #     selected = c(unique(as.character(
  #       Protein_Correction$Model
  #     ))),
  #     multiple = TRUE,
  #     showValueAsTags = TRUE,
  #     width = '100%'
  #   )
  # ),
  column(
    2,
    virtualSelectInput(
      inputId = "species",
      label = "Species:",
      choices = c("human", "human (predicted from swine)", "swine", "rat"),
      selected = c(unique(
        as.character(Protein_Correction$Species)
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
      label = "Sample Location:",
      choices = c(unique(
        as.character(Protein_Correction$`Sample Location`)
      )),
      selected = c(unique(
        as.character(Protein_Correction$`Sample Location`)
      )),
      multiple = TRUE,
      showValueAsTags = TRUE,
      width = '100%'
    )
  ),
  column(
    6,
    virtualSelectInput(
      inputId = "analyte",
      label = "Protein Form:",
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
      selected = unique(as.character(Protein_Correction$`Protein Form`)),
      showValueAsTags = TRUE,
      multiple = TRUE,
      width = '100%'
    )
  ),
  column(
    2,
    virtualSelectInput(
      inputId = "measure",
      label = "Calculation:",
      choices = c(unique(
        as.character(Protein_Correction$Calculation)
      )),
      selected = c(unique(
        as.character(Protein_Correction$Calculation)
      )),
      multiple = TRUE,
      showValueAsTags = TRUE,
      width = '100%'
    )
  ),
  column(12,
         uiOutput("IAAO_note"))
),
fluidRow(br()),
# Create download button
fluidRow(
  style = "float: right;",
  dropdownButton(
    label = "Download",
    circle = FALSE,
    status = "secondary",
    icon = icon("download"),
    tags$div(
      downloadButton(
        "download_csv_bv",
        "CSV",
        class = "btn btn-secondary",
        icon = icon("file-csv")
      ),
      downloadButton(
        "download_excel_bv",
        "Excel",
        class = "btn btn-secondary",
        icon = icon("file-excel")
      )
    )
  )
),
# Create a new row for the table.
fluidRow(style = "padding-top: 20px;",
         DT::dataTableOutput("table"))
),
nav_panel(
  "EAA Composition Data",
  fluidRow(
    div(
      id = "info-toggle_3",
      class = "accordion-toggle",
      onclick = "if (event.target === this || event.target.tagName === 'P' || event.target.className.includes('column')) { Shiny.setInputValue('info_toggle_3_click', Math.random(), {priority: 'event'}); }",
      column(
        8,
        p("Definitions and Calculations", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
      ),
      column(4, uiOutput("toggleButton_3"))
    )
  ),
  fluidRow(hidden(
    div(
      id = "infoContent_3",
      class = "accordion-content",
      style = "background-color: #F6F6F6;padding-left: 30px;padding-right: 20px; padding-bottom: 20px; padding-top: 20px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
      p(
        "In this tab, you will find the food composition data collected for use in protein quality scoring."
      ),
      br(),
      p(style = "font-weight:strong; font-size:120%; ", "Column Definitions:"),
      HTML(
        '
  <table style="width:100%; border-collapse: collapse; margin-top: 20px; margin-bottom: 30px;">
    <thead>
      <tr>
        <th style="border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;">Variable Name(s)</th>
        <th style="border: 1px solid #ccc; padding: 8px; background-color: #f0f0f0;">Description</th>
      </tr>
    </thead>
    <tbody>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">FDC_ID</td><td style="border: 1px solid #ccc; padding: 8px;">FoodData Central (FDC) identifier, used to map correction factors to food composition data from FoodData Central</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">NI_ID</td><td style="border: 1px solid #ccc; padding: 8px;">Unique Nutrient Institute (NI) identifier for each correction factor</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Food description</td><td style="border: 1px solid #ccc; padding: 8px;">Description of the food provided by the food composition data source</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Protein (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of protein per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">His (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of histidine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Ile (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of isoleucine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Leu (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of leucine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Lys (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of lysine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Met+Cys (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of methionine and cystine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Phe+Tyr (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of phenylalanine and tyrosine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Thr (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of threonine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Trp (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of tryptophan per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Val (g/100g)</td><td style="border: 1px solid #ccc; padding: 8px;">Grams of valine per 100g of food</td></tr>
      <tr><td style="border: 1px solid #ccc; padding: 8px;">Food Composition Data Ref</td><td style="border: 1px solid #ccc; padding: 8px;">A citation indicating where food composition data was collected - citations created using CDFC Citation Generator</td></tr>
    </tbody>
  </table>
'
      )
    )
  )),
fluidRow(br()),
# Create a new Row in the UI for search options
fluidRow(style = "background-color: #dedede;padding-left: 20px;border: 0.5px solid #bdbdbd;font-weight: bold;",
         column(
           2,
           p("Search", style = "font-weight: bold;font-size: 20px; padding-top:10px;")
         )),
fluidRow(style = "background-color: #F6F6F6;padding-left: 10px;padding-bottom: 0px; padding-top: 0px;border-left: 0.5px solid #bdbdbd;border-right: 0.5px solid #bdbdbd;border-bottom: 0.5px solid #bdbdbd;",
         column(
           6,
           textInput(
             inputId = "search_tab3",
             label = "",
             placeholder = "Search for food (e.g. beans, chicken, cheese ...)",
             width = '100%'
           )
         )),
fluidRow(br()),
# Create download button
fluidRow(
  style = "float: right;",
  dropdownButton(
    label = "Download",
    circle = FALSE,
    status = "secondary",
    icon = icon("download"),
    tags$div(
      downloadButton(
        "download_csv_eaa",
        "CSV",
        class = "btn btn-secondary",
        icon = icon("file-csv")
      ),
      downloadButton(
        "download_excel_eaa",
        "Excel",
        class = "btn btn-secondary",
        icon = icon("file-excel")
      )
    )
  )
),
fluidRow(style = "float: right;",
         div(
           style = "font-size: 1.2em;",
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
         )),
# Create a new row for the table.
fluidRow(style = "padding-top: 20px;",
         DT::dataTableOutput("table_3"))
)
    )
  )
)


server <- function(input, output, session) {
  visibility <- reactiveVal(TRUE)

  output$pq_sample_ui <- renderUI({
    checkboxGroupInput(
      inputId = "pq_sample",
      label = "Sample Location",
      choices = c("fecal", "ileal"),
      selected = if (identical(input$score, "PDCAAS")) {
        "fecal"
      } else if (identical(input$score, "DIAAS")) {
        "ileal"
      } else {
        c("fecal", "ileal")
      }
    )
  })


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

  # Choice of EAA recommendations
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

  output$eaa9_note <- renderUI({
    if ("EAA-9" %in% input$scoreEAA - 9) {
      div(
        style = "display: inline-block; background-color: #f0f0f0;
               padding: 8px 12px; margin-right: 15px; margin-bottom: 10px;
               font-size: 14px; border-radius: 4px; max-width: 100%; color: #333;",
        tags$strong("Note: "),
        "correction factor is used when available by default"
      )
    }
  })

  output$IAAO_note <- renderUI({
    div(
      style = "display: inline-block; background-color: #f0f0f0;
               padding: 4px 10px; margin-right: 10px; margin-bottom: 5px;
               font-size: 16px; border-radius: 4px; width: 100%; color: #333;",
      tags$strong("Note: "),
      "IAAO correction factors are experimental"
    )
  })

  output$default_correction_note <- renderUI({
    div(
      style = "display: inline-block; background-color: #f0f0f0;
               padding: 8px 12px; margin-right: 15px; margin-top: 5px; margin-bottom: 10px; font-style: italic;
               font-size: 13px; border-radius: 4px; width: 100%; color: #333;",
      tags$strong("Note: "),
      "Default selections represent the gold standard for correction factors"
    )
  })

  # Content for downloads
  data_PQ <- reactive({
    PQ_df <- data.frame(NI_ID = character())

    if (!("EAA-9" %in% input$score |
          "PDCAAS" %in% input$score | "DIAAS" %in% input$score)) {
      DT::datatable(
        data.frame(Message = "Please select scores above"),
        rownames = FALSE,
        options = list(
          dom = 't',
          # hides filter, pagination, etc.
          ordering = FALSE,
          paging = FALSE
        )
      )
    } else {
      if ("EAA-9" %in% input$score) {
        if (input$EAA_rec == "Choose custom recommendations") {
          scoring_pattern[199, ] <-
            list(
              "Choose custom recommendations",
              "histidine",
              ifelse(is.numeric(input$hist), input$hist, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[200, ] <-
            list(
              "Choose custom recommendations",
              "leucine",
              ifelse(is.numeric(input$leu), input$leu, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[201, ] <-
            list(
              "Choose custom recommendations",
              "isoleucine",
              ifelse(is.numeric(input$ile), input$ile, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[202, ] <-
            list(
              "Choose custom recommendations",
              "lysine",
              ifelse(is.numeric(input$lys), input$lys, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[203, ] <-
            list(
              "Choose custom recommendations",
              "methionine+cysteine",
              ifelse(is.numeric(input$met_cys), input$met_cys, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[204, ] <-
            list(
              "Choose custom recommendations",
              "phenylalanine+tyrosine",
              ifelse(is.numeric(input$phe_tyr), input$phe_tyr, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[205, ] <-
            list(
              "Choose custom recommendations",
              "threonine",
              ifelse(is.numeric(input$thr), input$thr, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[206, ] <-
            list(
              "Choose custom recommendations",
              "tryptophan",
              ifelse(is.numeric(input$trp), input$trp, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[207, ] <-
            list(
              "Choose custom recommendations",
              "valine",
              ifelse(is.numeric(input$val), input$val, 1),
              "mg/kg/d",
              "custom",
              NA
            )
        }
        if (input$require_bioavail == TRUE) {
          EAA_composition <- EAA_composition %>%
            filter(NI_ID != "NA") %>%
            drop_na(NI_ID)
        }

        if (input$serving_size == "Use standard serving sizes") {
          EAA_9 <- EAA_composition %>%
            left_join(
              portion_sizes %>%
                select(fdcId, g_weight , portion) %>%
                mutate(fdcId = as.character(fdcId)) %>%
                mutate(portion = paste0(portion, " (", g_weight, " g)"))
            ) %>%
            mutate(portion = ifelse(is.na(portion), "not provided (100 g)", portion)) %>%
            mutate(g_weight = ifelse(is.na(g_weight), 100, g_weight)) %>%
            mutate(value = value * 1000) %>%
            mutate(value = value * (g_weight / 100))
        } else{
          EAA_9 <- EAA_composition %>%
            mutate(value = value * 1000) %>%
            mutate(value = value * (input$serving_weight / 100)) %>%
            mutate(portion = paste0(input$serving_weight, " g"))
        }

        if (input$EAA_rec == "Choose custom recommendations") {
          pattern <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec) %>%
            select(AA = Analyte, Amount)
        } else{
          pattern <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec) %>%
            filter(is.character(input$rec_age) &
                     Age == input$rec_age | !is.na(Age)) %>%
            select(AA = Analyte, Amount)
        }

        # Original logic
        EAA_9_og <- EAA_9 %>%
          left_join(pattern, by = "AA") %>%
          mutate(
            norm_value = Amount * input$weight,
            calculation = paste0(round(value, 2), "/", round(norm_value, 2)),
            value = value / norm_value
          ) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            portion,
            value,
            AA,
            calculation,
            `Food Composition Ref`
          )

        min_scores <- EAA_9_og %>%
          group_by(fdcId, `food identifier`, `food description`) %>%
          summarise(
            calculation = paste0(
              "min(",
              str_c(calculation, collapse = ", "),
              ") x 100 x Correction Factor \n = ",
              min(round(value, 2)),
              " x 100 x Correction Factor"
            ),
            .groups = "drop"
          )

        EAA_9_og <- EAA_9_og %>%
          select(-calculation) %>%
          left_join(min_scores, by = c("fdcId", "food identifier", "food description")) %>%
          group_by(
            fdcId,
            `food identifier`,
            `food description`,
            Protein,
            portion,
            `Food Composition Ref`
          ) %>%
          mutate(`EAA-9` = min(value)) %>%
          filter(value == `EAA-9`) %>%
          ungroup() %>%
          rename("Limiting AA" = "AA") %>%
          separate_longer_delim(NI_ID, delim = ";") %>%
          mutate(NI_ID = str_trim(NI_ID)) %>%
          left_join(
            Protein_Correction %>%
              mutate(Food = str_remove(Food, ", Average")) %>%
              group_by(
                Food,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              summarise(
                `Correction Factor  (%)` = round(sum(`Correction Factor  (%)`, na.rm = TRUE) /
                                                   n(), 2),
                NI_ID = paste0(NI_ID, collapse = "; "),
                .groups = "drop"
              ) %>%
              separate_longer_delim(NI_ID, delim = "; ") %>%
              distinct(),
            by = "NI_ID"
          ) %>%
          replace_na(list(`Data Collection Source` = "Not Available")) %>%
          mutate(
            indicator = case_when(
              `Data Collection Source` == `Food Composition Ref` ~ 1,
              str_detect(`Food Composition Ref`, "Standard Reference") ~ 2,
              TRUE ~ 3
            )
          ) %>%
          filter(!(
            Calculation == "metabolic availability" &
              !str_detect(`Food Composition Ref`, "Standard Reference")
          )) %>%
          group_by(`food identifier`) %>%
          mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available",
            2,
            min(indicator, na.rm = TRUE)
          )) %>%
          ungroup() %>%
          filter(indicator == min_indicator) %>%
          select(-indicator, -min_indicator) %>%
          mutate(Food = coalesce(Food, `food description`)) %>%
          group_by(
            Food,
            Species,
            `Protein Form`,
            `Sample Location`,
            Calculation,
            `Data Collection Source`
          ) %>%
          mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
          distinct() %>%
          ungroup() %>%
          mutate(
            `EAA-9` = round(
              `EAA-9` * ifelse(
                is.na(`Correction Factor  (%)`),
                1,
                `Correction Factor  (%)` / 100
              ) * 100,
              2
            ),
            calculation = ifelse(
              is.na(`Correction Factor  (%)`),
              str_remove(calculation, " x Correction Factor"),
              calculation
            ),
            calculation = paste0(calculation, " \n = ", `EAA-9`, "%")
          ) %>%
          filter(
            ifelse(
              !is.na(`Protein Form`),
              `Protein Form` == "crude protein" ,
              !is.na(`EAA-9`)
            )
          ) %>%
          select(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form`,
            Calculation,
            `Correction Factor  (%)`,
            `Limiting AA`,
            fdcId,
            portion,
            `EAA-9`,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          rename("serving size" = "portion", "EAA-9 (%)" = "EAA-9") %>%
          mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)) %>%
          replace_na(
            list(
              NI_ID = "Not Available",
              Calculation = "Not Available",
              `Sample Location` = "Not Available",
              `Protein Form` = "Not Available",
              Species = "Not Available",
              `Correction Factor  (%)` = "Not Available",
              `Data Collection Source` = "Not Available"
            )
          ) %>%
          distinct()

        #Altered logic for aa
        # --- AA digestibility lookups (AA-specific) ---
        aa_cf <- Protein_Correction %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          # Average CF at the Food/Species/... level and collect the NI_IDs that contributed
          dplyr::group_by(
            Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          # Re-expand NI_ID for matching by NI_ID later (still a single NI_ID column)
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # Keep only NI_ID + crude-protein CF for optional fallbacks/extensions
        cp_cf <- aa_cf %>%
          dplyr::filter(`Protein Form` == "crude protein") %>%
          dplyr::select(NI_ID, cp_cf = cf)

        # Map AA label -> Protein Form used in correction table
        map_pf <- function(aa) dplyr::case_when(
          aa == "lysine" ~ "reactive lysine",
          aa == "methionine+cysteine" ~ "methionine",
          aa == "phenylalanine+tyrosine" ~ "phenylalanine",
          TRUE ~ aa
        )

        # --- Per-AA rows with digestibility applied BEFORE min() ---
        EAA_9_alt_rows <- EAA_9 %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg = (value)) %>%         # mg AA (already per your input)
          dplyr::left_join(pattern, by = "AA") %>%
          dplyr::mutate(Amount = Amount * input$weight) %>%
          dplyr::mutate(`Protein Form Join` = map_pf(AA)) %>%
          # Bring AA-specific CF + metadata; align Protein Form strings for join
          dplyr::left_join(
            aa_cf %>%
              dplyr::select(
                NI_ID, `Protein Form`, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ) %>%
              dplyr::mutate(`Protein Form` = map_pf(`Protein Form`)),
            by = c("NI_ID", "Protein Form Join" = "Protein Form")
          ) %>%
          # Keep only rows where AA-specific digestibility exists (no crude fallback here)
          dplyr::filter(!is.na(cf)) %>%
          dplyr::mutate(dig = cf) %>%
          dplyr::mutate(value = (aa_mg * dig) / Amount) %>%  # ratio (not %)
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg, format = "f", digits = 2),
              formatC(dig,    format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`,
            AA, Amount, aa_mg, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Protein Form Join`, portion
          )

        # --- Build full calculation string (no "Correction Factor" text) ---
        temp_2 <- EAA_9_alt_rows %>%
          dplyr::group_by(
            fdcId, `food identifier`, Species, `food description`,
            `Sample Location`, Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            min_val = min(value, na.rm = TRUE),
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") x 100 = ",
              formatC(min_val, format = "f", digits = 4),
              " x 100 = ",
              formatC(min_val * 100, format = "f", digits = 2),
              "%"
            ),
            n_rows = dplyr::n(),
            .groups = "drop"
          )

        # --- Limiting AA and final table (EAA-9 stored as ratio; display shows %) ---
        EAA_9_alt <- EAA_9_alt_rows %>%
          dplyr::group_by(
            fdcId, NI_ID, `food identifier`, `food description`, `Food Composition Ref`
          ) %>%
          dplyr::mutate(`EAA-9 (%)` = min(value, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(value == `EAA-9 (%)`) %>%
          dplyr::left_join(
            temp_2,
            by = c(
              "fdcId", "food identifier", "food description",
              "Species", "Sample Location", "Calculation", "Data Collection Source"
            )
          ) %>%
          dplyr::filter(value == min_val) %>%
          dplyr::mutate(`EAA-9 (%)` = min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(
            indicator = dplyr::case_when(
              `Data Collection Source` == `Food Composition Ref` ~ 1L,
              stringr::str_detect(`Food Composition Ref`, "Standard Reference") ~ 2L,
              TRUE ~ 3L
            )
          ) %>%
          dplyr::filter(!(Calculation == "metabolic availability" &
                            !stringr::str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(
            min_indicator = ifelse(
              `Data Collection Source` == "Not Available",
              2L, min(indicator, na.rm = TRUE)
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator) %>%
          dplyr::select(!min_indicator) %>%
          # --- Rebuild Food label and RE-CONCATENATE NI_ID like the first half ---
          dplyr::mutate(Food = dplyr::coalesce(Food, `food description`)) %>%
          dplyr::group_by(
            Food, Species, `Protein Form Join`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::mutate(NI_ID = paste0(unique(NI_ID), collapse = "; ")) %>%
          dplyr::distinct() %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form` = `Protein Form Join`,
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            portion,
            `EAA-9 (%)` = round(`EAA-9 (%)`, 2) * 100,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::rename("serving size" = "portion") %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))


        EAA_9 <- dplyr::bind_rows(
          EAA_9_og  %>% mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)),
          EAA_9_alt %>% mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))
        )


        # Optional: final score/calc switch
        if (input$show_calc == "score_only") {
          EAA_9 <- EAA_9 %>% select(-calculation)
        } else {
          EAA_9 <- EAA_9 %>% select(-`EAA-9 (%)`) %>% rename("EAA-9 (%)" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- full_join(PQ_df, EAA_9)
        } else {
          PQ_df <- EAA_9
        }
      }

      if (ifelse(length(input$score) != 1,
                 "PDCAAS" %in% input$score,
                 "PDCAAS" == input$score)) {
        PDCAAS <- EAA_composition %>%
          drop_na(Protein) %>%
          drop_na(NI_ID) %>%
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
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            value,
            AA,
            calculation,
            `Food Composition Ref`
          )

        temp_2 <- PDCAAS %>%
          select(fdcId, NI_ID, `food description`, value, calculation) %>%
          group_by(fdcId, NI_ID, `food description`) %>%
          summarise(
            calculation = paste0(
              "min(",
              str_c(calculation, collapse = ", "),
              ", 1) x Correction Factor \n = ",
              paste0(ifelse(min(
                round(value, 2)
              ) >= 1, 1, min(
                round(value, 2)
              ))),
              " x Correction Factor"
            )
          ) %>%
          ungroup()

        PDCAAS <- PDCAAS %>%
          select(!calculation) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            value,
            AA,
            `Food Composition Ref`
          ) %>%
          group_by(fdcId,
                   NI_ID,
                   `food description`,
                   `food identifier`,
                   Protein,
                   `Food Composition Ref`) %>%
          mutate(PDCAAS = min(value)) %>%
          filter(value == PDCAAS) %>%
          ungroup() %>%
          left_join(temp_2) %>%
          mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            AA,
            PDCAAS,
            calculation,
            `Food Composition Ref`
          ) %>%
          rename("Limiting AA" = "AA") %>%
          separate_longer_delim(NI_ID, delim = ";") %>%
          mutate(NI_ID = str_trim(NI_ID)) %>%
          left_join(
            Protein_Correction %>%
              select(
                NI_ID,
                Food,
                `Correction Factor  (%)`,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              filter(`Sample Location` == "fecal") %>%
              mutate(Food = str_remove(Food, ", Average")) %>%
              group_by(
                Food,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              summarise(
                `Correction Factor  (%)` = round(sum(`Correction Factor  (%)`, na.rm = TRUE) /
                                                   n(), 2),
                NI_ID = paste0(NI_ID, collapse = "; ")
              ) %>%
              separate_longer_delim(NI_ID, delim = "; ") %>%
              distinct()
          ) %>%
          replace_na(list("Data Collection Source" = "Not Available")) %>%
          mutate(indicator = ifelse(
            `Data Collection Source` == `Food Composition Ref`,
            1,
            ifelse(
              str_detect(`Food Composition Ref`, "Standard Reference"),
              2,
              3
            )
          )) %>%
          filter(!(
            Calculation == "metabolic availability" &
              !str_detect(`Food Composition Ref`, "Standard Reference")
          )) %>%
          group_by(`food identifier`,
                   fdcId,
                   Species,
                   `Protein Form`,
                   `Sample Location`,
                   Calculation) %>%
          mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available",
            2,
            min(indicator, na.rm = TRUE)
          )) %>%
          ungroup() %>%
          filter(indicator == min_indicator) %>%
          select(!indicator) %>%
          select(!min_indicator) %>%
          mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
          group_by(
            Food,
            Species,
            `Protein Form`,
            `Sample Location`,
            Calculation,
            `Data Collection Source`
          ) %>%
          mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
          distinct() %>%
          mutate(PDCAAS = PDCAAS * ifelse(!is.na(`Correction Factor  (%)`), (
            as.numeric(`Correction Factor  (%)`) / 100
          ), 1)) %>%
          filter(
            `Protein Form` == "crude protein" |
              `Limiting AA` == `Protein Form` |
              (
                `Limiting AA` == "methionine+cysteine" &
                  `Protein Form` == "methionine"
              ) |
              (
                `Limiting AA` == "phenylalanine+tyrosine" &
                  `Protein Form` == "phenylalanine"
              ) | (
                `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
              )
          ) %>%
          mutate(PDCAAS = round(PDCAAS, 4)) %>%
          mutate(calculation = paste0(calculation, " \n = ", PDCAAS)) %>%
          select(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form`,
            Calculation,
            `Correction Factor  (%)`,
            `Limiting AA`,
            fdcId,
            PDCAAS,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          distinct() %>%
          mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)) %>%
          replace_na(
            list(
              Calculation = "Not Available",
              `Sample Location` = "Not Available",
              `Protein Form` = "Not Available",
              Species = "Not Available",
              `Correction Factor  (%)` = "Not Available",
              `Data Collection Source` = "Not Available"
            )
          ) %>%
          distinct()


        rm(temp_2)

        if (input$show_calc == "score_only") {
          PDCAAS <- PDCAAS %>%
            select(!calculation)
        } else{
          PDCAAS <- PDCAAS %>%
            select(!PDCAAS) %>%
            rename("PDCAAS" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- full_join(PQ_df, PDCAAS)
        } else{
          PQ_df <- PDCAAS
        }


      }

      if (ifelse(length(input$score) != 1,
                 "DIAAS" %in% input$score,
                 "DIAAS" == input$score)) {

        # --- AA-specific digestibility lookups (keep BOTH lysine + reactive lysine) ---
        aa_cf <- Protein_Correction %>%
          dplyr::filter(`Protein Form` %in% c(
            "histidine","leucine","isoleucine","valine",
            "tryptophan","lysine","reactive lysine",
            "methionine","threonine","phenylalanine"
          )) %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          # DO NOT drop plain lysine when reactive is present — we need both
          dplyr::select(
            NI_ID, Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`, `Correction Factor  (%)`
          ) %>%
          dplyr::distinct() %>%
          # Average CF at the Food/Species/Protein Form/... level and carry NI_IDs
          dplyr::group_by(
            Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          # Re-expand NI_ID for joining by NI_ID later
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # --- Crude protein digestibility lookup (unchanged) ---
        cp_cf <- Protein_Correction %>%
          dplyr::filter(`Protein Form` == "crude protein") %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          dplyr::select(
            NI_ID, Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`, `Correction Factor  (%)`
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # Map AA label -> Protein Form used in correction table
        # (Do NOT map lysine => reactive lysine here; we handle that via 'variant' below)
        map_pf <- function(aa) dplyr::case_when(
          aa == "methionine+cysteine" ~ "methionine",
          aa == "phenylalanine+tyrosine" ~ "phenylalanine",
          TRUE ~ aa
        )

        # --- Per-AA rows with digestibility applied BEFORE min()
        # Duplicate the rows across two variants so we compute BOTH versions:
        #  - variant = "lysine = reactive"  -> use reactive lysine CF for AA == "lysine"
        #  - variant = "lysine = plain"     -> use plain lysine CF for AA == "lysine"
        DIAAS_rows <- EAA_composition %>%
          dplyr::filter(NI_ID != "Not Available", NI_ID != "NA") %>%
          tidyr::drop_na(Protein, NI_ID) %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg_per_g = (value / Protein) * 1000) %>%
          dplyr::left_join(
            scoring_pattern %>%
              dplyr::rename(AA = Analyte) %>%
              dplyr::filter(`Pattern Name` == input$EAA_rec_DIAAS,
                            Age == input$rec_age_DIAAS) %>%
              dplyr::select(AA, Amount),
            by = "AA"
          ) %>%
          # Cross with the two lysine variants
          tidyr::crossing(variant = c("lysine = reactive", "lysine = plain")) %>%
          dplyr::mutate(
            `Protein Form Join` = dplyr::case_when(
              AA == "lysine" & variant == "lysine = reactive" ~ "reactive lysine",
              AA == "lysine" & variant == "lysine = plain"    ~ "lysine",
              TRUE ~ map_pf(AA)
            )
          ) %>%
          # Bring AA-specific digestibility with NI_ID re-expanded
          dplyr::left_join(
            aa_cf %>%
              dplyr::select(
                NI_ID, `Protein Form`, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ),
            by = c("NI_ID", "Protein Form Join" = "Protein Form")
          ) %>%
          dplyr::mutate(dig = cf) %>%
          # numeric DIAAS term per AA (ratio). NA if dig is missing.
          dplyr::mutate(value = (aa_mg_per_g * dig) / Amount) %>%
          # full numeric term string (showing which digestibility was used)
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg_per_g, format = "f", digits = 2),
              formatC(dig,        format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`, Protein,
            AA, Amount, aa_mg_per_g, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Protein Form Join`, variant
          )

        # --- Build full calculation string for AA-specific (include fdcId + description + variant) ---
        temp_2 <- DIAAS_rows %>%
          dplyr::select(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            AA, value, calc_piece, dig, variant
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            variant
          ) %>%
          dplyr::summarise(
            min_val = { vv <- value; if (all(is.na(vv))) NA_real_ else min(vv, na.rm = TRUE) },
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") = ",
              formatC(min_val, format = "f", digits = 4)
            ),
            n_rows = dplyr::n_distinct(AA[!is.na(dig) & !is.na(value)]),
            .groups = "drop"
          )

        # --- Limiting AA and final table for AA-specific (two rows per item: one per variant) ---
        DIAAS_aa <- DIAAS_rows %>%
          dplyr::left_join(
            temp_2,
            by = c(
              "fdcId","food identifier","food description","Species",
              "Sample Location","Calculation","Data Collection Source",
              "Food Composition Ref","variant"
            )
          ) %>%
          dplyr::mutate(DIAAS = min_val) %>%
          dplyr::filter(value == min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(indicator = dplyr::if_else(
            `Data Collection Source` == `Food Composition Ref`, 1L,
            dplyr::if_else(stringr::str_detect(`Food Composition Ref`, "Standard Reference"), 2L, 3L)
          )) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available", 2L, min(indicator, na.rm = TRUE)
          )) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator, !min_indicator) %>%
          # Rebuild Food label and re-concatenate NI_IDs within the row grouping (per variant)
          dplyr::mutate(Food = dplyr::coalesce(Food, `food description`)) %>%
          dplyr::group_by(
            Food, Species, `Protein Form Join`, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Food Composition Ref`, fdcId, AA, variant
          ) %>%
          dplyr::mutate(NI_ID = paste0(unique(NI_ID), collapse = "; ")) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::transmute(
            NI_ID,
            Food,
            Species, `Sample Location`,
            `Protein Form` = `Protein Form Join`,
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            # `Lysine Variant` = variant,   # <— tells you which lysine CF was used
            DIAAS = round(DIAAS, 2),
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))

        # --- Crude protein branch (unchanged) ---
        DIAAS_cp_rows <- EAA_composition %>%
          tidyr::drop_na(Protein, NI_ID) %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg_per_g = (value / Protein) * 1000) %>%
          dplyr::left_join(
            scoring_pattern %>%
              dplyr::rename(AA = Analyte) %>%
              dplyr::filter(`Pattern Name` == input$EAA_rec_DIAAS,
                            Age == input$rec_age_DIAAS) %>%
              dplyr::select(AA, Amount),
            by = "AA"
          ) %>%
          dplyr::left_join(
            cp_cf %>%
              dplyr::select(
                NI_ID, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ),
            by = "NI_ID"
          ) %>%
          dplyr::filter(!is.na(cf)) %>%
          dplyr::mutate(dig = cf) %>%
          dplyr::mutate(value = (aa_mg_per_g * dig) / Amount) %>%
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg_per_g, format = "f", digits = 2),
              formatC(dig, format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`, Protein,
            AA, Amount, aa_mg_per_g, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`
          )

        temp_3 <- DIAAS_cp_rows %>%
          dplyr::select(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            AA, value, calc_piece, dig
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`
          ) %>%
          dplyr::summarise(
            min_val = { vv <- value; if (all(is.na(vv))) NA_real_ else min(vv, na.rm = TRUE) },
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") = ",
              formatC(min_val, format = "f", digits = 4)
            ),
            .groups = "drop"
          )

        DIAAS_cp <- DIAAS_cp_rows %>%
          dplyr::left_join(
            temp_3,
            by = c(
              "fdcId","food identifier","food description","Species",
              "Sample Location","Calculation","Data Collection Source",
              "Food Composition Ref"
            )
          ) %>%
          dplyr::mutate(DIAAS = min_val) %>%
          dplyr::filter(value == min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(indicator = dplyr::if_else(
            `Data Collection Source` == `Food Composition Ref`, 1L,
            dplyr::if_else(stringr::str_detect(`Food Composition Ref`, "Standard Reference"), 2L, 3L)
          )) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available", 2L, min(indicator, na.rm = TRUE)
          )) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator, !min_indicator) %>%
          dplyr::transmute(
            NI_ID,
            Food = dplyr::coalesce(Food, `food description`),
            Species, `Sample Location`,
            `Protein Form` = "crude protein",
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            # `Lysine Variant` = NA_character_,  # not applicable in CP branch
            DIAAS = round(DIAAS, 2),
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))

        # --- Combine both approaches (AA-specific has two variants per item) ---
        DIAAS <- dplyr::bind_rows(DIAAS_aa, DIAAS_cp)

        # --- Output toggle ---
        if (input$show_calc == "score_only") {
          DIAAS <- DIAAS %>% dplyr::select(-calculation)
        } else {
          DIAAS <- DIAAS %>% dplyr::select(-DIAAS) %>% dplyr::rename("DIAAS" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- dplyr::full_join(PQ_df, DIAAS)
        } else {
          PQ_df <- DIAAS
        }
      }


      if ("crude protein" %in% debounced_filters()$analyte) {
        if (!("individual amino acids" %in% debounced_filters()$analyte)) {
          PQ_df <- PQ_df %>%
            filter(`Protein Form` == "crude protein")
        }
      }
      if ("individual amino acids" %in% debounced_filters()$analyte) {
        if (!("crude protein" %in% debounced_filters()$analyte)) {
          PQ_df <- PQ_df %>%
            filter(`Protein Form` != "crude protein")
        }
      }

      PQ_df <- PQ_df %>%
        arrange(Species,
                Calculation,
                `Sample Location`,
                `Protein Form`,
                Food) %>%
        filter((`Sample Location` %in% debounced_filters()$sample) |
                 (
                   `Sample Location` == "Not Available" &
                     input$require_bioavail == FALSE
                 )
        ) %>%
        filter(
          Calculation %in% debounced_filters()$measure |
            (
              Calculation == "Not Available" &
                input$require_bioavail == FALSE
            )
        ) %>%
        filter(
          Species %in% debounced_filters()$species |
            (
              Species == "Not Available" & input$require_bioavail == FALSE
            )
        ) %>%
        rename("Correction Factor Species" = "Species") %>%
        rename("Correction Factor Protein Form" = "Protein Form") %>%
        rename("Correction Factor Sample Location" = "Sample Location") %>%
        rename("Correction Factor Calculation" = "Calculation") %>%
        rename("Correction Factor Ref" = "Data Collection Source")  %>%
        mutate(across(where(is.character), ~ gsub("\n", "<br>", .))) %>%
        relocate(`Correction Factor Ref`, .after = last_col()) %>%
        relocate(`Food Composition Ref`, .after = last_col()) %>%
        mutate(
          `Limiting AA` = ifelse(
            `Limiting AA` == "methionine+cysteine",
            "methionine + cysteine",
            `Limiting AA`
          )
        ) %>%
        mutate(
          `Limiting AA` = ifelse(
            `Limiting AA` == "phenylalanine+tyrosine",
            "phenylalanine + tyrosine",
            `Limiting AA`
          )
        )


      if (input$search_tab1 != "") {
        PQ_df <- fuzzy_search_filter(PQ_df, input$search_tab1)

      }
    }
    PQ_df

  })
  data_bv <- reactive({
    correction_factors <- Protein_Correction

    if (input$search_tab2 != "") {
      correction_factors <- fuzzy_search_filter(correction_factors, input$search_tab2)

    }
    correction_factors %>%
      mutate(n = as.character(n)) %>%
      mutate(`Correction Factor SD` = as.character(`Correction Factor SD`)) %>%
      mutate(`Protein (g)` = as.character(`Protein (g)`)) %>%
      replace_na(
        list(
          Food = "not reported",
          "Protein (g)" = "not reported",
          n = "not reported",
          "Correction Factor SD" = "not reported",
          "Analysis method(s)" = "not reported",
          "Original Data Source(s)" = "not reported"
        )
      ) %>%
      filter(`Species` %in% input$species) %>%
      filter(`Sample Location` %in% input$sample) %>%
      filter(`Protein Form` %in% input$analyte) %>%
      filter(Calculation %in% input$measure)
  })
  data_eaa <- reactive({
    fdcmp_df <- EAA_composition
    if (input$search_tab3 != "") {
      fdcmp_df <- fuzzy_search_filter(fdcmp_df, input$search_tab3)
    }


    fdcmp_df <- fdcmp_df %>%
      select(NI_ID,
             fdcId,
             `food description`,
             Protein,
             AA,
             value,
             `Food Composition Ref`) %>%
      mutate("FDC_ID" = fdcId) %>%
      select(NI_ID,
             FDC_ID,
             `food description`,
             Protein,
             AA,
             value,
             `Food Composition Ref`) %>%
      distinct() %>%
      mutate(AA = factor(
        AA,
        levels = c(
          "histidine",
          "isoleucine",
          "leucine",
          "lysine",
          "methionine+cysteine",
          "phenylalanine+tyrosine",
          "threonine",
          "tryptophan",
          "valine"
        ),
        labels = c(
          "His (g/100g)",
          "Ile (g/100g)",
          "Leu (g/100g)",
          "Lys (g/100g)",
          "Met+Cys (g/100g)",
          "Phe+Tyr (g/100g)",
          "Thr (g/100g)",
          "Trp (g/100g)",
          "Val (g/100g)"
        )
      )) %>%
      pivot_wider(names_from = "AA", values_from = "value") %>%
      rename("Protein (g/100g)" = "Protein") %>%
      rename("Food description" = "food description") %>%
      distinct() %>%
      relocate(`Food Composition Ref`, .after = last_col())
    if (input$show_NI_ID == FALSE) {
      fdcmp_df <- fdcmp_df %>%
        select(!NI_ID)
    }
    fdcmp_df
  })


  # render table for protein correction factors
  output$table <- DT::renderDataTable(server = TRUE, {
    correction_factors <- Protein_Correction

    if (input$search_tab2 != "") {
      correction_factors <- fuzzy_search_filter(correction_factors, input$search_tab2)
    }

    sketch <- withTags(table(class = 'display',
                             thead(tr(
                               lapply(colnames(data_bv()), function(col) {
                                 tooltip <- col_meta$tooltip[match(col, col_meta$col)]
                                 tooltip <-
                                   ifelse(is.na(tooltip), "", tooltip)
                                 th(title = tooltip, col)
                               })
                             ))))


    DT::datatable(
      data_bv(),
      container = sketch,
      extensions = c('FixedHeader'),
      class = "display cell-border compact",
      rownames = FALSE,
      options = list(
        fixedHeader = TRUE,
        dom = 'ltip',
        pageLength = 25,
        lengthMenu = c(25, 50, 75, 100),
        columnDefs = list(list(
          targets = c(11, 12),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 100 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
            "}"
          )
        ))
      ),
      escape = FALSE
    ) %>%
      formatStyle(1:13,
                  'vertical-align' = 'top',
                  'overflow-wrap' = 'break-word') %>%
      formatStyle(11:12,
                  'overflow-wrap' = 'break-word',
                  'word-break' = 'break-word')

  })

  pq_filter_inputs <- reactive({
    list(
      species = input$pq_species,
      sample = input$pq_sample,
      analyte = input$pq_analyte,
      measure = input$pq_measure
    )
  })

  debounced_filters <- debounce(pq_filter_inputs, millis = 1000)


  # render table for protein quality scores
  output$table_2 <- DT::renderDataTable(server = TRUE, {
    PQ_df <- data.frame(NI_ID = character())

    if (!("EAA-9" %in% input$score |
          "PDCAAS" %in% input$score | "DIAAS" %in% input$score)) {
      DT::datatable(
        data.frame(Message = "Please select scores above"),
        rownames = FALSE,
        options = list(
          dom = 't',
          # hides filter, pagination, etc.
          ordering = FALSE,
          paging = FALSE
        )
      )
    } else {
      if ("EAA-9" %in% input$score) {
        if (input$EAA_rec == "Choose custom recommendations") {
          scoring_pattern[199, ] <-
            list(
              "Choose custom recommendations",
              "histidine",
              ifelse(is.numeric(input$hist), input$hist, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[200, ] <-
            list(
              "Choose custom recommendations",
              "leucine",
              ifelse(is.numeric(input$leu), input$leu, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[201, ] <-
            list(
              "Choose custom recommendations",
              "isoleucine",
              ifelse(is.numeric(input$ile), input$ile, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[202, ] <-
            list(
              "Choose custom recommendations",
              "lysine",
              ifelse(is.numeric(input$lys), input$lys, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[203, ] <-
            list(
              "Choose custom recommendations",
              "methionine+cysteine",
              ifelse(is.numeric(input$met_cys), input$met_cys, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[204, ] <-
            list(
              "Choose custom recommendations",
              "phenylalanine+tyrosine",
              ifelse(is.numeric(input$phe_tyr), input$phe_tyr, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[205, ] <-
            list(
              "Choose custom recommendations",
              "threonine",
              ifelse(is.numeric(input$thr), input$thr, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[206, ] <-
            list(
              "Choose custom recommendations",
              "tryptophan",
              ifelse(is.numeric(input$trp), input$trp, 1),
              "mg/kg/d",
              "custom",
              NA
            )
          scoring_pattern[207, ] <-
            list(
              "Choose custom recommendations",
              "valine",
              ifelse(is.numeric(input$val), input$val, 1),
              "mg/kg/d",
              "custom",
              NA
            )
        }
        if (input$require_bioavail == TRUE) {
          EAA_composition <- EAA_composition %>%
            filter(NI_ID != "NA") %>%
            drop_na(NI_ID)
        }

        if (input$serving_size == "Use standard serving sizes") {
          EAA_9 <- EAA_composition %>%
            left_join(
              portion_sizes %>%
                select(fdcId, g_weight , portion) %>%
                mutate(fdcId = as.character(fdcId)) %>%
                mutate(portion = paste0(portion, " (", g_weight, " g)"))
            ) %>%
            mutate(portion = ifelse(is.na(portion), "not provided (100 g)", portion)) %>%
            mutate(g_weight = ifelse(is.na(g_weight), 100, g_weight)) %>%
            mutate(value = value * 1000) %>%
            mutate(value = value * (g_weight / 100))
        } else{
          EAA_9 <- EAA_composition %>%
            mutate(value = value * 1000) %>%
            mutate(value = value * (input$serving_weight / 100)) %>%
            mutate(portion = paste0(input$serving_weight, " g"))
        }

        if (input$EAA_rec == "Choose custom recommendations") {
          pattern <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec) %>%
            select(AA = Analyte, Amount)
        } else{
          pattern <- scoring_pattern %>%
            filter(`Pattern Name` == input$EAA_rec) %>%
            filter(is.character(input$rec_age) &
                     Age == input$rec_age | !is.na(Age)) %>%
            select(AA = Analyte, Amount)
        }

        # Original logic
        EAA_9_og <- EAA_9 %>%
          left_join(pattern, by = "AA") %>%
          mutate(
            norm_value = Amount * input$weight,
            calculation = paste0(round(value, 2), "/", round(norm_value, 2)),
            value = value / norm_value
          ) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            portion,
            value,
            AA,
            calculation,
            `Food Composition Ref`
          )

        min_scores <- EAA_9_og %>%
          group_by(fdcId, `food identifier`, `food description`) %>%
          summarise(
            calculation = paste0(
              "min(",
              str_c(calculation, collapse = ", "),
              ") x 100 x Correction Factor \n = ",
              min(round(value, 2)),
              " x 100 x Correction Factor"
            ),
            .groups = "drop"
          )

        EAA_9_og <- EAA_9_og %>%
          select(-calculation) %>%
          left_join(min_scores, by = c("fdcId", "food identifier", "food description")) %>%
          group_by(
            fdcId,
            `food identifier`,
            `food description`,
            Protein,
            portion,
            `Food Composition Ref`
          ) %>%
          mutate(`EAA-9` = min(value)) %>%
          filter(value == `EAA-9`) %>%
          ungroup() %>%
          rename("Limiting AA" = "AA") %>%
          separate_longer_delim(NI_ID, delim = ";") %>%
          mutate(NI_ID = str_trim(NI_ID)) %>%
          left_join(
            Protein_Correction %>%
              mutate(Food = str_remove(Food, ", Average")) %>%
              group_by(
                Food,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              summarise(
                `Correction Factor  (%)` = round(sum(`Correction Factor  (%)`, na.rm = TRUE) /
                                                   n(), 2),
                NI_ID = paste0(NI_ID, collapse = "; "),
                .groups = "drop"
              ) %>%
              separate_longer_delim(NI_ID, delim = "; ") %>%
              distinct(),
            by = "NI_ID"
          ) %>%
          replace_na(list(`Data Collection Source` = "Not Available")) %>%
          mutate(
            indicator = case_when(
              `Data Collection Source` == `Food Composition Ref` ~ 1,
              str_detect(`Food Composition Ref`, "Standard Reference") ~ 2,
              TRUE ~ 3
            )
          ) %>%
          filter(!(
            Calculation == "metabolic availability" &
              !str_detect(`Food Composition Ref`, "Standard Reference")
          )) %>%
          group_by(`food identifier`) %>%
          mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available",
            2,
            min(indicator, na.rm = TRUE)
          )) %>%
          ungroup() %>%
          filter(indicator == min_indicator) %>%
          select(-indicator, -min_indicator) %>%
          mutate(Food = coalesce(Food, `food description`)) %>%
          group_by(
            Food,
            Species,
            `Protein Form`,
            `Sample Location`,
            Calculation,
            `Data Collection Source`
          ) %>%
          mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
          distinct() %>%
          ungroup() %>%
          mutate(
            `EAA-9` = round(
              `EAA-9` * ifelse(
                is.na(`Correction Factor  (%)`),
                1,
                `Correction Factor  (%)` / 100
              ) * 100,
              2
            ),
            calculation = ifelse(
              is.na(`Correction Factor  (%)`),
              str_remove(calculation, " x Correction Factor"),
              calculation
            ),
            calculation = paste0(calculation, " \n = ", `EAA-9`, "%")
          ) %>%
          filter(
            ifelse(
              !is.na(`Protein Form`),
              `Protein Form` == "crude protein" ,
              !is.na(`EAA-9`)
            )
          ) %>%
          select(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form`,
            Calculation,
            `Correction Factor  (%)`,
            `Limiting AA`,
            fdcId,
            portion,
            `EAA-9`,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          rename("serving size" = "portion", "EAA-9 (%)" = "EAA-9") %>%
          mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)) %>%
          replace_na(
            list(
              NI_ID = "Not Available",
              Calculation = "Not Available",
              `Sample Location` = "Not Available",
              `Protein Form` = "Not Available",
              Species = "Not Available",
              `Correction Factor  (%)` = "Not Available",
              `Data Collection Source` = "Not Available"
            )
          ) %>%
          distinct()

        #Altered logic for aa
        # --- AA digestibility lookups (AA-specific) ---
        aa_cf <- Protein_Correction %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          # Average CF at the Food/Species/... level and collect the NI_IDs that contributed
          dplyr::group_by(
            Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          # Re-expand NI_ID for matching by NI_ID later (still a single NI_ID column)
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # Keep only NI_ID + crude-protein CF for optional fallbacks/extensions
        cp_cf <- aa_cf %>%
          dplyr::filter(`Protein Form` == "crude protein") %>%
          dplyr::select(NI_ID, cp_cf = cf)

        # Map AA label -> Protein Form used in correction table
        map_pf <- function(aa) dplyr::case_when(
          aa == "lysine" ~ "reactive lysine",
          aa == "methionine+cysteine" ~ "methionine",
          aa == "phenylalanine+tyrosine" ~ "phenylalanine",
          TRUE ~ aa
        )

        # --- Per-AA rows with digestibility applied BEFORE min() ---
        EAA_9_alt_rows <- EAA_9 %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg = (value)) %>%         # mg AA (already per your input)
          dplyr::left_join(pattern, by = "AA") %>%
          dplyr::mutate(Amount = Amount * input$weight) %>%
          dplyr::mutate(`Protein Form Join` = map_pf(AA)) %>%
          # Bring AA-specific CF + metadata; align Protein Form strings for join
          dplyr::left_join(
            aa_cf %>%
              dplyr::select(
                NI_ID, `Protein Form`, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ) %>%
              dplyr::mutate(`Protein Form` = map_pf(`Protein Form`)),
            by = c("NI_ID", "Protein Form Join" = "Protein Form")
          ) %>%
          # Keep only rows where AA-specific digestibility exists (no crude fallback here)
          dplyr::filter(!is.na(cf)) %>%
          dplyr::mutate(dig = cf) %>%
          dplyr::mutate(value = (aa_mg * dig) / Amount) %>%  # ratio (not %)
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg, format = "f", digits = 2),
              formatC(dig,    format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`,
            AA, Amount, aa_mg, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Protein Form Join`, portion
          )

        # --- Build full calculation string (no "Correction Factor" text) ---
        temp_2 <- EAA_9_alt_rows %>%
          dplyr::group_by(
            fdcId, `food identifier`, Species, `food description`,
            `Sample Location`, Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            min_val = min(value, na.rm = TRUE),
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") x 100 = ",
              formatC(min_val, format = "f", digits = 4),
              " x 100 = ",
              formatC(min_val * 100, format = "f", digits = 2),
              "%"
            ),
            n_rows = dplyr::n(),
            .groups = "drop"
          )

        # --- Limiting AA and final table (EAA-9 stored as ratio; display shows %) ---
        EAA_9_alt <- EAA_9_alt_rows %>%
          dplyr::group_by(
            fdcId, NI_ID, `food identifier`, `food description`, `Food Composition Ref`
          ) %>%
          dplyr::mutate(`EAA-9 (%)` = min(value, na.rm = TRUE)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(value == `EAA-9 (%)`) %>%
          dplyr::left_join(
            temp_2,
            by = c(
              "fdcId", "food identifier", "food description",
              "Species", "Sample Location", "Calculation", "Data Collection Source"
            )
          ) %>%
          dplyr::filter(value == min_val) %>%
          dplyr::mutate(`EAA-9 (%)` = min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(
            indicator = dplyr::case_when(
              `Data Collection Source` == `Food Composition Ref` ~ 1L,
              stringr::str_detect(`Food Composition Ref`, "Standard Reference") ~ 2L,
              TRUE ~ 3L
            )
          ) %>%
          dplyr::filter(!(Calculation == "metabolic availability" &
                            !stringr::str_detect(`Food Composition Ref`, "Standard Reference"))) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(
            min_indicator = ifelse(
              `Data Collection Source` == "Not Available",
              2L, min(indicator, na.rm = TRUE)
            )
          ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator) %>%
          dplyr::select(!min_indicator) %>%
          # --- Rebuild Food label and RE-CONCATENATE NI_ID like the first half ---
          dplyr::mutate(Food = dplyr::coalesce(Food, `food description`)) %>%
          dplyr::group_by(
            Food, Species, `Protein Form Join`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::mutate(NI_ID = paste0(unique(NI_ID), collapse = "; ")) %>%
          dplyr::distinct() %>%
          dplyr::ungroup() %>%
          dplyr::transmute(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form` = `Protein Form Join`,
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            portion,
            `EAA-9 (%)` = round(`EAA-9 (%)`, 2) * 100,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::rename("serving size" = "portion") %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))


        EAA_9 <- dplyr::bind_rows(
          EAA_9_og  %>% mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)),
          EAA_9_alt %>% mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))
        )


        # Optional: final score/calc switch
        if (input$show_calc == "score_only") {
          EAA_9 <- EAA_9 %>% select(-calculation)
        } else {
          EAA_9 <- EAA_9 %>% select(-`EAA-9 (%)`) %>% rename("EAA-9 (%)" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- full_join(PQ_df, EAA_9)
        } else {
          PQ_df <- EAA_9
        }
      }

      if (ifelse(length(input$score) != 1,
                 "PDCAAS" %in% input$score,
                 "PDCAAS" == input$score)) {
        PDCAAS <- EAA_composition %>%
          drop_na(Protein) %>%
          drop_na(NI_ID) %>%
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
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            value,
            AA,
            calculation,
            `Food Composition Ref`
          )

        temp_2 <- PDCAAS %>%
          select(fdcId, NI_ID, `food description`, value, calculation) %>%
          group_by(fdcId, NI_ID, `food description`) %>%
          summarise(
            calculation = paste0(
              "min(",
              str_c(calculation, collapse = ", "),
              ", 1) x Correction Factor \n = ",
              paste0(ifelse(min(
                round(value, 2)
              ) >= 1, 1, min(
                round(value, 2)
              ))),
              " x Correction Factor"
            )
          ) %>%
          ungroup()

        PDCAAS <- PDCAAS %>%
          select(!calculation) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            value,
            AA,
            `Food Composition Ref`
          ) %>%
          group_by(fdcId,
                   NI_ID,
                   `food description`,
                   `food identifier`,
                   Protein,
                   `Food Composition Ref`) %>%
          mutate(PDCAAS = min(value)) %>%
          filter(value == PDCAAS) %>%
          ungroup() %>%
          left_join(temp_2) %>%
          mutate(PDCAAS = ifelse(PDCAAS >= 1, 1, PDCAAS)) %>%
          select(
            fdcId,
            NI_ID,
            `food identifier`,
            `food description`,
            Protein,
            AA,
            PDCAAS,
            calculation,
            `Food Composition Ref`
          ) %>%
          rename("Limiting AA" = "AA") %>%
          separate_longer_delim(NI_ID, delim = ";") %>%
          mutate(NI_ID = str_trim(NI_ID)) %>%
          left_join(
            Protein_Correction %>%
              select(
                NI_ID,
                Food,
                `Correction Factor  (%)`,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              filter(`Sample Location` == "fecal") %>%
              mutate(Food = str_remove(Food, ", Average")) %>%
              group_by(
                Food,
                Species,
                `Protein Form`,
                `Sample Location`,
                Calculation,
                `Data Collection Source`
              ) %>%
              summarise(
                `Correction Factor  (%)` = round(sum(`Correction Factor  (%)`, na.rm = TRUE) /
                                                   n(), 2),
                NI_ID = paste0(NI_ID, collapse = "; ")
              ) %>%
              separate_longer_delim(NI_ID, delim = "; ") %>%
              distinct()
          ) %>%
          replace_na(list("Data Collection Source" = "Not Available")) %>%
          mutate(indicator = ifelse(
            `Data Collection Source` == `Food Composition Ref`,
            1,
            ifelse(
              str_detect(`Food Composition Ref`, "Standard Reference"),
              2,
              3
            )
          )) %>%
          filter(!(
            Calculation == "metabolic availability" &
              !str_detect(`Food Composition Ref`, "Standard Reference")
          )) %>%
          group_by(`food identifier`,
                   fdcId,
                   Species,
                   `Protein Form`,
                   `Sample Location`,
                   Calculation) %>%
          mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available",
            2,
            min(indicator, na.rm = TRUE)
          )) %>%
          ungroup() %>%
          filter(indicator == min_indicator) %>%
          select(!indicator) %>%
          select(!min_indicator) %>%
          mutate(Food = ifelse(is.na(Food), `food description`, Food)) %>%
          group_by(
            Food,
            Species,
            `Protein Form`,
            `Sample Location`,
            Calculation,
            `Data Collection Source`
          ) %>%
          mutate(NI_ID = paste0(NI_ID, collapse = "; ")) %>%
          distinct() %>%
          mutate(PDCAAS = PDCAAS * ifelse(!is.na(`Correction Factor  (%)`), (
            as.numeric(`Correction Factor  (%)`) / 100
          ), 1)) %>%
          filter(
            `Protein Form` == "crude protein" |
              `Limiting AA` == `Protein Form` |
              (
                `Limiting AA` == "methionine+cysteine" &
                  `Protein Form` == "methionine"
              ) |
              (
                `Limiting AA` == "phenylalanine+tyrosine" &
                  `Protein Form` == "phenylalanine"
              ) | (
                `Limiting AA` == "lysine" & `Protein Form` == "reactive lysine"
              )
          ) %>%
          mutate(PDCAAS = round(PDCAAS, 4)) %>%
          mutate(calculation = paste0(calculation, " \n = ", PDCAAS)) %>%
          select(
            NI_ID,
            Food,
            Species,
            `Sample Location`,
            `Protein Form`,
            Calculation,
            `Correction Factor  (%)`,
            `Limiting AA`,
            fdcId,
            PDCAAS,
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          distinct() %>%
          mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`)) %>%
          replace_na(
            list(
              Calculation = "Not Available",
              `Sample Location` = "Not Available",
              `Protein Form` = "Not Available",
              Species = "Not Available",
              `Correction Factor  (%)` = "Not Available",
              `Data Collection Source` = "Not Available"
            )
          ) %>%
          distinct()


        rm(temp_2)

        if (input$show_calc == "score_only") {
          PDCAAS <- PDCAAS %>%
            select(!calculation)
        } else{
          PDCAAS <- PDCAAS %>%
            select(!PDCAAS) %>%
            rename("PDCAAS" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- full_join(PQ_df, PDCAAS)
        } else{
          PQ_df <- PDCAAS
        }


      }

      if (ifelse(length(input$score) != 1,
                 "DIAAS" %in% input$score,
                 "DIAAS" == input$score)) {

        # --- AA-specific digestibility lookups (keep BOTH lysine + reactive lysine) ---
        aa_cf <- Protein_Correction %>%
          dplyr::filter(`Protein Form` %in% c(
            "histidine","leucine","isoleucine","valine",
            "tryptophan","lysine","reactive lysine",
            "methionine","threonine","phenylalanine"
          )) %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          # DO NOT drop plain lysine when reactive is present — we need both
          dplyr::select(
            NI_ID, Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`, `Correction Factor  (%)`
          ) %>%
          dplyr::distinct() %>%
          # Average CF at the Food/Species/Protein Form/... level and carry NI_IDs
          dplyr::group_by(
            Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          # Re-expand NI_ID for joining by NI_ID later
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # --- Crude protein digestibility lookup (unchanged) ---
        cp_cf <- Protein_Correction %>%
          dplyr::filter(`Protein Form` == "crude protein") %>%
          dplyr::mutate(Food = stringr::str_remove(Food, ", Average")) %>%
          dplyr::select(
            NI_ID, Food, Species, `Protein Form`, `Sample Location`,
            Calculation, `Data Collection Source`, `Correction Factor  (%)`
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`
          ) %>%
          dplyr::summarise(
            `Correction Factor  (%)` = round(mean(`Correction Factor  (%)`, na.rm = TRUE), 2),
            NI_ID = paste0(NI_ID, collapse = "; "),
            .groups = "drop"
          ) %>%
          tidyr::separate_longer_delim(NI_ID, delim = "; ") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::distinct() %>%
          dplyr::mutate(cf = as.numeric(`Correction Factor  (%)`) / 100)

        # Map AA label -> Protein Form used in correction table
        # (Do NOT map lysine => reactive lysine here; we handle that via 'variant' below)
        map_pf <- function(aa) dplyr::case_when(
          aa == "methionine+cysteine" ~ "methionine",
          aa == "phenylalanine+tyrosine" ~ "phenylalanine",
          TRUE ~ aa
        )

        # --- Per-AA rows with digestibility applied BEFORE min()
        # Duplicate the rows across two variants so we compute BOTH versions:
        #  - variant = "lysine = reactive"  -> use reactive lysine CF for AA == "lysine"
        #  - variant = "lysine = plain"     -> use plain lysine CF for AA == "lysine"
        DIAAS_rows <- EAA_composition %>%
          dplyr::filter(NI_ID != "Not Available", NI_ID != "NA") %>%
          tidyr::drop_na(Protein, NI_ID) %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg_per_g = (value / Protein) * 1000) %>%
          dplyr::left_join(
            scoring_pattern %>%
              dplyr::rename(AA = Analyte) %>%
              dplyr::filter(`Pattern Name` == input$EAA_rec_DIAAS,
                            Age == input$rec_age_DIAAS) %>%
              dplyr::select(AA, Amount),
            by = "AA"
          ) %>%
          # Cross with the two lysine variants
          tidyr::crossing(variant = c("lysine = reactive", "lysine = plain")) %>%
          dplyr::mutate(
            `Protein Form Join` = dplyr::case_when(
              AA == "lysine" & variant == "lysine = reactive" ~ "reactive lysine",
              AA == "lysine" & variant == "lysine = plain"    ~ "lysine",
              TRUE ~ map_pf(AA)
            )
          ) %>%
          # Bring AA-specific digestibility with NI_ID re-expanded
          dplyr::left_join(
            aa_cf %>%
              dplyr::select(
                NI_ID, `Protein Form`, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ),
            by = c("NI_ID", "Protein Form Join" = "Protein Form")
          ) %>%
          dplyr::mutate(dig = cf) %>%
          # numeric DIAAS term per AA (ratio). NA if dig is missing.
          dplyr::mutate(value = (aa_mg_per_g * dig) / Amount) %>%
          # full numeric term string (showing which digestibility was used)
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg_per_g, format = "f", digits = 2),
              formatC(dig,        format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`, Protein,
            AA, Amount, aa_mg_per_g, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Protein Form Join`, variant
          )

        # --- Build full calculation string for AA-specific (include fdcId + description + variant) ---
        temp_2 <- DIAAS_rows %>%
          dplyr::select(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            AA, value, calc_piece, dig, variant
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            variant
          ) %>%
          dplyr::summarise(
            min_val = { vv <- value; if (all(is.na(vv))) NA_real_ else min(vv, na.rm = TRUE) },
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") = ",
              formatC(min_val, format = "f", digits = 4)
            ),
            n_rows = dplyr::n_distinct(AA[!is.na(dig) & !is.na(value)]),
            .groups = "drop"
          )

        # --- Limiting AA and final table for AA-specific (two rows per item: one per variant) ---
        DIAAS_aa <- DIAAS_rows %>%
          dplyr::left_join(
            temp_2,
            by = c(
              "fdcId","food identifier","food description","Species",
              "Sample Location","Calculation","Data Collection Source",
              "Food Composition Ref","variant"
            )
          ) %>%
          dplyr::mutate(DIAAS = min_val) %>%
          dplyr::filter(value == min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(indicator = dplyr::if_else(
            `Data Collection Source` == `Food Composition Ref`, 1L,
            dplyr::if_else(stringr::str_detect(`Food Composition Ref`, "Standard Reference"), 2L, 3L)
          )) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available", 2L, min(indicator, na.rm = TRUE)
          )) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator, !min_indicator) %>%
          # Rebuild Food label and re-concatenate NI_IDs within the row grouping (per variant)
          dplyr::mutate(Food = dplyr::coalesce(Food, `food description`)) %>%
          dplyr::group_by(
            Food, Species, `Protein Form Join`, `Sample Location`,
            Calculation, `Data Collection Source`,
            `Food Composition Ref`, fdcId, AA, variant
          ) %>%
          dplyr::mutate(NI_ID = paste0(unique(NI_ID), collapse = "; ")) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::transmute(
            NI_ID,
            Food,
            Species, `Sample Location`,
            `Protein Form` = `Protein Form Join`,
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            # `Lysine Variant` = variant,   # <— tells you which lysine CF was used
            DIAAS = round(DIAAS, 2),
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))

        # --- Crude protein branch (unchanged) ---
        DIAAS_cp_rows <- EAA_composition %>%
          tidyr::drop_na(Protein, NI_ID) %>%
          tidyr::separate_longer_delim(NI_ID, delim = ";") %>%
          dplyr::mutate(NI_ID = stringr::str_trim(NI_ID)) %>%
          dplyr::mutate(aa_mg_per_g = (value / Protein) * 1000) %>%
          dplyr::left_join(
            scoring_pattern %>%
              dplyr::rename(AA = Analyte) %>%
              dplyr::filter(`Pattern Name` == input$EAA_rec_DIAAS,
                            Age == input$rec_age_DIAAS) %>%
              dplyr::select(AA, Amount),
            by = "AA"
          ) %>%
          dplyr::left_join(
            cp_cf %>%
              dplyr::select(
                NI_ID, cf,
                Food, Species, `Sample Location`,
                Calculation, `Data Collection Source`
              ),
            by = "NI_ID"
          ) %>%
          dplyr::filter(!is.na(cf)) %>%
          dplyr::mutate(dig = cf) %>%
          dplyr::mutate(value = (aa_mg_per_g * dig) / Amount) %>%
          dplyr::mutate(
            calc_piece = sprintf(
              "(%s * %s) / %s",
              formatC(aa_mg_per_g, format = "f", digits = 2),
              formatC(dig, format = "f", digits = 3),
              Amount
            )
          ) %>%
          dplyr::select(
            fdcId, NI_ID, `food identifier`, `food description`, Protein,
            AA, Amount, aa_mg_per_g, dig, value, calc_piece,
            `Food Composition Ref`,
            Food, Species, `Sample Location`,
            Calculation, `Data Collection Source`
          )

        temp_3 <- DIAAS_cp_rows %>%
          dplyr::select(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`,
            AA, value, calc_piece, dig
          ) %>%
          dplyr::distinct() %>%
          dplyr::group_by(
            fdcId, `food identifier`, `food description`, Species,
            `Sample Location`, Calculation,
            `Data Collection Source`, `Food Composition Ref`
          ) %>%
          dplyr::summarise(
            min_val = { vv <- value; if (all(is.na(vv))) NA_real_ else min(vv, na.rm = TRUE) },
            calculation = paste0(
              "min(",
              stringr::str_c(calc_piece, collapse = ", "),
              ") = ",
              formatC(min_val, format = "f", digits = 4)
            ),
            .groups = "drop"
          )

        DIAAS_cp <- DIAAS_cp_rows %>%
          dplyr::left_join(
            temp_3,
            by = c(
              "fdcId","food identifier","food description","Species",
              "Sample Location","Calculation","Data Collection Source",
              "Food Composition Ref"
            )
          ) %>%
          dplyr::mutate(DIAAS = min_val) %>%
          dplyr::filter(value == min_val) %>%
          tidyr::replace_na(list(`Data Collection Source` = "Not Available")) %>%
          dplyr::mutate(indicator = dplyr::if_else(
            `Data Collection Source` == `Food Composition Ref`, 1L,
            dplyr::if_else(stringr::str_detect(`Food Composition Ref`, "Standard Reference"), 2L, 3L)
          )) %>%
          dplyr::group_by(`food identifier`) %>%
          dplyr::mutate(min_indicator = ifelse(
            `Data Collection Source` == "Not Available", 2L, min(indicator, na.rm = TRUE)
          )) %>%
          dplyr::ungroup() %>%
          dplyr::filter(indicator == min_indicator) %>%
          dplyr::select(!indicator, !min_indicator) %>%
          dplyr::transmute(
            NI_ID,
            Food = dplyr::coalesce(Food, `food description`),
            Species, `Sample Location`,
            `Protein Form` = "crude protein",
            Calculation,
            `Correction Factor  (%)` = dig * 100,
            `Limiting AA` = AA,
            fdcId,
            # `Lysine Variant` = NA_character_,  # not applicable in CP branch
            DIAAS = round(DIAAS, 2),
            calculation,
            `Food Composition Ref`,
            `Data Collection Source`
          ) %>%
          dplyr::distinct() %>%
          dplyr::mutate(`Correction Factor  (%)` = as.character(`Correction Factor  (%)`))

        # --- Combine both approaches (AA-specific has two variants per item) ---
        DIAAS <- dplyr::bind_rows(DIAAS_aa, DIAAS_cp)

        # --- Output toggle ---
        if (input$show_calc == "score_only") {
          DIAAS <- DIAAS %>% dplyr::select(-calculation)
        } else {
          DIAAS <- DIAAS %>% dplyr::select(-DIAAS) %>% dplyr::rename("DIAAS" = "calculation")
        }

        if (length(input$score) != 1) {
          PQ_df <- dplyr::full_join(PQ_df, DIAAS)
        } else {
          PQ_df <- DIAAS
        }
      }







      if ("crude protein" %in% debounced_filters()$analyte) {
        if (!("individual amino acids" %in% debounced_filters()$analyte)) {
          PQ_df <- PQ_df %>%
            filter(`Protein Form` == "crude protein")
        }
      }
      if ("individual amino acids" %in% debounced_filters()$analyte) {
        if (!("crude protein" %in% debounced_filters()$analyte)) {
          PQ_df <- PQ_df %>%
            filter(`Protein Form` != "crude protein")
        }
      }

      PQ_df <- PQ_df %>%
        arrange(Species,
                Calculation,
                `Sample Location`,
                `Protein Form`,
                Food) %>%
        filter((`Sample Location` %in% debounced_filters()$sample) |
                 (
                   `Sample Location` == "Not Available" &
                     input$require_bioavail == FALSE
                 )
        ) %>%
        filter(
          Calculation %in% debounced_filters()$measure |
            (
              Calculation == "Not Available" &
                input$require_bioavail == FALSE
            )
        ) %>%
        filter(
          Species %in% debounced_filters()$species |
            (
              Species == "Not Available" & input$require_bioavail == FALSE
            )
        ) %>%
        rename("Correction Factor Species" = "Species") %>%
        rename("Correction Factor Protein Form" = "Protein Form") %>%
        rename("Correction Factor Sample Location" = "Sample Location") %>%
        rename("Correction Factor Calculation" = "Calculation") %>%
        rename("Correction Factor Ref" = "Data Collection Source")  %>%
        mutate(across(where(is.character), ~ gsub("\n", "<br>", .))) %>%
        relocate(`Correction Factor Ref`, .after = last_col()) %>%
        relocate(`Food Composition Ref`, .after = last_col()) %>%
        mutate(
          `Limiting AA` = ifelse(
            `Limiting AA` == "methionine+cysteine",
            "methionine + cysteine",
            `Limiting AA`
          )
        ) %>%
        mutate(
          `Limiting AA` = ifelse(
            `Limiting AA` == "phenylalanine+tyrosine",
            "phenylalanine + tyrosine",
            `Limiting AA`
          )
        )


      if (input$search_tab1 != "") {
        PQ_df <- fuzzy_search_filter(PQ_df, input$search_tab1)

      }


      n_cols <- ncol(PQ_df)
      n_remaining <- n_cols - 6

      get_tooltip_label <- function(col_label) {
        tooltip <- col_meta$tooltip[match(col_label, col_meta$col)]
        tooltip <- ifelse(is.na(tooltip), "", tooltip)
        HTML(sprintf('<span title="%s">%s</span>', tooltip, col_label))
      }

      sketch <- withTags(table(class = 'display',
                               thead(
                                 tr(
                                   th(
                                     rowspan = 2,
                                     get_tooltip_label(colnames(PQ_df)[1])
                                   ),
                                   th(
                                     rowspan = 2,
                                     style = "border-right: 2px solid #bbb;",
                                     get_tooltip_label(colnames(PQ_df)[2])
                                   ),
                                   th(
                                     colspan = 5,
                                     style = "
          background-color: #f0f0f0;
          font-weight: bold;
          text-align: center;
          border-left: 2px solid #bbb;
          border-top: 2px solid #bbb;
          border-bottom: 2px solid #bbb;
          border-right: 2px solid #bbb;",
          get_tooltip_label("Correction Factor")
                                   ),
          lapply(8:n_cols, function(i)
            th(
              rowspan = 2,
              style = if (i == 8) "border-left: 2px solid #bbb;" else NULL,
              get_tooltip_label(colnames(PQ_df)[i])
            )
          )
                                 ),
          tr(
            lapply(c(
              "Species",
              "Sample Location",
              "Protein Form",
              "Calculation",
              "Correction Factor (%)"
            ), function(label)
              th(
                # style = "background-color: #fafafa;",
                get_tooltip_label(label)
              )
            )
          )
                               )
      ))





      DT::datatable(
        PQ_df %>% arrange(NI_ID, Food),
        class = "display cell-border compact",
        rownames = FALSE,
        extensions = c('FixedHeader'),
        container = sketch,
        options = list(
          fixedHeader = list(
            header = TRUE,
            headerOffset = 0
          ),
          dom = 'ltip',
          pageLength = 25,
          lengthMenu = c(15, 25, 50, 75, 100),
          order = list(
            list(2, 'asc'),
            list(3, 'asc'),
            list(4, 'asc'),
            list(5, 'asc')
          ),
          columnDefs = list(
            list(
              targets = c('Correction Factor Ref', 'Food Composition Ref'),
              render = JS(
                "function(data, type, row, meta) {",
                "return type === 'display' && data.length > 100 ?",
                "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
                "}"
              )
            )
          ),
          # Add this to help with header rendering
          autoWidth = FALSE,
          scrollX = FALSE
        ),
        escape = FALSE  # Allow HTML rendering
      ) %>%
        formatStyle(1:ncol(PQ_df),
                    'vertical-align' = 'top',
                    'overflow-wrap' = 'break-word') %>%
        formatStyle(1, width = '30px') %>%
        formatStyle(2, width = '120px') %>%
        formatStyle(3:6, width = '50px') %>%
        formatStyle(7, width = '90px') %>%
        formatStyle(8, width = '90px') %>%
        formatStyle(9:10, width = '40px') %>%
        formatStyle(11:ncol(PQ_df), width = '80px')
    }
  })

  # render table for food mappings
  output$table_3 <- DT::renderDataTable(server = TRUE, {
    fdcmp_df <- EAA_composition
    if (input$search_tab3 != "") {
      fdcmp_df <- fuzzy_search_filter(fdcmp_df, input$search_tab3)
    }


    fdcmp_df <- fdcmp_df %>%
      select(NI_ID,
             fdcId,
             `food description`,
             Protein,
             AA,
             value,
             `Food Composition Ref`) %>%
      mutate("FDC_ID" = fdcId) %>%
      select(NI_ID,
             FDC_ID,
             `food description`,
             Protein,
             AA,
             value,
             `Food Composition Ref`) %>%
      distinct() %>%
      mutate(AA = factor(
        AA,
        levels = c(
          "histidine",
          "isoleucine",
          "leucine",
          "lysine",
          "methionine+cysteine",
          "phenylalanine+tyrosine",
          "threonine",
          "tryptophan",
          "valine"
        ),
        labels = c(
          "His (g/100g)",
          "Ile (g/100g)",
          "Leu (g/100g)",
          "Lys (g/100g)",
          "Met+Cys (g/100g)",
          "Phe+Tyr (g/100g)",
          "Thr (g/100g)",
          "Trp (g/100g)",
          "Val (g/100g)"
        )
      )) %>%
      pivot_wider(names_from = "AA", values_from = "value") %>%
      rename("Protein (g/100g)" = "Protein") %>%
      rename("Food description" = "food description") %>%
      distinct() %>%
      relocate(`Food Composition Ref`, .after = last_col())
    if (input$show_NI_ID == FALSE) {
      fdcmp_df <- fdcmp_df %>%
        select(!NI_ID)
    }

    # Create sketch with tooltips
    sketch <- withTags(table(class = 'display',
                             thead(tr(
                               lapply(colnames(fdcmp_df), function(col) {
                                 tooltip <- col_meta$tooltip[match(col, col_meta$col)]
                                 tooltip <-
                                   ifelse(is.na(tooltip), "", tooltip)
                                 th(title = tooltip, col)  # preserve raw column name
                               })
                             ))))


    # Build the table
    DT::datatable(
      fdcmp_df %>% distinct(),
      container = sketch,
      extensions = c('Buttons', 'FixedHeader'),
      class = "display cell-border compact",
      rownames = FALSE,
      options = list(
        fixedHeader = TRUE,
        scrollCollapse = TRUE,
        dom = 'ltip',
        pageLength = 25,
        lengthMenu = c(15, 25, 50, 75, 100),
        columnDefs = list(list(
          targets = c(ncol(fdcmp_df) - 1),
          render = JS(
            "function(data, type, row, meta) {",
            "return type === 'display' && data.length > 100 ?",
            "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
            "}"
          )
        ))
      ),
      escape = FALSE  # Allow rendering of truncation in columnDefs
    ) %>%
      formatStyle(
        columns = 1:16,
        `vertical-align` = 'top',
        `overflow-wrap` = 'break-word'
      ) %>%
      formatStyle(columns = seq(ncol(fdcmp_df) - 10, ncol(fdcmp_df) - 1),
                  width = '5%') %>%
      formatStyle(columns = 1,
                  width = '5%')


  })



  # CSV Download Handler
  output$download_csv_PQ <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - Protein Quality Scoring -",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(data_PQ(), file, row.names = FALSE)
    }
  )

  output$download_csv_eaa <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - EAA Composition -",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(data_eaa(), file, row.names = FALSE)
    }
  )

  output$download_csv_bv <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - Correction Factors -",
            Sys.Date(),
            ".csv",
            sep = "")
    },
    content = function(file) {
      write.csv(data_bv(), file, row.names = FALSE)
    }
  )

  # Excel Download Handler
  output$download_excel_PQ <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - Protein Quality Scoring -",
            Sys.Date(),
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      write.xlsx(data_PQ(), file)
    }
  )

  output$download_excel_bv <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - Correction Factors -",
            Sys.Date(),
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      write.xlsx(data_bv(), file)
    }
  )

  output$download_excel_eaa <- downloadHandler(
    filename = function() {
      paste("Protein Quality Hub - EAA Composition -",
            Sys.Date(),
            ".xlsx",
            sep = "")
    },
    content = function(file) {
      write.xlsx(data_eaa(), file)
    }
  )

}

shinyApp(ui, server)
