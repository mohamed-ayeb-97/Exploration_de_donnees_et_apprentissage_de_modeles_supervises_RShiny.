library(shiny)
library(shinydashboard)
library(tidymodels)
library(skimr)
library(corrr)
library(shinyWidgets)
library(shinyBS)
library(ranger)
library(rpart)
library(vip)
library(kknn)

e = new.env()
e1 = new.env()

Sys.setlocale( locale='Chinese' )


source("function.R")

sidebar <- dashboardSidebar(
  sidebarMenu(
    tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 50px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
    id = "side_bar",
    menuItem("Introduction", tabName = "Introduction", icon = icon("house")),
    menuItem("Importation", icon = icon("cloud-upload"), tabName = "Importation"),
    menuItem("AED", tabName = "AED", icon = icon("chart-column")),
    menuItem("APD", icon = icon("sitemap"), tabName = "APD"),
    conditionalPanel(
      condition = "input.side_bar == 'Importation'",
      selectInput(inputId = "data_type", "selectionnez le type de Data",
                  choices = c("Choose"="", "Txt File", "Excel File", "Rdata File")),
      # Txt:
      conditionalPanel(
        condition = "input.data_type == 'Txt File'",
        
        fileInput(inputId = "file_txt", label = "Choisir un Fichier"),
        checkboxInput(inputId = "col_name", label = "Premiere ligne comme noms ?", value = TRUE),
        radioButtons(inputId = "delim", "selectionnez un Delim",
                     choices = c(Virgule = ",",
                                 "point virgule" = ";",
                                 Tab = "\t",
                                 Autre = "Other"),
                     selected = ","),
        conditionalPanel(
          condition = "input.delim == 'Other'",
          textInput("delim_Autre", label = "Delimator")
        ),
        radioButtons(inputId = "sep_dec", "Selectionnez ce qui separe le nombre decimal", 
                     choices = c(Comma = ",", Point = "."), selected = "."),
        checkboxInput("skip", label = "Ignorer les premieres lignes ?"),
        conditionalPanel(
          condition = "input.skip",
          textInput("skipNb", label = "Nomber de ligne", value = 0))
      ),
      # Excel :
      conditionalPanel(
        condition = "input.data_type == 'Excel File'",
        fileInput(inputId = "file_excel", label = "Choisir un Fichier"),
        checkboxInput("col_name", label = "Premiere ligne comme noms ?", value = TRUE),
        checkboxInput("skip1", label = "Ignorer les premieres lignes ?"),
        conditionalPanel(condition = "input.skip1",
          textInput("skipNb1", label = "Row Nomber", value = 0)),
        selectInput(inputId = "sheet_choice", label = "Choisir le feuille a importer ?", choices = c("Choose"="",NULL))
      ),
      # Rdata :
      conditionalPanel(
        condition = "input.data_type == 'Rdata File'",
        fileInput(inputId = "file_rdata", label = "Choisir un Fichier"),
        selectInput(inputId = "Rdata_choice", label = "Choisir les Data pour Importer", choices = c("Choose"="",NULL))
      ),         
      actionButton("button", "Show")  
    ),
    conditionalPanel("input.side_bar == 'AED'",
                     selectInput(inputId = "id_data", "Selectionnez la base a Analyser", choices = c("Choose "= "", NULL)),
                     selectInput(inputId = "graph_id", "Selectionnez le Type d'Analyse", choices = c("Choose" = "","Analyse Univarie", 
                                                                                       "Analyse Bi-Varie", "Etude de Correlation"))
    ),
    conditionalPanel("input.side_bar == 'APD'",
                     numericInput("id_split", label = "Choisir La proportion de donnees a trainer/tester",max = 1, min = 0, value = 0.8),
                    
                    column(2,htmlOutput("sd_split_but")),
                    actionButton("split_but", "Diviser les Data"),
                    verbatimTextOutput("text"),
                    br(),
                    
                    column(2,htmlOutput("sd_rec_but")),
                    actionButton("rec_but", "Appliquer la Recipe"),
                    br(),
                    
                    column(2,htmlOutput("sd_pred_but")),
                    actionButton("pred_but", "Entrainer les Modeles"),
                    br(),
                    
                    column(2,htmlOutput("sd_res_but")), 
                    actionButton("res_but", "reinitialiser")
                    
                  )
  ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Loading...",id="loadmessage"))
)

body <- dashboardBody(
  tabItems(
    tabItem(
            tags$head(
              tags$style(HTML('
              @keyframes fadeIn {
      from { opacity: 0; }
      to { opacity: 1; }
    }
    .fade-in {
      animation: fadeIn 2s ease-in;
    }
                h1 {
                font-size: 48px; /* adjust the font size as needed */
                font-family: "Helvetica Neue", Helvetica, Arial, sans-serif; /* change the font family as desired */
                font-weight: bold; /* make the text bold */
                text-transform: uppercase; /* make the text uppercase */
                letter-spacing: 2px; /* add some letter spacing */
                  }
                '))
            ),
            tabName = "Introduction",
            fluidRow(
              div(style = "display:flex; align-items:center;",class = "text-center",
                  column(6,
                         h1("Machine Learning Web App",class = "fade-in")),
                  column(6,
                         img(src="robot_2.gif", align = "right",height='400px',width='400px'))
              )
            ),
            h2("1. Utilite de l'application:"),
            h4("Cette application est destinee a l'importation des donnees, faire une analyse exploratoire des donnees et une analyse predictive a l'aide des algorithmes de machine learning. "),
            br(),
            h2("2. Qu'est-ce que le Machine Learning et comment fonctionne-t-il ?"),
            br(),
            h4("Le Machine Learning (ML) est le processus qui consiste a utiliser des modeles mathematiques de donnees pour aider un ordinateur a apprendre sans instruction directe."),
            h4("Il est considere comme un sous-ensemble d'intelligence artificielle. Le Machine Learning utilise des algorithmes pour identifier des patterns dans les donnees,"),
            h4("et ces patterns sont ensuite utilises pour creer un modele de donnees qui peut effectuer des predictions."),
            br(),
            h2("3. Types d'analyses qu'on peut effectuer dans cette application:"),
            br(),
            fluidRow(
              column(6,
                     icon("gears", "fa-3x")),
              column(6,
                     icon("sitemap", "fa-3x"))
            ),
            br(),
            fluidRow(
              column(6,            
                     h3("3.1 Analyse exploratoire des donnees"),
                     h4("(a) statistiques descriptives des variables"),
                     h4("(b) analyse unidimensionnelle des variables."),
                     h4("(c) analyse bidimensionnelle des variables.")
              ),
              column(6,
                     
                     h3("3.2 Analyse Predictive des donnees"),
                     h4("(a) Diviser les donnees"),
                     h4("(b) Pre-Process les donnees :"),
                     h5(" - Outliers"),
                     h5(" - Des valeurs manquantes"),
                     h5(" - Normalisation"),
                     h5(" - Dummification"),
                     h5(" - Desequilibre des classes"),
                     h4("(c) Entrainer les modeles"),
                     h4("(d) Evaluer les modeles")
                     )
            )

    ),
    
    tabItem(tabName = "Importation",
            h2("Importation et premier apercu des data"),
            tabsetPanel(
              tabPanel("Overview", value = "Overview",
                       DT::dataTableOutput(outputId = "tab")
              ),
              tabPanel("Processing", value = "Processing",
                       column(3,
                              selectInput(inputId = "tran", "Choisir une Transformation", choices = c("Choose"="","Conversion", "Renaming")),
                              
                              conditionalPanel("input.tran == 'Renaming'",
                                               textInput("new_name_id", "Enter the New Name Here : "))),
                       column(3,
                              selectInput(inputId = "variable", "Choisir une Variable", choices = c("Choose"="",NULL)),
                              br(),
                              conditionalPanel("input.tran == 'Renaming'",
                                               actionButton("button12", "Rename"))),
                       column(3,
                              selectInput(inputId = "to_class", "Choisir une Classe ", choices = c("Choose"="",
                                                                                            'as_integer',
                                                                                            'as_numeric',
                                                                                            'as_character',
                                                                                            'as_factor',
                                                                                            'as_mdy',
                                                                                            'as_dmy',
                                                                                            'as_ymd',
                                                                                            'as_ymd_hms',
                                                                                            'as_ymd_hm',
                                                                                            'as_mdy_hms',
                                                                                            'as_mdy_hm',
                                                                                            'as_dmy_hms',
                                                                                            'as_dmy_hm',
                                                                                            'as_hms',
                                                                                            'as_hm'
                              )),
                              conditionalPanel("input.tran == 'Conversion'",
                                               actionButton("button11", "Convert"))
                              
                              ),
                       br(),
                       
                       DT::dataTableOutput(outputId = "trans_tab"),
                       
                       tags$style(type="text/css",
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
                       
              ),
              tabPanel("Summary", value = "Summary",
                       fluidRow(
                         column(12,verbatimTextOutput(outputId = "sum")
                                )
                       )
                       
              )
              
            )
    ),
    tabItem(tabName = "AED",
            h2("Analyse Exploratoire"),
            br(),
            br(),
            h4("L'analyse exploratoire des donnees (AED) est utilisee pour analyser et identifier clairement les informations utiles que les donnees peuvent reveler, souvent en employant des methodes de visualisation des donnees."), 
            h4("Elle permet d'avoir une meilleure comprehension des variables d'un ensemble de donnees ainsi que des relations qui existent entre elles. Ainsi, decouvrir plus facilement les modeles (patterns) et identifier des anomalies"),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            fluidRow(
              column(1, icon("arrow-left", "fa-3x")
                     ),
              column(3, h4( "Choississez le type d'analyse a faire"))
            ),
            br(),
            br(),
            conditionalPanel("input.graph_id == 'Analyse Univarie'",
                             fluidRow(
                               column(6,
                                      selectInput("uni_var1", "Choisir une Variable", choices = c("Choose"="",NULL)),
                                      plotOutput("uni_plt1"),
                                 
                               ),
                               column(6,
                                      selectInput("uni_var2", "Choisir une Variable", choices = c("Choose"="",NULL)),
                                      plotOutput("uni_plt2"),
                                      
                               )),
                               fluidRow(
                                 
                               column(6,
                                      selectInput("uni_var3", "Choisir une Variable", choices = c("Choose"="",NULL)),
                                      plotOutput("uni_plt3"),
                                      
                               ),
                               column(6,
                                      selectInput("uni_var4", "Choisir une Variable", choices = c("Choose"="",NULL)),
                                      plotOutput("uni_plt4"),
                                      
                               ))
            ),
            conditionalPanel("input.graph_id == 'Analyse Bi-Varie'",
                             fluidRow(
                               column(6,
                                      fluidRow(
                                        column(3,
                                               selectInput("bi_var11", "Choisir une Variable", choices = c("Choose"="",NULL))),
                                        column(3,
                                               selectInput("bi_var12", "Choisir une Variable", choices = c("Choose"="",NULL))
                                        )),
                                      plotOutput("bi_plt1"),
                                      
                               ),
                               column(6,
                                      fluidRow(
                                        column(3,
                                               selectInput("bi_var21", "Choisir une Variable", choices = c("Choose"="",NULL))),
                                        column(3,
                                               selectInput("bi_var22", "Choisir une Variable", choices = c("Choose"="",NULL))
                                               )),
                                      plotOutput("bi_plt2"),
                                      )),
                             fluidRow(
                               column(6,
                                      fluidRow(
                                        column(3,
                                               selectInput("bi_var31", "Choisir une Variable", choices = c("Choose"="",NULL))),
                                        column(3,
                                               selectInput("bi_var32", "Choisir une Variable", choices = c("Choose"="",NULL))
                                               )),
                                      plotOutput("bi_plt3"),
                                      ),
                               column(6,
                                      fluidRow(
                                        column(3,
                                               selectInput("bi_var41", "Choisir une Variable", choices = c("Choose"="",NULL))
                                               ),
                                        column(3,
                                               selectInput("bi_var42", "Choisir une Variable", choices = c("Choose"="",NULL)),
                                               )
                                      ),
                                      plotOutput("bi_plt4"),
                               ))
                             ),
            conditionalPanel("input.graph_id == 'Etude de Correlation'",
                            DT::dataTableOutput("tab_corr"),
                            br(),
                            br(),
                            br(),
                            fluidRow(
                              column(6,
                                plotOutput("plt_corr1")
                              )))
           ),
    tabItem(tabName = "APD",
            h2("Analyse Predictive"),
            br(),
            h4("L'analyse predictive est un type d'analyse de donnees qui utilise les statistiques, la Data Science, le machine learning et d'autres techniques pour predire ce qui va se passer dans le futur. "),
            h4("Elle consiste a repondre a la question : qu'est-ce qui a le plus de chances de se produire a l'avenir sur la base des tendances historiques ?"),
            br(),
            br(),
            tabsetPanel(
              tabPanel("Preparation", value = "prep",
                       br(),
                       br(),    
                       fluidRow(
                         column(3,
                                varSelectInput("pred_y", label = "Choissir la variables a expliquer", data = NULL, selected = NULL)
                         ),
                         column(3,
                                varSelectInput("pred_x", label = "Choissir les variables explicatives", data = NULL, selected = NULL, multiple = TRUE),
                         )),
                       conditionalPanel("input.split_but >= 1",
                                        fluidRow(
                                          column(3, h4( "Suivez cet ordre en ajoutant des Steps")),
                                          column(1, icon("arrow-right", "fa-2x"))
                                        ),
                                        fluidRow(
                                          
                                          column(3,
                                                 checkboxInput("id_nor", "Add a step normalize?"),htmlOutput("Sd_nor"),
                                                 varSelectInput("id_nor_var", "Choisir les variables continues pour normaliser",
                                                                data = NULL, selected = NULL, multiple = TRUE)),
                                          column(3,
                                                 checkboxInput("id_log", "Add a step log?"),htmlOutput("Sd_log"),
                                                 varSelectInput("id_log_var", "Choisir les variables continues pour appliquer la transformation log",
                                                                data = NULL, selected = NULL, multiple = TRUE)),
                                          column(3,
                                                 checkboxInput("id_boxcox", "Add a step boxcox?"),htmlOutput("Sd_box"),
                                                 varSelectInput("id_boxcox_var", "Choisir les variables continues pour appliquer la transformation Boxcox", 
                                                                data = NULL, selected = NULL, multiple = TRUE)),
                                          
                                          column(3,
                                                 checkboxInput("id_mean", "Add a step mean ?"),htmlOutput("Sd_mean"),
                                                 varSelectInput("id_mean_var", "Choisir les variables continues avec NA", 
                                                                data = NULL, selected = NULL, multiple = TRUE))

                                          ),
                                          fluidRow(
                                            column(3,
                                                   checkboxInput("id_median", "Add a step median ?"),htmlOutput("Sd_median"),
                                                   varSelectInput("id_median_var", "Choisir les variables continues avec NA", 
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_mode", "Add a step mode ?"),htmlOutput("Sd_mode"),
                                                   varSelectInput("id_mode_var", "Choisir les variables categorielles avec NA", 
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_smote", "Add a step smote?"),htmlOutput("Sd_smote"),
                                                   varSelectInput("id_smote_var", "Choisir les variables categorielles avec les classes desequilibres",
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_down", "Add a step downsample?"),htmlOutput("Sd_down"),
                                                   varSelectInput("id_down_var", "Choisir les variables categorielles avec les classes desequilibres", 
                                                                  data = NULL, selected = NULL, multiple = TRUE))

                                            
                                          ),
                                          fluidRow(
                                            column(3,
                                                   checkboxInput("id_other", "Add a step other?"),htmlOutput("Sd_other"),
                                                   varSelectInput("id_other_var", "Choisir les variables categorielles", 
                                                                  data = NULL, selected = NULL, multiple = TRUE),
                                                   numericInput("id_th2", "Donnez une seuil", max = 1, min = 0, value = 0.1)),
                                          
                                            column(3,
                                                   checkboxInput("id_up", "Add a step upsample?"),htmlOutput("Sd_up"),
                                                   varSelectInput("id_up_var", "Choisir les variables categorielles avec les classes desequilibres", 
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_na", "Add a step naomit ?"),htmlOutput("Sd_na"),
                                                   varSelectInput("id_na_var", "Choisir les variables avec NA",
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_unknown", "Add a step unknown?"),htmlOutput("Sd_unknown"),
                                                   varSelectInput("id_unknown_var", "Choisir les variables categorielles avec NA", 
                                                                  data = NULL, selected = NULL, multiple = TRUE))
                                            
                                          ),
                                          fluidRow(
                                            column(3,
                                                   checkboxInput("id_dummy", "Add a step dummy?"),htmlOutput("Sd_dummy"),
                                                   varSelectInput("id_dummy_var", "Choisir les variables categorielles pour dummifier",
                                                                  data = NULL, selected = NULL, multiple = TRUE)),
                                            column(3,
                                                   checkboxInput("id_corr", "Add a step corr ?"),htmlOutput("Sd_corr"),
                                                   numericInput("id_th", "Donnez une seuil", max = 1, min = 0, value = 0.8)),
                                            column(3,
                                                   htmlOutput("Sd_zv"),
                                                   checkboxInput("id_zv", "Add a step zv ?"))
                                        )
                       ),
                       verbatimTextOutput("txt"),
                       
                       verbatimTextOutput("texte"),
                       verbatimTextOutput("texte4"),
                       br(),
                       br(),
                       DT::dataTableOutput("tableee")
                       
                
              ),
              tabPanel("Evaluation" , value = "eval",
                       DT::dataTableOutput("res_tab"),
                       fluidRow(
                         column(6,DT::dataTableOutput("m_tab1")),
                         column(6,DT::dataTableOutput("m_tab2"))
                       ),
                       fluidRow(
                         column(6,DT::dataTableOutput("m_tab3")),
                         column(6,DT::dataTableOutput("m_tab4"))
                       ),
                       fluidRow(
                         column(6, plotOutput("res_plt")),
                         column(6, plotOutput("vip_plt"))
                       )
                       
                       ),
              tabPanel("Final Prediction", value = "final_pred",
                       br(),
                       br(),
                       fluidRow(
                         column(4,
                                selectInput("mod_id", "Selectionnez le Meilleur Modele", choices = c("Choose" ="",NULL)), offset = 1
                         )),
                       fluidRow(
                         column(6,
                                DT::dataTableOutput("f_res"))
                       )
                       )
              
            )
            
            )
    
    
    
    
    
    
           )
    
)

# Put them together into a dashboardPage

ui <-dashboardPage(
  dashboardHeader(title = "Machine Learning Rocks"),
  sidebar,
  body
)

server <- function(input, output, session){
  

  data0 <- reactiveValues(df = NULL)
  file_name <- reactiveValues(name = NULL)
  
  df_list <- reactive({
    purrr::map(purrr::set_names(readxl::excel_sheets(input$file_excel$datapath)),
               readxl::read_excel, path = input$file_excel$datapath)
  })

  
  ##### Importation :
  
  ## Excel :
  
  observeEvent(input$file_excel, {
    
    input$file_excel
    
    lapply(names(df_list()), function(x)
      assign(x, df_list()[[x]], envir = e))
    
    listes = ls(name = df_list(), e)
    
    updateSelectInput(session = session, inputId = "sheet_choice", choices = c("Choose"="",listes))
    
  })
  
  donneesExcel <- reactive({
    if (is.null(input$file_excel)) return (NULL)
    don = NULL
    try({
      don = readxl::read_excel(
        input$file_excel$datapath,
        sheet = input$sheet_choice,
        skip = as.integer(input$skipNb1),
        col_names = input$col_name)
    })
    don
  })
  
  # Txt :
  
  donneesTXT <- reactive({
    if (is.null(input$file_txt)) return (NULL)
    else {
      if (input$delim == 'Other') sep = input$delim_Autre
      else sep = input$delim
      don = NULL
      try({
        don = read.table(
          input$file_txt$datapath,
          header = input$col_name,
          sep = sep,
          dec = input$sep_dec,
          skip = input$skipNb,
          stringsAsFactors = FALSE)
      }, silent = TRUE)
      don
      
    }
  })

  # Rdata:
  
  observeEvent(input$file_rdata, {
    
    input$file_rdata
    
    load(input$file_rdata$datapath, envir = e1)
    
    liste = ls(e1)
    
    updateSelectInput(session = session, inputId = "Rdata_choice", choices = c("Choose"="",liste))
  })
  
  donneesRDATA <- reactive({
    don = NULL
    try({
      don = get(input$Rdata_choice, envir = e1)
    }, silent = TRUE)
    don
  })
  
  ################################## Importation
  
  ## Import Data
  observeEvent(input$button,{
    
    if (input$data_type == "Txt File") {
      data0$df <- donneesTXT()
      file_name$name <- input$file_txt$name
    } else if (input$data_type == "Excel File"){
      data0$df <- donneesExcel()
      file_name$name <- input$file_excel$name
    } else {
      if (input$data_type == "Rdata File") {
        data0$df <- donneesRDATA()
        file_name$name <-  input$file_rdata$name
      }
    }
    updateSelectInput(session, "id_data", choices = c(file_name$name))
   
  })
  
  ######### ================ Affichage 
  shiny::observe({
    updateSelectInput(session, "variable",
                      choices = c("Choose"="",names(data0$df)))
  })
  
  output$tab <- DT::renderDataTable({
    DT::datatable(data0$df,
                  options = list(
                    scrollX = TRUE
                  ))
  })
  
  #### ======================== Conversion
  output$trans_tab <- DT::renderDataTable({
    custom_glimpse(data0$df)
  }) 

  observeEvent(input$button11, {
    variable <- data0$df[[input$variable]]
    to_class <- input$to_class
    if (to_class == "as_numeric") {
      data0$df[[input$variable]] <- as.numeric(variable)
    } else if (to_class == "as_integer") {
      data0$df[[input$variable]] <- as.integer(variable)
    } else if (to_class == "as_character") {
      data0$df[[input$variable]] <- as.character(variable)
    } else if (to_class == "as_factor") {
      data0$df[[input$variable]] <- as.factor(variable)
    } else if (to_class == "as_mdy") {
      data0$df[[input$variable]] <- as_mdy(variable)
    } else if (to_class == "as_dmy") {
      data0$df[[input$variable]] <- as_dmy(variable)
    } else if (to_class == "as_ymd") {
      data0$df[[input$variable]] <- as_ymd(variable)
    } else if (to_class == "as_ymd_hms") {
      data0$df[[input$variable]] <- as_ymd_hms(variable)
    } else if (to_class == "as_ymd_hm") {
      data0$df[[input$variable]] <- as_ymd_hm(variable)
    } else if (to_class == "as_mdy_hms") {
      data0$df[[input$variable]] <- as_mdy_hms(variable)
    } else if (to_class == "as_mdy_hm") {
      data0$df[[input$variable]] <- as_mdy_hm(variable)
    } else if (to_class == "as_dmy_hms") {
      data0$df[[input$variable]] <- as_dmy_hms(variable)
    } else if (to_class == "as_dmy_hm") {
      data0$df[[input$variable]] <- as_dmy_hm(variable)
    } else if (to_class == "as_hms") {
      data0$df[[input$variable]] <- as_hms(variable)
    } else if (to_class == "as_hm") {
      data0$df[[input$variable]] <- as_hm(variable)
    }
  })
  
  ######## ================ Renaming
  observeEvent(input$button12, {
    req(input$new_name_id)
    
    variable <- input$variable
    
    names(data0$df)[names(data0$df) == variable ] <- input$new_name_id

  })
  
  #### ======================= Summary
  output$sum <-  renderPrint({
    req(data0$df)
    skim(data0$df)
  })
  
  ################################################# AED
  
  observeEvent(input$id_data,{
    if (is.null(data_prep$df)){
      updateSelectInput(session, "uni_var1",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "uni_var2",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "uni_var3",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "uni_var4",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var11",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var12",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var21",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var22",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var31",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var32",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var41",
                        choices = c("Choose"="",names(data0$df)))
      updateSelectInput(session, "bi_var42",
                        choices = c("Choose"="",names(data0$df)))
      
      
      ## univarie
      
      
      observeEvent(input$uni_var1,{
        output$uni_plt1 <- renderPlot({
          graph_uni(data0$df,input$uni_var1)
        }) 
      })
      
      observeEvent(input$uni_var2,{
        output$uni_plt2 <- renderPlot({
          graph_uni(data0$df,input$uni_var2)
        }) 
      })
      
      observeEvent(input$uni_var3,{
        output$uni_plt3 <- renderPlot({
          graph_uni(data0$df,input$uni_var3)
        }) 
      })
      
      observeEvent(input$uni_var4,{
        output$uni_plt4 <- renderPlot({
          graph_uni(data0$df,input$uni_var4)
        }) 
      })
      
      ## bi-varie
      
      observeEvent(c(input$bi_var11,
                     input$bi_var12),
                   {
                     output$bi_plt1 <- renderPlot({
                       graph_bi(data0$df,input$bi_var11, input$bi_var12)
                     }) 
                   })
      
      observeEvent(c(input$bi_var21,
                     input$bi_var22),
                   {
                     output$bi_plt2 <- renderPlot({
                       graph_bi(data0$df,input$bi_var21, input$bi_var22)
                     }) 
                   })
      
      observeEvent(c(input$bi_var31, 
                     input$bi_var32),
                   {
                     output$bi_plt3 <- renderPlot({
                       graph_bi(data0$df,input$bi_var31, input$bi_var32)
                     }) 
                   })
      
      observeEvent(c(input$bi_var41, 
                     input$bi_var42),
                   {
                     output$bi_plt4 <- renderPlot({
                       graph_bi(data0$df,input$bi_var41, input$bi_var42)
                     }) 
                   })
      
      ## correlation
      
      df_corr <- reactive({
        req(data0$df)
        data0$df%>%
          dplyr::select_if(is.numeric)%>%
          cor() 
      })
      
      output$plt_corr1 <- renderPlot({
        req(data0$df)
        df_corr()%>%
          ggcorrplot::ggcorrplot()
      })
      
      output$tab_corr <-DT::renderDataTable({
        df_corr()%>%
          data.frame()
      })
    }else {
      if(input$id_data == file_name$name){
        updateSelectInput(session, "uni_var1",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "uni_var2",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "uni_var3",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "uni_var4",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var11",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var12",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var21",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var22",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var31",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var32",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var41",
                          choices = c("Choose"="",names(data0$df)))
        updateSelectInput(session, "bi_var42",
                          choices = c("Choose"="",names(data0$df)))
        
        
        ## univarie
        
        
        observeEvent(input$uni_var1,{
          output$uni_plt1 <- renderPlot({
            graph_uni(data0$df,input$uni_var1)
          }) 
        })
        
        observeEvent(input$uni_var2,{
          output$uni_plt2 <- renderPlot({
            graph_uni(data0$df,input$uni_var2)
          }) 
        })
        
        observeEvent(input$uni_var3,{
          output$uni_plt3 <- renderPlot({
            graph_uni(data0$df,input$uni_var3)
          }) 
        })
        
        observeEvent(input$uni_var4,{
          output$uni_plt4 <- renderPlot({
            graph_uni(data0$df,input$uni_var4)
          }) 
        })
        
        ## bi-varie
        
        observeEvent(c(input$bi_var11,
                       input$bi_var12),
                     {
                       output$bi_plt1 <- renderPlot({
                         graph_bi(data0$df,input$bi_var11, input$bi_var12)
                       }) 
                     })
        
        observeEvent(c(input$bi_var21,
                       input$bi_var22),
                     {
                       output$bi_plt2 <- renderPlot({
                         graph_bi(data0$df,input$bi_var21, input$bi_var22)
                       }) 
                     })
        
        observeEvent(c(input$bi_var31, 
                       input$bi_var32),
                     {
                       output$bi_plt3 <- renderPlot({
                         graph_bi(data0$df,input$bi_var31, input$bi_var32)
                       }) 
                     })
        
        observeEvent(c(input$bi_var41, 
                       input$bi_var42),
                     {
                       output$bi_plt4 <- renderPlot({
                         graph_bi(data0$df,input$bi_var41, input$bi_var42)
                       }) 
                     })
        
        ## correlation
        
        df_corr <- reactive({
          req(data0$df)
          data0$df%>%
            dplyr::select_if(is.numeric)%>%
            cor() 
        })
        
        output$plt_corr1 <- renderPlot({
          req(data0$df)
          df_corr()%>%
            ggcorrplot::ggcorrplot()
        })
        
        output$tab_corr <-DT::renderDataTable({
          df_corr()%>%
            data.frame()
        })
      } else if (input$id_data == "Training Set"){
        
        updateSelectInput(session, "uni_var1",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "uni_var2",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "uni_var3",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "uni_var4",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var11",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var12",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var21",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var22",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var31",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var32",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var41",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        updateSelectInput(session, "bi_var42",
                          choices = c("Choose"="",names(juice(data_prep$df))))
        
        
        
        ## univarie
        
        
        observeEvent(input$uni_var1,{
          output$uni_plt1 <- renderPlot({
            graph_uni(juice(data_prep$df),input$uni_var1)
          }) 
        })
        
        observeEvent(input$uni_var2,{
          output$uni_plt2 <- renderPlot({
            graph_uni(juice(data_prep$df),input$uni_var2)
          }) 
        })
        
        observeEvent(input$uni_var3,{
          output$uni_plt3 <- renderPlot({
            graph_uni(juice(data_prep$df),input$uni_var3)
          }) 
        })
        
        observeEvent(input$uni_var4,{
          output$uni_plt4 <- renderPlot({
            graph_uni(juice(data_prep$df),input$uni_var4)
          }) 
        })
        
        ## bi-varie
        
        observeEvent(c(input$bi_var11,
                       input$bi_var12),
                     {
                       output$bi_plt1 <- renderPlot({
                         graph_bi(juice(data_prep$df),input$bi_var11, input$bi_var12)
                       }) 
                     })
        
        observeEvent(c(input$bi_var21,
                       input$bi_var22),
                     {
                       output$bi_plt2 <- renderPlot({
                         graph_bi(juice(data_prep$df),input$bi_var21, input$bi_var22)
                       }) 
                     })
        
        observeEvent(c(input$bi_var31, 
                       input$bi_var32),
                     {
                       output$bi_plt3 <- renderPlot({
                         graph_bi(juice(data_prep$df),input$bi_var31, input$bi_var32)
                       }) 
                     })
        
        observeEvent(c(input$bi_var41, 
                       input$bi_var42),
                     {
                       output$bi_plt4 <- renderPlot({
                         graph_bi(juice(data_prep$df),input$bi_var41, input$bi_var42)
                       }) 
                     })
        
        ## correlation
        
        df_corr <- reactive({
          req(juice(data_prep$df))
          data0$df%>%
            dplyr::select_if(is.numeric)%>%
            cor() 
        })
        
        output$plt_corr1 <- renderPlot({
          req(juice(data_prep$df))
          df_corr()%>%
            ggcorrplot::ggcorrplot()
        })
        
        output$tab_corr <-DT::renderDataTable({
          df_corr()%>%
            data.frame()
        })
      }
    }
    
  })

      
     

  
  
  ################################################# APD
  
  observeEvent(input$button,{
    
    updateVarSelectInput(session, "pred_y", data = data0$df)
    updateVarSelectInput(session, "pred_x", data = data0$df)
    updateVarSelectInput(session, "id_mean_var", data = data0$df)
    updateVarSelectInput(session, "id_nor_var",data = data0$df)
    updateVarSelectInput(session, "id_dummy_var", data = data0$df)
    updateVarSelectInput(session, "id_smote_var", data = data0$df)
    updateVarSelectInput(session, "id_median_var", data = data0$df)
    updateVarSelectInput(session, "id_mode_var", data = data0$df)
    updateVarSelectInput(session, "id_up_var", data = data0$df)
    updateVarSelectInput(session, "id_down_var", data = data0$df)
    updateVarSelectInput(session, "id_na_var", data = data0$df)
    updateVarSelectInput(session, "id_log_var", data = data0$df)
    updateVarSelectInput(session, "id_boxcox_var", data = data0$df)
    updateVarSelectInput(session, "id_unknown_var", data = data0$df)
    updateVarSelectInput(session, "id_other_var", data = data0$df)
    
  })
  
  output$Sd_corr <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_corr will potentially remove variables that have large absolute correlations with other variables."
      )
    )
  })
  
  output$Sd_median <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "Step_medianimpute will substitute missing values of numeric variables by the median of those variables.")
      )
    
  })
  
  output$Sd_mean <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "Step_impute_mean will substitute missing values of numeric variables by the mean of those variables.")
        )
  })
  
  output$Sd_mode <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_impute_mode will substitute missing values of nominal variables by the mode of those variables."
      )
    )
  })
  
  output$Sd_nor <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_normalize will normalize numeric data to have a standard deviation of one and a mean of zero."
      )
    )
  })
  
  output$Sd_dummy <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_dummy will convert nominal data (e.g. character or factors) into one or more numeric binary model terms for the levels of the original data."
      )
    )
  })
  
  output$Sd_smote <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "Step_smote will generate new examples of the minority class using KNN.",

      )
    )
  })
  
  output$Sd_up <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
              "Step_upsample will replicate rows of a data set to make the classes Balanced.")
    )
  })
  
  output$Sd_down <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
              "Step_downsample will remove rows of a data set to make the classes Balanced."
      )
    )
  })
  
  output$Sd_na <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_naomit will remove observations (rows of data) if they contain NA or NaN values."
      )
    )
  })
  
  output$Sd_zv <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_zv will remove variables that contain only a single value (zero variance)"
      )
    )
  })
  
  output$Sd_log <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_log will log transform data."
      )
    )
  })
  
  output$Sd_box <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_BoxCox will transform data using a simple Box-Cox transformation."
      )
    )
  })
  
  output$Sd_unknown <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_unknown will assign a missing value in a factor level to unknown. "
      )
    )
  })
  
  output$Sd_other <- renderUI({
    tags$span(
      tipify(
        icon("circle-question"),
        "step_other will potentially pool infrequently occurring values into an extra category labeled other "
      )
    )
  })
  
  output$sd_split_but <- renderUI({
    tags$span(
      tipify(
        icon("1"),
        "Diviser les donnees en un ensemble le Testing Set"
      )
    )
  })
  

  output$sd_rec_but <- renderUI({
    tags$span(
      tipify(
        icon("2"),
        " Apres avoir applique cette transformation, vous pouvez retourner a AED et verifier le resultat de votre transformation"
      )
    )
  })
  
  output$sd_pred_but <- renderUI({
    tags$span(
      tipify(
        icon("3"),
        " Appliquer les modeles de classification sur le Training Set"
      )
    )
  })
  
  output$sd_res_but <- renderUI({
    tags$span(
      tipify(
        icon("exclamation"),
        " Apres la reinitialisation , Veuillez repeter tout les etapes ci-dessus"
      )
    )
  })
  

  ##################################################### Machinie Learning
  
  data1 <- reactiveValues(rec = NULL)
  data_train <- reactiveValues(df = NULL)
  data_test <- reactiveValues(df = NULL)
  data_prep <- reactiveValues(df = NULL)
  
  df1 <- reactiveValues(df_knn = NULL)
  df2 <- reactiveValues(df_rf = NULL)
  df3 <- reactiveValues(df_tree = NULL)
  df4 <- reactiveValues(df_logreg = NULL)
  
  ####### Data split
  
  observeEvent(input$res_but,{
    
    data1$rec <- NULL
    data_prep$df <- NULL
    
    df1$df_knn    <- NULL
    df2$df_rf     <- NULL
    df3$df_tree   <- NULL
    df4$df_logreg <- NULL
    
    
    output$res_plt  <-  renderPlot({
      NULL
    }) 
    
    output$vip_plt  <- renderPlot({
      NULL
    }) 
    
    updateCheckboxInput(session, "id_corr", value = FALSE)
    updateCheckboxInput(session, "id_mean", value = FALSE)
    updateCheckboxInput(session, "id_median", value = FALSE)
    updateCheckboxInput(session, "id_mode", value = FALSE)
    updateCheckboxInput(session, "id_nor", value = FALSE)
    updateCheckboxInput(session, "id_dummy", value = FALSE)
    updateCheckboxInput(session, "id_smote", value = FALSE)
    updateCheckboxInput(session, "id_down", value = FALSE)
    updateCheckboxInput(session, "id_up", value = FALSE)
    updateCheckboxInput(session, "id_na", value = FALSE)
    updateCheckboxInput(session, "id_log", value = FALSE)
    updateCheckboxInput(session, "id_boxcox", value = FALSE)
    updateCheckboxInput(session, "id_unknown", value = FALSE)
    updateCheckboxInput(session, "id_zv", value = FALSE)
    updateCheckboxInput(session, "id_other", value = FALSE)
    
  })
  
  observeEvent(input$split_but,{
    
    req(data0$df)
    req(input$pred_y)
    req(input$pred_x)
    
    data_split <- initial_split(data0$df, strata = input$pred_y, prop = input$id_split)
    
    output$text <- renderPrint({
      data_split
    })
    
    data_train$df <- training(data_split)
    
    data_test$df <- testing(data_split)
    
    data1$rec <- recipe(as.formula(paste(input$pred_y, " ~ ", paste(input$pred_x, collapse = "+"))), data = data0$df)
    
    updateSelectInput(session, "id_data", choices = c(file_name$name, "Training Set"))
    
  })
  
  shiny::observe({
    output$txt <- renderPrint({
      data1$rec
    })
  })

  ###### Recette
  
  observeEvent(input$id_corr,{
    if (input$id_corr == TRUE){
      data1$rec <- data1$rec%>%
        step_corr(all_numeric_predictors(), threshold = input$id_th)
    }
  })
  
  observeEvent(input$id_mean, {
    if(input$id_mean == TRUE){
      data1$rec <- data1$rec%>%
        step_impute_mean(!!!input$id_mean_var)
    }
  })
  
  observeEvent(input$id_median,{
    if(input$id_mean == TRUE){
      data1$rec <- data1$rec%>%
        step_impute_median(!!!input$id_median_var)
    }
  })
  
  observeEvent(input$id_mode,{
    if(input$id_mode == TRUE){
      data1$rec <- data1$rec%>%
        step_impute_mode(!!!input$id_mode_var)
    }
  })
  
  observeEvent(input$id_nor,{
    if(input$id_nor == TRUE){
      data1$rec <- data1$rec%>%
        step_normalize(!!!input$id_nor_var)
    }
  })
  
  observeEvent(input$id_dummy,{
    if(input$id_dummy == TRUE){
      data1$rec <- data1$rec%>%
        step_dummy(!!!input$id_dummy_var, one_hot = TRUE)
    }
  })
  
  observeEvent(input$id_other,{
    if(input$id_other == TRUE){
      data1$rec <- data1$rec%>%
        step_other(!!!input$id_other_var, threshold = input$id_th2, other = "other values")
    }
  })
  
  observeEvent(input$id_smote,{
    req(input$pred_y)
    req(input$pred_x)
    req(input$id_smote)
    req(data1$rec)
    if((input$id_smote) == TRUE & (!is.null(input$id_smote))){
      l1 <- length(input$id_smote)
      for (x in 1:l1) {
        data1$rec <- data1$rec%>%
          themis::step_smote(!!input$id_smote_var[[x]], skip = TRUE)
      }
    }
  })
  
  observeEvent(input$id_down,{
    req(input$pred_y)
    req(input$pred_x)
    req(input$id_down)
    req(data1$rec)
    if((input$id_down == TRUE) & (!is.null(input$id_down))){
      l2 <- length(input$id_down)
      for (i in 1:l2) {
        data1$rec <- data1$rec%>%
          themis::step_downsample(!!input$id_down_var[[i]])
      }
    }
  })
  
  observeEvent(input$id_up,{
    req(input$pred_y)
    req(input$pred_x)
    req(input$id_up)
    req(data1$rec)
    if((input$id_up == TRUE) & (!is.null(input$id_up))){
      l3 <- length(input$id_up)
      for (y in 1:l3) {
        data1$rec <- data1$rec%>%
          themis::step_upsample(!!input$id_up_var[[y]])        
      }

    }
  })
  
  observeEvent(input$id_na,{
    if(input$id_na==TRUE){
      data1$rec <- data1$rec%>%
        step_naomit(!!!input$id_na_var)
    }
  })
  
  observeEvent(input$id_log,{
    if(input$id_log==TRUE){
      data1$rec <- data1$rec%>%
        step_log(!!!input$id_log_var)
    }
  })
  
  observeEvent(input$id_boxcox,{
    if(input$id_boxcox == TRUE){
      data1$rec <- data1$rec%>%
        step_BoxCox(!!!input$id_boxcox_var)
    }
  })
  
  observeEvent(input$id_unknown,{
    if(input$id_unknown == TRUE){
      data1$rec <- data1$rec%>%
        step_unknown(!!!input$id_unknown_var, new_level = "unknown")
    }
  })
  
  observeEvent(input$id_zv,{
    if(input$id_zv == TRUE){
      data1$rec <- data1$rec%>%
        step_zv(all_predictors())
    }
  })
  
  observeEvent(input$rec_but,{
    req(data1$rec)
    
    data_prep$df <- data1$rec%>%
      prep()
    
    output$txt <- renderPrint({
      data_prep$df
    })
    
    output$tableee <- DT::renderDataTable({
      
      juice(data_prep$df)
      
      })
    
    
    
  })
  
  ################################## Application de modele

  ##create a workflow
  
  ## Train Logistic Regression :
  glm_spec <- logistic_reg()%>%
    set_engine("glm")
  
  knn_spec <- nearest_neighbor()%>%
    set_engine("kknn")%>%
    set_mode("classification")
  
  ## Train a decision tree :
  
  tree_spec <- decision_tree()%>%
    set_engine("rpart")%>%
    set_mode("classification")
  
  ## Train Random Forest :
  
  rf_spec <- rand_forest(trees = 500)%>%
    set_engine("ranger")%>%
    set_mode("classification")
  
  observeEvent(input$pred_but,{
    req(data1$rec)
    
    if(class(data0$df[[input$pred_y]]) == "numeric") {
      showModal(modalDialog(
        title = "Important message",
        "veuillez selectionner une variable categorielle pour predire!",
        easyClose = TRUE
      ))
    } else if(class(data0$df[[input$pred_y]]) == "factor"){
      
      
    projet_wf <-workflow()%>%
      add_recipe(data1$rec)
    
    ## Model evaluation with resampling :
    
    set.seed(123)
    
    folds <- vfold_cv(data_train$df, v = 5)
    
    output$texte <- renderPrint({
      folds
    })
    
    #1: Logistic Reg:
    c <- as.numeric(length(levels(data0$df[[input$pred_y]])))

    if( c == 2){
      
      set.seed(1234)
      
      glm_rs <- projet_wf %>%
        add_model(glm_spec)%>%
        fit_resamples(
          resamples = folds,
          metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::precision, yardstick::recall, yardstick::f_meas),
          control = control_resamples(save_pred = TRUE)
        )
    }

    knn_rs <- projet_wf %>%
      add_model(knn_spec)%>%
      fit_resamples(
        resamples = folds,
        metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::precision, yardstick::recall, yardstick::f_meas),
        control = control_resamples(save_pred = TRUE)
      )
  
    #2: Random Forest:
    set.seed(1234)
    
    rf_rs <- projet_wf %>%
      add_model(rf_spec)%>%
      fit_resamples(
        resamples = folds,
        metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::precision, yardstick::recall, yardstick::f_meas),
        control = control_resamples(save_pred = TRUE)
      )
    
    #4: decision trees:
    set.seed(1234)
    
    tree_rs <- projet_wf %>%
      add_model(tree_spec)%>%
      fit_resamples(
        resamples = folds,
        metrics = metric_set(yardstick::roc_auc, yardstick::accuracy, yardstick::precision, yardstick::recall, yardstick::f_meas),
        control = control_resamples(save_pred = TRUE)
      )
    

    
    output$m_tab1 <- DT::renderDataTable({
      DT::datatable(tree_rs%>%conf_mat_resampled(),
                    caption = "Matrice de confusion d'Arbre de Decision")
    })
    
    output$m_tab2 <- DT::renderDataTable({
      DT::datatable(rf_rs%>%conf_mat_resampled(),
                    caption = "Matrice de confusion de Foret Aleatoire")
    })
    
    output$m_tab3 <- DT::renderDataTable({
      DT::datatable(knn_rs%>%conf_mat_resampled(),
                caption = "Matrice de confusion de KNN")
    })
    
    if (c == 2){
      output$m_tab4 <- DT::renderDataTable({
        DT::datatable(glm_rs%>%conf_mat_resampled(),
                      caption = "Matrice de confusion de Regression Logistique")
      })
    }
    
    a <- tree_rs%>% unnest(.predictions)%>%names()
    
    roc_var <- a[5]
    
    ## Evaluation des modeles : 
    
    output$res_tab <-  DT::renderDataTable({
      
      df1$df_knn <- knn_rs%>%
        collect_metrics()%>%
        select(.metric, mean)%>%
        rename('KNN' = 'mean')%>%
        rename('scores' = '.metric')
      
      df2$df_rf <- rf_rs%>% 
        collect_metrics()%>%
        select(.metric, mean)%>%
        rename('rf' = 'mean')%>%
        rename('scores' = '.metric')
      
      df3$df_tree <- tree_rs%>% 
        collect_metrics()%>%
        select(.metric,mean)%>%
        rename('Tree' = 'mean')%>%
        rename('scores' = '.metric')
      
      if (c == 2)  {
        
        df4$df_logreg <- glm_rs%>% 
          collect_metrics()%>%
          select(.metric, mean)%>%
          rename('glm' = 'mean')%>%
          rename('scores' = '.metric')
      }
      

    if(c == 2){
      merge(merge(df1$df_knn, df2$df_rf, by = c("scores")), merge(df3$df_tree, df4$df_logreg, by = c("scores")), by = c("scores"))
    } else {
      merge(merge(df1$df_knn, df2$df_rf, by = c("scores")), df3$df_tree, by = c("scores"))
      
    }
    

    })

    ## ROC_Curve :
    
    if(c == 2){

            output$res_plt <- renderPlot({
        
            knn_rs%>%
              unnest(.predictions) %>%
              mutate(model= "KNN ") %>%
              bind_rows(glm_rs%>%
                          unnest(.predictions)%>%
                          mutate(model= "glm"))%>%
              bind_rows(rf_rs%>%
                          unnest(.predictions)%>%
                          mutate(model= "Foret aleatoire"))%>%
              bind_rows(tree_rs%>%
                          unnest(.predictions)%>%
                          mutate(model= "Arbres de decision"))%>%
              group_by(model)%>%
              roc_curve(!!input$pred_y, as.factor(roc_var))%>%
              autoplot()
      })
      
    
    
        ## importances des variables:
        
        output$vip_plt <- renderPlot({
          
          data_split <- initial_split(data0$df, strata = input$pred_y, prop = input$id_split)
          
          randf_spec <- rand_forest(trees = 500)%>%
            set_engine("ranger",importance= "permutation")%>%
            set_mode("classification")
          
          workflow() %>% 
            add_model(randf_spec) %>% 
            add_recipe(data1$rec)%>%
            last_fit(data_split)%>%
            extract_fit_parsnip() %>% 
            vip(geom = "point")
          
        })
        
        if(c == 2){
          updateSelectInput(session, "mod_id", choices = c("Choose" = "", "Regression Logistique", "Foret Aleatoire",
                                                           "KNN", "Arbre de Decision"))
        } else {
          updateSelectInput(session, "mod_id", choices = c("Choose" = "", "Foret Aleatoire", "KNN", "Arbre de Decision"))
          
        }
        
        ## Predecting on test data:
        
        observeEvent(input$mod_id,{
          
          data_split <- initial_split(data0$df, strata = input$pred_y, prop = input$id_split)
          
          req(input$mod_id)
          
          if(input$mod_id == "Regression Logistique"){
            data_final <- projet_wf %>%
              add_model(glm_spec)%>%
              last_fit(data_split)
          } else if (input$mod_id == "KNN"){
            data_final <- projet_wf %>%
              add_model(knn_spec)%>%
              last_fit(data_split)
          } else if (input$mod_id == "Foret Aleatoire"){
            data_final <- projet_wf %>%
              add_model(rf_spec)%>%
              last_fit(data_split)
          } else if (input$mod_id == "Arbre de Decision"){
            data_final <- projet_wf %>%
              add_model(tree_spec)%>%
              last_fit(data_split)
          }
          
          output$f_res <- DT::renderDataTable({
            collect_metrics(data_final)
          })
          
          
        })
        
  }
        
        

}
  
    })
  
}

shinyApp(ui, server)




