#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(repos = c(CRAN = "https://cloud.r-project.org"))
# Vérifie si les packages nécessaires sont installés et les ajoute si besoin

required_packages <- c("shiny", "shinyjs", "shinyFiles", "shinydashboard", "shinythemes", "mvtnorm","tidyverse", 
                       "doFuture", "doRNG", "readxl","sas7bdat", "miceadds", "dplyr", "reticulate", "future.apply", 
                       "rlang","sf", "rockchalk", "shinyWidgets")
for (i in required_packages) {
  if (!require(i, character.only = TRUE)) {
    install.packages(i, dependencies = TRUE)
    require(i, character.only = TRUE)
  }
}

# Installe ces packages stockés dans le dossier de la shiny (non disponibles sur le CRAN)
# Model_install <- c("TarifQC","Billonage", "SimulateurSaMARE", "ExtractMap")
# for (i in Model_install) {
#   if (!require(i, character.only = TRUE)) {
#     if (i == "TarifQC") {
#       install.packages(
#         "./Packages/TarifQC_1.1.6.tar.gz",
#         repos = NULL,
#         type = "source",
#         dependencies = TRUE
#       )
#     }
#     
#     if (i == "Billonage") {
#       install.packages(
#         "./Packages/Billonage_0.1.0.tar.gz",
#         repos = NULL,
#         type = "source",
#         dependencies = TRUE
#       )
#     }
#     if (i == "SimulateurSaMARE") {
#       install.packages(
#         "./Packages/SimulateurSaMARE_2.3.0.tar.gz",
#         repos = NULL,
#         type = "source",
#         dependencies = TRUE
#       )
#     }
#     if (i == "ExtractMap") {
#       install.packages(
#         "./Packages/ExtractMap_1.3.0.tar.gz",
#         repos = NULL,
#         type = "source",
#         dependencies = TRUE
#       )
#     }
#     require(i, character.only = TRUE)
#   }
# }



library(shiny)
library(readr)
library(shinyjs)
library(SimulateurSaMARE)
library(Billonage)
library(ExtractMap)
library(sf)
library(shinyWidgets)
library(OutilsDRF)
# Define UI for application that draws a histogram

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #00FF00; 
      }
    "))
  ), 
  tags$style(HTML("
    .btn {
      background-color: #008000;
      border-color: #008000;
      color: white;
    }
    .well {
        background-color: #d8f3dc; 
    }
    .btn:hover {
      background-color: darkgreen;
    }
    .progress-bar {
      background-color: #008000;
    }
    input[type='radio']:checked {
      background-color: #008000;
      border-color: #008000;
    }
    input[type='radio'] {
      appearance: none;
      -webkit-appearance: none;
      -moz-appearance: none;
      width: 1em;
      height: 1em;
      border: 0.15em solid black;
      border-radius: 50%;
      outline: none;
      cursor: pointer;
      position: relative;
      transition: all 0.2s ease-in-out;
    }
    input[type='radio']:checked::before {
      content: '';
      display: block;
      width: 60%;
      height: 60%;
      background: #008000;
      border-radius: 50%;
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
    }
    .navbar-default {
      background-color: #d3f4d7; /* vert pâle */
      border-color: #d3f4d7;
    }
    .navbar-default .navbar-nav > li > a {
      color: #000; /* couleur du texte */
    }
    .navbar-default .navbar-brand {
      color: #000; /* couleur du texte */
    }
  ")),
  navbarPage(
    "",
    id = "all_pages",
    tabPanel(
      img(src = "SaMARE_MRNF_Final.jpg", height = 50, width = 70), # logo de Samare
      div(
        id = "main_app",
        tags$div(style = "font-size: 10px;", "SaMARE 2.3.3"),
        sidebarLayout(
          uiOutput("dynamic_sidebar"),
          mainPanel(
            uiOutput("dynamic_mainpanel")
          )
        ),
        div(
          # bouton pour fermer l'application
          tags$button(
            id = 'close',
            type = "button",
            class = "btn action-button",
            icon("close"),
            onclick = "setTimeout(function(){window.close();},500);",
            style = "color: #fff; background-color: #008000; border-color: #2e6da4; hover: black",
            "Fermer l'application"
          ),
          style = "margin-top: 20px;"
        )
      )
    ),
    # page d'aide
    tabPanel(
      img(src = "Aide2.webp", height = 50, width = 70),
      div(
        tags$div(
          tags$style(HTML("
            .content-wrapper {
              background: url('https://yourimageurl.com/image.png') no-repeat center center fixed;
              background-size: cover;
              color: white;
              padding: 20px;
              font-family: Arial, sans-serif;
            }
            .box {
              background-color: (0, 0, 0, 0.7);
              padding: 20px;
              border-radius: 10px;
            }
            .box h2 {
              font-size: 36px;
            }
            .box p {
              font-size: 18px;
            }
          ")),
          fluidRow(
            column(width = 12, 
                   div(class = "box",
                       h2("À Propos de SaMARE"),
                       p("Le simulateur de croissance SaMARE est un modèle de croissance à l’échelle de l’arbre. Il est adapté pour les peuplements feuillus et mixtes de la forêt tempérée du Québec. Il prévoit l’accroissement en diamètre, la probabilité de mortalité et l’évolution de la vigueur des arbres de plus de 9,0 cm de diamètre ainsi que le recrutement de nouveaux arbres au seuil de 9,1 cm."),
                       p("Des informations sur le modèle SaMARE ou son utilisation sont disponibles dans le document ci-dessous."),
                       downloadButton("downloadPDF", "Téléchargez le fichier d'aide.")
                   ),
                   div(class = "box",
                     id = "help_page",
                     h2("Fichiers d'exemple"),
                     p("Pour voir la structure des fichiers d'inventaire et essayer une simulation, vous pouvez utiliser le fichier ci-dessous."),
                     downloadButton("downloadPDFtest", "Téléchargez le fichier des arbres."),
                     downloadButton("downloadPDFtestgaules", "Téléchargez le fichier des gaules"),
                   ),
                   div(class = "box",
                       h2("Contactez-nous"),
                       p("Pour plus d'informations, contactez-nous :"),
                       p("recherche.forestiere@mrnf.gouv.qc.ca")
                   )
            )
          )
        ),
        
       
      )
    )
  )
)



# Définir le serveur
server <- function(input, output,session) {
  
  stopSimulation <- function() {
    system("taskkill /f /im R.exe")
    system("taskkill /f /im Rterm.exe")
  }
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
  output$dynamic_sidebar <- renderUI({
     #pannel principale 
    
    sidebarPanel(
     
      
      fileInput( 
      inputId="file",
      label="Données d'inventaire de départ (format csv)",
      multiple = FALSE,
      accept = NULL,
      width = NULL,
      buttonLabel = "Parcourir",
      placeholder = "Aucun fichier sélectionné",
      capture = NULL),
      
      htmlOutput("statut"),
      htmlOutput("error"),
      htmlOutput("statut_data"),
      htmlOutput("error_data"),
      numericInput("Horizon", "Nombre d'années de simulation (multiple de 5)",min = 5 ,value = NA, step = 5),
      radioButtons("RecruesGaules", "Recrutement basé sur les gaules pour les érablières",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0,
                   inline = TRUE),
      conditionalPanel(
        condition = "input.RecruesGaules == 1",
       
        fileInput( 
          inputId="gauleFile",
          label="Dénombrement des gaules (format csv)",
          multiple = FALSE,
          accept = NULL,
          width = NULL,
          buttonLabel = "Parcourir",
          placeholder = "Aucun fichier sélectionné",
          capture = NULL),
        htmlOutput("statut_data_gaules"),
        htmlOutput("error_data_gaules"),
        htmlOutput("statut_data_gaules_vall"),
        htmlOutput("error_data_gaules_vall"),
        htmlOutput("statut_data_gaules_plac"),
        htmlOutput("error_data_gaules_plac")
      ),
    
      numericInput("NbIter", "Nombre d'itérations", min = 2, value = NA, step = 2),
      radioButtons("corticale", "Maladie corticale du hêtre",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0,
                   inline = TRUE),
      
      uiOutput("conditionalButton")
    )
  
  })
  # Lire le fichier arbres avec un point-virgule comme délimiteur
  data <- reactive({
    req(input$file)
   # read.csv(input$file$datapath, sep  = ";")
    read.delim(input$file$datapath, sep   = ";")
  })
  
  # Lire le fichier des gaules avec un point-virgule comme délimiteur
  gaule_data <- reactive({
    req(input$gauleFile)
    # read.csv(input$gauleFile$datapath, sep  = ";")
    read.delim(input$gauleFile$datapath, sep = ";")
    })
  
  
  simulation_status <- reactiveVal("Attente de la simulation...")
  
  #variable reactive 
  processed_Simul <- reactiveVal(NULL)
  processed_data_ArbreSamare <- reactiveVal(NULL)
  processed_data_DendroSamare <- reactiveVal(NULL)
  processed_data_DendroIterSamare <- reactiveVal(NULL)
  processed_data_htvol <- reactiveVal(NULL)
  processed_absent <- reactiveVal(NULL)
  processed_absent_gaules <- reactiveVal(NULL)
  valider_data <- reactiveVal(NULL)
  valider_data_gaules <- reactiveVal(NULL)
  valider_placette_gaules <- reactiveVal(NULL)
  processed_Billonage <- reactiveVal(NULL)
  graph_to_print <- reactiveVal(NULL)
  graph<- reactiveVal(NULL)
  vevifier_meteo<- reactiveVal(NULL)
  placette <- reactiveVal(NULL)
  isFile <- reactiveValues(flag = FALSE)
  show_grade2 <- reactiveVal(TRUE)
  show_grade3 <- reactiveVal(FALSE)
  donnees_graphique <- reactiveVal(NULL)
  
  # Fonction pour afficher les erreurs
  afficher_erreurs <- function(erreurs, statut_output, erreur_output,emplacement ) {
    if (length(erreurs) != 0) {
      list_string <- paste(erreurs, collapse = ', ')
      output[[statut_output]] <- renderText({
        HTML("<b>Les erreurs suivantes ont été détectées </b><b>", emplacement, "</b>:")
      })
      output[[erreur_output]] <- renderText({
        HTML(paste("<span style='color:red;'>", list_string, "</span>"))
      })
    } else {
      output[[statut_output]] <- renderText({ HTML("") })
      output[[erreur_output]] <- renderText({ HTML("") })
    }
  }
  
  # Fonction pour afficher le bouton de simulation
  afficher_bouton_simulation <- function() {
    output$conditionalButton <- renderUI({
      if (!is.null(input$file) && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1) {
        actionButton("simulateButton", "Simulation")
      }
    })
  }
  
  
  observeEvent(input$file, {
    output$dynamic_mainpanel <- renderUI({ tableOutput("table") })
    isFile$flag<-TRUE
    validate_files()
  })
  
  # Observateur pour l'événement input$gauleFile
  observeEvent(input$gauleFile, {
   
      validate_files()
   
  })
  
  
  
  
  validate_files <- function() {
    if (is.null(input$file)) {
      afficher_erreurs("Le fichier data est manquant.", "statut_data_gaules", "error_data_gaules","")
      return()
    }
    processed_absent(trouver_noms_absents(data()))
    valider_data(valide_data(data()))
    
    afficher_erreurs(processed_absent(), "statut_data", "error_data", "dans le fichier des arbres")
    afficher_erreurs(valider_data(), "statut", "error", "dans le fichier des arbres")
    
    if(input$RecruesGaules ==1 && !is.null(input$gauleFile)){
      
      processed_absent_gaules(trouver_noms_absents_gaules(gaule_data()))
      valider_data_gaules (valide_data_gaules(gaule_data()))
      valider_placette_gaules(valide_placette_gaules(data_arbre = data(), data_gaules = gaule_data()))
      
      afficher_erreurs(processed_absent_gaules(), "statut_data_gaules", "error_data_gaules", "dans le fichier des gaules")
      afficher_erreurs(valider_data_gaules(), "statut_data_gaules_vall", "error_data_gaules_vall", "dans le fichier des gaules")
      afficher_erreurs(valider_placette_gaules(), "statut_data_gaules_plac", "error_data_gaules_plac","dans le fichier des gaules")
      
     # print(length(processed_absent_gaules()))
      # print('valide gaules', length(valider_data_gaules()))
      # print('valide placette gaules', length(valider_placette_gaules()))
    }
    
    
  }
  
  

  
  observe({
    # Réagir aux changements dans les inputs
    input$RecruesGaules
    input$Horizon
    input$NbIter
   
    output$conditionalButton <- renderUI({
      if (!is.null(input$file)  && !is.na(input$Horizon)&& input$Horizon >=5 && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==0 && length(processed_absent()) == 0 && length(valider_data()) == 0 && isFile$flag && input$Horizon %% 5 ==0 ) {
        #placette(unique(data()$Placette))
        actionButton("simulateButton", "Simulation")
       
      }else if(!is.null(input$file)  && !is.na(input$ Horizon) && input$Horizon >=5 && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==1 && !is.null(input$gauleFile) && length(processed_absent()) == 0 && length(valider_data()) == 0 && length(processed_absent_gaules()) == 0 && length(valider_data_gaules()) == 0 && length(valider_placette_gaules()) == 0 && input$Horizon %% 5 ==0 ){
        #placette(unique(data()$Placette))
        actionButton("simulateButton", "Simulation")
       
        
        
      }
    })
  })
  

  
  observeEvent(input$simulateButton, {
    isFile$flag<-FALSE
    
    simulation_status("Simulation en cours...")
   
    vevifier_meteo(vevifier_variable_meteo(data()))
    output$dynamic_sidebar <- renderUI({
    sidebarPanel(
      h3("Simulation en cours..."),
    )
    })


    withProgress(message = 'Simulation en cours...', value = 0, {
      if (input$RecruesGaules == 1) {
        result <- SimulSaMARE(NbIter=input$NbIter, Horizon= (input$Horizon/5),
                              RecruesGaules= input$RecruesGaules, Data = vevifier_meteo(), 
                              Gaules=gaule_data(), MCH = input$corticale)
      } else {
        result <- SimulSaMARE(NbIter=input$NbIter, Horizon= (input$Horizon/5),
                              RecruesGaules= input$RecruesGaules, Data = vevifier_meteo(), 
                              MCH = input$corticale)
      }
      
      placette(unique(result$Placette))
      Sys.sleep(5)
      simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")
    })
    
    # SAUVEGARDER : Données de base pour graphiques (ne changent jamais)
    donnees_graphique(result)           # ← POUR LES GRAPHIQUES UNIQUEMENT
   
  
    processed_data_htvol(result)
    processed_Simul(result) 
    
    observeEvent(input$add_grade2, {
      show_grade2(TRUE)
    })
    
    observeEvent(input$remove_grade2, {
      show_grade2(FALSE)
      show_grade3(FALSE)  # Si on supprime Grade 2, supprimer aussi Grade 3
      
      # Réinitialiser les valeurs
      updateTextInput(session, "nom_grade2", value = "")
      updateSelectInput(session, "long_grade2", selected = "")
      updateNumericInput(session, "diam_grade2", value = NA)
      
      updateTextInput(session, "nom_grade3", value = "")
      updateSelectInput(session, "long_grade3", selected = "")
      updateNumericInput(session, "diam_grade3", value = NA)
    })
    
    observeEvent(input$add_grade3, {
      if (show_grade2()) {
        show_grade3(TRUE)
      }
    })
    
    observeEvent(input$remove_grade3, {
      show_grade3(FALSE)
      
      # Réinitialiser les valeurs du Grade 3
      updateTextInput(session, "nom_grade3", value = "")
      updateSelectInput(session, "long_grade3", selected = "")
      updateNumericInput(session, "diam_grade3", value = NA)
    })
    
    
    output$dynamic_sidebar <- renderUI({
      tags$head(
        tags$style(HTML("
        .shiny-input-radiogroup .radio {
          display: inline-block;
          margin-right: 20px;
        }
      "))
      )
      sidebarPanel(
        
        textOutput("simulationStatus"),
        tags$br(),
        
        switchInput(
          inputId = "affichegraph",
          label = "Afficher le graphique",         
          onLabel = "oui",
          offLabel = "non",
          value = FALSE
        ),
        
      
        conditionalPanel(
          condition = "input.affichegraph == true",
          
          radioButtons("graphType", "Type de graphiques à créer:",
                       choices = list("Évolution des variables par placette",
                                      "Distribution diamétrale moyenne")),
          tags$br(),
          
          selectInput("Espece", "Groupe d'espèces", choices = c("")),
          
        
          conditionalPanel(
            condition = "input.graphType == 'Évolution des variables par placette'",
            selectInput("variable", "Choix de la variable", 
                        choices = c("ST_HA", "Vol_HA", "DQM", "HDomM", "nbTi_HA"))
          ),
          
          pickerInput(
            inputId = "placette", 
            label = "Choix des placettes", 
            choices = placette(), 
            selected = placette(),
            multiple = TRUE,
            options = list(
              `actions-box` = TRUE,
              `deselect-all-text` = "Tout supprimer",
              `select-all-text` = "Tout sélectionner",
              `none-selected-text` = "Rien de sélectionné"
            )
          ),
          
          actionButton("valider", "Valider votre choix"),
          tags$br(),
          tags$br(),
          downloadButton("downloadPlot", "Télécharger le Graphique")
        ),
        tags$br(),
        tags$br(),
        radioButtons("simplifier", "Toutes les années de simulation",
                     choices = list("Oui" = FALSE, "Non" = TRUE),
                     selected = FALSE,
                     inline = TRUE),
        tags$br(),
        selectInput("Sortie",
                    label = "Choix de la sortie",
                    choices = c("Arbre par itération" = "arbre_par_iteration", "Sommaire dendrométrique par itération" = "dendro_par_iteration", "Sommaire dendrométrique par placette" = "dendro_par_placette", "À l'échelle du billon"="echelle_billon", "Sommaire classes DHP" = "Sommaire_Classes_DHP", "test"="test"),
                    selected = "arbre_par_iteration"
      ),  
      tags$br(),
      
      conditionalPanel(
        condition = "input.Sortie == 'echelle_billon'", 
        selectInput("typeBillonnage", "Type de Billonnage :",
                     choices = list("DHP_Régionalisé" = "DHP", "ABCD_Régionalisé" = "ABCD", "1234_Provincial" = "1234", "MSCR_Provincial" = "MSCR","DHP_Provincial" = "DHP2015", "ABCD_Provincial" = "ABCD2015"),
                    selected = "DHP"
                    )
      ),
      conditionalPanel(
        condition = "input.Sortie == 'echelle_billon'",
        h5("Paramètres Sybille", style = "color: #856404; font-weight: bold; margin-top: 15px;"),
        # DHS parameter
        div(
          style = "margin-bottom: 10px;",
          numericInput("dhs_input",
                       label = "DHS (Diamètre à hauteur de souche):",
                       value = 0.15,
                       min = 0.01,
                       max = 1.0,
                       step = 0.01)
        ),
        # Grade 1 parameters
        div(
          style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
          h6("Grade 1", style = "color: #495057; font-weight: bold;"),
          textInput("nom_grade1", "Nom du grade 1:", value = "sciage court"),
          selectInput("long_grade1", "Longueur (pieds):",
                      choices = c("Indéfini", "4", "8", "12"),
                      selected = "8"),
          numericInput("diam_grade1", "Diamètre au fin bout(cm):",
                       value = 20, min = 0, max = 100, step = 0.1)
        ),
        # Grade 2 parameters
        # Bouton pour ajouter Grade 2 (si pas affiché)
        uiOutput("add_grade2_button"),
        
        # Grade 2 parameters (affiché conditionnellement)
        uiOutput("grade2_section"),
        
        # Bouton pour ajouter Grade 3 (seulement si Grade 2 affiché)
        uiOutput("add_grade3_button"),
        
        # Grade 3 parameters (affiché conditionnellement)
        uiOutput("grade3_section"),
        div(
          style = "margin-top: 15px; text-align: center;",
          actionButton("calculer_billonnage",
                       "Calculer le billonnage",
                       style = "background-color: #008000; color: white; width: 100%;",
                       icon = icon("calculator"))
        )
      ),
      downloadButton("download", "Télécharger le résultat", 
                     style = "background-color: #008000; color: white; border-color: #3c8dbc;"),
      tags$br(),
      tags$br(),
      actionButton("reset_button", "Réinitialiser"),
        
      )
    
    })
    
    output$add_grade2_button <- renderUI({
      if (!show_grade2()) {
        div(
          style = "text-align: center; margin-bottom: 15px; padding: 10px; border: 2px dashed #008000; border-radius: 5px; background-color: #f0f9ff;",
          actionButton("add_grade2",
                       "Ajouter Grade 2",
                       style = "background-color: #008000; color: white; border: none; padding: 8px 20px; border-radius: 20px;",
                       icon = icon("plus-circle"))
        )
      }
    })
    
    # Section du Grade 2
    output$grade2_section <- renderUI({
      if (show_grade2()) {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px; position: relative; border-left: 4px solid #008000; animation: fadeIn 0.3s ease-in;",
          
          tags$style(HTML("
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(-10px); }
          to { opacity: 1; transform: translateY(0); }
        }
      ")),
          
          # Bouton X pour supprimer le Grade 2
          div(
            style = "position: absolute; top: 10px; right: 10px;",
            actionButton("remove_grade2",
                         "",
                         style = "background-color: #dc3545; color: white; border: none; padding: 4px 8px; border-radius: 50%; font-size: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
                         icon = icon("times"),
                         title = "Supprimer le Grade 2 (et Grade 3 si présent)")
          ),
          
          h6("Grade 2", style = "color: #008000; font-weight: bold; margin-right: 40px;"),
          p("(Optionnel)", style = "font-size: 0.8em; color: #6c757d; margin: 0 0 15px 0;"),
          
          textInput("nom_grade2", "Nom du grade 2:", value = "pate"),
          selectInput("long_grade2", "Longueur (pieds):",
                      choices = c("-- Aucune --", "Indéfini", "4", "8", "12"), selected = "4"),
          numericInput("diam_grade2", "Diamètre au fin bout(cm):",
                       value = 8, min = 0, max = 100, step = 0.1)
        )
      }
    })
    
    # Bouton pour ajouter le Grade 3 (seulement si Grade 2 existe)
    output$add_grade3_button <- renderUI({
      if (show_grade2() && !show_grade3()) {
        div(
          style = "text-align: center; margin-bottom: 15px; padding: 10px; border: 2px dashed #008000; border-radius: 5px; background-color: #f0f9ff;",
          actionButton("add_grade3",
                       "Ajouter Grade 3",
                       style = "background-color: #008000; color: white; border: none; padding: 8px 20px; border-radius: 20px;",
                       icon = icon("plus-circle"))
        )
      }
    })
    
    # Section du Grade 3
    output$grade3_section <- renderUI({
      if (show_grade3()) {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px; position: relative; border-left: 4px solid #008000; animation: fadeIn 0.3s ease-in;",
          
          # Bouton X stylisé
          div(
            style = "position: absolute; top: 10px; right: 10px;",
            actionButton("remove_grade3",
                         "",
                         style = "background-color: #dc3545; color: white; border: none; padding: 4px 8px; border-radius: 50%; font-size: 12px; box-shadow: 0 2px 4px rgba(0,0,0,0.2);",
                         icon = icon("times"),
                         title = "Supprimer le Grade 3")
          ),
          
          h6("Grade 3", style = "color: #008000; font-weight: bold; margin-right: 40px;"),
          p("(Optionnel)", style = "font-size: 0.8em; color: #6c757d; margin: 0 0 15px 0;"),
          
          textInput("nom_grade3", "Nom du grade 3:", value = ""),
          selectInput("long_grade3", "Longueur (pieds):",
                      choices = c("-- Aucune --", "Indéfini", "4", "8", "12"),
                      selected = "-- Aucune --"),
          numericInput("diam_grade3", "Diamètre au fin bout(cm):",
                       value = NA, min = 0, max = 100, step = 0.1)
        )
      }
    })

    output$dynamic_mainpanel <- renderUI({
      # Vérifier que input$affichegraph existe et n'est pas NULL
      if(!is.null(input$affichegraph) && input$affichegraph == TRUE){
        tabsetPanel(type = "tabs",
                    tabPanel("Graphique", plotOutput("plot"))
        )
      }else{
        div("")
      }
    })

    simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")
    
  })
  
  # Fonction pour mettre à jour le graphique
  mettre_a_jour_graphique <- function() {
    if(input$affichegraph == T){
      
      if(is.null(donnees_graphique())) {
        showNotification("Effectuez d'abord une simulation", type = "warning", duration = 3)
        return()
      }
      
      # Vérifications des paramètres
      if(is.null(input$Espece) || input$Espece == "") {
        showNotification("Sélectionnez une espèce", type = "warning", duration = 3)
        return()
      }
      
      if(is.null(input$variable)) {
        showNotification("Sélectionnez une variable", type = "warning", duration = 3)
        return()
      }
      
      if(is.null(input$placette) || length(input$placette) == 0) {
        showNotification("Sélectionnez au moins une placette", type = "warning", duration = 3)
        return()
      }
      
      tryCatch({
        # Utiliser la fonction Graph() modifiée avec les vrais noms de colonnes
        result <- Graph(SimulHtVol = donnees_graphique(),
                        Espece = input$Espece, 
                        Variable = input$variable, 
                        listePlacette = input$placette)
        
        graph(result)
        
      }, error = function(e) {
        cat("ERREUR graphique:", e$message, "\n")
        showNotification(paste("Erreur lors de la génération du graphique:", substr(e$message, 1, 80)), 
                         type = "error", duration = 5)
      })
    }
  }
  
  observeEvent(input$affichegraph, {
    mettre_a_jour_graphique()
  })
 
  observeEvent(input$Espece, {
    mettre_a_jour_graphique()
  })


  observeEvent(input$variable, {
    mettre_a_jour_graphique()
  })


  observeEvent(input$valider, {
    mettre_a_jour_graphique()
  })

  
  
  output$plot <- renderPlot({
   
    
    graphiques <- graph()
    
   
    if(input$graphType == "Évolution des variables par placette") {
      graph_to_print (graphiques[[1]])
    } else {
      graph_to_print(graphiques[[2]])
    }
    
   
    print(graph_to_print())
  })
  
  observe({
    listeEspece <-unique(processed_data_htvol()$GrEspece)
    listeEspece2 <- append("TOT",listeEspece)
    updateSelectInput( session = session,
                       inputId = "Espece",
                       label = "Groupe d'espèces",
                       choices = listeEspece2 )
  })
  
  
  observeEvent(input$reset_button, {
    # Afficher la boîte de dialogue modale pour confirmation
    showModal(modalDialog(
      title = "Confirmation",
      "Êtes-vous sûr de vouloir réinitialiser ?",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_reset", "Confirmer", class = "btn-primary")
      )
    ))
  })
  
  
  
  observeEvent(input$confirm_reset, {
    
    processed_Simul(NULL)
    processed_data_ArbreSamare(NULL)
    processed_data_DendroSamare(NULL)
    processed_data_DendroIterSamare(NULL)
    processed_data_htvol(NULL)
    processed_absent(NULL)
    processed_absent_gaules(NULL)
    valider_data(NULL)
    valider_data_gaules(NULL)
    processed_Billonage(NULL)
    graph_to_print(NULL)
    graph(NULL)
    vevifier_meteo(NULL)
    donnees_graphique(NULL)
    removeModal()
    
    #updateFileInput(session, "file", value = NULL)
    updateNumericInput(session, "Horizon", value = NA)
    updateRadioButtons(session, "RecruesGaules", selected = 0)
    #updateFileInput(session, "gauleFile", value = NULL)
    updateNumericInput(session, "NbIter", value = NA)
    updateRadioButtons(session, "corticale", selected = 0)
    updateSelectInput(session, "variable", selected = NULL)
    updatePickerInput(session, "placette", selected = NULL)
    
    # updateFileInput(session, "file", label = "Données d'inventaire de départ (format csv)", placeholder = "Aucun fichier sélectionné")
    # updateFileInput(session, "gauleFile", label = "Dénombrement des gaules (format csv)", placeholder = "Aucun fichier sélectionné")
    
    
    output$dynamic_mainpanel <- renderUI({
     NULL
    })
    
    
    output$dynamic_sidebar <- renderUI({
      
      sidebarPanel(
        
        fileInput( 
          inputId="file",
          label="Données d'inventaire de départ (format csv)",
          multiple = FALSE,
          accept = NULL,
          width = NULL,
          buttonLabel = "Parcourir",
          placeholder = "Aucun fichier sélectionné",
          capture = NULL),
        
        htmlOutput("statut"),
        htmlOutput("error"),
        htmlOutput("statut_data"),
        htmlOutput("error_data"),
        numericInput("Horizon", "Nombre d'années de simulation (multiple de 5)",min = 5 ,value = NA, step = 5),
        radioButtons("RecruesGaules", "Recrutement basé sur les gaules pour les érablières",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0,
                     inline = TRUE),
        conditionalPanel(
          condition = "input.RecruesGaules == 1",
          
          fileInput( 
            inputId="gauleFile",
            label="Dénombrement des gaules (format csv)",
            multiple = FALSE,
            accept = NULL,
            width = NULL,
            buttonLabel = "Parcourir",
            placeholder = "Aucun fichier sélectionné",
            capture = NULL),
          htmlOutput("statut_data_gaules"),
          htmlOutput("error_data_gaules"),
          htmlOutput("statut_data_gaules_vall"),
          htmlOutput("error_data_gaules_vall"),
          htmlOutput("statut_data_gaules_plac"),
          htmlOutput("error_data_gaules_plac")
        ),
        numericInput("NbIter", "Nombre d'itérations", min = 2, value = NA, step = 2),
        radioButtons("corticale", "Maladie corticale du hêtre",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0,
                     inline = TRUE),
        
        uiOutput("conditionalButton")
      )
    
      
    })
    
    
    
    
  })
  
  observeEvent(input$close, {
    # Afficher la boîte de dialogue modale pour confirmation
    showModal(modalDialog(
      title = "Confirmation",
      "Êtes-vous sûr de vouloir fermer l'application ?",
      footer = tagList(
        modalButton("Annuler"),
        actionButton("confirm_close", "Confirmer", class = "btn-primary")
      )
    ))
  })
  
  observeEvent(input$confirm_close,{
    stopSimulation()
    # Arrete la shiny
    stopApp()
    
  })

  observeEvent(input$Sortie,{
    
    if(input$Sortie == "echelle_billon" && !is.null(input$Sortie)) {
      req(input$typeBillonnage)
      processed_Billonage(SortieBillonnage(Data= processed_data_htvol(), Type = input$typeBillonnage))
    }
  })
  
  observeEvent(input$calculer_billonnage, {
    req(input$dhs_input, input$typeBillonnage, processed_data_htvol())
    
    # Validation minimale : Grade 1 obligatoire
    if(is.null(input$nom_grade1) || input$nom_grade1 == "") {
      showNotification("Le nom du Grade 1 est obligatoire", type = "error", duration = 5)
      return()
    }
    
    withProgress(message = 'Calcul du billonnage en cours...', value = 0, {
      
      incProgress(0.1, detail = "Validation des paramètres...")
      
      dhs_val <- as.numeric(input$dhs_input)
      
      simplifier_val <- input$simplifier
      
      suppressWarnings({
        incProgress(0.2, detail = "Traitement des longueurs...")
        
        # Grade 1 (toujours présent)
        long_grade1_val <- if(is.null(input$long_grade1) || input$long_grade1 == "Indéfini") {
          NA_real_
        } else {
          as.numeric(input$long_grade1)
        }
        
        # Validation Grade 1
        if(!is.null(input$long_grade1) && input$long_grade1 == "Indéfini" &&
           (is.null(input$nom_grade1) || input$nom_grade1 == "") &&
           (is.null(input$diam_grade1) || is.na(input$diam_grade1))) {
          showNotification("Grade 1 : Si la longueur est définie (même comme 'Indéfini'), le nom et le diamètre doivent être fournis",
                           type = "error", duration = 10)
          return()
        }
        
        # Grade 2 - Seulement si affiché ET les inputs existent
        long_grade2_val <- NA_real_
        if (show_grade2() && !is.null(input$long_grade2)) {
          long_grade2_val <- if(input$long_grade2 == "Indéfini" || input$long_grade2 == "-- Aucune --") {
            NA_real_
          } else {
            as.numeric(input$long_grade2)
          }
          
          # Validations pour Grade 2
          if(input$long_grade2 == "Indéfini" &&
             (is.null(input$nom_grade2) || input$nom_grade2 == "") &&
             (is.null(input$diam_grade2) || is.na(input$diam_grade2))) {
            showNotification("Grade 2 : Si la longueur est définie (même comme 'Indéfini'), le nom et le diamètre doivent être fournis",
                             type = "error", duration = 10)
            return()
          }
          
          if((!is.null(input$nom_grade2) && input$nom_grade2 != "") || 
             (!is.null(input$diam_grade2) && !is.na(input$diam_grade2))) {
            if(input$long_grade2 == "" || input$long_grade2 == "-- Aucune --") {
              showNotification("Grade 2 : Vous avez défini un nom ou un diamètre mais aucune longueur.",
                               type = "error", duration = 6)
              return()
            }
          }
        }
        
        # Grade 3 - Seulement si affiché ET les inputs existent
        long_grade3_val <- NA_real_
        if (show_grade3() && !is.null(input$long_grade3)) {
          long_grade3_val <- if(input$long_grade3 == "Indéfini" || input$long_grade3 == "-- Aucune --") {
            NA_real_
          } else {
            as.numeric(input$long_grade3)
          }
          
          # Validations pour Grade 3
          if(input$long_grade3 == "Indéfini" &&
             (is.null(input$nom_grade3) || input$nom_grade3 == "") &&
             (is.null(input$diam_grade3) || is.na(input$diam_grade3))) {
            showNotification("Grade 3 : Si la longueur est définie (même comme 'Indéfini'), le nom et le diamètre doivent être fournis",
                             type = "error", duration = 10)
            return()
          }
          
          if((!is.null(input$nom_grade3) && input$nom_grade3 != "") || 
             (!is.null(input$diam_grade3) && !is.na(input$diam_grade3))) {
            if(input$long_grade3 == "" || input$long_grade3 == "-- Aucune --") {
              showNotification("Grade 3 : Vous avez défini un nom ou un diamètre mais aucune longueur.",
                               type = "error", duration = 6)
              return()
            }
          }
        }
        
        incProgress(0.3, detail = "Traitement des diamètres...")
        
        # Gestion des diamètres
        diam_grade1_val <- if(is.null(input$diam_grade1) || is.na(input$diam_grade1)) {
          NA_real_
        } else {
          as.numeric(input$diam_grade1)
        }
        
        diam_grade2_val <- if(show_grade2() && !is.null(input$diam_grade2) && !is.na(input$diam_grade2)) {
          as.numeric(input$diam_grade2)
        } else {
          NA_real_
        }
        
        diam_grade3_val <- if(show_grade3() && !is.null(input$diam_grade3) && !is.na(input$diam_grade3)) {
          as.numeric(input$diam_grade3)
        } else {
          NA_real_
        }
        
        incProgress(0.4, detail = "Préparation des noms de grades...")
        
        # Gestion des noms
        nom_grade1_val <- as.character(input$nom_grade1)
        
        nom_grade2_val <- if(show_grade2() && !is.null(input$nom_grade2) && input$nom_grade2 != "") {
          as.character(input$nom_grade2)
        } else {
          NA_character_
        }
        
        nom_grade3_val <- if(show_grade3() && !is.null(input$nom_grade3) && input$nom_grade3 != "") {
          as.character(input$nom_grade3)
        } else {
          NA_character_
        }
      }) # Fin suppressWarnings
      
      incProgress(0.5, detail = "Exécution du calcul de billonnage...")
      
      # Exécuter SortieBillesFusion
      tryCatch({
        processed_Billonage(SortieBillesFusion(
          Data = processed_data_htvol(),
          Type = as.character(input$typeBillonnage),
          dhs = dhs_val,
          nom_grade1 = nom_grade1_val,
          long_grade1 = long_grade1_val,
          diam_grade1 = diam_grade1_val,
          nom_grade2 = nom_grade2_val,
          long_grade2 = long_grade2_val,
          diam_grade2 = diam_grade2_val,
          nom_grade3 = nom_grade3_val,
          long_grade3 = long_grade3_val,
          diam_grade3 = diam_grade3_val,
          Simplifier = simplifier_val
        ))
        
        incProgress(0.8, detail = "Finalisation...")
        
        if(!is.null(input$Sortie) && input$Sortie == "echelle_billon") {
          processed_Simul(processed_Billonage())
        }
        
        incProgress(1, detail = "Terminé!")
        showNotification("Billonnage calculé avec succès!", type = "message", duration = 3)
        
      }, error = function(e) {
        cat("✗ Erreur billonnage:", e$message, "\n")
        showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
        processed_Billonage(NULL)
      })
      
    }) # Fin du withProgress
  })
  
  observe({
    
    
    req(input$Sortie)
    sortie <- input$Sortie
    simplifier <- input$simplifier
    
    
  
    if(is.null(sortie) || sortie == ""){
      return()
    }else if(is.null(processed_data_htvol())){
      return()
    }
    
    # Exécution de switch avec une valeur valide de sortie
    switch(sortie,
           "arbre_par_iteration" = {
             processed_Simul(SortieArbreSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "dendro_par_placette" = {
             processed_Simul(SortieDendroSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "echelle_billon" = {
             processed_Simul(processed_Billonage())
           },
           "dendro_par_iteration" = {
             processed_Simul(SortieDendroIterSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "Sommaire_Classes_DHP"= {
             processed_Simul(Sommaire_Classes_DHP(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "test" ={
             processed_Simul(processed_data_htvol())
           },
           
         
           default = {
             # Gérer le cas par défaut. Peut-être afficher un message ou effectuer une autre action par défaut.
           }
    )
  })
  
  
  
  # Afficher le contenu du fichier CSV dans un tableau
  output$table <- renderTable({
      data()[1:20,]
  })
  

  # Télécharger le résultat après simulation 
  output$download <- downloadHandler(
    filename = function() {
      if (input$RecruesGaules == 1) {
        
        if(input$Sortie == "echelle_billon"){
          nom <- paste("resultat_Gaule", input$Sortie, "_",input$typeBillonnage ,"-",input$NbIter,"i","_", Sys.Date(), ".csv", sep = "")
        }else{
          nom <- paste("resultat_Gaule", input$Sortie,"-",input$NbIter,"i","_", Sys.Date(), ".csv", sep = "")
        }
        
        return(nom)
        
      } else {
        
        if(input$Sortie == "echelle_billon"){
          nom <- paste("resultat_", input$Sortie, "_",input$typeBillonnage ,"-",input$NbIter,"i","_", Sys.Date(), ".csv", sep = "")
        }else{
        nom <- paste("resultat_", input$Sortie,"-",input$NbIter,"i","_", Sys.Date(), ".csv", sep = "")
        }
        
        return(nom)
      }
    },
    content = function(file) {
      req(processed_Simul())
       # write.csv(processed_Simul(), file, row.names = FALSE )
        write.table(processed_Simul(), file, sep = ";",row.names = FALSE)
    }
  )
  
  output$downloadPlot <- downloadHandler(
    filename = function() {
      
      if(input$graphType == 'Évolution des variables par placette'){
        
        paste("graphique","_","évol","_",input$variable,"_",input$Espece,"_", Sys.Date(), ".png", sep="")
      }else{
        paste("graphique","_","dist","_",input$Espece,"_", Sys.Date(), ".png", sep="")
      }
    
      },
    
    content = function(file) {
      
      plot <- graph_to_print()
      
      # Enregistrer le graphique dans le fichier spécifié
      ggsave(file,plot )
    }
  )
  
 
  output$simulationStatus <- renderText({
    simulation_status()
  })
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      "Aide application R du simulateur SaMARE.pdf"
    },
    content = function(file) {
      file.copy("WWW/Aide application R du simulateur SaMARE.pdf", file)
    }
  )
  
  output$downloadPDFtest <- downloadHandler(
    filename = function() {
      "Test2500m2.csv"
    },
    content = function(file) {
      file.copy("WWW/Test2500m2.csv", file)
    }
  )
  
  output$downloadPDFtestgaules <- downloadHandler(
    filename = function() {
      "GaulesTest2500m2.csv"
    },
    content = function(file) {
      file.copy("WWW/GaulesTest2500m2.csv", file)
    }
  )
  
  
  
  
  
  
  observeEvent(input$help_btn, {
    updateNavbarPage(session, "all_pages", selected = "Aide")
  })
  
  observeEvent(input$back_btn, {
    updateNavbarPage(session, "all_pages", selected = "Main")
  })
}



vevifier_variable_meteo <- function(data){
  
  select=dplyr::select
  data<-renommer_les_colonnes(data = data)
  
  data <- data %>%
    rename(id_pe = Placette, latitude = Latitude, longitude = Longitude)
  
  
  mes_variables <- c('GrwDays', 'Ptot', 'Tmoy')
  fonctions_validation <- list(valide_GrwDays, valide_Ptot, valide_Tmoy)
  noms_remplacement <- c("growingseasonlength", "totalprecipitation", "tmean")
  
  
  for (i in seq_along(mes_variables)) {
    if (mes_variables[i] %in% names(data) && !fonctions_validation[[i]](data)) {
      data <- select(data, -!!rlang::sym(mes_variables[i]))
    }
  }
  
  
  variables_presentes <- intersect(mes_variables, names(data))
  for (col_names in variables_presentes) {
    if (!length(unique(data[[col_names]])) == 1) {
      data <- select(data, -!!rlang::sym(col_names))
    }
  }
  
  
  map_noms_variables <- c(GrwDays = "growingseasonlength",
                          Ptot = "totalprecipitation",
                          Tmoy = "tmean")
  
  variables_non_trouvees <- setdiff(mes_variables, names(data))
  
  if(!is_empty(variables_non_trouvees)){
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]
    
    data <- extract_map_plot(file=data, liste_raster="cartes_climat", variable=variables_a_extraire)
    
    if('tmean' %in% variables_a_extraire) {
      data <- rename(data, Tmoy = tmean)
    }
    
    if('totalprecipitation' %in% variables_a_extraire) {
      data <- rename(data, Ptot = totalprecipitation)
    }
    
    if('growingseasonlength' %in% variables_a_extraire) {
      data <- rename(data, GrwDays = growingseasonlength)
    }
  }
  
  data<-data %>% rename(Placette=id_pe)
  
  return (data)
}
# Run the application 


tryCatch({
  shinyApp(ui, server)
}, error = function(e) {
 
  cat("ERROR: Shiny app crashed with the following message:\n")
  print(e)
  q()
})

