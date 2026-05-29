#
# SaMARE - Application Shiny
# Version modifiée pour déploiement VBS (corrections critiques uniquement)
#

options(repos = c(CRAN = "https://cloud.r-project.org"))

#Packages__________________________________________________________________####
library(shiny)
library(shinyjs)
library(shinythemes)
library(readr)
library(dplyr)
library(rlang)
library(ggplot2)
library(SimulateurSaMARE)
library(BillonnagePetro)
library(ExtractMap)
library(sf)
library(shinyWidgets)
library(OutilsDRF)

#DetectionContexte_________________________________________________________####
# Détecter si on est dans RStudio (interactif) ou lancé en batch (VBS)
# Cela permet de ne pas tuer la session R quand on est dans RStudio
is_rstudio <- Sys.getenv("RSTUDIO") == "1"
is_interactive_session <- interactive() && is_rstudio

#Code_UI___________________________________________________________________####
ui <- fluidPage(
  useShinyjs(),

  tags$head(
    # JavaScript pour gérer la fermeture de fenêtre et protection simulation
    tags$script(HTML("
      // Variable pour tracker si une simulation est en cours
      var simulationEnCours = false;

      // Handler pour fermer la fenêtre
      Shiny.addCustomMessageHandler('closeWindow', function(message) {
        window.close();
        setTimeout(function() {
          alert('Vous pouvez maintenant fermer cette fenêtre.');
        }, 500);
      });

      // Handler pour marquer le début/fin de simulation
      Shiny.addCustomMessageHandler('setSimulationStatus', function(status) {
        simulationEnCours = status;
      });

      // Avertir l'utilisateur s'il ferme pendant une simulation
      window.onbeforeunload = function(e) {
        if (simulationEnCours) {
          var message = 'Une simulation est en cours. Si vous fermez maintenant, les résultats seront perdus.';
          e.returnValue = message;
          return message;
        }
      };
    "))
  ),

  # CSS personnalisé SaMARE (couleurs vertes originales)
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
      background-color: #d3f4d7;
      border-color: #d3f4d7;
    }
    .navbar-default .navbar-nav > li > a {
      color: #000;
    }
    .navbar-default .navbar-brand {
      color: #000;
    }
  ")),

  navbarPage(
    "",
    id = "all_pages",
    tabPanel(
      img(src = "SaMARE_MRNF_Final.jpg", height = 50, width = 70),
      div(
        id = "main_app",
        tags$div(style = "font-size: 10px;", paste("SaMARE", as.character(packageVersion("SimulateurSaMARE")))),
        sidebarLayout(
          uiOutput("dynamic_sidebar"),
          mainPanel(
            uiOutput("dynamic_mainpanel")
          )
        ),
        # Bouton fermer
        div(
          style = "margin-top: 20px;",
          actionButton(
            "close",
            "Fermer l'application",
            style = "color: #fff; background-color: #008000; border-color: #008000;",
            icon = icon("power-off")
          )
        )
      )
    ),
    # Page d'aide
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
                       p("Le simulateur de croissance SaMARE est un modèle de croissance à l'échelle de l'arbre. Il est adapté pour les peuplements feuillus et mixtes de la forêt tempérée du Québec. Il prévoit l'accroissement en diamètre, la probabilité de mortalité et l'évolution de la vigueur des arbres de plus de 9,0 cm de diamètre ainsi que le recrutement de nouveaux arbres au seuil de 9,1 cm."),
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
        )
      )
    )
  )
)

#Code_Server_______________________________________________________________####
server <- function(input, output, session) {

  # Fermeture propre de la session (quand l'utilisateur ferme l'onglet)
  session$onSessionEnded(function() {
    # Nettoyage mémoire
    gc()
    message("Session fermée.")

    # Quitter R seulement si on n'est PAS dans RStudio (lancé via VBS)
    if (!is_interactive_session) {
      Sys.sleep(0.5)
      q(save = "no")
    }
  })

  rv <- reactiveValues(
    data_valid = FALSE,
    extraction_choice_made = FALSE,
    extraction_completed = FALSE,
    climat_annuel = NULL,
    climat_mensuel = NULL,
    extraction_option = NULL,
    extraction_horizon = NULL,
    age_moy_valid = TRUE,
    mode_visualisation = FALSE,
    resultats_simulation = NULL,
    placette = NULL,
    processed_Billonage = NULL,
    processed_Simul = NULL,
    listeEspece = NULL,
    simulation_terminee = FALSE,
    show_grade2 = FALSE,
    show_grade3 = FALSE

  )

  output$dynamic_sidebar <- renderUI({
    sidebarPanel(
      fileInput(
        inputId = "file",
        label = "Données d'inventaire de départ (format csv)",
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
      numericInput("Horizon", "Nombre d'années de simulation (multiple de 5)", min = 5, value = 5, step = 5),
      radioButtons("RecruesGaules", "Recrutement basé sur les gaules pour les érablières",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0,
                   inline = TRUE),
      conditionalPanel(
        condition = "input.RecruesGaules == 1",
        fileInput(
          inputId = "gauleFile",
          label = "Dénombrement des gaules (format csv)",
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

      numericInput("NbIter", "Nombre d'itérations (nombre pair)", min = 2, value = 2, step = 2),
      radioButtons("corticale", "Maladie corticale du hêtre",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0,
                   inline = TRUE),

      # Section traitement de coupe
      radioButtons("TraitCoupe", "Traitement de coupe",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0,
                   inline = TRUE),
      conditionalPanel(
        condition = "input.TraitCoupe == 1",
        div(
          style = "margin-top: 10px; padding: 10px; background-color: #e6f9e9; border-radius: 3px;",
          p("Configurez les traitements de coupe par tranche de 5 ans:",
            style = "font-size: 0.9em; margin-bottom: 10px;"),
          uiOutput("ui_pentade_coupe"),

          selectInput("type_coupe", "Type de coupe:",
                      choices = c("Aucune coupe" = "NA",
                                  setNames(c(0:1,6:9,12:19), c("0- Coupe d'amélioration","1- Coupe d'éclaircie","6- Coupe de jardinage","7- Coupe progressive",
                                                               "8- Éclaircie commerciale","9- Éclaicie sélective","12- Coupe progressive (CPI_CP)",
                                                               "13- Coupe progressive (CPI_RL)","14- Coupe réserve semanciers","15- Jardinage CIMOTFF",
                                                               "16- Jarinage gr. arbres CIMOTFF", "17- CPI_CP CIMOTFF","18- CPI_RL CIMOTFF","19- CPRS"))),
                      #setNames(0:1,c("Éclaicie","CPRS"))),
                      selected = "NA"),

          radioButtons("type_modif", "Type de modificateur",
                       choices = list(
                         "Modificateur simple (même valeur pour toutes les essences)" = "simple",
                         "Fichier (modificateurs par essence)" = "excel"),
                       selected = "simple",
                       inline = FALSE),

          conditionalPanel(
            condition = "input.type_modif == 'simple'",
            numericInput("modif_coupe", "Modificateur (%):",
                         value = 0, min = -80, max = 160, step = 5)),

          conditionalPanel(
            condition = "input.type_modif == 'excel'",
            div(
              fileInput("modif_excel_file", "Fichier des modificateurs:",
                        accept = c(".xlsx", ".xls", ".csv")),
              div(
                style = "font-size: 0.85em; color: #6c757d; font-style: italic;",
                "Le fichier doit contenir les colonnes 'ess_ind' et 'modifier'
                (Excel ou CSV) modifier doit se situer entre -80 et 160%")))
          ,
          div(
            style = "margin-bottom: 10px;",
            actionButton("clear_coupes", "Effacer toutes les coupes",
                         style = "background-color: #dc3545; color: white; width: 100%;")),

          h6("Configuration actuelle des coupes:"),
          verbatimTextOutput("display_coupes")
        ),


      ),
      uiOutput("conditionalButton")

    )

  })

  # pentade de coupe

  output$ui_pentade_coupe <- renderUI({
    req(input$Horizon)
    selectInput(
      inputId = "pentade_coupe",
      label   = "Période de coupe",
      choices = setNames(seq(5, input$Horizon, by = 5),
                         paste0(seq(0, input$Horizon - 5, by = 5), " - ", seq(5, input$Horizon, by = 5))),
      selected = NULL
    )
  })

  # Boutons pour effacer
  observeEvent(input$clear_coupes, {
    if (!is.null(rv$coupe_on_vector)) {
      rv$coupe_on_vector <- rep(NA_real_, length(rv$coupe_on_vector))
      rv$coupe_modif_vector <- vector("list", length(rv$coupe_modif_vector))
      showNotification("Toutes les coupes ont été effacées", type = "message", duration = 2)
    }
  })

  #
  observeEvent(input$TraitCoupe, {
    if (input$TraitCoupe==1 && !is.null(input$Horizon)) {
      horizon <- input$Horizon / 5
      # Initialiser seulement si pas déjà fait
      if (is.null(rv$coupe_on_vector)) {
        rv$coupe_on_vector <- rep(NA_real_, horizon)
        rv$coupe_modif_vector <- vector("list", horizon)
      }
    } else {
      # Réinitialiser les vecteurs quand la case est décochée
      rv$coupe_on_vector <- NULL
      rv$coupe_modif_vector <- NULL
    }
  })


  # Observateurs pour appliquer les modifications aux vecteurs
  observeEvent( list(input$type_coupe, input$modif_coupe, input$modif_excel_file), {
    req(input$pentade_coupe, input$type_coupe, input$Horizon)

    if (input$type_coupe == "NA") {
      showNotification("Impossible d'appliquer une configuration avec 'Aucune coupe' sélectionnée.",
                       type = "warning", duration = 4)
      return()
    }

    decennie_idx <- (as.numeric(input$pentade_coupe)/5)

    if (input$type_coupe == "NA") {
      rv$coupe_on_vector[decennie_idx] <- NA_real_
      rv$coupe_modif_vector[[decennie_idx]] <- NA
    } else {
      rv$coupe_on_vector[decennie_idx] <- as.numeric(input$type_coupe)

      if (!is.null(input$type_modif) && input$type_modif == "simple") {
        rv$coupe_modif_vector[[decennie_idx]] <- input$modif_coupe
      } else if (!is.null(input$type_modif) && input$type_modif == "excel") {
        if (!is.null(input$modif_excel_file) && !is.null(input$modif_excel_file$datapath)) {
          tryCatch({
            # Détecter le type de fichier par l'extension
            file_ext <- tools::file_ext(input$modif_excel_file$name)

            if (file_ext %in% c("xlsx", "xls")) {
              modif_data <- readxl::read_excel(input$modif_excel_file$datapath)
            } else if (file_ext == "csv") {
              modif_data <- read.csv(input$modif_excel_file$datapath, sep = ";", header = TRUE)
            } else {
              showNotification("Format de fichier non supporté. Utilisez Excel (.xlsx, .xls) ou CSV.",
                               type = "error", duration = 5)
              return()
            }

            if (!all(c("ess_ind", "modifier") %in% colnames(modif_data))) {
              showNotification("Le fichier doit contenir les colonnes 'ess_ind' et 'modifier'",
                               type = "error", duration = 5)
              return()
            }

            # Ajouter le nom du fichier au data.frame
            attr(modif_data, "filename") <- input$modif_excel_file$name
            rv$coupe_modif_vector[[decennie_idx]] <- modif_data

          }, error = function(e) {
            showNotification(paste("Erreur lors de la lecture du fichier:", e$message),
                             type = "error", duration = 5)
            return()
          })
        } else {
          showNotification("Veuillez sélectionner un fichier",
                           type = "error", duration = 5)
          return()
        }
      } else {
        rv$coupe_modif_vector[[decennie_idx]] <- 0
      }
    }

    showNotification(paste("Coupe appliquée à la période", input$pentade_coupe),
                     type = "message", duration = 2)
  })

  # Affichage des vecteurs actuels
  output$display_coupes <- renderText({
    if (!is.null(rv$coupe_on_vector) && length(rv$coupe_on_vector) > 0) {
      coupe_display <- ifelse(is.na(rv$coupe_on_vector), "NA", as.character(rv$coupe_on_vector))

      modif_display <- sapply(seq_along(rv$coupe_modif_vector), function(i) {
        x <- rv$coupe_modif_vector[[i]]
        if (is.null(x) || (length(x) == 1 && is.na(x))) {
          "NA"
        } else if (is.numeric(x) && length(x) == 1) {
          paste0(x, "%")
        } else if (is.data.frame(x) && nrow(x) > 0) {
          filename <- attr(x, "filename")
          if (!is.null(filename)) {
            filename
          } else {
            paste0("Excel (", nrow(x), " essences)")
          }
        } else {
          "Vide"
        }
      })

      paste0("Coupe_ON: [", paste(coupe_display, collapse = ", "), "]\n",
             "Modif: [", paste(modif_display, collapse = ", "), "]")
    } else {
      "Aucune configuration"
    }
  })

  #
  observe({
    if (!is.null(input$Horizon) && !is.null(input$TraitCoupe) && input$TraitCoupe==1) {
      new_horizon <- input$Horizon / 5

      # Redimensionner le vecteur coupe_on
      if (is.null(rv$coupe_on_vector) || length(rv$coupe_on_vector) != new_horizon) {
        old_vector <- rv$coupe_on_vector
        rv$coupe_on_vector <- rep(NA_real_, new_horizon)

        # Conserver les anciennes valeurs si elles existent
        if (!is.null(old_vector) && length(old_vector) > 0) {
          copy_length <- min(length(old_vector), new_horizon)
          rv$coupe_on_vector[1:copy_length] <- old_vector[1:copy_length]
        }
      }

      # Redimensionner la liste coupe_modif
      if (is.null(rv$coupe_modif_vector) || length(rv$coupe_modif_vector) != new_horizon) {
        old_list <- rv$coupe_modif_vector
        rv$coupe_modif_vector <- vector("list", new_horizon)

        # Conserver les anciennes valeurs si elles existent
        if (!is.null(old_list) && length(old_list) > 0) {
          copy_length <- min(length(old_list), new_horizon)
          rv$coupe_modif_vector[1:copy_length] <- old_list[1:copy_length]
        }
      }
    }
  })

  # Lire le fichier arbres
  data <- reactive({
    req(input$file)
    read.delim(input$file$datapath, sep = ";")
  })

  # Lire le fichier des gaules
  gaule_data <- reactive({
    req(input$gauleFile)
    read.delim(input$gauleFile$datapath, sep = ";")
  })

  simulation_status <- reactiveVal("Attente de la simulation...")

  # Variables réactives
  processed_Simul <- reactiveVal(NULL)
  processed_data_ArbreSamare <- reactiveVal(NULL)
  processed_data_DendroSamare <- reactiveVal(NULL)
  processed_data_DendroIterSamare <- reactiveVal(NULL)
  processed_data_htvol <- reactiveVal(NULL)
  processed_absent <- reactiveVal(NULL)
  processed_optionnel <- reactiveVal(NULL)
  processed_absent_gaules <- reactiveVal(NULL)
  valider_data <- reactiveVal(NULL)
  valider_data_gaules <- reactiveVal(NULL)
  valider_placette_gaules <- reactiveVal(NULL)
  processed_Billonage <- reactiveVal(NULL)
  graph_to_print <- reactiveVal(NULL)
  graph <- reactiveVal(NULL)
  vevifier_meteo <- reactiveVal(NULL)
  placette <- reactiveVal(NULL)
  isFile <- reactiveValues(flag = FALSE)
  show_grade2 <- reactiveVal(TRUE)
  show_grade3 <- reactiveVal(FALSE)
  donnees_graphique <- reactiveVal(NULL)

  # Fonction pour afficher les erreurs
  afficher_erreurs <- function(erreurs, statut_output, erreur_output, emplacement) {
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

  # Fonction pour afficher les avertissements
  afficher_champs_optionnels <- function(erreurs, statut_output, erreur_output, emplacement) {
    if (length(erreurs) != 0) {
      list_string <- paste(erreurs, collapse = ', ')
      output[[statut_output]] <- renderText({
        HTML("<b>Champ optionnel absent </b><b>", emplacement, "</b>:")
      })
      output[[erreur_output]] <- renderText({
        HTML(paste("<span style='color:#FF8C00;'>", list_string, "</span>"))
      })
    } 
  }

  observeEvent(input$file, {
    output$dynamic_mainpanel <- renderUI({ tableOutput("table") })
    isFile$flag <- TRUE
    validate_files()
  })

  observeEvent(input$gauleFile, {
    validate_files()
  })

  validate_files <- function() {
    if (is.null(input$file)) {
      afficher_erreurs("Le fichier data est manquant.", "statut_data_gaules", "error_data_gaules", "")
      return()
    }
    processed_absent(trouver_noms_absents(data()))
    processed_optionnel(trouver_noms_optionels(data()))
    valider_data(valide_data(data()))

    afficher_erreurs(processed_absent(), "statut_data", "error_data", "dans le fichier des arbres")
    afficher_erreurs(valider_data(), "statut", "error", "dans le fichier des arbres")
    afficher_champs_optionnels(processed_optionnel(),"statut_data", "error_data","dans le fichier des arbres")

    if (input$RecruesGaules == 1 && !is.null(input$gauleFile)) {
      processed_absent_gaules(trouver_noms_absents_gaules(gaule_data()))
      valider_data_gaules(valide_data_gaules(gaule_data()))
      valider_placette_gaules(valide_placette_gaules(data_arbre = data(), data_gaules = gaule_data()))

      afficher_erreurs(processed_absent_gaules(), "statut_data_gaules", "error_data_gaules", "dans le fichier des gaules")
      afficher_erreurs(valider_data_gaules(), "statut_data_gaules_vall", "error_data_gaules_vall", "dans le fichier des gaules")
      afficher_erreurs(valider_placette_gaules(), "statut_data_gaules_plac", "error_data_gaules_plac", "dans le fichier des gaules")
    }
  }

  # fonction réactive pour valider les champs optionels
  valider_champ_optionel <- reactive({

    champ_optionel_absent <- trouver_noms_optionels(data())

    return(champ_optionel_absent)
  })

  observe({
    input$RecruesGaules
    input$Horizon
    input$NbIter

    output$conditionalButton <- renderUI({
      if (!is.null(input$file) && !is.na(input$Horizon) && input$Horizon >= 5 && !is.na(input$NbIter) && input$NbIter > 1 && input$NbIter %% 2 == 0 && input$RecruesGaules == 0 && length(processed_absent()) == 0 && length(valider_data()) == 0 && isFile$flag && input$Horizon %% 5 == 0) {
        actionButton("simulateButton", "Simulation")
      } else if (!is.null(input$file) && !is.na(input$Horizon) && input$Horizon >= 5 && !is.na(input$NbIter) && input$NbIter > 1 && input$NbIter %% 2 == 0 && input$RecruesGaules == 1 && !is.null(input$gauleFile) && length(processed_absent()) == 0 && length(valider_data()) == 0 && length(processed_absent_gaules()) == 0 && length(valider_data_gaules()) == 0 && length(valider_placette_gaules()) == 0 && input$Horizon %% 5 == 0) {
        actionButton("simulateButton", "Simulation")
      }
    })
  })

  observeEvent(input$simulateButton, {
    isFile$flag <- FALSE

    # Protection: Activer l'avertissement de fermeture
    session$sendCustomMessage("setSimulationStatus", TRUE)

    simulation_status("Simulation en cours...")

    vevifier_meteo(vevifier_variable_meteo(data()))
    output$dynamic_sidebar <- renderUI({
      sidebarPanel(
        h3("Simulation en cours...")
      )
    })

    # Gestion des paramètres de coupe
    coupe_on <- if (input$TraitCoupe==1) {
      rv$coupe_on_vector
    } else {
      NULL
    }

    coupe_modif <- if (input$TraitCoupe==1) {
      as.list(rv$coupe_modif_vector)
    } else {
      NULL
    }

    withProgress(message = 'Simulation en cours...', value = 0, {
      tryCatch({
        if (input$RecruesGaules == 1) {
          result <- SimulSaMARE(NbIter = input$NbIter, Horizon = (input$Horizon / 5),
                                RecruesGaules = input$RecruesGaules, Data = vevifier_meteo(),
                                Gaules = gaule_data(), MCH = input$corticale
                                ,Coupe_ON = coupe_on, Coupe_modif = coupe_modif)
        } else {
          result <- SimulSaMARE(NbIter = input$NbIter, Horizon = (input$Horizon / 5),
                                RecruesGaules = input$RecruesGaules, Data = vevifier_meteo(),
                                MCH = input$corticale
                                , Coupe_ON = coupe_on, Coupe_modif = coupe_modif)
        }

        placette(unique(result$Placette))
        Sys.sleep(5)
        simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")

        donnees_graphique(result)
        processed_data_htvol(result)
        processed_Simul(result)

      }, error = function(e) {
        showNotification(paste("Erreur de simulation:", e$message), type = "error", duration = 10)
        simulation_status("Erreur lors de la simulation.")
      }, finally = {
        # Protection: Désactiver l'avertissement de fermeture
        session$sendCustomMessage("setSimulationStatus", FALSE)
      })
    })

    # Observateurs pour les grades (billonnage)
    observeEvent(input$add_grade2, {
      show_grade2(TRUE)
    })

    observeEvent(input$remove_grade2, {
      show_grade2(FALSE)
      show_grade3(FALSE)
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
      updateTextInput(session, "nom_grade3", value = "")
      updateSelectInput(session, "long_grade3", selected = "")
      updateNumericInput(session, "diam_grade3", value = NA)
    })

    # Interface post-simulation
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
                    choices = c("Arbre par itération" = "arbre_par_iteration",
                                "Sommaire dendrométrique par itération" = "dendro_par_iteration",
                                "Sommaire dendrométrique par placette" = "dendro_par_placette",
                                "À l'échelle du billon" = "echelle_billon",
                                "Sommaire classes DHP" = "Sommaire_Classes_DHP"),
                    selected = "arbre_par_iteration"
        ),
        tags$br(),

        conditionalPanel(
          condition = "input.Sortie == 'echelle_billon'",
          selectInput("typeBillonnage", "Type de Billonnage :",
                      choices = list("DHP_Régionalisé" = "DHP", "ABCD_Régionalisé" = "ABCD",
                                     "1234_Provincial" = "1234", "MSCR_Provincial" = "MSCR",
                                     "DHP_Provincial" = "DHP2015", "ABCD_Provincial" = "ABCD2015"),
                      selected = "DHP"
          )
        ),

        # Section billonnage
        conditionalPanel(
          condition = "input.Sortie == 'echelle_billon'",
          h5("Paramètres Sybille", style = "color: #856404; font-weight: bold; margin-top: 15px;"),
          numericInput("dhs_input", label = "DHS (Diamètre à hauteur de souche):",
                       value = 0.15, min = 0.01, max = 1.0, step = 0.01),
          div(
            style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
            h6("Grade 1", style = "color: #495057; font-weight: bold;"),
            textInput("nom_grade1", "Nom du grade 1:", value = "sciage court"),
            selectInput("long_grade1", "Longueur (pieds):",
                        choices = c("Indéfini", "4", "8", "12"), selected = "8"),
            numericInput("diam_grade1", "Diamètre au fin bout(cm):",
                         value = 20, min = 0, max = 100, step = 0.1)
          ),
          uiOutput("add_grade2_button"),
          uiOutput("grade2_section"),
          uiOutput("add_grade3_button"),
          uiOutput("grade3_section"),
          div(
            style = "margin-top: 15px; text-align: center;",
            actionButton("calculer_billonnage", "Calculer le billonnage",
                         style = "background-color: #008000; color: white; width: 100%;",
                         icon = icon("calculator"))
          )
        ),

        downloadButton("download", "Télécharger le résultat",
                       style = "background-color: #008000; color: white; border-color: #3c8dbc;"),
        tags$br(),
        tags$br(),
        actionButton("reset_button", "Réinitialiser")
      )
    })

    # Render UI pour les grades
    output$add_grade2_button <- renderUI({
      if (!show_grade2()) {
        div(
          style = "text-align: center; margin-bottom: 15px; padding: 10px; border: 2px dashed #008000; border-radius: 5px; background-color: #f0f9ff;",
          actionButton("add_grade2", "Ajouter Grade 2",
                       style = "background-color: #008000; color: white; border: none; padding: 8px 20px; border-radius: 20px;",
                       icon = icon("plus-circle"))
        )
      }
    })

    output$grade2_section <- renderUI({
      if (show_grade2()) {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px; position: relative; border-left: 4px solid #008000;",
          div(
            style = "position: absolute; top: 10px; right: 10px;",
            actionButton("remove_grade2", "",
                         style = "background-color: #dc3545; color: white; border: none; padding: 4px 8px; border-radius: 50%; font-size: 12px;",
                         icon = icon("times"))
          ),
          h6("Grade 2", style = "color: #008000; font-weight: bold;"),
          p("(Optionnel)", style = "font-size: 0.8em; color: #6c757d;"),
          textInput("nom_grade2", "Nom du grade 2:", value = "pate"),
          selectInput("long_grade2", "Longueur (pieds):",
                      choices = c("-- Aucune --", "Indéfini", "4", "8", "12"), selected = "4"),
          numericInput("diam_grade2", "Diamètre au fin bout(cm):",
                       value = 8, min = 0, max = 100, step = 0.1)
        )
      }
    })

    output$add_grade3_button <- renderUI({
      if (show_grade2() && !show_grade3()) {
        div(
          style = "text-align: center; margin-bottom: 15px; padding: 10px; border: 2px dashed #008000; border-radius: 5px; background-color: #f0f9ff;",
          actionButton("add_grade3", "Ajouter Grade 3",
                       style = "background-color: #008000; color: white; border: none; padding: 8px 20px; border-radius: 20px;",
                       icon = icon("plus-circle"))
        )
      }
    })

    output$grade3_section <- renderUI({
      if (show_grade3()) {
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 10px; position: relative; border-left: 4px solid #008000;",
          div(
            style = "position: absolute; top: 10px; right: 10px;",
            actionButton("remove_grade3", "",
                         style = "background-color: #dc3545; color: white; border: none; padding: 4px 8px; border-radius: 50%; font-size: 12px;",
                         icon = icon("times"))
          ),
          h6("Grade 3", style = "color: #008000; font-weight: bold;"),
          p("(Optionnel)", style = "font-size: 0.8em; color: #6c757d;"),
          textInput("nom_grade3", "Nom du grade 3:", value = ""),
          selectInput("long_grade3", "Longueur (pieds):",
                      choices = c("-- Aucune --", "Indéfini", "4", "8", "12"), selected = "-- Aucune --"),
          numericInput("diam_grade3", "Diamètre au fin bout(cm):",
                       value = NA, min = 0, max = 100, step = 0.1)
        )
      }
    })

    output$dynamic_mainpanel <- renderUI({
      if (!is.null(input$affichegraph) && input$affichegraph == TRUE) {
        tabsetPanel(type = "tabs",
                    tabPanel("Graphique", plotOutput("plot"))
        )
      } else {
        div("")
      }
    })

    simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")
  })

  # FERMETURE de l'application
  observeEvent(input$close, {
    showNotification(
      "🔒 Fermeture de l'application en cours...",
      type = "message",
      duration = 2
    )

    # JavaScript pour fermer la fenêtre/onglet du navigateur
    session$sendCustomMessage(
      type = "closeWindow",
      message = "closing"
    )

    # Nettoyage mémoire
    gc()

    # Attendre un peu
    Sys.sleep(0.5)

    # Arrêter le serveur Shiny
    stopApp()

    # Quitter R seulement si on n'est PAS dans RStudio (lancé via VBS)
    if (!is_interactive_session) {
      q(save = "no")
    }
  })

  # Réinitialisation
  observeEvent(input$reset_button, {
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
    processed_optionnel(NULL)
    processed_absent_gaules(NULL)
    valider_data(NULL)
    valider_data_gaules(NULL)
    processed_Billonage(NULL)
    graph_to_print(NULL)
    graph(NULL)
    vevifier_meteo(NULL)
    donnees_graphique(NULL)
    removeModal()

    updateNumericInput(session, "Horizon", value = 5)
    updateRadioButtons(session, "RecruesGaules", selected = 0)
    updateNumericInput(session, "NbIter", value = 2)
    updateRadioButtons(session, "corticale", selected = 0)
    updateSelectInput(session, "variable", selected = NULL)
    updatePickerInput(session, "placette", selected = NULL)
    updatePickerInput(session, "TraitCoupe", selected = 0)

    output$dynamic_mainpanel <- renderUI({ NULL })

    output$dynamic_sidebar <- renderUI({
      sidebarPanel(
        fileInput(
          inputId = "file",
          label = "Données d'inventaire de départ (format csv)",
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
        numericInput("Horizon", "Nombre d'années de simulation (multiple de 5)", min = 5, value = NA, step = 5),
        radioButtons("RecruesGaules", "Recrutement basé sur les gaules pour les érablières",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0,
                     inline = TRUE),
        conditionalPanel(
          condition = "input.RecruesGaules == 1",
          fileInput(
            inputId = "gauleFile",
            label = "Dénombrement des gaules (format csv)",
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
        numericInput("NbIter", "Nombre d'itérations (nombre pair)", min = 2, value = 2, step = 2),
        radioButtons("corticale", "Maladie corticale du hêtre",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0,
                     inline = TRUE),
        # Section traitement de coupe
        radioButtons("TraitCoupe", "Traitement de coupe",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0,
                     inline = TRUE),
        conditionalPanel(
          condition = "input.TraitCoupe == 1",
          div(
            style = "margin-top: 10px; padding: 10px; background-color: #e6f9e9; border-radius: 3px;",
            p("Configurez les traitements de coupe par tranche de 5 ans:",
              style = "font-size: 0.9em; margin-bottom: 10px;"),
            uiOutput("ui_pentade_coupe"),

            selectInput("type_coupe", "Type de coupe:",
                        choices = c("Aucune coupe" = "NA",
                                    setNames(c(0:1,6:9,12:19), c("0- Coupe d'amélioration","1- Coupe d'éclaircie","6- Coupe de jardinage","7- Coupe progressive",
                                                                 "8- Éclaircie commerciale","9- Éclaicie sélective","12- Coupe progressive (CPI_CP)",
                                                                 "13- Coupe progressive (CPI_RL)","14- Coupe réserve semanciers","15- Jardinage CIMOTFF",
                                                                 "16- Jarinage gr. arbres CIMOTFF", "17- CPI_CP CIMOTFF","18- CPI_RL CIMOTFF","19- CPRS"))),
                        #setNames(0:1,c("Éclaicie","CPRS"))),
                        selected = "NA"),

            radioButtons("type_modif", "Type de modificateur",
                         choices = list(
                           "Modificateur simple (même valeur pour toutes les essences)" = "simple",
                           "Fichier (modificateurs par essence)" = "excel"),
                         selected = "simple",
                         inline = FALSE),

            conditionalPanel(
              condition = "input.type_modif == 'simple'",
              numericInput("modif_coupe", "Modificateur (%):",
                           value = 0, min = -80, max = 160, step = 5)),

            conditionalPanel(
              condition = "input.type_modif == 'excel'",
              div(
                fileInput("modif_excel_file", "Fichier des modificateurs:",
                          accept = c(".xlsx", ".xls", ".csv")),
                div(
                  style = "font-size: 0.85em; color: #6c757d; font-style: italic;",
                  "Le fichier doit contenir les colonnes 'ess_ind' et 'modifier'
                (Excel ou CSV) modifier doit se situer entre -80 et 160%")))
            ,
            div(
              style = "margin-bottom: 10px;",
              actionButton("clear_coupes", "Effacer toutes les coupes",
                           style = "background-color: #dc3545; color: white; width: 100%;")),

            h6("Configuration actuelle des coupes:"),
            verbatimTextOutput("display_coupes")
          ),


        ),
        uiOutput("conditionalButton")
      )
    })
  })

  # Fonctions de graphique
  mettre_a_jour_graphique <- function() {
    if (input$affichegraph == T) {
      if (is.null(donnees_graphique())) {
        showNotification("Effectuez d'abord une simulation", type = "warning", duration = 3)
        return()
      }
      if (is.null(input$Espece) || input$Espece == "") {
        showNotification("Sélectionnez une espèce", type = "warning", duration = 3)
        return()
      }
      if (is.null(input$variable)) {
        showNotification("Sélectionnez une variable", type = "warning", duration = 3)
        return()
      }
      if (is.null(input$placette) || length(input$placette) == 0) {
        showNotification("Sélectionnez au moins une placette", type = "warning", duration = 3)
        return()
      }

      tryCatch({
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

  observeEvent(input$affichegraph, { mettre_a_jour_graphique() })
  observeEvent(input$Espece, { mettre_a_jour_graphique() })
  observeEvent(input$variable, { mettre_a_jour_graphique() })
  observeEvent(input$valider, { mettre_a_jour_graphique() })

  output$plot <- renderPlot({
    graphiques <- graph()
    if (input$graphType == "Évolution des variables par placette") {
      graph_to_print(graphiques[[1]])
    } else {
      graph_to_print(graphiques[[2]])
    }
    print(graph_to_print())
  })

  observe({
    listeEspece <- unique(processed_data_htvol()$GrEspece)
    listeEspece2 <- append("TOT", listeEspece)
    updateSelectInput(session = session, inputId = "Espece",
                      label = "Groupe d'espèces", choices = listeEspece2)
  })

  # Gestion des sorties
  observe({
    req(input$Sortie)
    sortie <- input$Sortie
    simplifier <- input$simplifier

    if (is.null(sortie) || sortie == "") {
      return()
    } else if (is.null(processed_data_htvol())) {
      return()
    }

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
           "Sommaire_Classes_DHP" = {
             processed_Simul(Sommaire_Classes_DHP(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "test" = {
             processed_Simul(processed_data_htvol())
           }
    )
  })

  observeEvent(input$Sortie, {
    if (input$Sortie == "echelle_billon" && !is.null(input$Sortie)) {
      req(input$typeBillonnage)
      processed_Billonage(SortieBillonnage(Data = processed_data_htvol(), Type = input$typeBillonnage))
    }
  })

  # Observateur pour le calcul du billonnage
  observeEvent(input$calculer_billonnage, {
    req(input$dhs_input, input$typeBillonnage, processed_data_htvol())

    if (is.null(input$nom_grade1) || input$nom_grade1 == "") {
      showNotification("Le nom du Grade 1 est obligatoire", type = "error", duration = 5)
      return()
    }

    withProgress(message = 'Calcul du billonnage en cours...', value = 0, {
      incProgress(0.1, detail = "Validation des paramètres...")

      dhs_val <- as.numeric(input$dhs_input)
      simplifier_val <- input$simplifier

      suppressWarnings({
        incProgress(0.2, detail = "Traitement des longueurs...")

        long_grade1_val <- if (is.null(input$long_grade1) || input$long_grade1 == "Indéfini") {
          NA_real_
        } else {
          as.numeric(input$long_grade1)
        }

        long_grade2_val <- NA_real_
        if (show_grade2() && !is.null(input$long_grade2)) {
          long_grade2_val <- if (input$long_grade2 == "Indéfini" || input$long_grade2 == "-- Aucune --") {
            NA_real_
          } else {
            as.numeric(input$long_grade2)
          }
        }

        long_grade3_val <- NA_real_
        if (show_grade3() && !is.null(input$long_grade3)) {
          long_grade3_val <- if (input$long_grade3 == "Indéfini" || input$long_grade3 == "-- Aucune --") {
            NA_real_
          } else {
            as.numeric(input$long_grade3)
          }
        }

        incProgress(0.3, detail = "Traitement des diamètres...")

        diam_grade1_val <- if (is.null(input$diam_grade1) || is.na(input$diam_grade1)) {
          NA_real_
        } else {
          as.numeric(input$diam_grade1)
        }

        diam_grade2_val <- if (show_grade2() && !is.null(input$diam_grade2) && !is.na(input$diam_grade2)) {
          as.numeric(input$diam_grade2)
        } else {
          NA_real_
        }

        diam_grade3_val <- if (show_grade3() && !is.null(input$diam_grade3) && !is.na(input$diam_grade3)) {
          as.numeric(input$diam_grade3)
        } else {
          NA_real_
        }

        incProgress(0.4, detail = "Préparation des noms de grades...")

        nom_grade1_val <- as.character(input$nom_grade1)

        nom_grade2_val <- if (show_grade2() && !is.null(input$nom_grade2) && input$nom_grade2 != "") {
          as.character(input$nom_grade2)
        } else {
          NA_character_
        }

        nom_grade3_val <- if (show_grade3() && !is.null(input$nom_grade3) && input$nom_grade3 != "") {
          as.character(input$nom_grade3)
        } else {
          NA_character_
        }
      })

      incProgress(0.5, detail = "Exécution du calcul de billonnage...")

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

        if (!is.null(input$Sortie) && input$Sortie == "echelle_billon") {
          processed_Simul(processed_Billonage())
        }

        incProgress(1, detail = "Terminé!")
        showNotification("Billonnage calculé avec succès!", type = "message", duration = 3)

      }, error = function(e) {
        cat("✗ Erreur billonnage:", e$message, "\n")
        showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
        processed_Billonage(NULL)
      })
    })
  })

  # Téléchargements
  output$table <- renderTable({
    data()[1:20, ]
  })

  output$download <- downloadHandler(
    filename = function() {
      if (input$RecruesGaules == 1) {
        if (input$Sortie == "echelle_billon") {
          paste("resultat_Gaule", input$Sortie, "_", input$typeBillonnage, "-", input$NbIter, "i_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("resultat_Gaule", input$Sortie, "-", input$NbIter, "i_", Sys.Date(), ".csv", sep = "")
        }
      } else {
        if (input$Sortie == "echelle_billon") {
          paste("resultat_", input$Sortie, "_", input$typeBillonnage, "-", input$NbIter, "i_", Sys.Date(), ".csv", sep = "")
        } else {
          paste("resultat_", input$Sortie, "-", input$NbIter, "i_", Sys.Date(), ".csv", sep = "")
        }
      }
    },
    content = function(file) {
      req(processed_Simul())
      write.table(processed_Simul(), file, sep = ";", row.names = FALSE)
    }
  )

  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (input$graphType == 'Évolution des variables par placette') {
        paste("graphique_évol_", input$variable, "_", input$Espece, "_", Sys.Date(), ".png", sep = "")
      } else {
        paste("graphique_dist_", input$Espece, "_", Sys.Date(), ".png", sep = "")
      }
    },
    content = function(file) {
      plot <- graph_to_print()
      ggsave(file, plot)
    }
  )

  output$simulationStatus <- renderText({
    simulation_status()
  })

  # Téléchargements fichiers d'aide (chemins www)
  output$downloadPDF <- downloadHandler(
    filename = function() { "Aide application R du simulateur SaMARE.pdf" },
    content = function(file) { file.copy("www/Aide application R du simulateur SaMARE.pdf", file) }
  )

  output$downloadPDFtest <- downloadHandler(
    filename = function() { "Test2500m2.csv" },
    content = function(file) { file.copy("data/Test2500m2.csv", file) }
  )

  output$downloadPDFtestgaules <- downloadHandler(
    filename = function() { "GaulesTest2500m2.csv" },
    content = function(file) { file.copy("data/GaulesTest2500m2.csv", file) }
  )

  observeEvent(input$help_btn, {
    updateNavbarPage(session, "all_pages", selected = "Aide")
  })

  observeEvent(input$back_btn, {
    updateNavbarPage(session, "all_pages", selected = "Main")
  })
}

# Fonction utilitaire pour vérification météo
vevifier_variable_meteo <- function(data) {
  select <- dplyr::select
  data <- renommer_les_colonnes(data = data)

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

  if (!is_empty(variables_non_trouvees)) {
    variables_a_extraire <- map_noms_variables[variables_non_trouvees]

    data <- extract_map_plot(file = data, liste_raster = "cartes_climat", variable = variables_a_extraire)

    if ('tmean' %in% variables_a_extraire) {
      data <- rename(data, Tmoy = tmean)
    }

    if ('totalprecipitation' %in% variables_a_extraire) {
      data <- rename(data, Ptot = totalprecipitation)
    }

    if ('growingseasonlength' %in% variables_a_extraire) {
      data <- rename(data, GrwDays = growingseasonlength)
    }
  }

  data <- data %>% rename(Placette = id_pe)

  return(data)
}

#StartApp__________________________________________________________________####
# Note: Ce fichier est lancé via runApp() depuis runShinyApp.R
# Il suffit de retourner l'objet shinyApp
shinyApp(ui, server)
