#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(shinyjs)

# Define UI for application that draws a histogram

ui <- fluidPage(
  useShinyjs(),  # Initialise shinyjs
  tags$head(
    tags$style(HTML("
      .sidebar {
        background-color: #00FF00; /* Vert lumineux comme exemple */
      }
    "))
  ),
  navbarPage("Application", id = "all_pages",
             tabPanel("Main",
                      div(id = "main_app",  # Div pour l'application principale
                               # titlePanel("Samare"),

                                titlePanel(img(src = "SaMARE_MRNF_Final.jpg", height = 150, width = 270)),
                          #helpText("Version 2.1.0"),
                          tags$div(style = "font-size: 10px;", "Version 2.1.0"),
                        #  HTML("<small>Version 2.1.0"),
                                sidebarLayout(

                                  uiOutput("dynamic_sidebar"),
                                  # Afficher le contenu du fichier CSV dans un tableau
                                  mainPanel(
                                    uiOutput("dynamic_mainpanel"),
                                    #tableOutput("table")
                                    
                                    
                                    
                                  )
                                )
                            )
             ),
             tabPanel("Aide",
                      div(id = "help_page",
                          h2("Page d'Aide"),
                          p("Pour information contacter :"),
                          p("recherche forestière - recherche.forestiere@mrnf.gouv.qc.ca"),
                          downloadButton("downloadPDF", "Télécharger le PDF")
                        
                      )
             )
  )
)

# 
# ui <- fluidPage(
#   tags$head(
#     tags$style(HTML("
#       .sidebar {
#         background-color: #00FF00; /* Vert lumineux comme exemple */
#       }
#     "))
#   ),
#   useShinyjs(),  # Initialise shinyjs
#   div(id = "main_app",  # Div pour l'application principale
#      # titlePanel("Samare"),
# 
#       titlePanel(img(src = "SaMARE_MRNF_Final.jpg", height = 150, width = 270)),
#      actionButton("help_btn", "Aide"),
#       sidebarLayout(
#         
#         uiOutput("dynamic_sidebar"),
#         # Afficher le contenu du fichier CSV dans un tableau
#         mainPanel(
#           uiOutput("dynamic_mainpanel"),
#           #tableOutput("table")
#         )
#       )
#   ),
#   hidden(  # Cache la page d'aide par défaut
#     div(id = "help_page",  # Div pour la page d'aide
#         h2("Page d'Aide"),
#         p("Pour information contacter :"),
#         p("Hugues Power - hugues.power@mrnf.gouv.qc.ca"),
#         p("Filip Havreljuk - Filip.Havreljuk@mrnf.gouv.qc.ca"),
#         actionButton("back_btn", "Retour")
#     )
#   )
#  
# )


# Définir le serveur
server <- function(input, output,session) {
 
  output$dynamic_sidebar <- renderUI({
     
    sidebarPanel(
      fileInput("file", "Choisissez un fichier CSV"),
      htmlOutput("statut"),
      htmlOutput("error"),
      htmlOutput("statut_data"),
      htmlOutput("error_data"),
      numericInput("Horizon", "Nombre d'années de simulation ( multiple de 5)",min = 0 ,value = NA, step = 5),
      radioButtons("RecruesGaules", "Recrutement Basé sur les gaules",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0),
      conditionalPanel(
        condition = "input.RecruesGaules == 1",
        fileInput("gauleFile", "Fichier Gaule doit être téléchargé avant la simulation")
      ),
      htmlOutput("statut_data_gaules"),
      htmlOutput("error_data_gaules"),
      numericInput("NbIter", "Nombre d'itérations", min = 2,value = NA),
     
      
      uiOutput("conditionalButton")
    )
  
  })

  # Lire le fichier CSV avec un point-virgule comme délimiteur
  data <- reactive({
    req(input$file)
   # read.csv(input$file$datapath, sep  = ";")
    read_delim(input$file$datapath, delim   = ";")
  })
  gaule_data <- reactive({
    req(input$gauleFile)
    # read.csv(input$gauleFile$datapath, sep  = ";")
    read_delim(input$gauleFile$datapath, delim   = ";")
    
  })
  simulation_status <- reactiveVal("Attente de la simulation...")
  
  # Appeler la fonction pour la simulation avec le dataframe lu
  processed_Simul <- reactiveVal(NULL)
  processed_data_ArbreSamare <- reactiveVal(NULL)
  processed_data_DendroSamare <- reactiveVal(NULL)
  processed_data_DendroIterSamare <- reactiveVal(NULL)
  processed_data_htvol <- reactiveVal(NULL)
  processed_absent <- reactiveVal(NULL)
  processed_absent_gaules <- reactiveVal(NULL)
  valider_data <- reactiveVal(NULL)
  valider_data_gaules <- reactiveVal(NULL)
  processed_Billonage <- reactiveVal(NULL)
  graph_to_print <- reactiveVal(NULL)
  graph<- reactiveVal(NULL)
  vevifier_meteo<- reactiveVal(NULL)
  
  observeEvent(input$file, {
    
    output$dynamic_mainpanel <- renderUI({
      
      tableOutput("table")
    })
    processed_absent(trouver_noms_absents(data()))
    valider_data(valide_data(data()))
    
    if (length(processed_absent()) != 0) {
      list_string <- paste(processed_absent(), collapse = ', ')
      output$statut_data <- renderText({
        HTML("<b>Les colonnes suivantes sont manquantes :</b>")
      })
      output$error_data <- renderText({
        HTML(paste("<span style='color:red;'>", list_string, "</span>"))
      })
    } else {
      output$statut_data <- renderText({
        HTML("")
      })
      output$error_data <- renderText({
        HTML("")
      })
      
      
     
      
      
      output$conditionalButton <- renderUI({
        if (!is.null(input$file) && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1) {
          actionButton("simulateButton", "Simulation")
        }
      }) 
    }
    
    if (length(valider_data()) != 0) {
      list_string1 <- paste(valider_data(), collapse = ', ')
      output$statut <- renderText({
        HTML("<b>Les erreurs suivantes ont été détectées :</b>")
      })
      output$error <- renderText({
        HTML(paste("<span style='color:red;'>", list_string1, "</span>"))
      })
    } else {
      output$statut <- renderText({
        HTML("")
      })
      output$error <- renderText({
        HTML("")
      })
      
      
      
      
      
      output$conditionalButton <- renderUI({
        if (!is.null(input$file) && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1) {
          actionButton("simulateButton", "Simulation")
        }
      }) 
    }
    
    
    
  })
  

  observeEvent(input$gauleFile, {
    processed_absent_gaules(trouver_noms_absents_gaules(gaule_data()))
    valider_data_gaules(valide_data_gaules(gaule_data()))
    
    if (length(processed_absent_gaules()) != 0) {
      list_string_gaules <- paste(processed_absent_gaules(), collapse = ', ')
      output$statut_data_gaules <- renderText({
        HTML("<b>Les colonnes suivantes sont manquantes :</b>")
      })
      output$error_data_gaules <- renderText({
        HTML(paste("<span style='color:red;'>", list_string_gaules, "</span>"))
      })
    } else {
      output$statut_data_gaules <- renderText({
        HTML("")
      })
      output$error_data_gaules <- renderText({
        HTML("")
      })
      
      
      
      
      
      output$conditionalButton <- renderUI({
        if (!is.null(input$file) && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1) {
          actionButton("simulateButton", "Simulation")
        }
      }) 
    }
    
    if (length(valider_data()) != 0) {
      list_string1 <- paste(valider_data(), collapse = ', ')
      output$statut <- renderText({
        HTML("<b>Les erreurs suivantes ont été détectées :</b>")
      })
      output$error <- renderText({
        HTML(paste("<span style='color:red;'>", list_string1, "</span>"))
      })
    } else {
      output$statut <- renderText({
        HTML("")
      })
      output$error <- renderText({
        HTML("")
      })
      
      
      
      
      
      output$conditionalButton <- renderUI({
        if (!is.null(input$file) && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1) {
          actionButton("simulateButton", "Simulation")
        }
      }) 
    }
    
    
    

  })

  
  
  observe({
    # Réagir aux changements dans les inputs
    input$RecruesGaules
    input$Horizon
    input$NbIter
   
    output$conditionalButton <- renderUI({
      if (!is.null(input$file)  && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==0 && length(processed_absent()) == 0 && length(valider_data()) == 0) {
        actionButton("simulateButton", "Simulation")
      }else if(!is.null(input$file)  && !is.na(input$ Horizon) && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==1 && !is.null(input$gauleFile) && length(processed_absent()) == 0 && length(valider_data()) == 0){
        
        actionButton("simulateButton", "Simulation")
      }
    })
  })
  
  

  # observeEvent(input$file, {
  #   
  #  colonnes_abs<-trouver_noms_absents(data())
  #  if(length(colonnes_abs) ==0){
  #    
  #    abs=TRUE
  #    
  #  }else{
  #    
  #    abs=FALSE
  #    
  #  }
  #  
  #   
  # })
  
  observeEvent(input$simulateButton, {
    simulation_status("Simulation en cours...")
   
    vevifier_meteo(vevifier_variable_meteo(data()))
    output$dynamic_sidebar <- renderUI({
    sidebarPanel(
      h3("Simulation en cours..."),
    )
    })

      withProgress(message = 'Simulation en cours...', value = 0, {
       
        if (input$RecruesGaules == 1) {
          # # Vérifier si un fichier Gaule a été téléchargé
          
         
          processed_Simul(SimulSaMARE(NbIter=input$NbIter, Horizon= (input$Horizon/5),RecruesGaules= input$RecruesGaules, Data = vevifier_meteo(), Gaules=gaule_data() ))
        } else {
          processed_Simul(SimulSaMARE(NbIter=input$NbIter, Horizon= (input$Horizon/5),RecruesGaules= input$RecruesGaules, Data = vevifier_meteo()))
        } 
        
        
        Sys.sleep(5) 
        
       
        simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")
      })
   
    
   
     
   
    processed_data_htvol(processed_Simul())
    # processed_data_ArbreSamare(SortieArbreSamare(SimulHtVol = processed_Simul()))
    # processed_data_DendroSamare(SortieDendroSamare(SimulHtVol = processed_Simul()))
    # processed_data_DendroIterSamare(SortieDendroIterSamare(SimulHtVol = processed_Simul()))
    
    
    output$dynamic_sidebar <- renderUI({
      sidebarPanel(
        
        textOutput("simulationStatus"),
        tags$br(),
        radioButtons("graphType", "Type de graphique à créer:",
                     choices = list("Évolution des variables" ,
                                    "Distribution diamétrale" )),
        tags$br(),
        selectInput("Espece", "Groupe d'espece", choices = c("" )),
        selectInput("variable", "choix de la variable", choices = c("ST_HA", "Vol_HA", "DQM","HDomM", "nbTi_HA")),
        downloadButton("downloadPlot", "Télécharger le Graphique"),
        tags$br(),
        tags$br(),
        radioButtons("simplifier", "Toutes les années de simulation",
                     choices = list("Oui" = FALSE, "Non" = TRUE),
                     selected = FALSE),
        selectInput("Sortie",
                    label = "Choisis la sortie",
                    choices = c("Arbre par itération" = "arbre", "sommaire dendrometrique par itération" = "itera", "sommaire dendrometrique par placcette" = "placette", "à l'échelle du billon"="billon")
      ),
      conditionalPanel(
        condition = "input.Sortie == 'billon'", 
        selectInput("typeBillonnage", "Type de Billonnage :",
                     choices = list("DHP_Régionalisé" = "DHP", "ABCD_Régionalisé" = "ABCD", "1234_2015" = "1234", "MSCR_2015" = "MSCR","DHP_2015" = "DHP2015", "ABCD_2015" = "ABCD2015")
                    )
      ),
        downloadButton("download", "Télécharger le résultat"),
      tags$br(),
      tags$br(),
      actionButton("reset_button", "Réinitialiser"),
        
      )
    
    })

    output$dynamic_mainpanel <- renderUI({
      
     
       
        tabsetPanel(type = "tabs",
                    tabPanel("Graphique", plotOutput("plot")),
                    tabPanel("Table", tableOutput("table"))
        )
      
    })
    
    simulation_status("Simulation terminée. Vous pouvez maintenant télécharger le résultat.")
    
  })
  
  observeEvent(input$Espece,{
    
    graph(Graph(SimulHtVol = processed_data_htvol(), Espece = input$Espece, Variable = input$variable))
  })
  
  observeEvent(input$variable,{
    
    graph(Graph(SimulHtVol = processed_data_htvol(), Espece = input$Espece, Variable = input$variable))
  })
  
  output$plot <- renderPlot({
    # Ici, on suppose que `Graph` renvoie une liste de deux éléments graphiques
    
    graphiques <- graph()
    
    # Utiliser input$graphType pour décider quel graphique afficher
    if(input$graphType == "Évolution des variables") {
      graph_to_print (graphiques[[1]])
    } else {
      graph_to_print(graphiques[[2]])
    }
    
    # Afficher le graphique sélectionné
    print(graph_to_print())
  })
  
  observe({
    listeEspece <-unique(processed_data_htvol()$GrEspece)
    listeEspece2 <- append("TOT",listeEspece)
    updateSelectInput( session = session,
                       inputId = "Espece",
                       label = "Groupe d'espece",
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
    removeModal()
    output$dynamic_mainpanel <- renderUI({
     NULL
    })
    output$dynamic_sidebar <- renderUI({
      
      sidebarPanel(
        fileInput("file", "Choisissez un fichier CSV"),
        htmlOutput("statut"),
        htmlOutput("error"),
        htmlOutput("statut_data"),
        htmlOutput("error_data"),
        numericInput("Horizon", "Nombre d'années de simulation ( multiple de 5)",min = 0 ,value = NA, step = 5),
        radioButtons("RecruesGaules", "Recrutement Basé sur les gaules",
                     choices = list("Oui" = 1, "Non" = 0),
                     selected = 0),
        conditionalPanel(
          condition = "input.RecruesGaules == 1",
          fileInput("gauleFile", "Fichier Gaule doit être téléchargé avant la simulation")
        ),
        htmlOutput("statut_data_gaules"),
        htmlOutput("error_data_gaules"),
        numericInput("NbIter", "Nombre d'itérations", min = 2,value = NA),
        
        
        uiOutput("conditionalButton")
      )
    
      
    })
    
    
    
    
  })
  #   
  #   observeEvent(input$typeBillonnage,{
  #     if(input$Billonnage == '1' && !is.null(input$Billonnage)) {
  #     processed_Billonage(SIMBillonnageABCD_DHP(data= processed_Simul(), type = input.Billonnage))
  #     }
  #     simulation_status(input$typeBillonnage)
  #   })
  #   
  # })

  observeEvent(input$Sortie,{
    
    if(input$Sortie == "billon" && !is.null(input$Sortie)) {
      req(input$typeBillonnage)
      processed_Billonage(SIMBillonnageABCD_DHP(data= processed_data_htvol(), type = input$typeBillonnage))
    }
  })
  
  observeEvent(input$typeBillonnage,{
    if(input$Sortie == "billon" && !is.null(input$Sortie)) {
      processed_Billonage(SIMBillonnageABCD_DHP(data= processed_data_htvol(), type = input$typeBillonnage))
    }
  })
  
  observe({
    
    
    req(input$Sortie)
    sortie <- input$Sortie
    simplifier <- input$simplifier
    
    
  
    if(is.null(sortie) || sortie == ""){
      return()
    }
    
    # Exécution de switch avec une valeur valide de sortie
    switch(sortie,
           "arbre" = {
             processed_Simul(SortieArbreSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "placette" = {
             processed_Simul(SortieDendroSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "billon" = {
             processed_Simul(processed_Billonage())
           },
           "itera" = {
             processed_Simul(SortieDendroIterSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
         
           default = {
             # Gérer le cas par défaut. Peut-être afficher un message ou effectuer une autre action par défaut.
           }
    )
  })
  
  
  
  # Afficher le contenu du fichier CSV dans un tableau
  output$table <- renderTable({
    if (is.null(processed_Simul()) && is.null(data()) ){
      return()
    } else if(is.null(processed_Simul())){
      data()
      }else{
      processed_Simul()
    }
  })
  
  # Télécharger le résultat après simulation 
  output$download <- downloadHandler(
    filename = function() {
      if (input$RecruesGaules == 1) {
        return("resultat_Gaule.csv")
      } else {
        return(paste("resultat_", Sys.Date(), ".csv", sep = ""))
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
      paste("graphique-", Sys.Date(), ".png", sep="")
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
      "test.pdf"
    },
    content = function(file) {
      file.copy("WWW/SuiviHoraireVariable_janvier_Junior_Peumi.pdf", file)
    }
  )
  
  observeEvent(input$help_btn, {
    updateNavbarPage(session, "all_pages", selected = "Aide")
  })
  
  observeEvent(input$back_btn, {
    updateNavbarPage(session, "all_pages", selected = "Main")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
