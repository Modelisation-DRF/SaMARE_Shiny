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
                          p("Hugues Power - hugues.power@mrnf.gouv.qc.ca"),
                          p("Filip Havreljuk - Filip.Havreljuk@mrnf.gouv.qc.ca"),
                        
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
      numericInput("Horizon", "Nombre d'années de simulation ( multiple de 5)",min = 0 ,value = NA, step = 5),
      radioButtons("RecruesGaules", "Recrues gaules",
                   choices = list("Oui" = 1, "Non" = 0),
                   selected = 0),
      conditionalPanel(
        condition = "input.RecruesGaules == 1",
        fileInput("gauleFile", "Fichier Gaule doit être téléchargé avant la simulation")
      ),
      
      numericInput("NbIter", "Nombre d'itérations", min = 2,value = NA),
      radioButtons("Billonnage", "Billonnage ?",
                   choices = list("Oui" = "1", "Non" = "0"), # Notez les valeurs comme des chaînes de caractères
                   selected = "0"),
      
      conditionalPanel(
        condition = "input.Billonnage === '1'", # Assurez-vous que la condition utilise une chaîne de caractères
        radioButtons("typeBillonnage", "Type de Billonnage :",
                     choices = list("DHP" = "DHP", "ABCD" = "ABCD", "1234" = "1234", "MSCR" = "MSCR"),
                     selected = "DHP")
      ),
      uiOutput("conditionalButton")
    )
  
  })
  output$dynamic_mainpanel <- renderUI({
    
    tableOutput("table")
  })
  # Lire le fichier CSV avec un point-virgule comme délimiteur
  data <- reactive({
    req(input$file)
   # read.csv(input$file$datapath, sep  = ";")
    read_delim(input$file$datapath, delim   = ";")
  })
  simulation_status <- reactiveVal("Attente de la simulation...")
  
  # Appeler la fonction pour la simulation avec le dataframe lu
  processed_Simul <- reactiveVal(NULL)
  processed_data_ArbreSamare <- reactiveVal(NULL)
  processed_data_DendroSamare <- reactiveVal(NULL)
  processed_data_DendroIterSamare <- reactiveVal(NULL)
  processed_data_htvol <- reactiveVal(NULL)
  processed_absent <- reactiveVal(NULL)
  processed_Billonage <- reactiveVal(NULL)
  reponse_bio<- reactiveVal(NULL)
  graph_to_print <- reactiveVal(NULL)
  graph<- reactiveVal(NULL)
  observeEvent(input$file, {
    processed_absent(trouver_noms_absents(data()))
    
    if (length(processed_absent()) != 0) {
      list_string <- paste(processed_absent(), collapse = ', ')
      output$statut <- renderText({
        HTML("<b>Les colonnes suivantes sont manquantes :</b>")
      })
      output$error <- renderText({
        HTML(paste("<span style='color:red;'>", list_string, "</span>"))
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
      if (!is.null(input$file)  && !is.na(input$Horizon) && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==0 && length(processed_absent()) == 0) {
        actionButton("simulateButton", "Simulation")
      }else if(!is.null(input$file)  && !is.na(input$ Horizon) && !is.na(input$NbIter) && input$NbIter > 1 && input$RecruesGaules ==1 && !is.null(input$gauleFile) && length(processed_absent()) == 0){
        
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

    output$dynamic_sidebar <- renderUI({
    sidebarPanel(
      h3("Simulation en cours..."),
    )
    })

      withProgress(message = 'Simulation en cours...', value = 0, {
       
        if (input$RecruesGaules == 1) {
          # # Vérifier si un fichier Gaule a été téléchargé
          
          gaule_data <- reactive({
            req(input$gauleFile)
            # read.csv(input$gauleFile$datapath, sep  = ";")
            read_delim(input$gauleFile$datapath, delim   = ";")
            
          })
          processed_Simul(SimulSaMARE(NbIter=input$NbIter,AnneeDep = 0, Horizon= (input$Horizon/5),RecruesGaules= input$RecruesGaules, Data = data(), Gaules=gaule_data() ))
        } else {
          processed_Simul(SimulSaMARE(NbIter=input$NbIter,AnneeDep = 0, Horizon= (input$Horizon/5),RecruesGaules= input$RecruesGaules, Data = data()))
        } 
        
        if(input$Billonnage == '1' && !is.null(input$Billonnage)) {
          req(input$typeBillonnage)
          processed_Billonage(SIMBillonnageABCD_DHP(data= processed_Simul(), type = input$typeBillonnage))
        }
        reponse_bio(input$Billonnage)
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
        selectInput("Espece", "Groupe d'espece", choices = c("" )),
        selectInput("variable", "choix de la variable", choices = c("ST_HA", "Vol_HA", "DQM","HDomM", "nbTi_HA")),
        downloadButton("downloadPlot", "Télécharger le Graphique"),
        tags$br(),
        radioButtons("graphType", "Type de graphique à créer:",
                     choices = list("Évolution des variables" ,
                                    "Distribution diamétrale" )),
        tags$br(),
        radioButtons("simplifier", "Synthétiser ?",
                     choices = list("Oui" = TRUE, "Non" = FALSE),
                     selected = FALSE),
        selectInput("Sortie", "Type de sortie", choices = c("Sélectionner" = "")),
        downloadButton("download", "Télécharger le résultat")
        
      )
    
    })

    # output$dynamic_mainpanel <- renderUI({
    # 
    #   plotOutput("plot")
    # })
    
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
  
  observe({
    req(reponse_bio())
    if(reponse_bio() == '1' ) {

      updateSelectInput( session = session,
                         inputId = "Sortie",
                         label = "Choisis le sortie",
                         choices = c("Arbre par itération", "sommaire dendrometrique par itération", "sommaire dendrometrique par placcette", "à l'échelle du billon"))
    } else {
      # Optionnel: réinitialiser les choix ou ajuster selon les besoins
      updateSelectInput(session = session,
                        inputId = "Sortie",
                        label = "Choisis le sortie",
                        choices = c("Arbre par itération", "sommaire dendrometrique par itération", "sommaire dendrometrique par placcette"))
    }
  })
  #   
  #   
  #   observeEvent(input$typeBillonnage,{
  #     if(input$Billonnage == '1' && !is.null(input$Billonnage)) {
  #     processed_Billonage(SIMBillonnageABCD_DHP(data= processed_Simul(), type = input.Billonnage))
  #     }
  #     simulation_status(input$typeBillonnage)
  #   })
  #   
  # })

  
  observe({
    # Assignation directe pour simplifier la lecture
    sortie <- input$Sortie
    simplifier <- input$simplifier
    
    # Vérifie si sortie est NULL ou non défini. Si oui, sortez de la fonction observe.
    # Ceci prévient l'exécution de switch avec une expression non valide.
    if(is.null(sortie) || sortie == ""){
      return()
    }
    
    # Exécution de switch avec une valeur valide de sortie
    switch(sortie,
           "Arbre par itération" = {
             processed_Simul(SortieArbreSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "sommaire dendrometrique par placcette" = {
             processed_Simul(SortieDendroSamare(SimulHtVol = processed_data_htvol(), simplifier = simplifier))
           },
           "à l'échelle du billon" = {
             processed_Simul(processed_Billonage())
           },
           "sommaire dendrometrique par itération" = {
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
      data()
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
  
  # observeEvent(input$help_btn, {
  #   hide("main_app")  
  #   show("help_page")  
  # })
  # 
  # observeEvent(input$back_btn, {
  #   show("main_app")  
  #   hide("help_page")  
  # })
  
  observeEvent(input$help_btn, {
    updateNavbarPage(session, "all_pages", selected = "Aide")
  })
  
  observeEvent(input$back_btn, {
    updateNavbarPage(session, "all_pages", selected = "Main")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
