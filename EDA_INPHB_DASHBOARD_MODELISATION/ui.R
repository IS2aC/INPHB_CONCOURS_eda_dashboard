library(shiny)
library(shinydashboard)


dashboardPage(skin = "red",
  #header
  dashboardHeader( title = "Analyse descriptive INPHB-2017/2018",
                   titleWidth = 400),
  #sidebar
  dashboardSidebar(
    #sidebarmenu
    sidebarMenu(
      id = "sidebar",
      
      #menuItems
      menuItem("Dataset", tabName = "data", icon = icon("database")),
      menuItem(text = "Dashboard ", tabName = "viz", icon=icon("dashboard")),
      #dropdown annee -------------
      selectInput("annee", "Annee", c("tous" = "tous","2017" = "2017","2018" = "2018"),width = 100),
      selectInput("etat", "Etat", c("admissible" = "ADMISSIBLE","admis" = "ADMIS"),width = 150),
      selectInput("genre", "genre", c("tous" =  "tous", "masculin" = "M", "feminin" = "F")),
      selectInput("note", "note", c("moyenneTest" = "moyenneTest", 
                                            "ANGLAIS"="anglais",
                                            "CULTURE GENERALE" = "cg",
                                            "CulTURE SCIENTIFIQUE" = "cs")),
      menuItem(text = "Prediction", tabName = "pred", icon=icon("chart-line"))
    )
  ),
  
  #body -- dashboard
  dashboardBody(
    tabItems(
      #item -- data--------------------------------------------------------------------------
      tabItem(tabName = "data",
              #tab box
              tabBox(id="t1", width = 40,
                     tabPanel("A propos",icon = icon("address-card"),
                              fluidRow(
                                column(8, tags$img(src="C:/Users/AGRIDEM/Desktop/PROJET_TUTORE/logoProjet.JPG", width =600 , height = 300),
                                       tags$br()
                                       ),
                                column(4, tags$br() ,
                                       tags$p("Chaque annee, le service des concours et mobilites de  la sous-direction de la scolarite, de l'acceuil et de l'information organise des concours en vu de recruter ses futurs etudiants. Nous allons  nous interesser au concours niveau BAC. Ce concours permet au nouveau bachelier(le Baccalaureat  de l'annee en cours) d'integrer  l'INPHB en preparant les concours DTS(Diplome de technicien Superieur)  ou en integrant les ecoles preparatoires de l'INPHB. Ainsi il s'agira pour nous d'analyser et de modeliser des donnees des candidats au concours d'entree a l'INPHB des annees 2017 et 2018", height=200)
                              
                                      )
                              )),
                     #tabPanel(title = "Data" ,icon = icon("address-card"), ),
                     tabPanel("Dictionnaire des variables",icon = icon("address-card"), tableOutput("dtT")),
                     tabPanel("Resume des variables", icon = icon("address-card"), verbatimTextOutput("summarize"))
                     #tabPanel(title = "Summary stats",icon = icon("address-card"), )
              )
      ),
      
      #item -- vizualisation------------------------------------------------
      tabItem(tabName = "viz",
              #tabBox
              tabBox(id="t2", width=15,
                     #first-panel
                     tabPanel("Statistiques generales", value = "gen", icon =  icon("chart-line"),
                              #valueBox
                              fluidRow(
                                #inscrits
                                valueBoxOutput("inscrBox"),
                                
                                #admissibles
                                valueBoxOutput("admissBox"),
                                
                                #admis
                                valueBoxOutput("admBox"),

                                #nombre des colleges/lycees presents au concours
                                valueBoxOutput("colbox"),
                                
                                #Pourcentage de  candidature feminine
                                valueBoxOutput("candfem"),
                                
                                #DREN representes / nombre d'abandons
                                valueBoxOutput("nombreChoix")

                              ),
                              
                              #bar-plot des candidatures internationales
                              fluidRow(box(plotOutput("inter")),
                                       column(6,
                                              #barplot des candidatures H/F
                                              box("Candidature homme-femme",
                                                  plotOutput("plot1"), width = 60)
                                       )
                              ),
                              
                              #Pyramide des ages aux concours
                              fluidRow(box(plotOutput("plot2")))
                     ),
                     
                     #secondpanel
                     tabPanel("Process admissions",value = "admiss", icon = icon("chart-line"),
                              fluidRow(
                                column(6,
                                       plotOutput("plot4")
                                       ),
                                column(6,
                                       #top5 de preferences
                                       plotOutput("nuageA")
                                       )
                              )
                     ),
                     
                     #thirdpanel
                     tabPanel("Statistiques sur les admis", value = "adm", icon =icon("chart-line"),
                              #top3 school
                              fluidRow(
                                column(6,
                                       h4("Top 3 des meilleurs ecoles au concours"),
                                       box(plotOutput("plot3"), width = 15)
                                       ),
                                column(6,
                                       h4("Nombre d'admis par filiere d'acceuil"),
                                       box(plotOutput("piechart"), width = 15)
                                       )
                              ),
                              
                              fluidRow(
                                column(6,
                                       #treemap des series -- pour les admis
                                      box(h4("#"),
                                        plotOutput("treemap"), width=15)
                                      ),
                                column(6,
                                       #distribution des notes au test
                                       box(h4("Boxplot des etudiants"),
                                           plotOutput("boxtest"), width = 15)
                                       ),
                              )
                              
                              
                     )
              )
       ),
      
      #item -- prediction ---------------------------------------------------
      tabItem(tabName = "pred",
              fluidRow(
                column(6,
                       h4("formulaire de prediction"),
                       box(
                         sliderInput("moyenneDossier", "MOYENNE DE DOSSIER", min = 0, max = 20, value = 0),
                         sliderInput("anglais", "ANGLAIS : ", min = 0, max = 20, value = 0),
                         sliderInput("cg", "CULTURE GENERALE : ", min = 0, max = 20, value = 0),
                         sliderInput("cs", "CULTURE SCIENTIFIQUE : ", min = 0, max = 20, value = 0),
                         actionButton("go", "VALIDER")
                       )
                       ),
                column(6,
                       h4("reponse du modele"),
                       box(
                          textOutput("value"),
                          height = 300
                          )
                      )
              )
              
      )
              
              
      )
    )
  )
