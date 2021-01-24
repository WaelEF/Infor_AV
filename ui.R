source("helpers.R")

dashboardPage(title = "Value at Risk",
              #Logo Ecole
              dashboardHeader(title = tags$a(href='https://waelef.shinyapps.io/Infor_Shiny/',
                                             tags$img(src='Essai.png',height="46px"))),
              
              
              dashboardSidebar(
                
              
                #Inputs du sidebar pour les paramétres de calcul de la VaR
                fileInput("file1", label = strong(h3("Base de Données"))),
                
               
                numericInput("alpha", "Niveau de confiance:",value=95,min=1,max=99),

                numericInput("Jours","Nombre de jours",value=1),

                #### Sidebar
                sidebarMenu(id="tabs",
                            menuItem("Spécification du portefeuille", tabName = "table", icon=icon("table"), selected=TRUE),
                            menuItem("Visualisation",startExpanded = TRUE, tabName="viz", icon=icon("line-chart"),
                                     menuSubItem("Value at Risk Portefeuille", tabName = "var_portefeuille", icon = icon("angle-right")),
                                     menuSubItem("Titre individuel", tabName = "individuel", icon = icon("angle-right")),
                                     
                                     menuSubItem("Value at Risk Individuelle", tabName = "var_ind", icon = icon("angle-right")),
                                     menuSubItem("Test de Normalité", tabName = "exploration", icon = icon("angle-right"))
                                     
                            ),
                            menuItem("Visualisation Avancée", tabName = "analyse_graphique", icon=icon("dashboard"), selected=FALSE),
                            menuItem("À propos", tabName = "info", icon = icon("question"))
                )
              ),
              dashboardBody(
                theme_onenote_modifie,
                
                fluidRow(
                  uiOutput("Box")
                  
                ),
                
                
                tabItems(
                
                tabItem(
                  tabName = "individuel",
                  selectizeInput(inputId = "Choix", label ="Actions à Visualiser",choices=NULL,multiple=TRUE,
                                 options =list( placeholder = 'Titres à examiner')),
                  highchartOutput("hcontainer",height = "500px")

                      ),
                tabItem(
                  tabName = "analyse_graphique",
                  selectizeInput(inputId = "Choix2", label ="Actions à Visualiser",choices=NULL,multiple=FALSE,
                                 options =list( placeholder = 'Titres à examiner')),
                  highchartOutput("hcontainer2",height = "500px")

                ),
               
                                                 ###################################
                                             #############Test de Normalité#############
                                                ###################################
                tabItem(
                  tabName = "exploration",
                        fluidRow(
                          column(4,br(),br(),br(),br(),
                                 selectizeInput(inputId="choix_normal",label="Titres à éxaminer",choices = NULL,multiple=FALSE,
                                                options = list(placeholder ="Titres à éxaminer")),
                                 wellPanel(
                                   strong("H0: Les données suivent une distribution normale."),br(),
                                   strong("Ha: Les données ne sont pas normalement distribuées."),br(),br(),
                                   em("Test de Normalité Shapiro-Wilk:"),
                                   tableOutput("sw"))),
                          column(6,
                      
                            br(),
                            plotOutput("qqplot"),
                            bsPopover("qqplot","Q-Q plot","Si les points sont alignés sur la première bissectrice, alors la distribution suit probablement une loi de distribution gaussienne normalisée.",
                                      trigger="hover",placement="left")))
                          
                          ),
                                                ###################################
                                            ##############VaR Individuelle##############
                                                ###################################
                tabItem(
                  tabName = "var_ind",
                  fluidRow(
                    column(8,
                           
                           selectizeInput(inputId="choix_var",label="Titre à éxaminer",choices = NULL,multiple=FALSE,
                                          options = list(placeholder ="Titre à éxaminer")),
                           fluidRow(
                           valueBoxOutput("Monte_ind"), valueBoxOutput("Histo_ind"),valueBoxOutput("Param_ind")),
                          
                           hr(),
                           plotOutput("histograph"),
                           hr(),
                           valueBoxOutput("Monte_ind_BT"), valueBoxOutput("Histo_ind_BT"),valueBoxOutput("Param_ind_BT")),
                  
                    column(4,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
                           tableOutput("stats"),  wellPanel(
                             strong("H0: La Value at Risk est en adéquation avec les données historiques."),br(),
                             strong("Ha: La Value at Risk est mal estimée."),br(),br(),
                             em("Intervalle de confiance en terme de jours:"),br(),
                             textOutput("intervalle2")
                           )
      
                  )
                  )),
                                                  
                                                  ###################################
                                                  ##### VaR Portefeuille ############
                                                  ###################################
                tabItem(
                  tabName="var_portefeuille",
                 
                  fluidRow(
                    column(8,
                           
                           fluidRow(
                                 
                           valueBoxOutput("Histo_Portefeuille"),valueBoxOutput("Para_Portefeuille"),valueBoxOutput("Monte_Portefeuille")),
                           
                           hr(),
                           plotOutput("histo_portefeuille"),
                           hr(),
                            box(valueBoxOutput("Back_Histo_PF"),valueBoxOutput("Back_Para_PF"),valueBoxOutput("Back_Monte_PF") ,title="Backtesting",solidHeader = TRUE,status="primary",width=12)
                           ),
                    column(4,
                           br(),br(),br(),br(),br(),br(),br(),br(),br(),
                           box( tableOutput("Contenu_PF"),title="Contenu du Portefeuille",solidHeader = TRUE, status="primary",width=8)
                           ),
                    column(4,
                           wellPanel(
                             strong("H0: La Value at Risk est en adéquation avec les données historiques."),br(),
                             strong("Ha: La Value at Risk est mal estimée."),br(),br(),
                             em("Intervalle de confiance en terme de jours:"),br(),
                             textOutput("intervalle1")
                           ))
                  )),
         
                                                  ###################################
                                                  #####Composition du portefeuille###
                                                  ###################################
                tabItem(tabName = "table",
                  fluidRow(
                    column(
                     12,
                      box(width = NULL,uiOutput("allInputs"),
                          actionButton("appendInput", "Ajouter un titre"), 
                          title = "Titres du portefeuille", solidHeader = TRUE, status = "primary")
                  
                          )
                      )
                      ),
                tabItem(tabName = "info",
                        fluidPage(
                          tags$iframe(src = './info.html', 
                                      width = '100%', height = '800px',
                                      frameborder = 0, scrolling = 'auto')
                        )
                )
                
                )
              )
              )

              
