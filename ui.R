
# User Interface ----------------------------------------------------------

ui <- fluidPage(
  # Add waiter dependencies
  useWaiter(),
  useShinyjs(),
  
  # Add custom waiter
  waiterShowOnLoad(
    html = tagList(
      tags$img(src = "logo2.png", height = "100px"),
      h3("Chargement en cours...", style = "color: white;"),
      spin_flower(),
      p("Préparation du tableau de bord", style = "color: white;")
    ),
    color = "#2c3e50"
  ),
  tags$head(
    tags$style(css),
    includeHTML("google_analytics.html")
  ),
  
  tags$div(
    class = "header-container",
    tags$div(
      class = "header-logos",
      tags$img(src = "logo2.png", height = 59),
      tags$img(src = "logo1.png", height = 49)
    ),
    tags$div(
      class = "header-content",
      tags$div(
        class = "header-title-row",
        tags$h2("Introduction du vaccin antipaludique au Burkina Faso"),
        tags$span(class = "last-update", "Dernière mise à jour: ", textOutput("last_update", inline = TRUE))
      )
    )
  ),br(),
# Tabsetpanel
tabsetPanel(
  id = "panel_tab",br(),
  tabPanel("Niveau National",
           icon = icon("chart-line"),
           fluidRow(
             column(3,
                    div(class = "metric-card",
                        div(class = "metric-value", textOutput("introduction_date")),
                        div(class = "metric-label", icon("calendar-days"), "Date d'introduction du vaccin")
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        div(class = "metric-value", textOutput("days_intro")),
                        div(class = "metric-label", icon("clock"), "Nombre de jours depuis le lancement")
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        div(class = "metric-value", textOutput("total_districts")),
                        div(class = "metric-label", icon("hospital"), "Districts concernés par l'introduction")
                    )
             ),
             column(3,
                    div(class = "metric-card",
                        div(class = "metric-value", textOutput("districts_one_reports")),
                        div(class = "metric-label", icon("square-check"), "Districts avec au moins un rapport")
                    )
             ),
             column(3,
                    div(class = "metric-card2",
                        div(class = "metric-value", textOutput("coverage_vap1")),
                        div(class = "metric-label", icon("syringe"), "Doses administrées et couverture vaccinale VAP1")
                    )
             ),
             column(3,
                    div(class = "metric-card2",
                        div(class = "metric-value", textOutput("coverage_vap2")),
                        div(class = "metric-label", icon("syringe"), "Doses administrées et couverture vaccinale VAP2")
                    )
             ),
             column(3,
                    div(class = "metric-card2",
                        div(class = "metric-value", textOutput("coverage_vap3")),
                        div(class = "metric-label", icon("syringe"), "Doses administrées et couverture vaccinale VAP3")
                    )
             ),
             column(3,
                    div(class = "metric-card2",
                        div(class = "metric-value", textOutput("aefi_total")),
                        div(class = "metric-label", icon("syringe"), "Nombre total de doses de VAP administrées (cumul)")
                    )
             ),
             column(7,
                    div(
                      class = "map-container",
                      shinydashboardPlus::box(
                        title = "Couverture vaccinale VAP par région",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        elevation = 3,
                        height = "650px",
                        div(
                          style = "height: 550px;",
                          leafletOutput("map_coverage", height = "100%")
                        )
                      )
                    )
             ),
             column(5,
                    div(
                      class = "map-container",
                      shinydashboardPlus::box(
                        title = "",
                        status = "primary",
                        solidHeader = TRUE,
                        width = 12,
                        elevation = 3,
                        height = "650px",
                        div(
                          style = "height: 600px;",
                          highchartOutput("evolution_national", height = "550px")
                        )
                      )
                    )
                    
                    
             )
           )
           
           ),
  tabPanel("Niveau Région",
           icon = icon("globe"),br(),
        fluidRow(
          column(2,
                 wellPanel(
                   selectInput("choose_region",
                               "Choisir la région",
                               choices = unique(db_vaccination$orgunitlevel2)
                                           
                               ),br(),
                   p(
                     "Choisir la période d'analyse des données"
                   ),br(),
                   selectInput("startMonth",
                               "Mois de début:",
                               choices = c("2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", 
                                           "2024-06-01", "2024-07-01", "2024-08-01", "2024-09-01","2024-10-01"),
                               selected = "2024-02-01"),
                   
                   selectInput("endMonth",
                               "Mois de fin:",
                               choices = c("2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", 
                                           "2024-06-01", "2024-07-01", "2024-08-01", "2024-09-01","2024-10-01"),
                               selected = "2024-10-01"),hr(),
                   textOutput("selectedPeriod"),
                   textOutput("validationMessage"),br(),
                   selectInput("doseid",
                               "Choisir la dose de vaccin",
                               c("VAP 1","VAP 2","VAP 3")
                               )
                   
                 )     
            ),
          column(3,
                 div(class = "metric-card2",
                     div(class = "metric-value", textOutput("vap1_region")),
                     div(class = "metric-label", 
                         icon("syringe"), 
                         textOutput("label_output_vap1") 
                     )
                 )),
          column(3,
                 div(class = "metric-card2",
                     div(class = "metric-value", textOutput("vap2_region")),
                     div(class = "metric-label", icon("syringe"),
                         textOutput("label_output_vap2"))
                 )),
          column(3,
                 div(class = "metric-card2",
                     div(class = "metric-value", textOutput("vap3_region")),
                     div(class = "metric-label", icon("syringe"),
                         textOutput("label_output_vap3")
                         )
                 )),
          column(10,
                 fluidRow(
                   column(6,highchartOutput("monthly_coverage",height = "500px")),
                   column(6,highchartOutput("coverage_trend",height = "500px")),
                   column(6,highchartOutput("vaccination_strategy",height = "550px")),
                   column(6,highchartOutput("comparison_regions",height = "550px"))
                   
                 )
                 
            )
          
        )
           
    ),
  tabPanel("Niveau district",
           icon = icon("map"),br(),
          fluidRow(
            column(2,
           wellPanel(
             selectInput("choose_region1",
                         "Choisir la région",
                         choices = unique(db_vaccination$orgunitlevel2)
                         
             ),br(),
             selectInput("choose_district",
                         "Choisir le district",
                         choices = c(unique(db_vaccination$orgunitlevel4)
                         )
             ),br(),
             p(
               "Choisir la période d'analyse des données"
             ),br(),
             selectInput("startMonth1",
                         "Mois de début:",
                         choices = c("2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", 
                                     "2024-06-01", "2024-07-01", "2024-08-01", "2024-09-01","2024-10-01"),
                         selected = "2024-02-01"),
             
             selectInput("endMonth1",
                         "Mois de fin:",
                         choices = c("2024-02-01", "2024-03-01", "2024-04-01", "2024-05-01", 
                                     "2024-06-01", "2024-07-01", "2024-08-01", "2024-09-01","2024-10-01"),
                         selected = "2024-10-01"),hr(),
             textOutput("selectedPeriod1"),
             textOutput("validationMessage1")
             
             
           )),
           column(3,
                  div(class = "metric-card",
                      div(class = "metric-value", textOutput("vap1_district")),
                      div(class = "metric-label", icon("syringe"), 
                          textOutput("label_output_vap1_district"))
                  )
           ),
           column(3,
                  div(class = "metric-card",
                      div(class = "metric-value", textOutput("vap2_district")),
                      div(class = "metric-label", icon("syringe"), 
                          textOutput("label_output_vap2_district"))
                  )
           ),
           column(3,
                  div(class = "metric-card",
                      div(class = "metric-value", textOutput("vap3_district")),
                      div(class = "metric-label", icon("syringe"), 
                          textOutput("label_output_vap3_district"))
                  )
           ),
           column(10,
               fluidRow(
                 column(6,highchartOutput("monthly_coverage_district",height = "500px")),
                 column(6,highchartOutput("coverage_trend_district",height = "500px")),
                 column(4,highchartOutput("vap1_doses_strategy")),
                 column(4,highchartOutput("vap2_doses_strategy")),
                 column(4,highchartOutput("vap3_doses_strategy")),
                 column(12,highchartOutput("comparison_district",height = "500px"))
               )
              
          )
           
           
           )
      ),
  tabPanel("Synthèse des performances",
           icon = icon("chart-bar"),
           br(),
           fluidRow(
             column(2,
                    wellPanel(
                      selectInput("choose_month",
                                  "Selectionnez un mois:",
                                  choices = unique(as.Date(db_vaccination$period_date)),
                                selected = max(unique(as.Date(db_vaccination$period_date))))
                      
                    ) 
             ),
             column(10,
                    tags$footer(
                      h5("Légendes:"),
                      tags$ul(
                        tags$li(HTML("<span style='color: green'>&#x2191;</span> Évolution du nombre de doses administrées par rapport au mois précédent")),
                        tags$li(HTML("<span style='color: red'>&#x2193;</span> Diminution du nombre de doses administrées par rapport au mois précédent")),
                        tags$li("Couverture vaccinale cumulative VAP:"),
                        tags$ul(
                          tags$li(HTML("<span style='color: red'>Rouge: < 30%</span>")),
                          tags$li(HTML("<span style='color: orange'>Orange: 30% - 49%</span>")),
                          tags$li(HTML("<span style='color: black'>Jaune: 50% - 79%</span>")),
                          tags$li(HTML("<span style='color: green'>Vert: ≥ 80%</span>"))
                        )
                      ))    
                )
             
            ),
           
    fluidRow(
      column(12,
             DTOutput("table_overall")
             )
    )
           
      
    )
  
  
),
# Footer
tags$footer(
  class = "footer",
  div(
    class = "footer-content",
#   div(
#      class = "footer-section",
#      tags$img(src = "logo1.png", height = 40, style = "margin-right: 15px;"),
#      tags$img(src = "logo2.png", height = 40)
#    ),
    div(
      class = "footer-section",
      h4("Contact"),
      p(a(icon("envelope"), "LOMPO Yempabou", href = "mailto:xavierlompo@yahoo.fr")),
      p(a(icon("envelope"), "Dr Franck Mboussou", href = "mailto:mboussouf@who.int"))
    ),
    div(
      class = "footer-section",
      h4("Liens utiles"),
      tags$ul(
        tags$li(tags$a(href = "https://www.who.int/publications/i/item/who-wer-9919-225-248", "WHO latest position paper on malaria vaccine")),
        tags$li(tags$a(href = "https://www.who.int/news-room/questions-and-answers/item/q-a-on-rts-s-malaria-vaccine#:~:text=What%20do%20we%20know%20about,around%205%20months%20of%20age.", "FAQ sur le paludisme")),
        tags$li(actionLink("about_link", "À propos", class = "footer-link"))
      )
    )
  ) #,
#  div(
#    class = "footer-bottom",
#    p("© 2024 Tous droits réservés")
#  )
)
  
  
  
  
  
  
)