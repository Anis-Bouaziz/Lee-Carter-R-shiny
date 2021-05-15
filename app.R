#### 4DS5 Sujet 3 
#Med Anis Bouaziz
#Khalil Bagbag
#Issam Ben Moussa
#Oussema Ben Hassine
library(shiny)
library(ggplot2)
library(highcharter)
library(timevis)
library(shinyWidgets)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(withr)
library(DT)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)
library(demography)
library(treemap)
UK_data <-
  read.table(
    file = "fltper_1x1.txt",
    header = TRUE,
    skip = 1 ,
    sep = "",
    dec = "."
  )
## 1. header -------------------------------
header <-
  dashboardHeader(
    title = NULL,
    titleWidth = 0,
    
    tags$li(
      class = "dropdown",
      id = "logo",
      style = "font-size:20px",
      
      tags$span(
        strong(
          'Tarification d’une rente viagère et projection de la mortalité par le modèle de Lee-Carter'
        ),
        style = "font-size:20px;color:white;margin-right:30px"
      ),
      tags$span(
        tags$img(src = 'IRAlogo.png', height = '54'),
        tags$img(src = 'Logo_ESPRIT.png', height =
                   '54')
      )
    )
  )


## 2. siderbar ------------------------------
siderbar <-
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = 'sidebar',
      style = "position: relative; overflow: visible;",
      menuItem("Presentation",
               tabName = 'presentation',
               icon = icon('book')),
      menuItem(
        "Données et Description",
        tabName = 'data',
        icon = icon('table'),
        startExpanded = F,
        menuSubItem('Données', tabName = "Données", icon = icon('table')),
        menuSubItem('Description', tabName = "info", icon = icon('info'))
      ),
      menuItem(
        "Modele de Lee Carter",
        tabName = 'modele_lee_carter',
        icon = icon('brain') ,
        startExpanded = F,
        menuSubItem('Description', tabName = "Description", icon = icon('info')),
        menuSubItem(
          'Calibrage',
          tabName = "Estimation_des_parametres",
          icon = icon("cog", lib = "glyphicon")
        ),
        menuSubItem(
          'Lissage',
          tabName = "Lissage",
          icon = icon("screenshot", lib = "glyphicon")
        ),
        menuSubItem(
          'Parametres du modéle',
          tabName = "Plots",
          icon =  icon("signal", lib = "glyphicon")
        ),
        menuSubItem(
          "simulation et projection ",
          tabName = 'simulation_et_projection',
          icon = icon('forward')
        ),
        menuSubItem(
          "Histogramme ",
          tabName = 'Histogramme',
          icon = icon('chart-bar')
        )
      )
      
      ,
      
      menuItem(" Notebook",
               tabName = "notebook",
               icon = icon('code'))
      ,
      
      
      menuItem(" Github",
               href = "https://github.com/Anis-Bouaziz",
               icon = icon('github'))
    )
  )


## 3. body --------------------------------
body <- dashboardBody(
  setBackgroundImage(src = 'bg.png', shinydashboard = TRUE) ,
  style = ('color:white'),
  tags$head(tags$style(
    HTML(
      "#mytable thead {color:white;background-color:rgba(255,255,255,0.5);}"
    )
  )),
  tabItems(
    tabItem(
      tabName = "presentation",
      fluidRow(
        style = "display: flex !important; justify-content: center !important;",
        
        tags$img(src = "IRAlogo.png", style = "width:25%"),
        tags$img(src = "Logo_ESPRIT.png", style = "width:25%")
        
      ),
      fluidRow(
        style = "display: flex !important; justify-content: center !important;",
        
        tags$div(
          style = 'padding:10px',
          h1("Actuariat Vie"),
          h3('Groupe1: 4DS5 '),
          h4('Oussama Ben Hassine'),
          h4('Khalil Bagbag'),
          h4('Issam ben Moussa'),
          h4('Mohamed Anis Bouaziz'),
          h3('Sujet 3:'),
          h3(
            'Tarification d’une rente viagère et projection de la mortalité par le modèle de Lee-Carter '
          ),
          align = 'center'
        )
        
      )
    ),
    tabItem(
      tabName = "Données",
      h2("United Kingdom, Life tables (period 1x1), Females", align =
           'center'),
      DT::dataTableOutput("mytable")
    ),
    tabItem(
      tabName = "info",
      h2("",    align = "center", style = "color:black"),
      
      tags$table(
        border = 5,
        align = 'center',
        width = '80%',
        height = '50%',
        style = "font-size:18px",
        tags$thead(align = 'center',
                   tags$tr(
                     tags$th("column"),
                     tags$th("indication", style = 'text-align: center!important;')
                   )),
        tags$tbody(
          style = 'text-align: center!important;',
          tags$tr(
            tags$td('Year'),
            tags$td("Année ou plage d'années (pour les données de période et de cohorte)")
          ),
          tags$tr(
            tags$td('Age'),
            tags$td(
              "Groupe d'âge pour l'intervalle de n ans entre l'âge exact x et juste avant l'âge exact x+n, où n=1, 4, 5 ou ∞ (intervalle d'âge ouvert)."
            )
          ),
          tags$tr(
            tags$td('m(x)'),
            tags$td('Taux de mortalité central entre les âges x et x+n')
          ),
          tags$tr(
            tags$td('q(x)'),
            tags$td('Probabilité de décès entre les âges x et x+n')
          ),
          tags$tr(
            tags$td('a(x)'),
            tags$td(
              "Durée moyenne de survie entre les âges x et x+n pour les personnes décédées dans l'intervalle"
            )
          ),
          tags$tr(
            tags$td('l(x)'),
            tags$td("Nombre de survivants à l'âge exact x, en supposant que l(0) = 100 000")
          ),
          tags$tr(tags$td('d(x)'),
                  tags$td('Nombre de décès entre les âges x et x+n')),
          tags$tr(
            tags$td('L(x)'),
            tags$td("Nombre d'années-personnes vécues entre les âges x et x+n")
          ),
          tags$tr(
            tags$td('T(x)'),
            tags$td("Nombre d'années-personnes restantes après l'âge exact x")
          ),
          tags$tr(
            tags$td('e(x)'),
            tags$td("Espérance de vie à l'âge exact x (en années)")
          )
        )
      ),
      tags$div(
        align = 'center',
        h2("source: ", align = 'center'),
        tags$a(
          href = "https://www.mortality.org/cgi-bin/hmd/country.php?cntr=GBR&level=2",
          "https://www.mortality.org/cgi-bin/hmd/country.php?cntr=GBR&level=2"
        )
      )
      
      
    ),
    tabItem(
      tabName = "Description",
      tags$div(
        tags$h1('Le modèle Lee – Carter', align = 'center', style = 'font-size:45px;font-weight: bold;color:green'),
        tags$h4(
          "Le modèle Lee – Carter est un algorithme numérique utilisé dans la prévision de la mortalité
          et la prévision de l’espérance de vie. L’entrée dans le modèle est une matrice de taux de mortalité
          par âge classés de façon monotone par le temps, généralement avec des âges en colonnes et des
          années en rangées. La sortie est une autre matrice prévue des taux de mortalité."
          
        )
        ,
        tags$h4(
          "Il s’agit d’une méthode d’extrapolation des tendances passées initialement utilisée
          sur des données américaines, qui est devenue rapidement un standard. La
          modélisation retenue pour le taux instantané de mortalité est la suivante :",
          tags$br(),
          HTML(
            '<center><img src="lee_carter.jpg" style="width:60%;margin-top:10px"></center>'
          )
        ),
        style = 'border:solid 3px;padding:15px'
      )
      
    ),
    tabItem(tabName = "Estimation_des_parametres", fluidRow(
      box(
        title = "Taux de mortalité des femmes au Royaume-Uni ",
        solidHeader = TRUE,
        tags$img(src = 'female_death_rates.png', style = 'width:100%'),
        status = 'success'
      ),
      box(
        title = "Volatilité des taux de mortalité ",
        solidHeader = TRUE,
        tags$img(src = 'volatilite_taux_mortalite.png', style = 'width:100%'),
        status = 'success'
      )
    ),
    fluidRow(column(
      12,
      align = 'center',
      box(
        background = "green",
        solidHeader = TRUE,
        "Mise à part l'âge 0 et 10, le comportement des taux de mortalité reste constant durant toutes les années c'est pour ca qu'on va prendre en considération tous les années 1960 -to- 2018 ,t ∈ [1960, 2018]"
      ),
      box(
        background = "green",
        solidHeader = TRUE,
        "Vu qu'il y'a une forte volatilité des taux de mortalité pour les âges > 100
        c'est pour cette raison que nous allons nous limiter à x ∈ [0, 100]."
      )
      ))),
    
    tabItem(
      style = 'text-align:center',
      
      tabName = "Lissage",
      fluidRow(
        style = "display: flex !important; justify-content: center !important;",
        box(
          width = 7,
          title = 'Lissage',
          status = "success",
          solidHeader = TRUE,
          tags$img(src = "lissage.png", style = "width:100%")
        )
      ),
      
      
      
      fluidRow(
        style = "display: flex !important; justify-content: center !important;",
        box(
          background = "green",
          " mspline (lissage monotone) et spline représente le mieux la variation du taux de mortalité on peut choisir l'un des deux."
        )
      )
      
    ),
    tabItem(
      tabName = "Plots",
      tabsetPanel(
        tabPanel(
          "Parametres Estimés",
          tabsetPanel(
            tabPanel(
              "ax",
              fluidRow(
                box(
                  width = 5,
                  title = 'Ax',
                  status = "success",
                  solidHeader = TRUE,
                  tags$img(src = 'ax2.png', style = "width:100%")
                ),
                style = "display: flex !important; justify-content: center !important;"
              ),
              fluidRow(
                style = "display: flex !important; justify-content: center !important;",
                box(
                  background = "green",
                  " Ce paramètre ax représente la tendance liée à l’effet isolé de l’âge sur les taux de mortalité
                  (moyenne temporelle du logarithme du taux de mortalité par âge). La courbe de ax suit la tendance
                  des courbes des données empiriques. Les âges faibles ont une décroissance jusqu’à atteindre un minimum
                  absolu à de l’âge de 12 ans, puis une croissance exponentielle a partir de l'age de 60 ans ."
                )
                )
                ),
            tabPanel(
              "bx",
              fluidRow(
                box(
                  width = 5,
                  title = 'Bx',
                  status = "success",
                  solidHeader = TRUE,
                  tags$img(src = 'bx2.png', style = "width:100%")
                ),
                style = "display: flex !important; justify-content: center !important;"
              ),
              fluidRow(
                style = "display: flex !important; justify-content: center !important;",
                box(
                  background = "green",
                  " Le paramètre bx représente l'interaction de l'effet des années civiles sur les taux de mortalité. Cet effet est toujours positif mais sa valeur diminue avec l'âge. En d'autres termes, l'effet des années civiles agit surtout avant 30 ans et de moins en moins par la suite. Ceci peut être expliqué par le fait que l'amélioration des conditions de vie et de la médecine ont largement fait baisser la mortalité infantile."
                )
              )
            ),
            tabPanel(
              "kt",
              fluidRow(
                box(
                  width = 5,
                  title = 'Kt',
                  status = "success",
                  solidHeader = TRUE,
                  tags$img(src = 'kt2.png', style = "width:100%")
                ),
                style = "display: flex !important; justify-content: center !important;"
              ),
              fluidRow(
                style = "display: flex !important; justify-content: center !important;",
                box(
                  background = "green",
                  "La courbe de kt (Evolution temporelle du taux de mortalité) est en baisse constante."
                )
              )
            )
            
            
                )
          ),
        tabPanel(
          "Residus du modele",
          fluidRow(
            box(
              width = 7,
              title = 'Residuals',
              status = "success",
              solidHeader = TRUE,
              tags$img(src = 'residuals3.png', style = "width:100%")
            ),
            style = "display: flex !important; justify-content: center !important;"
          )
        )
      )
      
      ,
      align = 'center'
    )
    ,
    tabItem(
      tabName = "simulation_et_projection",
      fluidRow(
        box(
          width = 8,
          title = 'Projection sur 25 ans',
          status = "success",
          solidHeader = TRUE,
          tags$img(src = 'simulation.png', style = "width:100%")
        ),
        box(
          width = 8,
          title = 'log taux de mortalités projetés pour la cohorte d’assurés,',
          status = "success",
          solidHeader = TRUE,
          tags$img(src = 'logmort.png', style = "width:100%")
        ),
        style = "display: flex !important; justify-content: center !important;"
      )
      
    ),
    tabItem(
      tabName = "Histogramme",
      fluidRow(
        box(
          width = 7,
          title = 'Histogramme des esperances de vie',
          status = "success",
          solidHeader = TRUE,
          tags$img(src = 'hist.png', style = "width:100%")
        ),
        style = "display: flex !important; justify-content: center !important;"
      ),
      fluidRow(
        style = "display: flex !important; justify-content: center !important;",
        box(
          background = "green",
          "On remarque que l'esperance de vie de l'année 2010 est meilleure que l'année 2000 cela s'explique par le #developpement du systeme medicale. Par rapport à l'esperance totale on remarque que sa valeur est en dessous
          #des deux autres valeurs puisque ca inclut toutes les autres années ou l'esperance de vie est moins importante."
        )
      )
      
    ),
    tabItem(tabName = "notebook",
            fluidPage(htmlOutput("inc"))
    )
    
    
    )
  )


ui <-
  dashboardPage(header, siderbar, body ,
                useShinyjs(), skin = "green")
# Define server logic required to draw a histogram ----
UK <-
  hmd.mx(
    country = "GBR_NP",
    username = "khalil.bagbag1@esprit.tn",
    password = "1620595063",
    label = "UK"
  )
server <- function(input, output, session) {
  output$mytable = renderDataTable(UK_data ,
                                   options = list(
                                     pageLength = 13,
                                     searching = FALSE ,
                                     lengthChange = FALSE
                                   ))
  
 
  output$inc<-renderUI({includeHTML("Notebook.html")})
  
}
shinyApp(ui = ui, server = server)
