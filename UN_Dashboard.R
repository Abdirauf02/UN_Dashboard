#import library and packages. 
#install.packages("devtools")
#devtools::install_github("ewenme/shinya11y")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinycssloaders)
library(plotly)
library(shinya11y)

#Input the data
sdg_data <- load("SDG_data.RData")

#input table for user guide
#A table for the Users guid 
table1 <- read_csv("www/Tabledesc.csv")


#provides the ditinct countries code
sdg_new %>% 
  select(country_code) %>% 
  distinct(country_code) -> country_code_choice

#provides the distinct country name
sdg_new %>% 
  select(country) %>% 
  distinct(country) -> country_choice

#provides the distinct year
sdg_new %>% 
  select(year) %>% 
  distinct(year) -> year

#provides the SDG scores of the data
sdg_new %>% 
  select(sdg_index_score,starts_with("goal_"))->scores

#Our User Interface
ui <- function(request){
  
  #calls the dashboard page
  dashboardPage(
    #calls the dashboard header 
    dashboardHeader(
      # the title with the strathclyde university logo 
      title = span(img(src="uni_logo.jpeg",width=110,height=60, alt= "image of university")," UN-SDG")
    ),
    #calls the dashboard sidebar
    dashboardSidebar(
      #Dynamic sidebar menu output
      sidebarMenuOutput("sidebar")
    ),
    #calls dashboard body 
    dashboardBody(
      #Uses shinya11y tota11y to see the accessibility
      use_tota11y(), 
      #uses the cssfile in the www file for customization
      includeCSS("www/style.css"),
      #contain the tabs 
      tabItems(
        tabItem( #first tab
          tabName = "home_tab",
          fluidRow(
            #centers the logo
            div(tags$img(src="UN_logo.jpeg",width=300,height=150, 
                         alt= "Image of United Nations Logo"), style="text-align: center;"),
            #centers the Welcome
            div(tags$h1("Welcome!"),style="text-align: center;"),
            box(
              width = 12,
              background = "blue",
              #centers the header
              div(tags$h2("Sustainabilty Development Report"),style="text-align: center;"),
              #description of SDR
              tags$par("The Sustainabilty Development Report (SDR) reviews the progress
              made each year on the Sustainabilty Development Goals (SDG) for SDG 2030.
              The 193 United Nation (UN) Member States have made their adoptions 
              to these goals in 2015. The SDR was published on the eve of the 
              2023 Paris Summit for New Global Financial Pact."),
              solidHeader = FALSE,
              collapsible = TRUE,
              collapsed = FALSE
              
            ),
            box(
              width = 12,
              background = "blue",
              #centers the header
              div(tags$h2("The SDR dataset"),style="text-align: center;"),
              #description of SDR dataset
              tags$par("The SDR datasets comprises of data related to the sustainabilty and
              progress towards the SDGs from 166 countries of the United Nations Regional Group (UNRG)
              from the years 2000 to 2023. The country sustainabilty score is out of 100, which the name 
              of the ragion and its classification, and a breakdown of the individual
              regions scores across 17 different goals."),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE
            ),
            box(
              width = 12,
              background = "blue",
              #centers the header
              div(tags$h2("United Nations Association Dashboard"),style="text-align: center;"),
              #description of SDR dataset
              tags$par("This dashboards monitors and how different countries across the world 
              are progressing towards their SDGs. The main elements in this are the Progression of the countries, 
              Continential comparison with the United Nations Regional Groups."),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE
            ),
            box(
              width = 12,
              background = "blue",
              #centers the header
              div(tags$h2("Download Dataset"),style="text-align: center;"),
              #output of the download link
              uiOutput("URL"),
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE
            )
            
          )
        ),
        tabItem(
          tabName = "plots", #first mini tab
          fluidRow(
            box(
              #centers the header
              div(tags$h2("Progression of a Countries SDG scores"),style="text-align: center;"),
              width = 12, 
              #selects a country code input
              selectInput(
                "countryId", "Select Country Code", choices = country_code_choice,
              ),
              #Produces an output of the country name 
              uiOutput("idtoname1"),
              tags$br(),
              #selects a relevant SDG score input
              selectInput(
                "scores", "Select the SDG score", choices = c("sdg_index_score",
                                                              "goal_1_score","goal_2_score", "goal_3_score","goal_4_score",
                                                              "goal_5_score","goal_6_score", "goal_7_score", "goal_8_score",
                                                              "goal_9_score","goal_10_score", "goal_11_score","goal_12_score",
                                                              "goal_13_score","goal_14_score","goal_15_score","goal_16_score",
                                                              "goal_17_score"
                ),
                selected = TRUE
              ),
            ),
            box(
              width = 12,
              #centers the header
              div(tags$h3("Plots of the Progression of Countries"),style="text-align: center;"),
              column(
                width = 6,
                #plots a interactive line graph with spinners 
                withSpinner(plotlyOutput("timeseries"),type = 4, color = "blue",
                            size=1.5,proxy.height = "200px")
              ),
              column(
                width = 6,
                #plots a interactive box plot with spinners 
                withSpinner(plotlyOutput("box"),type = 4, color = "blue",
                            size=1.5,proxy.height = "200px")
              )
            )
            
          ),
          #first bookmark
          bookmarkButton(id="bookmark_graphs"),
        ),
        tabItem( # second mini tab
          tabName = "tables",
          fluidRow(
            box(
              #centers the header
              div(tags$h2("Progression of a Countries SDG scores"),style="text-align: center;"),
              
              width = 12, 
              #Selects the country code input
              selectInput(
                "countryId", "Select Country Code", choices = country_code_choice,
              ),
              #shows the country name 
              uiOutput("idtoname2"),
              tags$br(),
              #Check box of SDG score 
              checkboxGroupInput(
                "scores1", "Select the SDG score (Maximum 7 boxes)", choices = c("sdg_index_score",
                                                                                 "goal_1_score","goal_2_score", "goal_3_score","goal_4_score",
                                                                                 "goal_5_score","goal_6_score", "goal_7_score", "goal_8_score",
                                                                                 "goal_9_score","goal_10_score", "goal_11_score","goal_12_score",
                                                                                 "goal_13_score","goal_14_score","goal_15_score","goal_16_score",
                                                                                 "goal_17_score"
                ),
                selected = TRUE, inline = TRUE, 
              )
            ),
            box(
              width = 12,
              #centers the header
              div(tags$h3("Table of progression of countries"),style="text-align: center;"),
              #produces a data table with spinners
              withSpinner(dataTableOutput("table"),type = 4, color = "blue",
                          size=1.5,proxy.height = "200px") 
            )
          ),
          #second bookmark
          bookmarkButton(id="bookmark_table"),
        ),
        
        tabItem(# Third Tab
          tabName ="UNRG_tab",
          fluidRow(
            box(
              width=12,
              #centers the header
              div(tags$h3("United Nations Regional Group Map"),style="text-align: center;"),
              #second header information of the scores and selection 
              tags$h4("This will show the overall SDG scores across the world. Select the region you want to investigate"),
              #slects an the continent needed inputs 
              selectInput("continent", "Select Continent:", choices = c("world","africa", "asia", "europe", 
                                                                        "north america", "south america","oceania"), 
                          selected = "world"),
              #header for selection 
              tags$h4("Select two years"),
              column(
                width = 6,
                #selects the first year
                selectInput("year", "Select first year:", choices = year, selected = "2000")
              ),
              column(
                width = 6,
                #selects the second year input
                selectInput("year1", "Select second year:", choices = year, selected = "2001")
              ),
              #action button to produce the maps
              div(actionButton("button","Apply", icon = icon("circle-check")),style="text-align: center;"),
              
              column(
                width = 6,
                #plots first interactive map with spinners
                withSpinner(plotlyOutput("maps"),type = 4, color = "green",
                            size=1.5,proxy.height = "200px")
              ),
              column(
                width = 6,
                #plots second interactive map with spinners
                withSpinner(plotlyOutput("maps1"),type = 4, color = "green",
                            size=1.5,proxy.height = "200px")
              )
            ),
          ),
          #thirds bookmard
          bookmarkButton(id="bookmark_maps"),
        ),
        tabItem(
          tabName = "user", #fourth tab
          fluidRow(
            box(
              #Users guide 
              width = 12,
              div(tags$h2("Users Guide"), style="text-align: center;"),
              tags$h3("General Information"),
              tags$h4("For the users trying to navigate this dashboard, this is a 
                      simple guide to help facilitate your experience as a user. 
                      The data is provided by the United Nation’s Sustainability 
                      Development Plan (SDP). The SDP reviews goals of the Sustainability
                      Development goals (SDG) of each member country since their 
                      adoption in 2013. The SDR dataset comprises of sustainable 
                      data and progression of the SDG of the 166 member states from 
                      United Nation Regional Groups. The United Nations Regional 
                      Group has five continental states which are Africa, Asia-Pacific,
                      Eastern European, Western European and Latin America and 
                      Caribbean States. Within these states we have numerous member 
                      countries. The Home page has more information and a good 
                      introduction to the dashboard. Note that the boxes are 
                      collapsible, and we also have a box where anyone can download the dataset."),
              tags$br(),
              tags$h3("Purpose"),
              tags$h4("The data has a dimension of 3984 by 21, the 21 columns 
                      are shown in Table below. The Table provides the description 
                      of the SDG data. The first three columns are countries 
                      code, the countries name, and the year. we have the SDG 
                      scores, the first being the overall score and then the 17
                      different scores as shown in the Table. The main purpose of 
                      this dashboard is to show the differences in the UN member 
                      countries SDG scores and how each country has developed over the last 23 years."),
              tags$br(),
              tableOutput("desc"),
              tags$br(),
              tags$h3("Navigating the App"),
              tags$h4("The app first opens on the 'Home' tab where we have the 
                      UN logo and four blue boxes. The boxes have a simple 
                      description of the main goals of the UN's SDP, The dataset
                      and the dashboard’s purpose. We also download the dataset 
                      if needed to explore yourself. " ),
              tags$br(),
              tags$h4("The dashboard has five items in the sidebar. The home tab, 
                      the progression of countries tab with two sub tabs which 
                      are the plot and table. The United Nations Regional Group 
                      tab, the link to download the dataset and the link for sharing."),
              tags$br(),
              tags$h4("The progression of countries has two different sub tabs the
                      first is the plots and the second is the table. The plot tab 
                      allows you to choose a country code and the SDG score. 
                      This will plot two different interactive plots the first 
                      being a line graph of the years against the scores chosen. 
                      The second plot being an interactive boxplot of a scores of 
                      the given country. This shows the distribution and summary 
                      statistics of the countries chosen SDG scores throughout the
                      years. Notice if you don’t know a desired countries code the 
                      names are provided the selection of the country code."),
              tags$br(),
              tags$h4("The table tab shows the countries code, and again the name
                      of the country is given upon selection.  We see we have 18
                      different check boxes. This is to compare how the different 
                      scores compare to one another. We can only select 7 boxes. 
                      If you exceed this number, the table resets to zero."),
              tags$br(),
              tags$h4("The United Nations Regions Group tab compares the SDG 
                      overall scores of seven different regions, and you can 
                      select all but the Oceania. Please choose two different 
                      years and you will see the differences in the region’s 
                      numbers change in the world map. The world maps are also 
                      interactive meaning you can zoom in and out for a better 
                      look and understanding."),
              tags$br(),
              tags$h3("Interactive Elements"),
              tags$h4("There are five interactive elements as mentioned before.
                      The line graph, the box plot, the data table and the two 
                      maps are interactive. There were produced by plot.ly (ver. 2.11.1).
                      You can zoom in and out, download it as an image (.png) file, 
                      pan through it, auto scale it, reset the axis, show the closets 
                      data when you hover and compare the data when you hover. 
                      The data table is also somewhat interactive where you can 
                      click on a row, and it will highlight it in blue. If you
                      hover over rows this will highlight the row in a grey colour."),
              tags$br(),
              tags$br(),
              tags$h3("Thank you for reading this user guide.")
              
              
            )
            
          )
        )
      )
    )
  )
  
}

server <- function(input, output, session) {
  Sys.sleep(2) #timer to see the spinners 
  #Reactive data with the inputs of countryId 
  country_react <- reactive(
    sdg_new %>% 
      select(country_code,country,year,sdg_index_score,starts_with("goal_")) %>% 
      group_by(year, country_code) %>% 
      filter(input$countryId %in% country_code)
  )
  
  #Url link for downloading the data set 
  url <- a("DATASET ", href="https://classes.myplace.strath.ac.uk/mod/resource/view.php?id=1974841",style="color:#e7f3ff;")
  output$URL <- renderUI({
    tagList("Can be downloaded from  :", url, )
  })
  #Reactive data with the inputs of country name
  countryonly <- reactive(
    sdg_new %>% 
      distinct(country_code,country) %>% 
      filter(input$countryId == country_code) %>% 
      select(country)
  )
  
  #Outputs the first country name 1
  output$idtoname1 <- renderUI(
    countryonly()
  )
  #Outputs the second country name 
  output$idtoname2 <- renderUI(
    countryonly()
  )
  
  #outputs the reactive plot_ly line graph 
  output$timeseries <- renderPlotly(
    country_react() %>% 
      plot_ly(x =~year, y=as.formula(paste0("~",input$scores)), type = "scatter", mode = "line" ) %>% 
      layout(title = list(text = "Line plot of the progression of SDG from 2000 to 2023"),
             xaxis = list(title = list(text ="Years")))
  )
  #outputs the reactive plot_ly box plot
  output$box <- renderPlotly(
    country_react() %>% 
      plot_ly(x =~country, y=as.formula(paste0("~",input$scores)), type = "box") %>% 
      layout(title = list(text = "Boxplot of the progression of SDG score"),
             xaxis = list(title = list(text ="Country")))
  )
  #outputs the reactive data table 
  output$table <- renderDataTable({
    country_react() %>% 
      select(country_code,country,year,input$scores1)
  },options = list(filter="none",scrollX = FALSE, pageLength = 15),
  )
  
  #If the observed checkbox selection is greater than 7, this will rest the table 
  observe({
    if(length(input$scores1) > 7){
      updateCheckboxGroupInput(session, "scores1", selected = FALSE)
    }
    
  })
  #outputs the reactive first plot_geo map in green colour
  observe({ 
    output$maps <- renderPlotly(
      plot_geo(sdg_new) %>%
        add_trace(
          z = ~sdg_index_score[year == input$year], locations = ~country_code[year == input$year] ,
          color = ~sdg_index_score[year == input$year], colors = 'Greens'
        ) %>%
        colorbar(title = "") %>% 
        layout(geo = list(
          validate(need(input$continent %in% c("world","africa", "asia", "europe", 
                                               "north america", "south america"), "ERROR: Location needs to be created, 
                      Please select world and zoom into oceania")),
          scope = input$continent,
          projection = list(type = "natural earth")),
          title = list(text ="Map of world")
        )
    )
  }) %>% 
    bindEvent(input$button) #triggers map when button is hit 
  
  #outputs the reactive second plot_geo map in green colour
  observe({
    output$maps1 <- renderPlotly(
      plot_geo(sdg_new) %>%
        add_trace(
          z = ~sdg_index_score[year == input$year1], locations = ~country_code[year == input$year1] ,
          color = ~sdg_index_score[year == input$year1], colors = 'Greens'
        ) %>%
        colorbar(title = "") %>% 
        layout(geo = list(
          validate(need(input$continent %in% c("world","africa", "asia", "europe", 
                                               "north america", "south america"), "ERROR: Location needs to be created, 
                      Please select world and zoom into oceania")),
          scope = input$continent,
          projection = list(type = "natural earth")),
          title = list(text ="Map of world")
        )
    )
  })%>% 
    bindEvent(input$button)#triggers map when button is hit 
  
  #produces the table fo information purposes
  output$desc <- renderTable(
    na.omit(table1), #omits the na variable in the table
    table1$`Column Number` <- as.integer(table1$`Column Number`)
  )
  
  #outbut the dynamic side bar 
  output$sidebar <- renderMenu({
    sidebarMenu(
      id= "side",
      menuItem("Home",tabName = "home_tab",icon = icon("house")),
      menuItem("Progression of Countries", tabName = "scores_tab",icon = icon("chart-simple"),startExpanded = TRUE,
               menuSubItem("Plots", tabName = "plots", icon = icon("chart-line")),
               menuSubItem("Table", tabName = "tables", icon = icon("table-list"))
      ),
      
      menuItem("United Nations Regional Group", tabName = "UNRG_tab", icon = icon("earth-africa")),
      menuItem("User Guide",tabName = "user", icon = icon("info")),
      menuItem("Download data here",href="https://classes.myplace.strath.ac.uk/mod/resource/view.php?id=1974841",
               newtab = TRUE,icon = icon("download")),
      menuItem("Share",href =  "http://127.0.0.1:4946", 
               newtab = TRUE,icon = icon("share-nodes"))# if the link of the webpage changes please update it here 
    )
  })
  

  #observation of the bookmarking 
  observe(session$doBookmark()) %>% bindEvent(input$bookmark_graphs)
  observe(session$doBookmark()) %>% bindEvent(input$bookmark_table)
  observe(session$doBookmark()) %>% bindEvent(input$bookmark_maps)
}

shinyApp(ui, server,enableBookmarking = "url")



