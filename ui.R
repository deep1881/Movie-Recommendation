library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)



title<-tags$strong(icon("play")," Movie Recommendation ")

shinyUI(
    dashboardPagePlus(skin = "blue", 
                      # enable_preloader = TRUE, loading_duration = 0, 
                      md = TRUE,
        dashboardHeader(title = title, titleWidth = 285,
                        
                        dropdownMenu(
                            type="message",
                            
                            
                            messageItem(from='Recomandation Update',message="Upcoming Movie to Look up For..!!",icon=icon("video")),
                            messageItem(from='Prediction Update',message="Fill up the form quickly",icon = icon("couch"),time="22:00"),
                            messageItem(from='Prediction Update',message="Select your taste",icon = icon("route"),time="22:00")
                        ),
                        
                        
                        # dropdownMenu(type="notification",
                        #              notificationItem(
                        #                  text = "2 new tabs added to DashBoard",
                        #                  icon = icon("dashboard"),
                        #                  status = "success"
                        #              ),
                        #              notificationItem(
                        #                  text = "Server is currently running at 95% Load",
                        #                  icon = icon("warning"),
                        #                  status = "warning"
                        #              )
                        #              
                        # ),
                        
                        dropdownMenu(
                            type = "tasks",
                            taskItem(
                                value=30,
                                color = "yellow",
                                "Shiny DashBoard Completion"
                            ),
                            
                            taskItem(
                                value=10,
                                color = "red",
                                "Shiny Recommendation Completion"
                            ), 
                            taskItem(
                                value=10,
                                color = "red",
                                "Shiny Prediction Completion"
                            ) 
                            
                        )
                        
                        
                        
                        
        ),
        
        dashboardSidebar(width = 285,
                        
            
            sidebarMenu(tags$style(HTML(".sidebar-menu li a { font-size: 15px; }")),tags$head(tags$style(".sidebar-menu li { margin-bottom: 10px; }")),
                menuItem("Charts",tabName = "dashboard",icon= icon("chart-pie")),
                menuItem("WatchMojo Suggestions", tabName="WatchMojo",icon = icon("magic")),
                menuItem("Watch Movies & TV Shows!",tabName="IMDB",icon = icon("file-video"),badgeLabel = "Hot!", badgeColor = "red"),
                #menuItem("Search Movies",tabName = "details",icon = icon("th")),
                menuItem("Movies Recommendation",tabName = "rec",icon= icon("film")),
                menuItem("TV Shows Recommendation",tabName = "rec2",badgeLabel = "new!", badgeColor = "green",icon= icon("tv"))
                
            )),
        dashboardBody(
            
        #     setBackgroundImage(
        #     src = "https://cdn1.bbcode0.com/uploads/2021/2/5/9b998871323eb5b9db0bf570b09e4604-full.jpg" , shinydashboard = TRUE
        # ),


            #CustomSkin
        #     tags$head(tags$style(HTML('
        # /* logo */
        # .skin-blue .main-header .logo {
        #                       background-color: #f4b943;
        #                       }
        # 
        # /* logo when hovered */
        # .skin-blue .main-header .logo:hover {
        #                       background-color: #f4b943;
        #                       }
        # 
        # /* navbar (rest of the header) */
        # .skin-blue .main-header .navbar {
        #                       background-color: #f4b943;
        #                       }        
        # 
        # /* main sidebar */
        # .skin-blue .main-sidebar {
        #                       background-color: #f4b943;
        #                       }
        # 
        # /* active selected tab in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
        #                       background-color: #ff0000;
        #                       }
        # 
        # /* other links in the sidebarmenu */
        # .skin-blue .main-sidebar .sidebar .sidebar-menu a{
        #                       background-color: #00ff00;
        #                       color: #000000;
        #                       }
        # 
        # /* other links in the sidebarmenu when hovered */
        #  .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
        #                       background-color: #ff69b4;
        #                       }
        # /* toggle button when hovered  */                    
        #  .skin-blue .main-header .navbar .sidebar-toggle:hover{
        #                       background-color: #ff69b4;
        #                       }
        #                       '))),
            
            tabItems(
                tabItem(
                    tabName = "dashboard",
                    fluidRow(
                        box(title = tags$h2(tags$strong("Welcome ")),width = 12, background = "blue",
                            strong(" Millions of Movies, TV Shows to discover. Explore now!")
                        )
                    ),
                    
                    fluidRow(
                        gradientBox(title="Most Liked Movies",
                            icon = "fa fa-heart",
                            gradientColor = "black", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            footer = plotOutput("barchart2")
                        ),
                        gradientBox(title="Top Rated Movies",
                            icon = "fa fa-star",
                            gradientColor = "black", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            footer = plotOutput("barchart")
                        ),
                        gradientBox(
                            title = "Bar Plot for Netflix DataSet",
                            icon = "far fa-chart-bar",
                            gradientColor = "blue", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            footer = plotOutput("barchart1"),
                            selectInput("Xaxis","Selected X axis",colnames(Netflix),selected = "title_year"),
                            selectInput("Yaxis","Selected Y axis",colnames(Netflix[ , purrr::map_lgl(Netflix, is.numeric)]),selected = "duration")
                        ),
                        
                        
                        gradientBox(
                            title = "Lolipop Plot for Netflix DataSet",
                            icon = "fa fa-clock",
                            gradientColor = "blue", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            varSelectInput("variable1", "Selected X axis:", Netflix,selected = "content_rating"),
                            varSelectInput("variable2", "Selected Y axis:", Netflix[ , purrr::map_lgl(Netflix, is.numeric)],selected = "duration"),
                            footer = plotOutput("lolipop2")
                        ),
                        
                        gradientBox(
                            title = "Pie Chart  for Netflix DataSet",
                            icon = "fa fa-clock",
                            gradientColor = "blue", 
                            boxToolSize = "xs", 
                            closable = TRUE,
                            
                            pickerInput(
                                inputId = "selectMany", 
                                label = "Select / deselect Genre : ", 
                                c(dataForPie$Genre), 
                                selected = c("Action","Romance","Comedy","Drama"),
                                options = list(
                                    `actions-box` = TRUE, 
                                    size = 10,
                                    `selected-text-format` = "count > 3"
                                ), 
                                multiple = TRUE
                            ),
                            
                            # selectInput(
                            #     "selectMany",
                            #     label = "Select Genre (You can choose multiple inputs) :",
                            #     choices = c(dataForPie$Genre),
                            #     selected = c("Action","Romance","Comedy","Drama"),
                            #     multiple = TRUE
                            # ),
                            footer = plotOutput("piechart")
                        )
                        ),
                    fluidRow(
                        widgetUserBox(
                            title = "Durvesh Danve",
                            subtitle = "Student at MIT WPU",
                            type = NULL,
                            src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
                            color = "aqua-active",
                            closable = TRUE,
                            collapsible = TRUE,
                            "MSc Computer Science!",
                            footer = tags$a(href="https://www.linkedin.com/in/durvesh22/","LinkedIn Profile")
                        ),
                        widgetUserBox(
                            title = "Deep Trivedi",
                            subtitle = "Student at MIT WPU",
                            type = NULL,
                            src = "https://adminlte.io/themes/AdminLTE/dist/img/user1-128x128.jpg",
                            color = "aqua-active",
                            closable = TRUE,
                            collapsible = TRUE,
                            "MSc Computer Science!",
                            footer = tags$a(href="https://www.linkedin.com/in/durvesh22/","LinkedIn Profile")
                        )
                      
                    )  
                    ),
                tabItem(
                    tabName="WatchMojo",align="center",
                    tags$h1(tags$b(" Top 10 Must Watch Movies for You : !! ")),
                    HTML('<iframe width="1270" height="720" src="https://www.youtube.com/embed/pdaMBWw8vPY?start=18" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                    
                    tags$h1(tags$b(" Top 10 Action Movies !! ")),
                    HTML('<iframe width="1270" height="720" src="https://www.youtube.com/embed/ITEigec_5Sg?controls=0&amp;start=50" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                    
                    tags$h1(tags$b(" Top 10 Comedy Movies !! ")),
                    HTML('<iframe width="1270" height="720" src="https://www.youtube.com/embed/7YxRZu7ICgg" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                    
                    tags$h1(tags$b(" Top 10 Sci-Fi Movies !! ")),
                    HTML('<iframe width="1270" height="720" src="https://www.youtube.com/embed/YyyEADhvlJ8?start=1" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                    
                    tags$h1(tags$b(" Top 10 Horror Movies !! ")),
                    HTML('<iframe width="1270" height="720" src="https://www.youtube.com/embed/Ecqz8wKCZUU" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'),
                ),
                    
                tabItem( 

                    tabName="IMDB",
                    align="center",
               
                # pickerInput(
                #     inputId = "month",
                #     label = "Select a month",
                #     choices = Poster$Title,
                #     multiple = FALSE,
                #     options = pickerOptions(
                #         actionsBox = TRUE,
                #         title = "Please select a month",
                #         header = "This is a title"
                #     )
                # ),
                    
                HTML('<iframe width="1280" height="720" src="https://openloadmovies.cam/" frameborder="1" allow=encrypted-media" allowfullscreen></iframe>'),
                  
                    # dropdownButton(
                    #     tags$h3("List of Input"),
                    #     selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
                    #     selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
                    #     sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
                    #     circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
                    #     tooltip = tooltipOptions(title = "Click to see inputs !")
                    # ),
                    # knobInput(
                    #     inputId = "myKnob",
                    #     label = "IMDB Ratings :",
                    #     value = 0.0,
                    #     min = 10.0,
                    #     displayPrevious = TRUE,
                    #     lineCap = "round",
                    #     fgColor = "#428BCA",
                    #     inputColor = "#428BCA"
                    # ),
                    # 
                ),
                
                # tabItem(
                #     tabName = "details",align="center",basicPage(
                #         tags$h2(tags$b("Movie Dataset")),
                #         DT::dataTableOutput("mytable")
                #     )
                # ),
                tabItem( class = "tabRec",
                    #CustomSkin
                        tags$head(tags$style(HTML('
                       .tabRec {
  background-image: url("https://i.ibb.co/X8Bn1h4/nbblurr.jpg");
  background-size: 100% 1280px;
  background-repeat: repeat-y;
 
                '))),
            
                    dropdownButton(
                    tags$h3(tags$b("Movie Dataset")),

                    
                    DT::dataTableOutput("mytable2"),
                    circle = TRUE, status = "info", icon = icon("search"), width = "100%",margin = "5px",
                    tooltip = tooltipOptions(title = "Click to check if movie is available in dataset !")
                ), 

                    tabName="rec",
                    
                # withTags({div(class="mov", checked=NA,
                #              h2(span( class="ht" ,"Enter Movie Name!!")),)}),
                # 
                # tags$head(tags$style(HTML('
                # .mov{
                #                  color: white;
                #                  font-style: bold,italic;
                #                  font-size: 40px;
                #                  font-family: Verdana, sans-serif;
                #                  text-transform: uppercase;
                #                  line-height: 1.5;
                #                  text-align: center;
                # }
                #                
                # '))),
                
                withTags({div(class="mov", checked=NA,
                                           h2(span("Enter Movie Name!!")),)}),
                
                tags$head(tags$style(HTML('
                .mov {
                    margin: 0 auto;
                    width: 500px;
  
                }
                h3{
                    
                    font-family: Verdana, sans-serif;
                    color:white;
                
                }
                h2 {
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    text-transform: uppercase;
                    line-height: 1.5;
                    text-align: center;
                    font-size: 40px;
                }
                
                h2 > span {
                    background-color: #D32;
                        color: #FFF;
                        box-shadow: -10px 0px 0 7px #D32,
                    10px 0px 0 7px #D32,
                    0 0 0 7px #D32;
                    box-decoration-break: clone;
                }
                '))),
                
                
                    # tags$h2(tags$b("Enter Movie Name!! "))
                    align="center",
                    tags$head(tags$style("#searchText{color: white;
                                 font-size: 1.8em;
                                 font-weight: bold;
                                 }"
                )
                ),

                
                autocomplete_input(id="searchText", options=Poster$Title,label = NULL, value = "", width = "100%",
                                   placeholder = " For Ex. Interstellar", max_options = 0, hide_values = FALSE),
                
                   # sidebarSearchForm(textId = "searchText", buttonId = "searchButton",label = "Enter Movie Name", icon = shiny::icon("search")),
                # searchInput(
                #     inputId = "searchText", 
                #     label = NULL, 
                #     placeholder = "For Ex. Batman v Superman: Dawn of Justice ", 
                #     btnSearch = icon("search"), 
                #     btnReset = icon("remove"), 
                #     width = "100%"
                # ),
                   
                      br(), br(),
               
                tags$head(tags$style("#rating{color: white;
                                 font-size: 20px;
                                 font-style: bold,italic;
                                 font-family: Verdana, sans-serif;
                                
                                 }"
                )
                ),
                
                tags$head(tags$style("#name{color: white;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 font-family: Verdana, sans-serif;
                                
                                 }"
                )
                ),   
                
                tags$head(tags$style("#genre{color: white;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 font-family: Verdana, sans-serif;
                                 
                                 }"
                )
                ),    
                uiOutput(outputId = "setCss"),
  
                
                     #tags$b(tags$h3("Poster : ",icon("film"))),
                withTags({div(class="movDet1", checked=NA,
                              
                              
                )}),
                
                tags$head(tags$style(HTML('
                .movDet1{
                
                                 color: white;
                                 font-size: 20px;
                                 font-style: bold,bold;
                                 font-family: Verdana, sans-serif;
                                 text-transform: uppercase;
                }'))),
                    
                
                #Movie Poster:
                
                        withTags({div(class="movDet", checked=NA,
                              h3("Poster : ",icon("film")),
                              
                              )}),
                       
                
                tags$head(tags$style(HTML('
                .movDet{
                                 color: white;
                                 font-size: 20px;
                                 font-style: bold,bold;
                                 font-family: Verdana, sans-serif;
                                 
                
                }'))),
                
                
                
                     uiOutput(outputId = "image"), br(),
                
                withTags({div(class="movDet", checked=NA,
                              h3("Name : ",icon("file-signature")),
                              
                )}),
                
                     #tags$b(tags$h3("Name : ",icon("file-signature"))),
                     uiOutput(outputId = "name"), br(), 
                
                withTags({div(class="movDet", checked=NA,
                              h3("IMDB Rating : ",icon("imdb")),
                              
                )}),
                     #tags$b(tags$h3("IMDB Rating : ",icon("imdb"))),
                     uiOutput(outputId = "rating"), br(),
                
                withTags({div(class="movDet", checked=NA,
                              h3("Genre : ",icon("artstation")),
                              
                )}),
                     #tags$b(tags$h3("Genre",icon("artstation"))),
                     uiOutput(outputId = "genre"),br(),
                     # tags$b(tags$h3("IMDB Link",icon("link"))),
                     # uiOutput(outputId = "imdb"),br(),br(),
                
                withTags({div(class="movDet", checked=NA,
                              h3("Cast : ",icon("user-friends")),br(),
                              
                )}),
                #tags$b(tags$h3("Genre",icon("artstation"))),
                fluidRow(
                  column(3,
                uiOutput(outputId = "Cast"),br(),
                ),
                column(3,
                       uiOutput(outputId = "Cast2"),br(),
                ),
                column(3,
                       uiOutput(outputId = "Cast3"),br(),
                ),
                column(3,
                       uiOutput(outputId = "Cast4"),br(),
                )
                
                
                
                ),
                withTags({div(class="movDet", checked=NA,br(),
                              h3("Wanna Stream it!? : ",icon("couch")),
                              
                )}),
                    #tags$h3(tags$b("Wanna Stream it!? ",icon("couch"))),
                    dropdownButton(
                    tags$h3(tags$b("Grab Some Snacks! We've got you covered ")),
                    
                    
                    uiOutput(outputId = "watch1"),
                    circle = TRUE, status = "warning", icon = icon("chevron-circle-down"), width = "100%",
                    tooltip = tooltipOptions(title = "Click here to Stream it !")
                ),
                     
                
                    
                     br(),
                    uiOutput(outputId = "rec"),br(),br(),
                    fluidRow(
                     column(3,uiOutput(outputId = "rec1img"),br(),
                     uiOutput(outputId = "rec1tit"),),
                     column(3,uiOutput(outputId = "rec2img"),br(),
                            uiOutput(outputId = "rec2tit"),),
                     column(3,uiOutput(outputId = "rec3img"),br(),
                            uiOutput(outputId = "rec3tit"),),
                     column(3,uiOutput(outputId = "rec4img"),br(),
                            uiOutput(outputId = "rec4tit"),),
                    )
                    
                   
                    
                    
                ),
                
                tabItem(
                    dropdownButton(
                        tags$h3(tags$b("TV Series Dataset")),
                        
                        
                        DT::dataTableOutput("mytable3"),
                        circle = TRUE, status = "info", icon = icon("search"), width = "100%",
                        tooltip = tooltipOptions(title = "Click to check if TV Show is available in dataset !")
                    ),
                    
                    tabName = "rec2",
               
                    tags$h1(tags$b("Enter TV Show Name!! ")),align="center",

                    tags$head(tags$style("#searchText1{color: white;
                                 font-size: 1.8em;
                                 font-weight: bold;
                                 }"
                    )
                    ),
                    
                    autocomplete_input(id="searchText1", options=TV2$Title,label = NULL, value = "", width = "100%",
                                       placeholder = "For Ex. Game of Thrones", max_options = 0, hide_values = FALSE),
                    
                    
                    br(), br(),
                    
                    tags$head(tags$style("#rating2{color: blue;
                                 font-size: 20px;
                                 font-style: bold,italic;
                                 }"
                    )
                    ),
                    
                    tags$head(tags$style("#name2{color: blue;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 }"
                    )
                    ),   
                    
                    tags$head(tags$style("#genre2{color: blue;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 }"
                    )
                    ), 
                    
                    tags$head(tags$style("#plot{color: blue;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 }"
                    )
                    ), 
                    
                    tags$head(tags$style("#seasons{color: blue;
                                 font-size: 20px;
                                 font-style: italic,bold;
                                 }"
                    )
                    ), 
                    
                    
                    tags$h3(tags$b("Poster : ",icon("film"))),
                    uiOutput(outputId = "image2"), br(),
                    tags$b(tags$h3("Name : ",icon("file-signature"))),
                    uiOutput(outputId = "name2"), br(), 
                    tags$b(tags$h3("Number of Seasons : ",icon("server"))),
                    uiOutput(outputId = "seasons"), br(),
                    tags$b(tags$h3("IMDB Rating : ",icon("imdb"))),
                    uiOutput(outputId = "rating2"), br(),
                    tags$b(tags$h3("Genre",icon("artstation"))),
                    uiOutput(outputId = "genre2"),br(),
                    tags$b(tags$h3("Plot : ",icon("book"))),
                    uiOutput(outputId = "plot"),br(),
                    
                    
                    
                    tags$h3(tags$b("Wanna Stream it!? ",icon("couch"))),
                    dropdownButton(
                        tags$h3(tags$b("Grab Some Snacks! We've got you covered ")),
                        
                        
                        uiOutput(outputId = "watch"),
                        circle = TRUE, status = "warning", icon = icon("chevron-circle-down"), width = "100%",
                        tooltip = tooltipOptions(title = "Click here to Stream it !")
                    ),
                    
                    # HTML('<iframe src="https://giphy.com/embed/UuYpMKLflpDhEkSerd" width="250" height="250" frameBorder="0" class="giphy-embed"></iframe>'),
                    br(),
                    uiOutput(outputId = "rec2"),br(),br(),
                    fluidRow(
                        column(6,uiOutput(outputId = "rec1img2"),br(),
                                 uiOutput(outputId = "rec1tit2"),),
                              #valueBox(uiOutput(outputId = "rec1tit2"),"Movie Title",width = 6, icon = icon("credit-card")),
                            
                        column(6,uiOutput(outputId = "rec2img2"),br(),
                               uiOutput(outputId = "rec2tit2"),),
                    )
                    
                )
                
                
              
                )
                
            )
            
            
            
        )
        
    )