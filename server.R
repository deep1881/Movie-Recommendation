#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.

library(shiny)
library(shinyWidgets)


shinyServer(function(input, output) {
    
    output$barchart <- renderPlot({
        
        my_bar <- data1  %>% mutate(name = fct_reorder(name, value)) %>% ggplot(aes(x=name, y=value)) + geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +coord_flip() + xlab("") + ylab("IMDB Ratings") + geom_label(aes(y = data1$value, label = round(data1$value, 2))) 
        my_bar
        
    }) 
    
    output$barchart1 <- renderPlot({
        bar2<-tapply(Netflix[,input$Yaxis],list(Netflix[,input$Xaxis]),sum)
        
        barplot(bar2,xlab = as.vector(input$Xaxis),ylab =as.vector(input$Yaxis),col = "blue" , border = "sky blue")
    }) 
    
    output$barchart2 <- renderPlot({
        
        my_bar2 <- data2  %>% mutate(name = fct_reorder(name, value)) %>% ggplot(aes(x=name, y=value)) + geom_bar(stat="identity", fill="blue", alpha=.6, width=.4) +coord_flip() + xlab("") + ylab("Movie Likes") + geom_label(aes(y = data2$value, label = round(data2$value, 2))) 
        my_bar2
        
    }) 
    
    output$lolipop2 <- renderPlot({
        ggplot(Netflix, aes(!!input$variable)) + geom_histogram()
        ggplot(Netflix, aes(x=!!input$variable1, y=!!input$variable2)) +
            geom_segment( aes(x=!!input$variable1, xend=!!input$variable1, y=0, yend=!!input$variable2), color="purple") +
            geom_point( color="blue", size=4, alpha=0.6) +
            theme_light() +
            coord_flip() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank()
            )
    }) 
    
    output$lolipopchart <- renderPlot({
        ggplot(data4, aes(x=Rating, y=Duration)) +
            geom_segment( aes(x=Rating, xend=Rating, y=0, yend=Duration), color="skyblue") +
            geom_point( color="blue", size=4, alpha=0.6) +
            theme_light() +
            coord_flip() +
            theme(
                panel.grid.major.y = element_blank(),
                panel.border = element_blank(),
                axis.ticks.y = element_blank()
            )

    }) 
    
    output$piechart = renderPlot({

      dataForPie2 <- dataForPie[dataForPie$`Genre` %in% input$selectMany,]
      
      shades<-c("#034694","#2a52be","#1F75FE","#6495ED","#00BFFF")
      pier<-pie3D(dataForPie2$count,labels = dataForPie2$Genre, main = "Pie Chart of Movies",explode=0.1, radius=.9, labelcex = 1.2,  start=0.7,col=shades)
      pier +  scale_fill_brewer(palette = "Blues")
    })
    

    
    output$mytable = DT::renderDataTable(options = list(scrollX = TRUE),filter = 'top',escape = F,editable = TRUE,{
        Netflix2
    })
    
    output$mytable2 = DT::renderDataTable(options = list(scrollX = TRUE),filter = 'top',escape = F,editable = TRUE,{
      Poster
    })
    
    output$mytable3 = DT::renderDataTable(options = list(scrollX = TRUE),filter = 'top',escape = F,editable = TRUE,{
      TV2
    })
    
    # output$filtered_table <- renderTable({
    #     req(input$searchButton == TRUE)
    #     Movies<-data.table(Netflix2)
    #     Movies[movie_title %like% input$searchText]
    # })

        
    #Movies
    output$image <- renderUI({
        req(input$searchText)
        m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
        res<-Poster[(Poster$Title == m[1]),]
        imgLink<-res$Poster
        imgLink
        
        tags$img(src = imgLink,height="275px", width="225px")
    })
    
    output$setCss <- renderUI({
      tags$head(tags$style(HTML('
                                  b {
                                      color: white;

                                  }
        ')))
    })
    
    output$name <- renderUI({
        req(input$searchText)
        m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
        res<-Poster[(Poster$Title == m[1]),]
        titleLink<-res$Title
        titleLink
  
        #tags$h3(tags$b(titleLink))
        
        
        withTags({div(class="movDet11", checked=NA,
                      h3(span(titleLink)),
                      
                      tags$head(tags$style(HTML('
                .movDet11 {
                    margin: 0 auto;
                    width: 500px;
  
                }
                
                h3 {
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2.0;
                    text-align: center;
                    font-size: 30px;
                }
                
                h3 > span {
                    background-color: #D32;
                        color: #FFF;
                        box-shadow: -10px 0px 0 7px #D32,
                    10px 0px 0 7px #D32,
                    0 0 0 7px #D32;
                    box-decoration-break: clone;
                }
                '))),
                      
        )})
 
    })
    
    
    output$rating <- renderUI({
        req(input$searchText)
        m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
        res<-Poster[(Poster$Title == m[1]),]
        imdbScoreLink<-res$IMDB.Score
        imdbScoreLink
        
        #tags$h3(tags$b(imdbScoreLink))
        withTags({div(class="movDet12", checked=NA,
                      h3(span(imdbScoreLink)),
                      
                      tags$head(tags$style(HTML('
                .movDet12 {
                    margin: 0 auto;
                    width: 500px;
  
                }
                
                h3 {
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2.0;
                    text-align: center;
                    font-size: 30px;
                }
                
                h3 > span {
                    background-color: #D32;
                        color: #FFF;
                        box-shadow: -10px 0px 0 7px #D32,
                    10px 0px 0 7px #D32,
                    0 0 0 7px #D32;
                    box-decoration-break: clone;
                }
                '))),
        )})
        
    })
    
    output$genre <- renderUI({
        req(input$searchText)
        m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
        res<-Poster[(Poster$Title == m[1]),]
        
        genreLink<-res$Genre
        genreLink
       
        #tags$h3(tags$b(genreLink))
        withTags({div(class="movDet13", checked=NA,
                      h3(span(genreLink)),
                      
                      tags$head(tags$style(HTML('
                .movDet13 {
                    margin: 0 auto;
                    width: 500px;
  
                }
                
                h3 {
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2.0;
                    text-align: center;
                    font-size: 30px;
                }
                
                h3 > span {
                    background-color: #D32;
                        color: #FFF;
                        box-shadow: -10px 0px 0 7px #D32,
                    10px 0px 0 7px #D32,
                    0 0 0 7px #D32;
                    box-decoration-break: clone;
                }
                '))),
        )})

        
    })
    
    output$imdb <- renderUI({
        req(input$searchText)
        m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
        res<-Poster[(Poster$Title == m[1]),]
        iLink<-res$Imdb.Link
        iLink
        
        tags$h2(tags$b(tags$a(href=iLink, "Click here to see more details!")))
        
    })
    
    
    output$Cast <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      res<-Poster[(Poster$Title == m[1]),]
      titleLink<-res$Title
      titleLink1<-gsub("\\(|\\)", "", titleLink)
      titleLink1<-removeNumbers(titleLink1)
      
      #complete web scrapping code
      link1<-"https://www.rottentomatoes.com/m/"
      titleLink1<-tolower(titleLink1)
      
      
      trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
      }
      
      titleLink1<-trim(titleLink1)
      titleLink1<-gsub(" +","_",titleLink1)
      titleLink1<-gsub("-","_",titleLink1)
  
      url<-paste0(link1,titleLink1)
      webpage <- read_html(url)
      
      # t<-html_nodes(webpage,'.posterImage')
      # 
      # 
      # t1<-stringi::stri_extract_all_regex(t, '(?<=").*?(?=")')
      # t2<-unlist(t1)
      # t2
      # url<-t2[3]
      # url
      
      castImg<-html_nodes(webpage,'#movie-cast :nth-child(1)')
      
      c<-castImg %>% html_nodes("img")
      
      c<-stri_extract_all_regex(c, '(?<=").*?(?=")')
      
      c2<-unlist(c)
      cast1<-c2[1]
    
      tags$img(src = cast1 ,height="200px", width="175px")
      
    })
    
    output$Cast2 <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      res<-Poster[(Poster$Title == m[1]),]
      titleLink<-res$Title
      titleLink1<-gsub("\\(|\\)", "", titleLink)
      titleLink1<-removeNumbers(titleLink1)

      #complete web scrapping code
      link1<-"https://www.rottentomatoes.com/m/"
      titleLink1<-tolower(titleLink1)


      trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
      }

      titleLink1<-trim(titleLink1)
      titleLink1<-gsub(" +","_",titleLink1)
      titleLink1<-gsub("-","_",titleLink1)

      url<-paste0(link1,titleLink1)
      webpage <- read_html(url)

      castImg<-html_nodes(webpage,'#movie-cast :nth-child(1)')

      c<-castImg %>% html_nodes("img")

      c<-stri_extract_all_regex(c, '(?<=").*?(?=")')

      c2<-unlist(c)

      cast2<-c2[4]

      
      tags$img(src = cast2 ,height="200px", width="175px")
      
    })
    
    output$Cast3 <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      res<-Poster[(Poster$Title == m[1]),]
      titleLink<-res$Title
      titleLink1<-gsub("\\(|\\)", "", titleLink)
      titleLink1<-removeNumbers(titleLink1)
      
      #complete web scrapping code
      link1<-"https://www.rottentomatoes.com/m/"
      titleLink1<-tolower(titleLink1)
      
      
      trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
      }
      
      titleLink1<-trim(titleLink1)
      titleLink1<-gsub(" +","_",titleLink1)
      titleLink1<-gsub("-","_",titleLink1)
      
      url<-paste0(link1,titleLink1)
      webpage <- read_html(url)
      
      castImg<-html_nodes(webpage,'#movie-cast :nth-child(1)')
      
      c<-castImg %>% html_nodes("img")
      
      c<-stri_extract_all_regex(c, '(?<=").*?(?=")')
      
      c2<-unlist(c)

      cast3<-c2[7]

      tags$img(src = cast3 ,height="200px", width="175px")
      
    })
    
    output$Cast4 <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      res<-Poster[(Poster$Title == m[1]),]
      titleLink<-res$Title
      titleLink1<-gsub("\\(|\\)", "", titleLink)
      titleLink1<-removeNumbers(titleLink1)
      
      #complete web scrapping code
      link1<-"https://www.rottentomatoes.com/m/"
      titleLink1<-tolower(titleLink1)
      
      
      trim <- function( x ) {
        gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
      }
      
      titleLink1<-trim(titleLink1)
      titleLink1<-gsub(" +","_",titleLink1)
      titleLink1<-gsub("-","_",titleLink1)
      
      url<-paste0(link1,titleLink1)
      webpage <- read_html(url)
      
      castImg<-html_nodes(webpage,'#movie-cast :nth-child(1)')
      
      c<-castImg %>% html_nodes("img")
      
      c<-stri_extract_all_regex(c, '(?<=").*?(?=")')
      
      c2<-unlist(c)
      
      cast4<-c2[10]
      
      tags$img(src = cast4 ,height="200px", width="175px")
      
    })
    
    output$rec <- renderUI({
        req(input$searchText)
        #tags$h1(tags$b("Recommendations For you!!"))
        withTags({div(class="movDet1", checked=NA,
                      h2(span("Recommendations for you!! ")),
                      
                      tags$head(tags$style(HTML('
                .movDet1 {
                    margin: 0 auto;
                    width: 500px;
  
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
                      
        )})
    
    })
    
    # Movie Recommendations
    output$rec1img <- renderUI({
      req(input$searchText)

      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)

      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Poster
      imglinkn<-r5[1]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec1tit <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Title
      r6<-r4$Imdb.Link
      titlelinkn<-r5[1]
      iLink<-r6[1]
      #tags$h3(tags$b(titlelinkn))
      
      withTags({div(class="movTit1", checked=NA,
                    (h3(b(titlelinkn))),
                    
                    tags$head(tags$style(HTML('
                .movTit1 {
                    margin: 0 auto;
    
                }
                
                h3 {
                    font-style: bold;
                    color: white;
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2;
                    text-align: center;
                    font-size: 25px;
                }

                '))),
                    
      )})
      
    })
    
    output$rec2img <- renderUI({
      req(input$searchText)
      
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Poster
      imglinkn<-r5[2]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec2tit <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Title
      titlelinkn<-r5[2]
      r6<-r4$Imdb.Link
      iLink<-r6[1]
      #tags$h3(tags$b(titlelinkn))
      #tags$h3(tags$b(titlelinkn))
      
      withTags({div(class="movTit2", checked=NA,
                    (h3(b(titlelinkn))),
                    
                    tags$head(tags$style(HTML('
                .movTit2 {
                    margin: 0 auto;
    
                }
                
                h3 {
                    font-style: bold;
                    color: white;
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2;
                    text-align: center;
                    font-size: 25px;
                }

                '))),
                    
      )})
    })
    
    output$rec3img <- renderUI({
      req(input$searchText)
      
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Poster
      imglinkn<-r5[3]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec3tit <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Title
      titlelinkn<-r5[3]
      r6<-r4$Imdb.Link
      iLink<-r6[1]
      #tags$h3(tags$b(titlelinkn))
      #tags$h3(tags$b(titlelinkn))
      withTags({div(class="movTit3", checked=NA,
                    (h3(b(titlelinkn))),
                    
                    tags$head(tags$style(HTML('
                .movTit3 {
                    margin: 0 auto;
    
                }
                
                h3 {
                    font-style: bold;
                    color: white;
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2;
                    text-align: center;
                    font-size: 25px;
                }

                '))),
                    
      )})
      
    })
    
    output$rec4img <- renderUI({
      req(input$searchText)
      
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
      n<-m[1]
      Poster2 <- subset(Poster2, Poster2$Title!=n)
      
      r3<-which(Poster2$Genre == g)
      r4<-Poster2[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Poster
      imglinkn<-r5[4]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec4tit <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      r2<-Poster[(Poster$Title == m[1]),]
      
      g<-r2$Genre 
      
  
      r3<-which(Poster2$Genre == g)
      r4<-Poster[r3,]
      r4<-r4 %>% arrange(desc(IMDB.Score))
      r4 <- r4[-1,]
      r5<-r4$Title
      titlelinkn<-r5[4]
      r6<-r4$Imdb.Link
      iLink<-r6[1]
      #tags$h3(tags$b(titlelinkn))
      #tags$h3(tags$b(titlelinkn))
      withTags({div(class="movTit4", checked=NA,
                    (h3(b(titlelinkn))),
                    
                    tags$head(tags$style(HTML('
                .movTit4 {
                    margin: 0 auto;
    
                }
                
                h3 {
                    font-style: bold;
                    color: white;
                    margin: 0;
                    padding: 0;
                    font-family: Verdana, sans-serif;
                    line-height: 2;
                    text-align: center;
                    font-size: 25px;
                }

                '))),
                    
      )})
      
    })
    
    #TV Shows
    output$image2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      imgLink<-res$Poster
      imgLink
      
      tags$img(src = imgLink,height="275px", width="225px")
    })
    
    output$name2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      titleLink<-res$Title
      titleLink
      
      tags$h3(tags$b(titleLink))
      
      
    })
    
    output$seasons <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      titleLink<-res$totalSeasons
      titleLink
      
      tags$h3(tags$b(titleLink))
      
      
    })
    
    output$rating2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      imdbScoreLink<-res$imdbRating
      imdbScoreLink
      
      tags$h3(tags$b(imdbScoreLink))
      
    })
    
    output$genre2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      
      genreLink<-res$Genre
      genreLink
      
      tags$h3(tags$b(genreLink))
      
    })
    
    output$plot <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      titleLink<-res$Plot
      titleLink
      
      tags$h3(tags$b(titleLink))
      
      
    })
    
    output$watch <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      res<-TV2[(TV2$Title == m[1]),]
      titleLink<-res$Title
      titleLink
      
      link1<-"-watch-full-series/"
      titleLink1<-tolower(titleLink)
      titleLink2<-gsub(" +","-",titleLink)
      y<-paste0(titleLink2,link1)
      
      link2<-"https://openloadmovies.cam/tvshows/"
      x<-paste0(link2,y)
      my_test <- tags$iframe(src=x, height=720, width=1270 ,allowfullscreen = TRUE)
      my_test

    })
    
    output$watch1 <- renderUI({
      req(input$searchText)
      m<-str_subset(Poster$Title, coll(input$searchText, ignore_case = TRUE))
      res<-Poster[(Poster$Title == m[1]),]
      titleLink<-res$Title
      titleLink
      
      link1<-"-watch-full-movie/"
      titleLink1<-tolower(titleLink)
      titleLink2<-gsub(" +","-",titleLink)
      y<-paste0(titleLink2,link1)
      
      link2<-"https://openloadmovies.cam/movies/"
      x<-paste0(link2,y)
      my_test <- tags$iframe(src=x, height=720, width=1270 ,allowfullscreen = TRUE)
      my_test
      
    })
    
    
    output$rec2 <- renderUI({
      req(input$searchText1)
      tags$h1(tags$b("Recommendations For you!!"))
      
    })
    
    output$rec1img2 <- renderUI({
      req(input$searchText1)
      
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      r2<-TV2[(TV2$Title == m[1]),]
      
      g<-r2$Genre
      
      #new code
      g2<-strsplit(g, split = "\\|")
      df<-data.frame(g2)
      
      res<-grep(df[1,], c(tv4$g1,tv4$g2,tv4$g3,tv4$g4), ignore.case = TRUE)
      x<-TV3[res,]
      x1<-x %>% arrange(desc(imdbRating))
      r4<-x1[-1,]
      r5<-r4$Poster
      imglinkn<-r5[1]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec1tit2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      r2<-TV2[(TV2$Title == m[1]),]
      
      g<-r2$Genre 
      
      g2<-strsplit(g, split = "\\|")
      df<-data.frame(g2)
      
      res<-grep(df[1,], c(tv4$g1,tv4$g2,tv4$g3,tv4$g4), ignore.case = TRUE)
      x<-TV3[res,]
      x1<-x %>% arrange(desc(imdbRating))
      r4<-x1[-1,]
      r5<-r4$Title
      titlelinkn<-r5[1]
      tags$h3(tags$b(titlelinkn))
    })
    
    output$rec2img2 <- renderUI({
      req(input$searchText1)
      
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      r2<-TV2[(TV2$Title == m[1]),]
      
      g<-r2$Genre 
      
      g2<-strsplit(g, split = "\\|")
      df<-data.frame(g2)
      
      res<-grep(df[1,], c(tv4$g1,tv4$g2,tv4$g3,tv4$g4), ignore.case = TRUE)
      x<-TV3[res,]
      x1<-x %>% arrange(desc(imdbRating))
      r4<-x1[-1,]
      r5<-r4$Poster
      imglinkn<-r5[2]
      
      
      tags$img(src = imglinkn,height="275px", width="225px")
      
    })
    
    output$rec2tit2 <- renderUI({
      req(input$searchText1)
      m<-str_subset(TV2$Title, coll(input$searchText1, ignore_case = TRUE))
      r2<-TV2[(TV2$Title == m[1]),]
      
      g<-r2$Genre 
      
      g2<-strsplit(g, split = "\\|")
      df<-data.frame(g2)
      
      res<-grep(df[1,], c(tv4$g1,tv4$g2,tv4$g3,tv4$g4), ignore.case = TRUE)
      x<-TV3[res,]
      x1<-x %>% arrange(desc(imdbRating))
      r4<-x1[-1,]
      r5<-r4$Title
      titlelinkn<-r5[2]
      tags$h3(tags$b(titlelinkn))
    })
  
})
