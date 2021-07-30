install.packages("ggeasy")
install.packages("shinydashboardPlus")
install.packages("leaflet")
install.packages("bootstraplib")
install.packages("dqshiny")
library(bootstraplib)
 library(ggeasy)
library(ggplot)
library(ggplot2)
library(forcats)
library(leaflet)
library(DT)
library(dplyr)
library(forcats)
library(tidyr)
library(ggrepel) 
library(plotrix)
library(stringr)
library(stringi)
library(dqshiny)


Netflix1 <-read.csv("D:\\Trimister-2\\R Lab Notes\\R Mini Projects\\DataSet\\movieAnalysis.csv")
Poster <- read.csv("D:\\Trimister-2\\R Lab Notes\\R Mini Projects\\DataSet\\MovieGenre.csv")
TV2<- read.csv("D:\\Trimister-2\\R Lab Notes\\R Mini Projects\\DataSet\\TV_shows_ver2.csv")
View(Poster)
Netflix <- data.frame(Netflix1)
head(Netflix)
View(Netflix)
Netflix$imdb_score
unique(Netflix$language)

install.packages("DT")




#removing Â from movie_title
Netflix <- Netflix %>% mutate(movie_title = gsub("Â", "", movie_title))
Netflix

bp<- ggplot(Netflix, aes(x="", y=content_rating, fill=content_rating))+geom_bar(width = 1, stat = "identity")
bp
pie <- bp + coord_polar("y", start=0)


#top 10 Movies by imdb score
sorted_m<-Netflix[order(Netflix$imdb_score),]
top10Movies <- tail(sorted_m,n=10)
top10Movies

#top 10 most liked movies
sorted_m2 <- Netflix[order(Netflix$movie_facebook_likes),]
top10Movies2<-tail(sorted_m2,n=10)
top10Movies2


data1 <- data.frame(
  name=top10Movies$movie_title,
  value=top10Movies$imdb_score
)

data1
data1<-unique(data1)

data2 <- data.frame(
  name=top10Movies2$movie_title,
  value=top10Movies2$movie_facebook_likes
)

data2

getwd()


#horizontal bar chart for top 9 movies


my_bar <- data1  %>% mutate(name = fct_reorder(name, value)) %>% ggplot(aes(x=name, y=value)) + geom_bar(stat="identity", fill="purple", alpha=.6, width=.4) +coord_flip() + xlab("") + ylab("IMDB Ratings") + geom_label(aes(y = data1$value, label = round(data1$value, 2))) 
my_bar

data3<-Netflix$genres
unique(data3)


?mutate


data4 <- data.frame(
  Rating=Netflix$content_rating,
  Duration=Netflix$duration
)


library(ggplot2)

# Horizontal version
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


imdb <- data.frame(
  Title=top10Movies2$movie_title,
  Genre=top10Movies2$genres,
  Duration=top10Movies2$duration,
  Link=top10Movies2$movie_imdb_link
)

imdb

x<-strsplit(imdb$Genre[],split=",")
x

genre<-strsplit(imdb$Genre, split = ",")

table(trimws(unlist(strsplit(imdb$Genre, split = ","))))


#DT
colnames(Netflix)
  Netflix$movie_imdb_link <- paste0("<a href='",Netflix$movie_imdb_link,"'>",Netflix$movie_imdb_link,"</a>")
  Netflix2<-select(Netflix , movie_title , genres , content_rating , imdb_score , duration , title_year , movie_imdb_link)
  Netflix2
View(Netflix2)


Netflix2$movie_imdb_link 



Netflix

sum(is.na(Netflix))
Netflix[is.na(Netflix)]<-0

rm(list = ls())




View(temp)
colnames(temp)
temp



dataForPie<-Netflix %>%
  separate_rows(genres) %>%
  count(genres)


dataForPie<-data.frame(dataForPie)

View(dataForPie)
class(nf)

dataForPie$n<-(dataForPie$n*100)/sum(dataForPie$n)



dataForPie<-data.frame(Genre=dataForPie$genres,count=dataForPie$n)

sum(dataForPie$count)

dataForPie$count=as.numeric(dataForPie$count)

class(dataForPie$count)

dataForPie$count<-format(round(dataForPie$count, 2), nsmall = 2)



View(dataForPie)
dataForPietTemp<-head(dataForPie,4)
dataForPietTemp

dataForPie$count=as.numeric(dataForPie$count)
dataForPie$count

pie3D(dataForPie$count,labels = dataForPie$Genre,explode = 0.1, main = "Pie Chart of Countries")
