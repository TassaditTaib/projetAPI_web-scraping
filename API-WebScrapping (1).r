#-----------------------------API------------------------------#
#--télechargement des packages nécessaire--#
#install.packages("jsonlite")
library('rvest')
library('tidyverse')
library('stringr')
library("jsonlite")

#-----l'API Studio Ghibli nous permet d'obtenir des informations sur les films--# 
#--Obtenir les information sur les films --#
url_req <- "https://ghibliapi.herokuapp.com/films"
film <- fromJSON(url_req)

#--Prendre que les colonnes 2,3,6--#
film<-film[,c(2,3,6)]

#View(film)
film_titre<-film$title
#--Parcourir les 20 films de l'API et les mettre  dans un vecteur tout en remplacantles caractéres nécessairesqui
#est espace par + et les concaténer avec le lien "https://animesvostfr.net/?s=" puis les mettre dans un vecteur--#
#--z va contenir les liens des pages qui contient le film--#
z<-c()
i<-1
while (i<=20) {
  l_film<-gsub("[ ]","+",film_titre[i])
  l_film<-paste0("https://animesvostfr.net/?s=",l_film)
  z<-c(z,l_film)
  i<-i+1
}

nbr_page<-1
note<-c();genre<-c();langue<-c();vue<-c();statut<-c();liberation<-c();type<-c();auteur<-c();lien<-c()

while (nbr_page<=20) {
  url<- z[nbr_page]
  url_content  <- read_html(url)
  
  #--------récuprérer le liens du film de l'a page'url spécifier dans le vecteur  -------------------------------
  url_mainnode_text <- html_nodes(url_content,".mlw-category a")%>% html_attr("href")
  v<-c(url_mainnode_text)
  v<-unique(v)
  i<-1;j<-1
  
  while (i<length(v)) {
    if (isTRUE(str_count(v[i],"/")==4)){
      url_film<- v[i]
      webpage  <- read_html(url_film)
      #-----lien de film--#
      lien<-c(lien,v[i])

      #------récuérer la note du film i contenu dans l'api------#
      note_html <- html_nodes(webpage,".averagerate")
      note1<- html_text(note_html)
      note1<-as.integer(note1)
      note<-c(note,note1)
      
      #-récuperer la classe qui contient le genre ,langue et vue-#      
      part1_html <- html_nodes(webpage,".mvici-left p")
      list(part1_html)
      part1<- html_text(part1_html)
      part1 <- gsub("\\n","",part1)
      
      #------genre------#
      genre1<-part1[1]
      taille_genre<-nchar(genre1)
      genre1<-substr(genre1,10,taille_genre)
      genre<-c(genre,genre1)
      genre <- genre[!is.na(genre)]
      
      #------langue----#
      langue1<-part1[2]
      taille_langue<-nchar(langue1)
      langue1<-substr(langue1,14,taille_langue)
      langue<-c(langue,langue1)
      langue <- langue[!is.na(langue)]
      
      #-------vues-----#      
      vue1<-part1[3]
      taille_vue<-nchar(vue1)
      vue1<-substr(vue1,12,taille_vue-4)
      vue<-c(vue,vue1)
      
      #-récuperer la classe qui contient statut etle type -#
      part2_html <- html_nodes(webpage,".mvici-right p")
      list(part2_html)
      part2<- html_text(part2_html)
      part2 <- gsub("\\n","",part2)
      
      #----statut-----#
      statut1<-part2[1]
      taille_statut<-nchar(statut1)
      statut1<-substr(statut1,10,taille_statut)
      statut<-c(statut,statut1)
      statut <- statut[!is.na(statut)]
      
      #------type-------#
      type1<-part2[3]
      taille_type<-nchar(type1)
      type1<-substr(type1,8,taille_type)
      type<-c(type,type1)
      type <- type[!is.na(type)]
      
      #------auteurs------#
      auteur_html <- html_nodes(webpage,"h5")
      auteur1<- html_text(auteur_html)
      auteur<-c(auteur,auteur1)
    } 
    i<-i+1
  }     
  nbr_page<-nbr_page+1
}
#--Construire un dataframe avec les colonnes récupérer--#
filmAPI<-data.frame(lien=lien,note=note,genre=genre,langue=langue, statut=statut,vue=vue, type=type, auteur=auteur)

#--Construire une dataframe avec la dataframe "filmAPI" et le datframe "film" --#
data<-bind_cols(film,filmAPI)
#View(data)


#-----------------------------WEB SCRAPING--------------------------# 
url<- 'https://animesvostfr.net/topic/movie/'
url_content  <- read_html(url)

#---parcours des pages-----#
i<-2
z<-c(url)
k<-2
#--Acceder aux pages qui contient les films--#
while (i<4) {
  url<-paste("https://animesvostfr.net/filter-advance/page/",i,"/?topic=movie",sep="")
  #---mettre les url dansle vecteur-----#
  z[k]<-url
  k<-k+1
  i<-i+1
}

nbr_page<-1
title  <-c();description<-c();note<-c();genre<-c();langue<-c();vue<-c();statut<-c();release_date<-c();type<-c();auteur<-c();lien<-c()
while (nbr_page<4) {
  url<- z[nbr_page]
  url_content  <- read_html(url)
  
  #--------récuprérer les liens des films dela page i---------------------------
  url_mainnode_text <- html_nodes(url_content,".mlw-category a")%>% html_attr("href")
  v<-c(url_mainnode_text)
  v<-unique(v)
  i=1;j=1
  #--Parcourir le vecteur et garder que les les liens des films--#
  while (i<length(v)) {
    if (isTRUE(str_count(v[i],"/")==4)){
      url_film<- v[i]
      webpage  <- read_html(url_film)

      #-----récupérer lien de film---#
      lien<-c(lien,v[i])
      
      #------récupérer le titre-----#
      title_html <- html_nodes(webpage,"h1[itemprop='name']")
      title1 <- html_text(title_html)
      title<-c(title,title1)
      
      #-----récupérer la description------#
      desc_html <- html_nodes(webpage,".desc")
      desc1 <- html_text(desc_html)
      desc1 <- gsub("\\n","",desc1)
      description<-c(description,desc1[1])
      
      #------récupérer la note------#
      note_html <- html_nodes(webpage,".averagerate")
      note1<- html_text(note_html)
      note1<-as.integer(note1)
      note<-c(note,note1)
      
      #--récuperer la classe qui contient le genre ,langue et vue--#      
      part1_html <- html_nodes(webpage,".mvici-left p")
      list(part1_html)
      part1<- html_text(part1_html)
      part1 <- gsub("\\n","",part1)

      #------récupérer le genre------#
      genre1<-part1[1]
      taille_genre<-nchar(genre1)
      genre1<-substr(genre1,10,taille_genre)
      genre<-c(genre,genre1)
      genre <- genre[!is.na(genre)]

      #------récupérer la langue----#
      langue1<-part1[2]
      taille_langue<-nchar(langue1)
      langue1<-substr(langue1,14,taille_langue)
      langue<-c(langue,langue1)
      langue <- langue[!is.na(langue)]
      
      #-------récupérer les vues-----#
      vue1<-part1[3]
      taille_vue<-nchar(vue1)
      vue1<-substr(vue1,12,taille_vue-4)
      vue<-c(vue,vue1)
       
      #--récuperer la classe qui contient le statut ,la liberation et le type--#                                        
      part2_html <- html_nodes(webpage,".mvici-right p")
      list(part2_html)
      part2<- html_text(part2_html)
      part2 <- gsub("\\n","",part2)
 
      #----récupérer le statut-----#
      statut1<-part2[1]
      taille_statut<-nchar(statut1)
      statut1<-substr(statut1,10,taille_statut)
      statut<-c(statut,statut1)
      statut <- statut[!is.na(statut)]
      
      #-----récupérer la liberation---#
      release_date1<-part2[2]
      taille_release_date<-nchar(release_date1)
      release_date1<-substr(release_date1,14,taille_release_date)
      release_date<-c(release_date,release_date1)
      release_date <- release_date[!is.na(release_date)]
      
      #------récupérer le type-------#
      type1<-part2[3]
      taille_type<-nchar(type1)
      type1<-substr(type1,8,taille_type)
      type<-c(type,type1)
      type <- type[!is.na(type)]
      
      #------récupérer les auteurs------#
      auteur_html <- html_nodes(webpage,"h5")
      auteur1<- html_text(auteur_html)
      auteur<-c(auteur,auteur1)
    } 
    i<-i+1
  }     
  nbr_page<-nbr_page+1
}

#--Construction d'un dataframe avec les données récupérer(données stocké dans le vecteur)--#
filmScrap<-data.frame(title=title,description= description[c(-65, -98)],release_date=release_date, note=note,
        genre=genre,langue=langue,statut=statut,vue=vue[c(-65, -98)], type=type, auteur=auteur,lien=lien[c(-65, -98)])
#view(filmScrap)

#--------Construction de la dataframe finale----------#
data2<-bind_rows(data,filmScrap)
View(data2)

#----Convertir le dataframe en un fichier CSV----#
write.table(data2, "data.csv", row.names=FALSE, sep="t",dec=",", na=" ")
