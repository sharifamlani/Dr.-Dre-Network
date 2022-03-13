#Sharif Amlani
#R 4.1.1
#Winter 2021

######################## Code Summary ##################

########################## Prelude #####################

rm(list=ls(all=TRUE))
options(stringsAsFactors = FALSE)
options(scipen = 3)
set.seed(1993)

######################### Functions ###################
'%!in%' <- function(x,y)!('%in%'(x,y))

#Turns a Regression into a data frame
Model.DF <- function(Model, Robust.SE = NULL) {
  
  #Extract Coefficients
  Model.Output <- as.data.frame(coef(summary(Model)))
  Model.Output$Label <- rownames(Model.Output)
  rownames(Model.Output) <- NULL
  
  #Generate Confidence Intervals
  CI <- as.data.frame(confint(Model, variable.names(Model), level=0.95))
  CI$Label <- rownames(CI)
  rownames(CI) <- NULL
  
  #Merge Model and CIs together 
  Model.Output.Final <- merge(x = Model.Output, y = CI, by =c("Label"))
  
  #Name the columns numeric
  colnames(Model.Output.Final) <- c("Label", "Coeff", "SE", "t.value", "P.Value", "lower", "upper")
  
  Model.Output.Final$Sig.05 <- ifelse(Model.Output.Final$P.Value <= .05, 1,0)
  Model.Output.Final$Sig.10 <- ifelse(Model.Output.Final$P.Value <= .10, 1,0)
  
  #Adjusted R Squared
  Model.Output.Final$AdJ.R2 <- summary(Model)$adj.r.squared
  
  #Dependent Variable
  Model.Output.Final$DV <- all.vars(formula(Model))[1]
  
  #Check for NA's in Model
  for(n in names(coef(Model))){
    if(is.na(Model$coefficients[[n]]) == T){
      newRow <- data.frame(Label=n, 
                           Coeff = NA, 
                           SE = NA, 
                           t.value = NA,
                           P.Value = NA,
                           lower = NA,
                           upper = NA,
                           AdJ.R2 = NA, 
                           Sig.05 = NA,
                           Sig.10 = NA,
                           DV=all.vars(formula(Model))[1])
      
      Model.Output.Final <- rbind(Model.Output.Final, newRow)
      
    }
  }
  
  #Option for Robust Standard Errors
  if(is.null(Robust.SE) == F){
    library(sandwich)
    x<- coeftest(Model, vcov = sandwich::vcovHC(Model, type=Robust.SE))
    xr<- setNames(data.frame(x[1:dim(x)[1], 2]), c("Robust Standard Errors"))
    xr$Label<- rownames(xr); rownames(xr) <- NULL
    
    Model.Output.Final <- merge(Model.Output.Final, xr, by = "Label")
    
  }
  
  return(Model.Output.Final)
  
}
######################### Library #####################
library(spotifyr)
library(lubridate)
library(tidyverse)
library(knitr)
library(margins)
library(ggridges)

######################## Upload API Key ##################

#Set Working Directory
setwd("C:/Users/Shari/OneDrive/R-Scripts/API/Spotify")

#Upload Data
load(file = "Spotify API Key.rda")

#################### Connect to Spotify API ##################

Sys.setenv(SPOTIFY_CLIENT_ID = clientID)
Sys.setenv(SPOTIFY_CLIENT_SECRET = clientSecret)

access_token <- get_spotify_access_token()

#################### Download Data ##################
Music.Pure <- get_artist_audio_features('jay-z', include_groups = "appears_on"); Music.1 <- Music.Pure

#################### Examine Data ##################
head(Music.1)
unique(Music.1$track_name)

#################### Subset Data ##################
Music.2 <- subset(Music.1, album_type != "compilation")

#################### Get Album Artist Name ##################
Album.Final <- NULL
for(i in unique(Music.2$album_id)){
  Album.1 <- get_albums(i)
  Album_Artists<- Album.1[[2]][[1]][[3]]
  
  Album.2 <- data.frame(Album_Artists, album_id = i)
  
  Album.Final <- rbind(Album.Final, Album.2)
  
}

Album.Final

#################### Merge Back to Dre Albums ##################
Music.3 <- merge(Music.2, Album.Final, by = "album_id")

#################### Code Dre Songs ##################
Music.3$Feature <- "No"
Music.3$Feature[grepl( "jay z" , Music.3$track_name ) == T] <- "Yes"

Music.3$Album_Artists <-  gsub("\\s*\\([^\\)]+\\)","",as.character(Music.3$Album_Artists))


#################### Create Edgelist Network ##################
Music.4 <- unique(with(Music.3, data.frame(artist_name, Album_Artists)))

#################### Create Network ##################
library(igraph)

network <- graph_from_data_frame(d=Music.4, directed=F) 

plot(network)

#What are the nodes
V(network)

##################### Plot ########################
setwd("C:/Users/Shari/OneDrive/University of California, Davis/Research Projects/Spotify/Dr. Dre Network")

png("Figure 2.png", 1000, 1000)
plot(network, vertex.shape="none", vertex.label=V(network)$name, 
     
     vertex.label.font=2, vertex.label.color="gray40",
     
     vertex.label.cex=1.2, edge.color="gray85")

dev.off()









