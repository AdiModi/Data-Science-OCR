library(png)
library(DBI)
library(RMySQL)
library(party)
library(randomForest)

#Path Declarations
rootFeaturesLocation <- "D:/Codes/Data-Science-OCR/Features"
rootReadLocation <- "D:/Codes/Data-Science-OCR/OCR Test Alphabets"
rootDiscretizedLocation <- "D:/Codes/Data-Science-OCR/OCR Test Alphabets-Discretized"
rootBinaritizedLocation <- "D:/Codes/Data-Science-OCR/OCR Test Alphabets-Binaritized"

#Creating Directory Hierarchy for Storing Image Pixel-Intensity Values as Discrete Values
createDirectoryDiscretize <- function(rootReadLocation, rootWriteLocation)
{
     if(!dir.exists(rootWriteLocation))
     {
          #Creating the Root Directory if it doesn't exist
          dir.create(rootWriteLocation)
     }
     
     for(dir in list.files(rootReadLocation))
     {
          if(!dir.exists(paste(rootWriteLocation,dir,sep = '/')))
          {
               #Creating the Sub-Directories if it doesn't exist
               dir.create(paste(rootWriteLocation,dir,sep = '/'))
          }
     }
}


discretize <-function(rootReadLocation, rootWriteLoaction)
{
     
     dirs <- list.dirs(rootReadLocation, full.names = FALSE, recursive = FALSE)
     
     progressTotal<-3000
     progressValue<-0
     filename <- 1
     progressBar <- winProgressBar(title = "Generating Pixel-Intensity Values", min = 0, max = progressTotal, width = 500)
     for(dir in dirs)
     {
          filenumber <- 1
          pngs <- list.files(file.path(rootReadLocation, dir), full.names = TRUE)
          for(png in pngs)
          {
               #Reading PNG Files
               png <- readPNG(png)
               name <- paste(as.character(filenumber),".csv", sep = "")
               #Writing CSV Files with Pixel-Intensity Values
               write.table(png, file = file.path(file.path(rootWriteLoaction, dir), name), sep = ",", row.names = FALSE, col.names = FALSE)
               progressValue<-progressValue+1
               setWinProgressBar(progressBar, progressValue, title=paste("Generating Pixel-Intensity Values: ", round(progressValue/progressTotal*100, 0), "% done", sep = ""))
               filenumber <- filenumber+1
          }
     }
     close(progressBar)
}

#Creating Directory Hierarchy for Storing Image Pixel-Intensity Values as Binary Values
createDirectoryBinaritize <- function(rootReadLocation, rootWriteLocation)
{
     if(!dir.exists(rootWriteLocation))
     {
          #Creating the Root Directory if it doesn't exist
          dir.create(rootWriteLocation)
     }
     
     for(dir in list.files(rootReadLocation))
     {
          if(!dir.exists(paste(rootWriteLocation,dir,sep = '/')))
          {
               #Creating the Sub-Directories if it doesn't exist
               dir.create(paste(rootWriteLocation,dir,sep = '/'))
          }
     }
}

binaritize <-function(rootReadLocation, rootWriteLocation)
{
     
     dirs <- list.dirs(rootReadLocation, full.names = FALSE, recursive = FALSE)     
     
     progressTotal<-3000
     progressValue<-0
     filename <- 1
     progressBar <- winProgressBar(title = "Generating Binary Intensity Values", min = 0, max = progressTotal, width = 500)
     for(dir in dirs)
     {
          filenumber <- 1
          csvs <- list.files(file.path(rootReadLocation, dir), full.names = TRUE)
          for(csv in csvs)
          {
               #Reading Discrete Pixel-Intensity Values
               csv <- read.table(csv, header = FALSE, sep = ',')
               csv <- as.matrix(csv)
               csv <- replace(csv, csv < 0.5, 0)
               csv <- replace(csv, csv >= 0.5, 1)
               name <- paste(as.character(filenumber),".csv", sep="")
               #Writing Binary Pixel-Intensity Values
               write.table(csv, file = file.path(file.path(rootWriteLocation, dir), name), sep = ",", row.names = FALSE, col.names = FALSE)
               progressValue<-progressValue+1
               setWinProgressBar(progressBar, progressValue, title=paste("Generating Binary-Intensity Values: ", round(progressValue/progressTotal*100, 0), "% done", sep = ""))
               filenumber <- filenumber+1
          }
     }
     close(progressBar)
}

createDirectoryFeature <- function(rootWriteLocation)
{
     if(!dir.exists(rootWriteLocation))
     {
          #Creating Root Directory For Storing Feature-Set
          dir.create(rootWriteLocation)
     }
}

createDiscretizedFeature <- function(rootReadLocation, rootWriteLocation, databaseConnection)
{
     dirs <- list.dirs(rootReadLocation, full.names = FALSE, recursive = FALSE)
     
     #Declaring essential Values
     progressTotal<-3000
     progressValue<-0
     imageDimension<-32
     widthOfZone <- 8
     filename <- 1
     progressBar <- winProgressBar(title = "Generating Discretized Feature-Set", min = 0, max = progressTotal, width = 500)
     
     featureTable<-matrix(nrow = 0, ncol = ((imageDimension/widthOfZone)*(imageDimension/widthOfZone))+4+1)
     
     for(dir in dirs)
     {
          filenumber <- 1
          csvs <- list.files(file.path(rootReadLocation, dir), full.names = TRUE)
          for(csv in csvs)
          {
               csv <- read.table(csv, header = FALSE, sep = ',')
               csv <- as.matrix(csv)
               featureVector <- vector(length =0)
               for(i in seq(1, nrow(csv), by = widthOfZone))
               {
                    for(j in seq(1, ncol(csv), by = widthOfZone))
                    {
                         sum <- 0
                         density <- 0
                         for(k in csv[seq(i, i+widthOfZone-1), seq(j, j+widthOfZone-1)])
                         {
                              sum <- sum+k
                         }
                         density <- sum/(widthOfZone*widthOfZone)
                         featureVector<-c(featureVector, density)
                    }
               }
               
               #Declaring Chords and their Flags to calculate Intersection Points
               chord1<-0
               chord1Flag<-csv[1,1]
               chord2<-0
               chord2Flag<-csv[1,imageDimension]
               chord3<-0
               chord3Flag<-csv[1,(imageDimension/2)]
               chord4<-0
               chord4Flag<-csv[(imageDimension/2),1]
               
               for(i in seq(1,imageDimension))
               {
                    for(j in seq(1,imageDimension))
                    {
                         if(i == j)
                         {
                              if(chord1Flag==0 && csv[i,j]>0)
                              {
                                   chord1Flag<-1
                                   chord1<-chord1+1
                              }
                              else
                              {
                                   chord1Flag<-csv[i,j]
                              }
                         }
                         if((i+j)==imageDimension-1)
                         {
                              if(chord2Flag==0 && csv[i,j]>0)
                              {
                                   chord2Flag<-1
                                   chord2<-chord2+1
                              }
                              else
                              {
                                   chord2Flag<-csv[i,j]
                              }
                         }
                         if(j==(imageDimension/2))
                         {
                              if(chord3Flag==0 && csv[i,j]>0)
                              {
                                   chord3Flag<-1
                                   chord3<-chord3+1
                              }
                              else
                              {
                                   chord3Flag<-csv[i,j]
                              }
                         }
                         if(i==(imageDimension/2))
                         {
                              if(chord4Flag==0 && csv[i,j]>0)
                              {
                                   chord4Flag<-1
                                   chord4<-chord4+1
                              }
                              else
                              {
                                   chord4Flag<-csv[i,j]
                              }
                         }
                    }
               }
               #Appending Feature values to the Feature-Set
               featureVector<-c(featureVector, chord1)
               featureVector<-c(featureVector, chord2)
               featureVector<-c(featureVector, chord3)
               featureVector<-c(featureVector, chord4)
               featureVector<-c(featureVector, dir)
               featureTable<-rbind(featureTable, featureVector)
               insertIntoDiscretizedFeatureTable(featureVector, databaseConnection)
               progressValue<-progressValue+1
               setWinProgressBar(progressBar, progressValue, title=paste("Generating Discretized Feature-Set: ", round(progressValue/progressTotal*100, 0), "% done", sep = ""))
          }
     }
     close(progressBar)
     #Defining Column Names
     colnames(featureTable)<-union(seq(1,20), "class")
     #Writing the Feature-Set
     write.table(featureTable, file = file.path(paste(rootWriteLocation, "Discretized Features.csv", sep = "/")), sep = ",", col.names = TRUE, row.names = FALSE)
     print("Feature-Set Generated")
     print(">Feature 1-16 represents Zone Densities")
     print(">Feature 17-20 represents number of Chord Intersections")
     print(">Feature 21 represents the Class")
}

createBinaratizedFeature <- function(rootReadLocation, rootWriteLocation, databaseConnection)
{
     dirs <- list.dirs(rootReadLocation, full.names = FALSE, recursive = FALSE)
     
     #Declaring essential Values
     progressTotal<-3000
     progressValue<-0
     imageDimension<-32
     widthOfZone <- 8
     filename <- 1
     progressBar <- winProgressBar(title = "Generating Binaritized Feature-Set", min = 0, max = progressTotal, width = 500)
     
     featureTable<-matrix(nrow = 0, ncol = ((imageDimension/widthOfZone)*(imageDimension/widthOfZone))+4+1)
     
     for(dir in dirs)
     {
          filenumber <- 1
          csvs <- list.files(file.path(rootReadLocation, dir), full.names = TRUE)
          for(csv in csvs)
          {
               csv <- read.table(csv, header = FALSE, sep = ',')
               csv <- as.matrix(csv)
               featureVector <- vector(length =0)
               for(i in seq(1, nrow(csv), by = widthOfZone))
               {
                    for(j in seq(1, ncol(csv), by = widthOfZone))
                    {
                         sum <- 0
                         density <- 0
                         for(k in csv[seq(i, i+widthOfZone-1), seq(j, j+widthOfZone-1)])
                         {
                              sum <- sum+k
                         }
                         density <- sum/(widthOfZone*widthOfZone)
                         featureVector<-c(featureVector, density)
                    }
               }
               
               #Declaring Chords and their Flags to calculate Intersection Points
               chord1<-0
               chord1Flag<-csv[1,1]
               chord2<-0
               chord2Flag<-csv[1,imageDimension]
               chord3<-0
               chord3Flag<-csv[1,(imageDimension/2)]
               chord4<-0
               chord4Flag<-csv[(imageDimension/2),1]
               
               for(i in seq(1,imageDimension))
               {
                    for(j in seq(1,imageDimension))
                    {
                         if(i == j)
                         {
                              if(chord1Flag==0 && csv[i,j]>0)
                              {
                                   chord1Flag<-1
                                   chord1<-chord1+1
                              }
                              else
                              {
                                   chord1Flag<-csv[i,j]
                              }
                         }
                         if((i+j)==imageDimension-1)
                         {
                              if(chord2Flag==0 && csv[i,j]>0)
                              {
                                   chord2Flag<-1
                                   chord2<-chord2+1
                              }
                              else
                              {
                                   chord2Flag<-csv[i,j]
                              }
                         }
                         if(j==(imageDimension/2))
                         {
                              if(chord3Flag==0 && csv[i,j]>0)
                              {
                                   chord3Flag<-1
                                   chord3<-chord3+1
                              }
                              else
                              {
                                   chord3Flag<-csv[i,j]
                              }
                         }
                         if(i==(imageDimension/2))
                         {
                              if(chord4Flag==0 && csv[i,j]>0)
                              {
                                   chord4Flag<-1
                                   chord4<-chord4+1
                              }
                              else
                              {
                                   chord4Flag<-csv[i,j]
                              }
                         }
                    }
               }
               #Appending Feature values to the Feature-Set
               featureVector<-c(featureVector, chord1)
               featureVector<-c(featureVector, chord2)
               featureVector<-c(featureVector, chord3)
               featureVector<-c(featureVector, chord4)
               featureVector<-c(featureVector, dir)
               featureTable<-rbind(featureTable, featureVector)
               insertIntoBinaritizedFeatureTable(featureVector, databaseConnection)
               progressValue<-progressValue+1
               setWinProgressBar(progressBar, progressValue, title=paste("Generating Binaritized Feature-Set: ", round(progressValue/progressTotal*100, 0), "% done", sep = ""))
          }
     }
     close(progressBar)
     #Defining Column Names
     colnames(featureTable)<-union(seq(1,20), "class")
     #Writing the Feature-Set
     write.table(featureTable, file = file.path(paste(rootWriteLocation, "Binaritized Features.csv", sep = "/")), sep = ",", col.names = TRUE, row.names = FALSE)
     print("Feature-Set Generated")
     print(">Feature 1-16 represents Zone Densities")
     print(">Feature 17-20 represents number of Chord Intersections")
     print(">Feature 21 represents the Class")
}

#Perform Random Forest on Feature-Set
performRandomForestFromFile<-function(rootLocation){
     output<-randomForest(formula = class ~ ., data = read.csv(rootLocation))
     print(output)
}

performRandomForestFromData<-function(data){
     output<-randomForest(formula = class ~ ., data = data)
     print(output)
}

connectToDatabase <- function()
{
     databaseConnection=dbConnect(MySQL(), user="root", password="", dbname="HindiOCR", host="localhost")
     print("Connection Established")
     return(databaseConnection)
}

disconnectToDatabase <- function()
{
     dbDisconnect(databaseConnection)
     print("Database Disconnected")
}

createDiscretizedFeatureTable <- function(databaseConnection)
{
     if(dbExistsTable(databaseConnection, "discretized_features"))
     {
          dbSendQuery(databaseConnection, paste("drop table", "discretized_features", sep = " "))
     }
     dbSendQuery(databaseConnection,"create table discretized_features(zone1 float, zone2 float, zone3 float, zone4 float, zone5 float, zone6 float, zone7 float, zone8 float,zone9 float, zone10 float, zone11 float, zone12 float, zone13 float, zone14 float, zone15 float, zone16 float, chord1 integer, chord2 integer, chord3 integer, chord4 integer, class varchar(10))")
     print("Discretized Table Created")
}

createBinaritizedFeatureTable <- function(databaseConnection)
{
     if(dbExistsTable(databaseConnection, "binaritized_features"))
     {
          dbSendQuery(databaseConnection, paste("drop table", "binaritized_features", sep = " "))
     }
     dbSendQuery(databaseConnection,"create table binaritized_features(zone1 float, zone2 float, zone3 float, zone4 float, zone5 float, zone6 float, zone7 float, zone8 float,zone9 float, zone10 float, zone11 float, zone12 float, zone13 float, zone14 float, zone15 float, zone16 float, chord1 integer, chord2 integer, chord3 integer, chord4 integer, class varchar(10))")
     print("Binaritized Table Created")
}

listTables <- function(databaseConnection)
{
     dbListTables(databaseConnection)
}

insertIntoDiscretizedFeatureTable <- function(featureVector, databaseConnection)
{
     query<-"insert into discretized_features(zone1, zone2, zone3, zone4, zone5, zone6, zone7, zone8, zone9, zone10, zone11, zone12, zone13, zone14, zone15, zone16, chord1, chord2, chord3, chord4, class)"
     values<-"values ("
     for(i in seq(1, length(featureVector)-1))
     {
          if(i==1)
          {
               values<-paste(values, featureVector[i], sep = "")
          }
          else
          {
               values<-paste(values, featureVector[i], sep = ", ")
          }
     }
     values<-paste(values, paste("'", featureVector[length(featureVector)], "'", sep = ""), sep = ", ")
     values<-paste(values, ");", sep = "")
     query<-paste(query, values, sep = " ")
     dbSendQuery(databaseConnection, query)
}

insertIntoBinaritizedFeatureTable <- function(featureVector, databaseConnection)
{
     query<-"insert into binaritized_features(zone1, zone2, zone3, zone4, zone5, zone6, zone7, zone8, zone9, zone10, zone11, zone12, zone13, zone14, zone15, zone16, chord1, chord2, chord3, chord4, class)"
     values<-"values ("
     for(i in seq(1, length(featureVector)-1))
     {
          if(i==1)
          {
               values<-paste(values, featureVector[i], sep = "")
          }
          else
          {
               values<-paste(values, featureVector[i], sep = ", ")
          }
     }
     values<-paste(values, paste("'", featureVector[length(featureVector)], "'", sep = ""), sep = ", ")
     values<-paste(values, ");", sep = "")
     query<-paste(query, values, sep = " ")
     dbSendQuery(databaseConnection, query)
}

#createDirectoryDiscretize(rootReadLocation, rootDiscretizedLocation)
#discretize(rootReadLocation,rootDiscretizedLocation)
#createDirectoryBinaritize(rootDiscretizedLocation, rootBinaritizedLocation)
#binaritize(rootDiscretizedLocation,rootBinaritizedLocation)

#databaseConnection<-connectToDatabase()
#createDiscretizedFeatureTable(databaseConnection)
#createBinaritizedFeatureTable(databaseConnection)
#listOfTables<-listTables(databaseConnection)

#createDirectoryFeature(rootFeaturesLocation)
#createDiscretizedFeature(rootDiscretizedLocation, rootFeaturesLocation, databaseConnection)
#createBinaratizedFeature(rootBinaritizedLocation, rootFeaturesLocation, databaseConnection)
#performRandomForestFromFile(paste(rootFeaturesLocation, "Discretized Features.csv", sep="/"))
#performRandomForestFromFile(paste(rootFeaturesLocation, "Binaritized Features.csv", sep="/"))

#disconnectToDatabase()