#installing and importing package
#install.packages("png")
library(png)

#declaring reading location
read_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets")

#declaring writing location
write_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets-Discretized")

#Getting the list of directories in the base read location
dirs_in_dataset <- list.dirs(read_loc)
dirs_in_dataset <- dirs_in_dataset[2:length(dirs_in_dataset)]

#Getting the list of directories in the base write location
dirs_in_discretized <- list.dirs(write_loc)
dirs_in_discretized <- dirs_in_discretized[2:length(dirs_in_discretized)]

#Reading .png(s) and storing it in the proper Folder
f <- 1
namef <- 1
name <- ""
for(dir in dirs_in_dataset)
{
     namef <- 1
     for(pngs in list.files(dir))
     {
          png <- readPNG(paste(dir, pngs, sep = '/'))
          name <- paste(as.character(namef),".csv")
          write.csv(png, file = file.path(dirs_in_discretized[f], name))
          namef <- namef+1
     }
     f <- f+1
}