#installing and importing package

#declaring reading location
read_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets-Discretized")

#declaring writing location
write_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets-Binaritized")

#Getting the list of directories in the base read location
dirs_in_discretized <- list.dirs(read_loc)
dirs_in_discretized <- dirs_in_discretized[2:length(dirs_in_discretized)]

#Getting the list of directories in the base write location
dirs_in_binaritized <- list.dirs(write_loc)
dirs_in_binaritized <- dirs_in_binaritized[2:length(dirs_in_binaritized)]

#Reding discretized .csv(s) files and storing it in the proper Folder
f <- 1
namef <- 1
name <- ""
for(dir in dirs_in_discretized)
{
     namef <- 1
     for(csvs in list.files(dir))
     {
          csvs <- read.csv(paste(dir, csvs, sep = '/'))
          csvs <- csvs[,2:ncol(csvs)]
          csvs <- as.matrix(csvs)
          csvs <- replace(csvs, csvs < 1, 0)
          name <- paste(as.character(namef),".csv")
          write.csv(csvs, file = file.path(dirs_in_binaritized[f], name))
          namef <- namef+1
     }
     f <- f+1
}