#declaring reading location
read_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets-Discretized")

#declaring writing location
write_loc <- ("D:/Codes/Data-Science-OCR/OCR Test Alphabets-Binaritized")

#Creating folder hierarchy
if(!dir.exists(write_loc))
     dir.create(write_loc)

for(dir in list.files(read_loc))
{
     if(!dir.exists(paste(write_loc,dir,sep = '/')))
     {
          dir.create(paste(write_loc,dir,sep = '/'))
     }
}