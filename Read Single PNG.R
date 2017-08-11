library(png)

loc <- ("C:/Users/Aditya/Desktop/Docs/Seminar+MP/Test/character_11_taamatar")
write_loc <- ("C:/Users/Aditya/Desktop/Docs/Seminar+MP/")
pngs <- list.files(loc)
print(pngs[50])
x <- readPNG(file.path(loc,pngs[50]))
write.csv(x, file.path(write_loc,"READ_PNG.csv"))