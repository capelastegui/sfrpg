

require(knitr)




basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"
tmpfile<- file.path(basedir,"raw","0-chapters","CharacterBuilding.html")

tmpfile2<- file.path(basedir,"raw","0-chapters","CharacterBuilding.pdf")


tmpfile<- file.path(basedir,"raw","0-chapters","Feats.html")
tmpfile2<- file.path(basedir,"raw","0-chapters","Feats.pdf")



 
wkhtmltopdf  <- "D:\\Programas\\wkhtmltopdf\\bin\\wkhtmltopdf.exe"

system(paste(wkhtmltopdf,"--zoom 1.25 ", tmpfile, tmpfile2))


