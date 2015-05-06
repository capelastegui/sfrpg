

require(knitr)

basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"
tmpfile<- file.path(basedir,"raw","0-chapters","CharacterBuilding.Rmd")

pandoc(tmpfile, format='html')

basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"
tmpfile<- file.path(basedir,"raw","0-chapters","CharacterBuilding.html")

tmpfile2<- file.path(basedir,"raw","0-chapters","CharacterBuilding.pdf")


tmpfile<- file.path(basedir,"raw","0-chapters","Feats.html")
tmpfile2<- file.path(basedir,"raw","0-chapters","Feats.pdf")

pandoc(tmpfile, format='pdf')
pandoc(tmpfile, format='pdf')

 
wkhtmltopdf  <- "D:\\Programas\\wkhtmltopdf\\bin\\wkhtmltopdf.exe"

system(paste(wkhtmltopdf, tmpfile, tmpfile2))


system()
