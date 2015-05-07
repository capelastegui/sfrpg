require(knitr)




basedir <- "C:/Users/acer/Documents/Perico/OCIO/SFRPG-web"
dir.in  <-  file.path(basedir,"raw","0-chapters")
htm.a1.intro<- file.path(dir.in,"Introduction.html")
htm.a2.charbuild<- file.path(dir.in,"CharacterBuilding.html")
htm.b3.feats<- file.path(dir.in,"Feats.html")

dir.out  <- file.path(basedir,"pdf")
pdf.a1.intro<- file.path(dir.out,"A1-Introduction.pdf")
pdf.a2.charbuild<- file.path(dir.out,"A2-CharacterBuilding.pdf")
pdf.b3.feats<- file.path(dir.out,"B3-Feats.pdf")


wkhtmltopdf  <- "D:\\Programas\\wkhtmltopdf\\bin\\wkhtmltopdf.exe"

toPdf <- function(f.in,f.out){
  system(paste(wkhtmltopdf,"--zoom 1.25 ", f.in, f.out))
}

toPdf(htm.a1.intro,pdf.a1.intro)
toPdf(htm.a2.charbuild,pdf.a2.charbuild)
toPdf(htm.b3.feats,pdf.b3.feats)


