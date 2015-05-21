require(knitr)

basedir  <- getwd()

dir.rmd  <-  file.path(basedir,"Rmd")
rmd.files <- list()
rmd.files$a1.intro<- file.path(dir.rmd,"1-1-Introduction.Rmd")
rmd.files$a2.charbuild<- file.path(dir.rmd,"1-2-CharacterBuilding.Rmd")
rmd.files$a3.combat<- file.path(dir.rmd,"1-3-Combat.Rmd")
rmd.files$b2.classes<- file.path(dir.rmd,"2-1-Classes.Rmd")
rmd.files$b3.feats<- file.path(dir.rmd,"2-2-Feats.Rmd")

dir.rmd  <-  file.path(basedir,"Rmd")
md.files  <- list()
md.files$a1.intro<- file.path(dir.rmd,"1-1-Introduction.md")
md.files$a2.charbuild<- file.path(dir.rmd,"1-2-CharacterBuilding.md")
md.files$a3.combat<- file.path(dir.rmd,"1-3-Combat.md")
md.files$b2.classes<- file.path(dir.rmd,"2-1-Classes.md")
md.files$b3.feats<- file.path(dir.rmd,"2-2-Feats.md")

dir.html  <-  file.path(basedir,"html")
htm.files  <- list()
htm.files$a1.intro<- file.path(dir.html,"Introduction.html")
htm.files$a2.charbuild<- file.path(dir.html,"CharacterBuilding.html")
htm.files$a3.combat<- file.path(dir.rmd,"Combat.html")
htm.files$b2.classes<- file.path(dir.html,"Classes.html")
htm.files$b3.feats<- file.path(dir.html,"Feats.html")

dir.pdf  <- file.path(basedir,"pdf")
pdf.files  <- list()
pdf.files$a1.intro<- file.path(dir.pdf,"A1-Introduction.pdf")
pdf.files$a2.charbuild<- file.path(dir.pdf,"A2-CharacterBuilding.pdf")
pdf.files$a3.combat<- file.path(dir.pdf,"A3-Combat.pdf")
pdf.files$b2.classes<- file.path(dir.pdf,"B2-Classes.pdf")
pdf.files$b3.feats<- file.path(dir.pdf,"B3-Feats.pdf")


toPdf <- function(f.in,f.out){
  system(paste("wkhtmltopdf","--zoom 1.25 ", f.in, f.out))
}


mapply(input=rmd.files,output_file=htm.files,FUN = rmarkdown::render)

mapply(f.in=htm.files,f.out=pdf.files,FUN = toPdf)




