knitRmd <- function()
{
  dir_base  <- here::here()
.Platform$file.sep <- "\\"

dir.htmpdf <- file.path('C:','\"Program Files\"','wkhtmltopdf','bin')

dir.rmd  <-  file.path(dir_base,"Rmd")
rmd.files <- list()
rmd.files$a1.intro<- file.path(dir.rmd,"1-1-Introduction.Rmd")
rmd.files$a2.charbuild<- file.path(dir.rmd,"1-2-CharacterBuilding.Rmd")
rmd.files$a3.combat<- file.path(dir.rmd,"1-3-Combat.Rmd")
rmd.files$b2.classes<- file.path(dir.rmd,"2-1-Classes.Rmd")
rmd.files$b3.feats<- file.path(dir.rmd,"2-2-Feats.Rmd")
rmd.files$a.rules<- file.path(dir.rmd,"1-SFRPG-Game-Rules.Rmd")
rmd.files$b.charbuild <- file.path(dir.rmd,"2-Character-Building.Rmd")


# dir.rmd  <-  file.path(dir_base,"rmd")
# md.files  <- list()
# md.files$a1.intro<- file.path(dir.rmd,"1-1-Introduction.md")
# md.files$a2.charbuild<- file.path(dir.rmd,"1-2-CharacterBuilding.md")
# md.files$a3.combat<- file.path(dir.rmd,"1-3-Combat.md")
# md.files$b2.classes<- file.path(dir.rmd,"2-1-Classes.md")
# md.files$b3.feats<- file.path(dir.rmd,"2-2-Feats.md")
# md.files$a.rules<- file.path(dir.rmd,"1-SFRPG-Game-Rules.md")
# md.files$b.charbuild <- file.path(dir.rmd,"2-CharacterBuilding.md")


dir.html  <-  file.path(dir_base,"html")
htm.files  <- list()
htm.files$a1.intro<- file.path(dir.html,"Introduction.html")
htm.files$a2.charbuild<- file.path(dir.html,"CharacterBuilding.html")
htm.files$a3.combat<- file.path(dir.html,"Combat.html")
htm.files$b2.classes<- file.path(dir.html,"Classes.html")
htm.files$b3.feats<- file.path(dir.html,"Feats.html")
htm.files$a.rules<- file.path(dir.html,"1-SFRPG-Game-Rules.html")
htm.files$b.charbuild <- file.path(dir.html,"2-CharacterBuilding.html")

dir.pdf  <- file.path(dir_base,"pdf")
pdf.files  <- list()
pdf.files$a1.intro<- file.path(dir.pdf,"A1-Introduction.pdf")
pdf.files$a2.charbuild<- file.path(dir.pdf,"A2-CharacterBuilding.pdf")
pdf.files$a3.combat<- file.path(dir.pdf,"A3-Combat.pdf")
pdf.files$b2.classes<- file.path(dir.pdf,"B2-Classes.pdf")
pdf.files$b3.feats<- file.path(dir.pdf,"B3-Feats.pdf")
pdf.files$a.rules<- file.path(dir.pdf,"1-SFRPG-Game-Rules.pdf")
pdf.files$b.charbuild <- file.path(dir.pdf,"2-CharacterBuilding.pdf")

toPdf <- function(f.in,f.out){
  system(paste(file.path(dir.htmpdf,"wkhtmltopdf"),"toc --zoom 1.25 ", f.in, f.out))
}


mapply(input=rmd.files,output_file=htm.files,FUN = rmarkdown::render)

mapply(f.in=htm.files,f.out=pdf.files,FUN = toPdf)

#toPdf(paste (htm.files, collapse=" "), file.path(dir.pdf,"SFRPG-full.pdf"))

}







