require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)


meltGridCsv <- function (gridfile)
{
  
  tmp.matrix <- as.matrix(read.csv(gridfile, sep=";",header=FALSE))
  colnames(tmp.matrix)<-NULL
  tmp.melt <-melt(tmp.matrix,varnames=c("y","x")) %.% 
    mutate(character=(value=="A"|value=="B")) %.%
    mutate(hline=(value=="-")) %.%
    mutate(dline=(value=="/")) %.% 
    select(x,y,value,character,hline,dline)  #reorder columns
  
}


plotGrid <- function(grid.df)
{
  
  grid.line.df <-grid.df %.% filter (hline|dline) %.% group_by(x) %.% mutate(z=y==max(y)) %.% arrange(z)
  
  ggplot(grid.df)+aes(y=-y,x=x)+
    geom_tile(color="black",fill="white")+
    geom_tile(color="black",fill="brown",alpha=grid.df$character)+
    geom_text(aes(label=value, alpha=character))+
    geom_path(data=grid.line.df%.% filter(z))+
    geom_path(data=grid.line.df%.% filter(!z))+
    scale_alpha_discrete(range = c(0, 1),guide=FALSE)
}