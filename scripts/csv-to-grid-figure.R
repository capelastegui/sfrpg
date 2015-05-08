require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)


meltGridCsv <- function (gridfile)
{
  
  tmp.matrix <- as.matrix(read.csv(gridfile, sep=";",header=FALSE))
  colnames(tmp.matrix)<-NULL
  melt(tmp.matrix,varnames=c("y","x"))

}

plotGrid <- function(grid.df)
{ 
  grid.df$label<-revalue(grid.df$value, 
                         c("0"="","X"="", "x"="", 
                           "."="", ":"="",
                           "A"="A","B"="B"))
    
  ggplot(grid.df)+aes(y=-y,x=x)+
    geom_tile(color="black", aes(fill=value))+
    geom_text(aes(label=label))+
    #scale_x_continuous(breaks=NULL)+
    #scale_y_continuous(breaks=NULL)+labs(x=NULL,y=NULL) + 
    scale_fill_manual(
      values = c("0"="lightgreen","X"="grey20", 
                 "x"="tan4", 
                 "."="tan", ":"="grey60",
                 "A"="red3","B"="brown3",
                 "-"=NULL, "/"=NULL),
      labels=c("0"="Ground","X"="Block", "x"="Cover", 
               "."="Cover Zone", ":"="Blocked Zone",
               "A"="Character A","B"="Character B",
               "-"="", "/"=""))
}

#todo: adapt this
#p+geom_segment(data=example.cover1.df,aes(x=x-0.5, xend=x+0.5, y =-y+0.5, yend=-y+0.5),size=1.5,color="grey30")
#p+geom_segment(data=example.cover1.df,aes(x=x-0.5, xend=x+0.5, y =-y+0.5, yend=-y+0.5),size=1.5,color="brown60")



#TODO: function to generate single line. Avoid artefacts.

#TODO: make this work with a list of n line plots. How to do ggplot.+  with lists?
plotGridLOS <- function(grid.df, grid.line.plot=NULL)
{ 
  if (is.null(grid.line.plot)){
    # to plot without line, have grid.line.plot=list(null,null)
    grid.line.plot <- plotLOS(grid.df)
  }
  
    plotGrid(grid.df)+
    grid.line.plot[1]+grid.line.plot[2]+
    grid.line.plot[3]+grid.line.plot[4]+
    scale_fill_manual(
      values = c("0"="lightgreen","X"="grey20", 
                 "x"="tan4", 
                 "."="tan", ":"="grey60",
                 "A"="red3","B"="brown3",
                 "-"=NULL, "/"=NULL),
      labels=c("0"="Ground","X"="Block", "x"="Cover", 
               "."="Cover Zone", ":"="Blocked Zone",
               "A"="Character A","B"="Character B",
               "-"="", "/"=""))
}


plotLOS <-  function(grid.df,color1="darkgreen",color2="darkgreen")
{
    list(geom_path(data=grid.df%.% filter(value=="/"), size=1.5,color=color1),
         geom_path(data=grid.df%.% filter(value=="-"), size=1.5, color=color2)  
    )
  
}