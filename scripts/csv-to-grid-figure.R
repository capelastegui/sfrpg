require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)


meltGridCsv <- function (gridfile)
{
  
  tmp.matrix <- as.matrix(read.csv(gridfile, sep=";",header=FALSE))
  colnames(tmp.matrix)<-NULL
  melt(tmp.matrix,varnames=c("y","x")) %.% 
    mutate(character=(value=="A"|value=="B")) %.%
    mutate(line1=(value=="/")) %.%
    mutate(line2=(value=="-")) %.%
    mutate(block=(value=="X")) %.% 
    mutate(cover=(value=="x")) %.% 
    mutate(shade=(value==".")) %.% 
    mutate(shade2=(value==":")) %.% 
    select(x,y,value,character,line1,line2,block,cover,shade,shade2)  #reorder columns
  
}

plotGrid <- function(grid.df)
{ 
  
  ggplot(grid.df)+aes(y=-y,x=x)+
    geom_tile(color="black",fill="white")+
    geom_tile(color="black",fill="pink",alpha=grid.df$character)+
    geom_tile(color="black",fill="grey18",alpha=grid.df$block)+
    geom_tile(color="black",fill="grey",alpha=grid.df$cover)+
    geom_tile(color="black",fill="lightskyblue",alpha=0.5*grid.df$shade)+
    geom_tile(color="black",fill="royalblue4",alpha=0.5*grid.df$shade2)+
    geom_text(aes(label=value, alpha=character))+
    scale_alpha_discrete(range = c(0, 1),guide=FALSE)+
    scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)+labs(x=NULL,y=NULL)
}


#TODO: function to plot single line. Avoid artefacts.

plotGridLOS <- function(grid.df, grid.line.plot=NULL)
{ 
  if (is.null(grid.line.plot)){
  # to plot without line, have grid.line.plot=list(null,null)
    grid.line.plot <- plotLOS(grid.df)
        }

  ggplot(grid.df)+aes(y=-y,x=x)+
    geom_tile(color="black",fill="white")+
    geom_tile(color="black",fill="pink",alpha=grid.df$character)+
    geom_tile(color="black",fill="grey18",alpha=grid.df$block)+
    geom_tile(color="black",fill="grey",alpha=grid.df$cover)+
    geom_tile(color="black",fill="lightskyblue",alpha=0.5*grid.df$shade)+
    geom_tile(color="black",fill="royalblue4",alpha=0.5*grid.df$shade2)+
    geom_text(aes(label=value, alpha=character))+
    grid.line.plot[1]+grid.line.plot[2]+
    grid.line.plot[3]+grid.line.plot[4]+
    scale_alpha_discrete(range = c(0, 1),guide=FALSE)+
    scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)+labs(x=NULL,y=NULL)
}

plotLOS <-  function(grid.df,color1="green",color2="green")
{

    list(geom_path(data=grid.df%.% filter(line1),color=color1),
         geom_path(data=grid.df%.% filter(line2),color=color2)  
    )
  
}