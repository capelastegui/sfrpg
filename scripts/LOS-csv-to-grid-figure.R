source(file.path(basedir,"scripts","csv-to-grid-figure.R"))

example1.raw <- file.path(basedir,"raw","Grid-LOS-example-1-no-blocking.csv")
example2.raw <- file.path(basedir,"raw","Grid-LOS-example-2-no-blocking.csv")

tmp <-meltGridCsv(example2.raw)
plotGrid(meltGridCsv(example1.raw))


# -- this is a hack, ignoring functions

example1.matrix <- as.matrix(read.csv(example1.raw, sep=";",header=FALSE))
colnames(example1.matrix)<-NULL
grid.df <-melt(example1.matrix,varnames=c("y","x")) %.% 
  mutate(character=(value=="A"|value=="B")) %.%
  mutate(hline=(value=="-")) %.%
  mutate(dline=(value=="/")) %.% 
  select(x,y,value,character,hline,dline) %.% #reorder columns 
  rbind(c(10,10,10,FALSE,FALSE,TRUE))


grid.line.df <-grid.df %.% filter (hline|dline) %.% group_by(x) %.% mutate(z=y==max(y)) %.% arrange(z)

p <- ggplot(grid.df)+aes(y=-y,x=x)+
  geom_tile(color="black",fill="white")+
  geom_tile(color="black",fill="brown",alpha=grid.df$character)+
  geom_text(aes(label=value, alpha=character))+
  geom_path(data=grid.line.df%.% filter(z))+
  geom_path(data=grid.line.df%.% filter(!z))+
  scale_alpha_discrete(range = c(0, 1),guide=FALSE)

p
