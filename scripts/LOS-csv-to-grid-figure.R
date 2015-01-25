source(file.path(basedir,"scripts","csv-to-grid-figure.R"))

example1.raw <- file.path(basedir,"raw","Grid-LOS-example-1-no-blocking.csv")

example1.matrix <- as.matrix(read.csv(example1.raw, sep=";",header=FALSE))
colnames(example1.matrix)<-NULL
example1.melt <-melt(example1.matrix,varnames=c("y","x")) %.% 
  mutate(character=(value=="A"|value=="B")) %.%
  mutate(hline=(value=="-")) %.%
  mutate(dline=(value=="/")) %.% 
  select(x,y,value,character,hline,dline)  #reorder columns


example1.melt.line <-example1.melt %.% filter (hline|dline) %.% group_by(x) %.% mutate(z=y==max(y)) %.% arrange(z)


           
           

# ggplot(example1.melt)+aes(y=-y,x=x)+
#   geom_tile(color="black",fill="white")+
#   geom_tile(color="black",fill="red",alpha=example1.melt$character)+
#   geom_text(aes(label=value, alpha=character))+
#   geom_segment(aes(x=x-0.35,xend=x+0.35,y=-y-0.35,yend=-y+0.35, alpha=dline))+
#   geom_segment(aes(x=x-0.45,xend=x+0.45,y=-y,yend=-y, alpha=hline))+
#   scale_alpha_discrete(range = c(0, 1),guide=FALSE)


ggplot(example1.melt)+aes(y=-y,x=x)+
  geom_tile(color="black",fill="white")+
  geom_tile(color="black",fill="red",alpha=example1.melt$character)+
  geom_text(aes(label=value, alpha=character))+
  geom_path(data=tmp%.% filter(z))+
  geom_path(data=tmp%.% filter(!z))+
  scale_alpha_discrete(range = c(0, 1),guide=FALSE)


