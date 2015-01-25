source(file.path(basedir,"scripts","csv-to-grid-figure.R"))

example1.raw <- file.path(basedir,"raw","Grid-LOS-example-1-no-blocking.csv")

example1.matrix <- as.matrix(read.csv(example1.raw, sep=";",header=FALSE))
colnames(example1.matrix)<-NULL
example1.melt <-melt(example1.matrix) %.% 
  mutate(character=(value=="A"|value=="B")) %.%
  mutate(hline=(value=="-")) %.%
  mutate(dline=(value=="/")) 


example1.melt.line <-example1.melt %.% filter (hline|dline)



           
           

ggplot(example1.melt)+aes(y=-Var1,x=Var2)+
  geom_tile(color="black",fill="white")+
  geom_tile(color="black",fill="red",alpha=example1.melt$character)+
  geom_text(aes(label=value, alpha=character))+
  geom_segment(aes(x=Var2-0.35,xend=Var2+0.35,y=-Var1-0.35,yend=-Var1+0.35, alpha=dline))+
  geom_segment(aes(x=Var2-0.45,xend=Var2+0.45,y=-Var1,yend=-Var1, alpha=hline))+
  scale_alpha_discrete(range = c(0, 1),guide=FALSE)


ggplot(example1.melt.line)+aes(y=-Var1,x=Var2)+
  geom_tile(color="black",fill="white")+geom_line()
