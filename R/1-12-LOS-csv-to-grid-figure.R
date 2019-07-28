updateLosPlots <- function()
{
example1.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-1-no-blocking.csv"))
example1.line.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-1-no-blocking-lines.csv"))
example2.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-2-no-blocking.csv"))
example3.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-3-blocking.csv"))
example4.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-4-wall.csv"))
example4.line1.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-4-wall-line1.csv"))
example4.line2.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-LOS-example-4-wall-line2.csv"))

plot1.file <- file.path(here::here(),"raw","Grid-LOS-example-1-no-blocking.csv")

plotGridLOS(example1.df, plotLOS(example1.line.df))
ggsave(file.path(here::here(),"figures","Grid-LOS-example-1-no-blocking.png"))
plotGridLOS(example2.df,plotLOS(example1.line.df,color2="darkred"))
ggsave(file.path(here::here(),"figures","Grid-LOS-example-2-no-blocking.png"))
plotGridLOS(example3.df,plotLOS(example1.line.df,color1="darkred",color2="darkred"))
ggsave(file.path(here::here(),"figures","Grid-LOS-example-3-blocking.png"))
# Note: the arrange(y) part is needed to order path points properly
plotGridLOS(example4.df, c(plotLOS(example4.line1.df)[2], plotLOS(example4.line2.df %>% arrange(y))[2]))
ggsave(file.path(here::here(),"figures","Grid-LOS-example-4-wall.png"))

example.cover1.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-cover-example-1-diag.csv"))
example.cover2.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-cover-example-2-diag-non-adj.csv"))
example.cover3.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-cover-example-3-vert.csv"))
example.cover4.df <- meltGridCsv(file.path(here::here(),"raw","combat","Grid-cover-example-4-combi.csv"))
plotGrid(example.cover1.df)
ggsave(file.path(here::here(),"figures","Grid-cover-example-1-diag.png"))
plotGrid(example.cover2.df)
ggsave(file.path(here::here(),"figures","Grid-cover-example-2-diag-non-adj.png"))
plotGrid(example.cover3.df)
ggsave(file.path(here::here(),"figures","Grid-cover-example-3-vert.png"))
plotGrid(example.cover4.df)
ggsave(file.path(here::here(),"figures","Grid-cover-example-4-combi.png"))
}



