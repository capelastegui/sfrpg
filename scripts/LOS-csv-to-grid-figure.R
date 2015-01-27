source(file.path(basedir,"scripts","csv-to-grid-figure.R"))

example1.df <- meltGridCsv(file.path(basedir,"raw","Grid-LOS-example-1-no-blocking.csv"))
example2.df <- meltGridCsv(file.path(basedir,"raw","Grid-LOS-example-2-no-blocking.csv"))
example3.df <- meltGridCsv(file.path(basedir,"raw","Grid-LOS-example-3-blocking.csv"))
example4.df <- meltGridCsv(file.path(basedir,"raw","Grid-LOS-example-4-wall.csv"))
example4b.df <- meltGridCsv(file.path(basedir,"raw","Grid-LOS-example-4b-wall.csv"))

plot1.file <- file.path(basedir,"raw","Grid-LOS-example-1-no-blocking.csv")

LOS1 <-plotLOS(example1.df)
plotGridLOS(example1.df)
plotGridLOS(example2.df,plotLOS(example1.df,color2="darkred"))
plotGridLOS(example3.df,plotLOS(example1.df,color1="darkred",color2="darkred"))
plotGridLOS(example4b.df)
plotGridLOS(example1.df)


example.cover1.df <- meltGridCsv(file.path(basedir,"raw","Grid-cover-example-1-diag.csv"))
example.cover2.df <- meltGridCsv(file.path(basedir,"raw","Grid-cover-example-2-diag-non-adj.csv"))
example.cover3.df <- meltGridCsv(file.path(basedir,"raw","Grid-cover-example-3-vert.csv"))
example.cover4.df <- meltGridCsv(file.path(basedir,"raw","Grid-cover-example-4-combi.csv"))
plotGrid(example.cover1.df)
plotGrid(example.cover2.df)
plotGrid(example.cover3.df)
plotGrid(example.cover4.df)