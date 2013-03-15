library(devtools)

install_github('fontsdb', 'wch')
install_github('fonts', 'wch')

library(fonts)
font_import()

###
library(fonts)
library(ggplot2)
setupPdfFonts()
fonts()

pdf('~/Desktop/fonttest.pdf', width=4, height=4)
p <- ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

# On Mac, Impact is available
p + opts(axis.title.x=theme_text(size=16, family="Impact", 
                                 colour="red"))

# On Linux, Purisa and/or Droid Serif may be available
p + opts(axis.title.x = theme_text(size=16, family="Arial", 
                                   colour="red"))
dev.off()
embedExtraFonts('~/Desktop/fonttest.pdf', outfile='fonttest-embed.pdf')
