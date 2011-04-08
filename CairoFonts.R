library(Cairo)

CairoFonts(
  regular="Gill Sans:style=Regular",
  bold="Gill Sans:style=Bold",
  italic="Gill Sans:style=Italic",
  bolditalic="Gill Sans:style=Bold Italic,BoldItalic",
  symbol="Symbol"
)

CairoPDF(file="file.pdf")

plot(1:10)
dev.off()
