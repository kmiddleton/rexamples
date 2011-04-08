library(pdf2)

pdf(file = "test.pdf")
plot(1:10)
text(5, 5, "Hello World", popup = "http://en.wikipedia.org/wiki/Hello_world", pcol = 'blue')
dev.off()

