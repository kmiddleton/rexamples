# See http://www.plosntds.org/static/figureGuidelines.action#arialR

set.seed(5)
x <- rnorm(100)

pdf(file="try_arial.pdf",
	onefile=F,
	width=4, height=4,
	family=c("/Users/kmm/Dropbox/R/Arial/arial.afm",
			 "/Users/kmm/Dropbox/R/Arial/arialbd.afm",
			 "/Users/kmm/Dropbox/R/Arial/arialbi.afm",
			 "/Users/kmm/Dropbox/R/Arial/ariali.afm"),
	pointsize=12)
hist(x)
dev.off()

pdf(file="try_helv.pdf",
	onefile=F,
	width=4, height=4,
	pointsize=12)
	hist(x)
dev.off()
