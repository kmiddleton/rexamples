setwd('/Users/kmm/Dropbox/biology.csusb.edu/birdinfo/images')

#convert [input-options] input-file [output-options] output-file
files <- list.files(pattern = "\\.jpg")

# 75%, strip thumbnail
for (i in files){
	outfile <- sub("\\.jpg", "_resize.jpg", i)
	x <- paste("convert ", i, " -quality 75 -strip ", i, sep = '')
	system(x)
	}

# 75%, strip thumbnail, width
for (i in files){
	x <- paste("convert ", i, " -quality 75 -strip -resize '350' ", i, sep = '')
	system(x)
	}
