library("RCurl")
opts <- curlOptions(header = FALSE, userpwd = "kmiddleton:MT14km!@#", netrc = FALSE)
tweet <- function(status){
	method <- "http://twitter.com/statuses/update.xml?status="
	encoded_status <- URLencode(status)
	request <- paste(method,encoded_status,sep = "")
	postForm(request,.opts = opts)
	}
tweet('twitter from R')