mail <- function(address, subject, message) {
	system(paste("echo '", message,
	"' | mail -s '", subject,
	"' ", address, sep=""))
	}

mail("kmm@csusb", "Test", "Hello world!")