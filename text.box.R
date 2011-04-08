#text.box.r

source('http://biostat.mc.vanderbilt.edu/twiki/pub/Main/TatsukiRcode/TatsukiRcodeTextBox.r')

txt1 <- c(
	'<b>Lorem ipsum</b> <br>dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.',
	'<p>Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>'
	)
	
default <- list(family='serif', font=1, col=1, cex=0.5)

plot(1:10)
text.box(tex = txt1, x = c(1, 5), y = 10, x.padding=c(1, 1), y.padding=c(1, 1), 
	vertical.stretch = 1.2, paragraph.break = 1.5, justify = FALSE, bkgr.col = 'white',
	slide.quote = TRUE, after. = 1.75)

