plot(1:10)
loc <- locator(1)

do.call(text, c(loc, "abc"))

loc

$x
[1] 6.555042

$y
[1] 5.188462