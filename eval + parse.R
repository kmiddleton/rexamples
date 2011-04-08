st <- "a$x"

x <- 1:10
y <- 11:20
a <- data.frame(x, y)

# Works
eval(parse(text = st))

# Does not work
get(st)
