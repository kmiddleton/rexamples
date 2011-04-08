tab5rows <- read.table("datatable.txt", header = TRUE, nrows = 5)
classes <- sapply(tab, class)
tabAll <- read.table("datatable.txt", header = TRUE, colClasses = classes)

