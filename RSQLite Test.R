library(RSQLite)
setwd('/Users/kmm/Desktop/ontime')

ontime <- dbConnect("SQLite", dbname = "ontime.sqlite3")

from_db <- function(sql) {
  dbGetQuery(ontime, sql)
}

from_db("select count(*), TailNum from ontime group by TailNum")

tails <- from_db("select distinct TailNum from ontime")
