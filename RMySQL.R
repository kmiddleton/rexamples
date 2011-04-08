library(RMySQL)
con <- dbConnect("MySQL", host="genome-mysql.cse.ucsc.edu", user = "genome", dbname = "hg18")
system.time(a <- dbGetQuery(con, "select name, chromEnd from snp129 where chrom='chr1' and chromStart between 1 and 1e8;")
