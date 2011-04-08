library(XML)
doc <-
xmlTreeParse("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=0_JYbpsax0ZAAPnOd7nFAX-29fXDpTk5t8M4hx9ytT-",
isURL = TRUE, useInternalNodes = TRUE)
sapply(c("//author", "//category"), xpathApply, doc = doc, fun = xmlValue)

# or BioC package annotate

# or in Python
from elementtree.ElementTree import XML
import urllib

url = 'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/erss.cgi?rss_guid=0_JYbpsax0ZAAPnOd7nFAX-29fXDpTk5t8M4hx9ytT-'
con = urllib.urlopen(url)
dat = con.read()
root = XML(dat)
items = root.findall("channel/item")
for item in items:
     category = item.find("category")
     print category.text
