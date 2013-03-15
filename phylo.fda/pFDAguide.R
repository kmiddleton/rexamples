#Step-by-Step Guide to R Script Usage
#Step 1. Prepare your data set.

#Prepare your data using your spreadsheet software. Your data should 
#be similar to below:

#taxon	measure1	measure2	?	measureN	groups
#Gen1_sp1	2.8	8.13	?	4.53	nocturnal
#Gen2_sp2	1.41	8.34	?	6.56	unknown
#Gen3_sp3	2.36	8.44	?	3.53	nocturnal
#Gen4_sp4	1.33	7.87	?	3.6	diurnal
#Gen5_sp5	2.72	6.5	?	5.79	diurnal
#Gen6_sp6	4.83	9.34	?	4.79	diurnal
#Gen7_sp7	1.62	8.09	?	4.39	unknown
#?	..	?	?	?	?
#GenM_spM	4.8	9.26	?	5.17	diurnal

#taxon: Species names or equivalents. Do not use space. They should 
#be in the order in which they appear in the phylogeny (i.e., NEXUS 
#file). 
#measure1 ... : Continuous variables representing measurements, 
#such as "eyeball_diameter" or "lens_ diameter".
#groups: A categorical variable of interest. In the example above, 
#diel activity pattern is used.

#Step 2: Start R and get it ready

#Start R, load relevant libraries (especially the packages ape and 
#geiger), set the working directory if necessary. You also need to 
#load functions in the phylo.fda.R, which is supplied earlier in 
#this supporting online material. 

#Step 3. Input Phylogeny

#Use Mesquite or some other software to prepare your phylogenetic 
# tree. Make sure that branch lengths are specified. We recommend 
#using divergence time to calculate branch lengths. Also, it is 
#critical to have the taxa in this file arranged in the same 
#sequence as they appear in the data set in Step 1. To reanalize 
#our data, use the Nexus file attached in this supporting online 
#material. Then:

treA <- read.nexus("tree.nex")

#Note that "your_file_name" should include the folder location if your 
#NEXUS file is not in the working directory. The current working 
#directory can be found by the command getwd(), and set by setwd(). 
#Next, check if the tree is binary. If not, force it to be binary.

if(!is.binary.tree(treA)) treA <- multi2di(treA, random = TRUE)

#Finally, if relevant, check if the tree is ultrametric. This would 
#be irrelevant if your tree has fossil taxa.

is.ultrametric(treA)

#Step 4: Input your data and prepare them

#4-1: Copy the data from spreadsheet
#If you are a Windows user, type the following command in R:

ddA <- as.data.frame(read.delim("clipboard"))
ddA

#Copy the relevant area of the spreadsheet into the clipboard and 
#then hit enter.  If you use Mac OS X or Linux, you need to save the 
#spreadsheet as a .csv file (say, data.csv), move it to the working 
#directory of R (you can find where it is by typing getwd() and 
#hitting enter in R), and load the file using the command:

ddA <- read.csv("data.csv",quote="")

#Confirm that the data were correctly read into R by typing:

ddA

#If you are not sure about the taxon order being identical between 
#your tree and data set, you can run the following command:

rownames(ddA) <- ddA$taxon
ddA <- ddA[treA$tip.label,]

#If you ran these two lines, then make sure that the rows sorted 
#correctly (i.e., type "ddA" , hit return, and go through the matrix 
#that is displayed). If the commands above gave you NAs in the data, 
#that means the taxon names do not match exactly between the tree 
#and spreadsheet.

#4-2: Extract group categories 
#If your spreadsheet had a heading "groups" for categories as in 
# the example in Step 1, then:

gA <- ddA$groups

#4-3: Extract taxon names 
#If your spreadsheet had a heading "taxon" for the taxon names, then:

taxaA <- ddA$taxon
rownames(ddA) <- taxaA       # for later convenience

#Make sure that taxon names are in the same order between the 
#spreadsheet and the NEXUS file that will be used in Step 4.

#4-4: Extract the measurements and transform(?) 
#If your measurements are in the second to fourth columns of ddA, 
#then:

XA <- ddA[,2:4]

#If you need to log transform your measurements, then:

XA <- log10(XA)
XA <- signif(XA, 3)

#4-5: Identifies the taxa that are to be used in test data
#If you identified test data taxa as "unknown" in your dataset, as 
#in the example in Step 1, then use the following commands:

testtaxa <- rownames(ddA[gA=="unknown",])
testtaxan <- row(ddA)[gA=="unknown",1]
trainingtaxa <- rownames(ddA[-testtaxan,])
X <- XA[-testtaxan,]
dd <- ddA[-testtaxan,]
g <- gA[-testtaxan]
g <- g[g %in% names(table(g))[table(g) > 0], drop=TRUE]

#4-6. Extract training data part of the tree
tre <- drop.tip(treA, testtaxa)

#Step 5: Find the optimal ? value.
#You need to find the strength of phylogenetic noise in your data in 
#terms of Pagel's ?. You can find this optimal ? value, as defined 
#in Motani and Schmitz (in press), in two alternative ways. You can 
#choose to find this value based on training data set only, or based 
#on the entire data set. Either way, only the training part of the 
#data set is used for most of the calculations. The only difference 
#would be whether the phylogenetic noise in the training taxa is 
#evaluated with some influence from test data set or not.

#5-1. Find the optimal ? value using training data only
#Run the commands below. It takes time (several minutes?) for the 
#second line to be processed.

filename_stem <- "NameOfYourChoice"
ol1 <- optLambda(X,g,tre,idc=filename_stem)
ol1$optlambda

#Take a note of the optimal ? value for "logLik". Also, a PDF file 
#is generated automatically to record the graphs that appear. The 
#file is named according to the filename_stem that you specify, and 
#saved in the working directory.

#5-2. Find the optimal ? value using training data, with influence 
#from test data
#Run the commands below. It takes time (several minutes?) for the 
#second line to be processed.

filename_stem <- "NameOfYourChoice"
ol2<-optLambda.pred(XA,gA,taxaA,treA,testtaxan,idc=filename_stem)
ol2$optlambda

#Take a note of the optimal ? value for "logLik". Also, a PDF file 
#is generated automatically to record the graphs that appear. The 
#file is named according to the filename_stem that you specify, and 
#saved in the working directory.

#Step 6: Run phylogenetic discriminant analysis

#6-1. Simplest analysis

optl <- 0.08 #replace with the optimal lambda value from Step 5
pfda <- phylo.fda.pred(XA,gA,taxaA,treA,testtaxan,val=optl)
pfda$testprediction

#6-2. Using specific prior probabilities

#Pior probabilities have a substantial effect on the outcome of 
#phylogenetic fda. When appropriate values can be estimated, it is 
#best to supply these values. Otherwise, the proportion that is found 
#in the training data set is used by default. Specify the prior 
#probabilities for the categories in g (defined in Step 4-5), in the 
#order as they appear in g (usually alpha-numerally sorted). The 
#numbers below is based on the estimated proportions among 
#cathemeral, diurnal, and nocturnal species among extant amniotes. 

pri  <- c(0.144,0.585,0.271)
priold <- c(0.1427,0.5864,0.2709)
#Note that pri as defined above only works if your g has three 
#categories. Run the commands below. Note the slight difference in 
#the second line compared to 6-1. The commands below were written 
#for the data set provided in supporting online material.

optl <- 0.08 #replace with the optimal lambda value from Step 5
pfda <- phylo.fda.pred(XA,gA,taxaA,treA,testtaxan,val=optl,priin=priold)
pfda$testprediction

pfda2<- phylo.fda(X, g, tre, val=optl, priin=priold)

#You should receive a warning from the second line but you can ignore it.

#6-3. Finding results across a range of ? values
#The script below would only work with our data set, which is provided as online supporting material. Given that the estimation of the optimal ? value may involve errors, it is worthwhile to check how changing the ? value may affect the outcome of phylogenetic fda. We first calculate the outcome across a range of ? values, in this case 0, 0.01,0.02,...,1.00. It should take some time to process these commands.

lambdalist <- c(0:100)/100
nlambda <- length(lambdalist)
lmbd <- 0
pfp <- phylo.fda.pred(XA,gA,taxaA,treA,testtaxan,lmbd,priin=pri)
outcome<-pfp$testprediction
for(i in 1:(nlambda-1)){
  lmbd <- lambdalist[i+1]
  pfp <- phylo.fda.pred(XA,gA,taxaA,treA,testtaxan,lmbd,priin=pri)
  outcome<-cbind(outcome,pfp$testprediction)
}

#We then convert the outcome to numbers and add names to rows and columns so that it is easier to plot.

outcomen<- matrix(0,nrow(outcome),ncol(outcome))
for(i in 1:ncol(outcome)){
  for(j in 1:nrow(outcome)){
    if(outcome[j,i]=="diurnal") outcomen[j,i]<-3
    if(outcome[j,i]=="cathemeral") outcomen[j,i]<-2
    if(outcome[j,i]=="nocturnal") outcomen[j,i]<-1
  }
}
colnames(outcome) <- lambdalist
colnames(outcomen) <- lambdalist
rownames(outcomen) <- rownames(outcome)

#You can save the outcome to keep record.

dataname <- "DinoPrediction"
dname <- paste("./",dataname,sep="")
dput(outcomen,paste(dname,"_num",sep=""))
dput(outcome,paste(dname,"_char",sep=""))

#We then make a plot for easier visualization.  First, specify the 
#colors to be used for categories, and give the category names to 
#appear on the plot.

dapcol <- c("black","blue","yellow")
lgd <- c("Nocturnal","Cathemeral/Crepuscular","Diurnal")

#If you would rather have a grayscale plot, then use:

dapcol <- c("black","gray","white")

#We then set parameters that are necessary for plotting.
	
yLabels <- rownames(outcomen); xLabels <- colnames(outcomen)
reverse <- nrow(outcomen):1;  yLabels <- yLabels[reverse]
routcomen <- outcomen[reverse,]
m1 <- 5; m2 <- 0

#Then run the following commands to display a plot on screen.	

x11(width=11,height=8.5)
par(mfrow=c(1,1),mar=c(m1,10,6,m1),oma=c(1,m2,m2,m2))
image(c(1:length(xLabels)), c(1:length(yLabels)), z=t(routcomen), 	col=dapcol,xlab=expression(lambda),ylab="",axes=FALSE, main= 	dataname, sub=paste("Optimum = ", optl, sep=""))
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, font=3, las= 	HORIZONTAL<-1,  cex.axis=0.7)
abline(v=c(which(xLabels==optl)),col=c(2)); abline(h=c(0:(length(yLabels)+1))-0.5); box()
par(xpd=NA)
legend(x=-20,y=42,lgd,fill=dapcol,bg="white")

#You can save the plot as a PDF file by running the commands below. You may need to adjust the position of the legend to suit your machine's setting by changing x and y values in the penultimate line.	

pdf(paste(dname,".pdf",sep=""),width=11,height=8.5);
par(mfrow=c(1,1),mar=c(m1,10,6,m1),oma=c(1,m2,m2,m2))
image(c(1:length(xLabels)), c(1:length(yLabels)), z=t(routcomen), 	col=dapcol,xlab=expression(lambda),ylab="",axes=FALSE, main= 	dataname, sub=paste("Optimum = ", optl, sep=""))
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, font=3, las= 	HORIZONTAL<-1,  cex.axis=0.7)
abline(v=c(which(xLabels==optl)),col=c(2)); abline(h=c(0:(length(yLabels)+1))-0.5); box()
par(xpd=NA)
legend(x=-20,y=39,lgd,fill=dapcol,bg="white")
dev.off()
