##  R code for mailing and mass-mailing
## 16 Jan: added auth, getPassword, angle brackets
## TODO: documentation?
##       Tcl/Tk interface?
##       allow multi-character delimiters
##       port to python?
##       wish list: attachments, GPG interface
##      password entry; exit on ENTER?
##  package?

print.read.socket <- function(socket) {
  repeat{
    ss <- read.socket(socket)
    if (ss == "") break
    cat(ss,"\n")
  }
}

fake.send.mail <- function(text,to) {
  cat("Sending mail to ",to,":\n",sep="")
  cat(text,sep="\n")
}

## from Barry Rowlingson, R list, 16 Feb 2006
getPassword <- function(){
   require(tcltk)
   tt <- tktoplevel()
   pass=tclVar("")
   label.widget <- tklabel(tt, text="Enter Password")
   password.widget <- tkentry(tt,show="*",textvariable=pass)
   ok <- tkbutton(tt,text="Ok",command=function()tkdestroy(tt))
   tkpack(label.widget, password.widget,ok)
   tkwait.window(tt)
   return(tclvalue(pass))
} 
  
send.mail <- function(text,
                      to,
                      from="bolker@zoo.ufl.edu",
                      hostname="bolker-lap.zoo.ufl.edu",
                      auth=FALSE,user=NULL,passwd=NULL,
                      server="smtp.ufl.edu",
                      verbose=FALSE,
                      port=25) {
  if (auth) {
    if (!require(caTools)) stop("install caTools package for base64encode()")
    if (is.null(passwd)) passwd <- getPassword()
    if (is.null(user)) stop("must specify user for authentication")
  }
  m <- make.socket(server,port=port)
  ws <- if(!verbose) {
    function(s) { write.socket(m,s) }
  } else {
    function(s) {cat(">>",s,"\n"); write.socket(m,paste(s,"\n",sep="")) }
  }
  rs <- function() { if (verbose) print(read.socket(m))}
  rs()
  on.exit(close.socket(m))
  ws(paste("EHLO",hostname))
  rs()
  if (auth) {
    ws("AUTH LOGIN")
    rs()
    ws(base64encode(user))
    rs()
    ws(base64encode(passwd))
    rs()
  }
  ws(paste("MAIL From:<",from,">",sep=""))
  rs()
  ws(paste("RCPT To:<",to,">",sep=""))
  rs()
  ws("DATA")
  sapply(text,function(t)ws(t))
  ws(".")
  rs()
  ws("QUIT")
  rs()
}

# send.mail(text=c("Subject: test","test"),to="bolker@zoo.ufl.edu",verbose=TRUE)
##send.mail(text=c("Subject: test","Reply-to: bolker@zoo.ufl.edu",
##            "test"),to="bolker@zoo.ufl.edu",verbose=TRUE)
testmail <- function() {
  send.mail(text=c("Subject: test","Reply-to: bolker@zoo.ufl.edu",
              "test"),to="bolker@zoo.ufl.edu",auth=TRUE,port=587,
            user="bolker",verbose=TRUE)
}

mailmerge <- function(data,  ## data frame with names corresponding to fields
                      msgfile, ## message text
                      delim="##", ## begin/end delimiters
                      mail=FALSE, ## send mail?
                      ofile=TRUE, ## create output files?
                      id="email",     ## data field to use as id
                      email="email",  ## name of email address field in data
                      fake=FALSE,     ## simulate sending mail only
                      auth=FALSE,
                      passwd=NULL,
                      trace=0,    ## tracing: 1= print ids/data, 2=also SMTP verbose
                      ...) {      ## additional args
  if (auth) {
    if (is.null(passwd)) passwd <- getPassword()
  }
  delim <- strsplit(delim,"")[[1]]
  if (length(delim)==1) delim <- rep(delim,2)
  fieldnames <- colnames(data)
  nfields <- ncol(data)
  ## error checking ...
  msg <- readLines(msgfile)
  for (i in 1:nrow(data)) {
    if (trace>0) cat(data[i,id],"\n")
    curmsg <- msg
    for (j in 1:length(msg)) {
      for (k in 1:nfields) {
        curmsg[j] <- gsub(pattern=paste(delim[1],fieldnames[k],delim[2],sep=""),
                       replacement=data[i,k],
                          x=curmsg[j])
      }
    }
    if (ofile) {
      cat(curmsg,file=paste("mmerge",data[i,id],"txt",sep="."),sep="\n")
    }
    if (mail) {
      if (nchar(data[i,email])>0) {
        if (fake) {
          fake.send.mail(curmsg,to=data[i,email],verbose=(trace>1),auth=auth,passwd=passwd,...)
        } else {
          send.mail(text=curmsg,to=data[i,email],verbose=(trace>1),auth=auth,passwd=passwd,...)
        }
      }
    }
  }
}

## examples
if (FALSE) {
  gdata <- as.matrix(read.csv("pastgrads2.csv",as.is=TRUE))
  mailmerge(gdata,"pastgrad-msg.txt",id="Last.Name",email="e.mail",mail=TRUE,
            fake=TRUE)
  mailmerge(gdata,"pastgrad-msg.txt",id="Last.Name",email="e.mail",mail=TRUE,
            fake=FALSE,verbose=TRUE)
}

if (FALSE) {
  setwd("~/book")
  gdata <- as.matrix(read.csv("emails3.txt"))
  mailmerge(gdata,"jun-email.txt",id="lastname",email="email",mail=TRUE,
            auth=TRUE,user="bolker",port=587,
            fake=FALSE,trace=2)
}

if (FALSE) {
  setwd("~/grants/mbs_igert")
  library(gdata)
  gdata <- read.xls("pinfo.xls")
  have.pinfo <- gdata$pinfo=="Y"
  have.cinfo <- gdata$collabinfo=="Y"
  gdata <- subset(gdata,
                  pinfo=="N" | collabinfo=="N" |
                  (need_biosketch=="Y" & biosketch=="N"))

  mailmerge(gdata,"igert-nag.txt",id="last",email="email",
            mail=TRUE,auth=TRUE,user="bolker",port=587,
            fake=TRUE

}

if (FALSE) {
## Tcl/Tk stuff ...
##
## drag and drop/file browser for email list (excel??),
##     message
library(tcltk)
tt <- tktoplevel()
tkwm.title(tt,"mailmerge")
tkpack(txt.w <- tkentry(tt,text="data file"))
tkpack(txt2.w <- tkentry(tt,text="msg file"))
sendmail <- tclVar(0)
createfiles <- tclVar(1)
tkpack(mail.w <- tkcheckbutton(tt,text="send mail?",variable=sendmail))
tkpack(file.w <- tkcheckbutton(tt,text="create files?",variable=createfiles))
## callback function 
eval.txt <- function() {
  print(tclvalue(tkget(txt.w)))
  print(tclvalue(tkget(txt2.w)))
  print(tclvalue(tkget(file.w)))
  cf <- as.logical(tclObj(createfiles))
  sm <- as.logical(tclObj(sendmail))
  print(cf)
  print(sm)
}
tkpack(but.w <- tkbutton(tt,text="Go", command=eval.txt))
tkdestroy(tt)


}

Rmailbug <- function(subject,...) {
  fn = tempfile("Rmailbug")
  on.exit(unlink(fn))
  bug.report(subject,method="none")
  z = readLines(fn)
  if (substr(toupper(readline("send bug report? ")),1,1)=="Y") {
    send.mail(to="r-bugs@r-project",text=z,...)
  }
}
  
