install.if=function(x) if(!x%in%installed.packages()[,"Package"]) install.packages(x)
install.if("httr")
install.if("rvest")
install.if("rrapply")
install.if("jsonlite")
install.if("caTools")


library(httr)
library(rvest)
library(rrapply)
library(jsonlite)
library(caTools)


if(!exists(date))  date <- readline("showtime date:  ") %>% gsub("\"","",.)



dir="/cloud/project/fandango/"; dir.create(dir); dir=dir %>% paste(.,date); dir.create(dir); setwd(dir)

data.frame(name=NA,chainName=NA,city=NA,state=NA,id=NA,latitude=NA,longitude=NA,i=NA)[-1,] %>% write.csv("theaters.csv",row.names = F)
data.frame(movieId=NA,title=NA,runtime=NA,releaseDate=NA,rating=NA,theaterId=NA)[-1,] %>% write.csv("movies.csv",row.names = F)
data.frame(time=NA,showId=NA,movieId=NA,date=NA,theaterId=NA)[-1,] %>% write.csv("showtimes.csv",row.names=F)

a="https://www.fandango.com/movie-theaters" %>% read_html %>% html_nodes("#page > div > div.width-75.tablet-width-100 > section:nth-child(3) > div > ul > li > a") %>% html_attr("href") %>% .[!grepl("null",.)]

i=read.csv("theaters.csv")$i %>% c(0,.) %>% max+1
while(i<=length(a)){
  b=paste0("https://www.fandango.com",a[i]) %>% read_html %>% html_nodes("#page > div > div.width-75.tablet-width-100 > section:nth-child(3) > div > ul > li > span.theaters-by-chain__theater-name.dark__link > a") %>% html_attr("href") %>% tryCatch(error=function(e) NULL); if(is.null(b)) a=a[-i]
  j=1 #j=j+1
  while(j<=length(b)){
    
    id=b[j] %>% strsplit("/|-") %>% unlist %>% rev %>% .[3] %>% toupper
    
    headers = c(referer = "https://www.fandango.com/amc-washington-square-12-aatij/theater-page" )
    params = list(startDate = date, partnerRestrictedTicketing = "")
    
    go=T; while(go) {res <- httr::GET(url = paste0("https://www.fandango.com/napi/theaterMovieShowtimes/",id), httr::add_headers(.headers=headers), query = params); if(status_code(res)==504) Sys.sleep(5) else go=F}
    
    find_name <- function(haystack, needle="showtimes") {if (hasName(haystack, needle)) {haystack[[needle]]} else if (is.list(haystack)) {for (obj in haystack) {ret <- Recall(obj, needle);  if (!is.null(ret)) return(ret)  } } else { NULL }}
    
    x=content(res)[[1]]$theater$detail[c("name","chainName","city","state","id")] %>% as.data.frame %>% cbind(content(res)[[1]]$theater$detail$geo %>% as.data.frame); s=data.frame(time=NA,showId=NA,movieId=NA,date,theaterId=NA); if(nrow(x)==0) x=data.frame(name=NA,chainName=a[i],city=NA,state=NA,id,latitude=NA,longitude=NA,i) else x$i=i
    m=content(res)[[1]]$movies %>% sapply("[",c("id","title","runtime","releaseDate","rating")); if(!is.null(dim(m))){m[m=="NULL"]=NA; m=m %>% t %>% as.data.frame %>% sapply(unlist); if(any(class(m)=="character")) m=m %>% t; m=m %>% as.data.frame; names(m)[1]="movieId"; m$theaterId=x$id} else m=data.frame(movieId=NA,title=NA,runtime=NA,releaseDate=NA,rating=NA,theaterId=x$id)
    v=content(res)[[1]]$movies %>% sapply("[[","variants")
    if(length(v)>0) {s=list(); for(u in 1:nrow(m)) {s[[u]]=v[[u]] %>% rrapply(classes="list",condition=function(x,.xname) .xname=="showtimes", how="flatten") %>% unlist(recursive=F) %>% sapply("[",c("date","id")) %>% t %>% as.data.frame %>% cbind(m$movieId[u]); names(s[[u]])=c("time","showId","movieId")}; s=do.call(rbind,s) %>% cbind(date,theaterId=x$id); row.names(s)=NULL; s=s  %>% sapply(unlist) %>% as.data.frame}
    
    
    print(paste0(i,"/",length(a)," ",j,"/",length(b)));flush.console()
    x %>% write.table("theaters.csv", sep=",", append=T, quote=T, col.names=F, row.names=F)
    m %>% write.table("movies.csv", sep=",", append=T, quote=T, col.names=F, row.names=F)
    s %>% write.table("showtimes.csv", sep=",", append=T, quote=T, col.names=F, row.names=F)
    j=j+1}
  i=i+1}

fi=list.files(dir,full=T); for(k in length(fi)) read.csv(fi[k],row.names=NULL) %>% .[!duplicated(.),] %>% write.csv(fi[k],row.names=F)
#####################################


gpush=function(d,path="newfile.csv",message=NA){
if(is.na(message)) message=paste(nrow(d),"rows")
nx=paste(names(d),collapse=",")
bx=apply(d, 1, paste, collapse=",")
dx=paste(c(nx,bx),collapse="\n")

tok="@CAHX-SuKmB2+3Hx7c" %>% paste0("!Z$zggs5JeI$8FS4si+rY4QEJ",.) %>% paste0("_",.) %>% paste0("p",.) %>% paste0("h",.) %>% paste0("g",.) %>% gsub("[^A-z0-9_]","",.)
gh=paste0("https://api.github.com/repos/charcoalcharts-open/movie-theaters/contents/",path)
sha=content(GET(gh))$sha
content=base64encode(dx)
body=paste0("{\"message\":\"",message,"\",\"content\":\"",content,"\",\"sha\":\"",sha,"\"}")
p=PUT(gh,body=body,add_headers(Authorization=paste("Bearer",tok)))
p$status_code}

tx="https://raw.githubusercontent.com/charcoalcharts-open/movie-theaters/refs/heads/main/theaters.csv" %>% read.csv %>% rbind("theaters.csv") %>% .[!duplicated(.$id),] %>% .[complete.cases(.),]
tx %>% gpush("theaters.csv")
mx=read.csv("movies.csv")
m2="https://raw.githubusercontent.com/charcoalcharts-open/movie-theaters/refs/heads/main/movies.csv" %>% read.csv %>% rbind(mx[,1:5]) %>% .[!duplicated(.$movieId),]
m2 %>% gpush("movies.csv")
read.csv("showtimes.csv") %>% .[complete.cases(.),] %>% gpush(paste0("workspace","/showtimes.csv"))



