library('httr')
library('jsonlite')
library('XML')
library('ggplot2')

url1 <- "http://rt.molit.go.kr/rtApt.do?cmd=srhLocalView"
url2 <- "http://rt.molit.go.kr/rtApt.do?cmd=searchGugun&aptGubun=1"
url3 <- "http://rt.molit.go.kr/rtApt.do?cmd=searchDong&gubunCode=1&houseType=Apt&aptGubun=1&gubunRadio2=1"
url4 <- "http://rt.molit.go.kr/rtApt.do?cmd=searchAptDanji"
url5 <- "http://rt.molit.go.kr/rtApt.do?cmd=getTradeAptLocal"

getDong <- function() {
  findArea <- function(url, code, post=FALSE, body=NULL) {
    if(post) html <- content(POST(url, body=body, encoding="EUC-KR"), as="text")
    else html <- content(GET(url, encoding="EUC-KR"), as="text")
    doc <- htmlParse(html, asText=T, encoding="UTF-8")
    query <- paste("//select[@name='", code, "']/option[@value!='ALL']", sep="")
    parse<-xpathSApply(doc, query, xmlValue)
    nums<-xpathSApply(doc, query, function(x) xmlAttrs(x)[['value']])
    for(i in 1:length(parse)) {
      cat(paste(i, parse[i], sep=".\t"))
      cat('\n')
    }
    i <- scan(what=integer(),nmax=1,quiet=T)
    cat(parse[i])
    cat('\n')
    nums[i]
  }
  sidoCode <- findArea(url1, "sidoCode")
  gugunCode <- findArea(url2, "gugunCode",post=TRUE, body=list(sidoCode=sidoCode, gugunCode="ALL", danjiCode="ALL", srhPeriod=1))
  findArea(url3,"dongCode",post=TRUE, body=list(sidoCode=sidoCode, gugunCode=gugunCode, danjiCode="ALL", srhPeriod=1))
}
dongCode <- getDong()
body <- list(dongCode=dongCode,gubunCode=1,srhYear=2015,srhPeriod=1)
response<-content(POST(url4, body=body, encode="form"))
l <- fromJSON(response)
apts <- l$moctJsonList$DANJI_NAME
for(i in 1:length(apts)) {
  cat(paste(i, apts[i], sep=".\t"))
  cat('\n')
}
i <- scan(what=integer(),nmax=1,quiet=T)
cat(apts[i])
cat('\n')
danjiCode <- l$moctJsonList$DANJI_CODE[i]

ua <- "Mozilla/5.0 (MSIE 9.0; Windows NT 6.1; Trident/5.0)"
data <- list()
for(year in 2006:2015) {
  cat(paste(year, "년 데이터 받아오는 중..", sep=" "))
  for(period in 1:4) {
    body <- list(dongCode=dongCode,danjiCode=danjiCode,srhYear=year,srhPeriod=period,gubunRadio2=1)
    response <- content(POST(url5, body=body, encode = "form", user_agent(ua)))
    l <- fromJSON(response)
    if(length(l$danjiList) > 0) {
      name <- l$danjiList$APT_NAME
      ls <- l$detailList
      ls$YEAR <- year
      data <- rbind.data.frame(data, ls)
    }
  }
}
data$SUM_AMT <- as.numeric(gsub(",","", data$SUM_AMT))
data$MONTH <- as.numeric(data$MONTH)
data$FLOOR <- as.numeric(data$FLOOR)
data$AREA <- as.numeric(data$AREA)
data$TIME <- as.Date(paste(data$YEAR, data$MONTH, "01"), "%Y %m %d")
data <- data[order(data$TIME),]
write.csv(data, file = paste(name, "csv", sep="."))
plot.new()

colours <- c("red","yellow","green","lightblue","darkblue")

g <- ggplot(data, aes(TIME, SUM_AMT, color=FLOOR))
g <- g + geom_point() 
g <- g + xlab("거래날짜") 
g <- g + ylab("거래가(만원)") 
g <- g + ggtitle(paste(name,"거래가")) 
g <- g + geom_smooth(method = "loess", size = 1) 
g <- g + theme_bw()
g <- g + scale_colour_gradientn(colours = colours)
print(g)

cat("[Enter] to see next plot")
line <- readline()
data <- data[order(data$FLOOR),]

g <- ggplot(data, aes(FLOOR, SUM_AMT, color=AREA))
g <- g + geom_point() 
g <- g + xlab("층수") 
g <- g + ylab("거래가(만원)") 
g <- g + ggtitle(paste(name,"층별 거래가")) 
g <- g + geom_smooth(method = "loess", size = 1) 
g <- g + theme_bw()
g <- g + scale_colour_gradientn(colours = colours)
print(g)

cat("[Enter] to see next plot")
line <- readline()
data <- data[order(data$AREA),]

g <- ggplot(data, aes(factor(AREA), SUM_AMT))
g <- g + geom_boxplot()
g <- g + geom_point() 
g <- g + xlab("제곱미터") 
g <- g + ylab("거래가(만원)") 
g <- g + ggtitle(paste(name,"평수별 거래가")) 
g <- g + theme_bw()
print(g)
