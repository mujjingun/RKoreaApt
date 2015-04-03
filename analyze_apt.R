library('httr')
library('jsonlite')
library('XML')

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
areacode = read.csv("areacode.csv")
data <- list()
for(year in 2006:2015) {
  print(year)
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
write.csv(data, file = paste(name, "csv", sep="."))
plot.new()
x <- 1:length(data$YEAR)
y <- data$SUM_AMT
colors <- colorRampPalette(c("red", "orange", "yellow", "green", "blue"))(6)
col <- colors[findInterval(data$FLOOR, c(3, 5, 10, 15, 20))+1]
plot(x, y, xaxt = "n", main=paste(name,"거래가"), xlab="날짜", ylab="거래가(만원)", pch=20, col = col)
axis(1, at=x, labels=paste(data$YEAR, data$MONTH, sep='-'))
lo<-loess(y~x)
lines(predict(lo), col='blue', lwd=2)
