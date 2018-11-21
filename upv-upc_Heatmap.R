library("data.table")
library(REmap)
logfile <- "game_log.tsv"
pcfile <- "portals_captured.tsv"
pvfile <- "portals_visited.tsv"

log <- fread(logfile,header = T,stringsAsFactors = F,fill = T,sep='\t')[,1:4]
log <- subset(log,`Event Lat`!='None')
data_pre <- function(pcfile){
  pc <- fread(pcfile,header = T,stringsAsFactors = F,fill = T,sep='\t')
  pc$Time <- substr(pc$Time,1,19)
  log$`Event Time` <- paste0(substr(log$`Event Time`,1,4),'-',substr(log$`Event Time`,6,19))
  pc <- cbind(pc,log[match(pc$Time,log$`Event Time`),2:3])
  pc[is.na(pc$`Event Lat`),3:4] <- log[match(paste(as.IDate(pc$Time[is.na(pc$`Event Lat`)]),as.ITime(pc$Time[is.na(pc$`Event Lat`)])-1),log$`Event Time`),2:3]
  pc[,3:4]<-subset(pc,`Event Lat` != 'NA')[match(pc$`Unique Portal ID`,subset(pc,`Event Lat` != 'NA')$`Unique Portal ID`),3:4]
  pc_mapdata <- pc[!is.na(pc$`Event Lat`),c(4,3,2)]
}
pc_mapdata <- data_pre(pcfile)
upc_mapdata <- unique(pc_mapdata)
pv_mapdata <- data_pre(pvfile)
upv_mapdata <- unique(pv_mapdata)

Heatmap<-function(mapdata,region,title){
  p <-remapH(mapdata,
             maptype = region,
             theme =get_theme(theme = "none",backgroundColor = "#698B69"),
             blurSize = 10,
             color = c('skyblue'),
             #minAlpha = 0.05,
             opacity = 0.9,
             title=title
  )
  p
}

# 括号中是数据参数，第一个是变量，后面两个是字符串。
# 第一个是存放作图数据的变量，可选pc_mapdata / upc_mapdata / pv_mapdata / upv_mapdata 变量不加引号！
# 第二个是作图区域,可选'world'/'china'/某个省名（貌似只能用汉字，后面不要加省或自治区之类的字）
# 第三个是标题，随便写啥都行，注意字符串要加引号，单双引号都行
# 下面是示例，不需要的前面加#注释掉就行，有几行就会出几个图

# Heatmap(pc_mapdata,'china','Heatmap of agent LickingDog portals captured')
 Heatmap(upc_mapdata,'china','Heatmap of agent LickingDog unique portals captured')
# Heatmap(pv_mapdata,'china','Heatmap of agent LickingDog portals visited')
 Heatmap(upv_mapdata,'china','Heatmap of agent LickingDog unique portals cvisited')

# Heatmap(pc_mapdata,'world','Heatmap of agent LickingDog portals captured')
# Heatmap(upc_mapdata,"四川",'Heatmap of agent LickingDog unique portals captured')
# Heatmap(pv_mapdata,'china','Heatmap of agent LickingDog portals visited')
# Heatmap(upv_mapdata,'广西','Heatmap of agent LickingDog unique portals visited')

Sys.sleep(60)
