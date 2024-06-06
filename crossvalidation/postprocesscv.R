## Postprocessing of simulations with data generated under SLFV and processed using PIV models

library("tools");
library("ggplot2");
library("RColorBrewer")
library("stringr");
library("forcats");
library("dplyr");


qp <- function(a,b)
{
    return(paste(a,b,sep=""));
}

get.model.col <- function(model.id,all.col)
{
    ifelse(str_detect(model.id,"ibm") == TRUE,
           return(all.col[1]),
    ifelse(str_detect(model.id,"iwn") == TRUE,
           return(all.col[2]),
    ifelse(str_detect(model.id,"iou") == TRUE,
           return(all.col[3]),
    ifelse(str_detect(model.id,"rrw") == TRUE,
           return(all.col[4]),-1))));
           ## return(all.col[3]),return(NA))));
}

b.prop = 0.1;
chain.len = 1000;

ibm.col = rgb(194/255,113/255,105/255,alpha=0.8);
iwn.col = rgb(177/255,198/255,209/255,alpha=0.8);
iou.col = rgb(127/255,120/255,209/255,alpha=0.8);
rrw.col = rgb(100/255,100/255,100/255,alpha=0.8);

model.names = c("ibm","iou","rrw");
n.models = length(model.names);

all.col = 1:n.models;
all.col=c(ibm.col,iwn.col,iou.col,rrw.col);

data.dir = "./";



## Populate list all.df, each element of it corresponding to a model. Each of these
## elements is itself a list of dataframes (as many dataframes as data
## sets analysed)

df.ibm = read.table(paste(data.dir,"ibm.bkp",sep="/"),header=F);
df.iou = read.table(paste(data.dir,"iou.bkp",sep="/"),header=F);
df.rrw = read.table(paste(data.dir,"rrw.bkp",sep="/"),header=F);

names(df.ibm)=c("Dum","Name","MSE","CvLon","CvLat","TrueLon","TrueLat","LK");
names(df.iou)=c("Dum","Name","MSE","CvLon","CvLat","TrueLon","TrueLat","LK");
names(df.rrw)=c("Dum","Name","MSE","CvLon","CvLat","TrueLon","TrueLat","LK");

cat("Completed reading data...\n");


df.ibm$Model = rep("ibm",length(df.ibm$Name));
df.iou$Model = rep("iou",length(df.iou$Name));
df.rrw$Model = rep("rrw",length(df.rrw$Name));

print(c(mean(df.ibm$MSE),quantile(df.ibm$MSE,p=seq(0,1,by=0.1))));
print(c(mean(df.iou$MSE),quantile(df.iou$MSE,p=seq(0,1,by=0.1))));
print(c(mean(df.rrw$MSE),quantile(df.rrw$MSE,p=seq(0,1,by=0.1))));


plot.df = rbind(df.ibm,df.rrw,df.iou);


p = plot.df %>%
    mutate(Model = fct_relevel(Model,"rrw","iou","ibm")) %>%
    ggplot(aes(x=MSE)) +
    geom_density(aes(fill=factor(Model))) +
    scale_fill_manual(values = sapply(levels(as.factor(plot.df$Model)),get.model.col,all.col)) +
    xlim(c(0,4000));

ggsave("/tmp/cv.pdf",plot=p);
