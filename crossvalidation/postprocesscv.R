## Postprocessing of simulations with data generated under SLFV and processed using PIV models

library("tools");
library("ggplot2");
library("RColorBrewer")
library("stringr");
library("forcats");
library("dplyr");
library("spData");
library("sp")

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
           return(all.col[4]),all.col[2]))));           
           ## return(all.col[3]),return(NA))));
}

b.prop = 0.1;
chain.len = 1000;


ibm.col = rgb(220/255,13/255,10/255,alpha=0.6);
iwn.col = rgb(177/255,198/255,209/255,alpha=0.6);
iou.col = rgb(250/255,210/255,1/255,alpha=0.6);
rrw.col = rgb(10/255,168/255,109/255,alpha=0.6);

model.names = c("ibm","iou","rrw");
n.models = length(model.names);

all.col = 1:n.models;
all.col=c(ibm.col,iwn.col,iou.col,rrw.col);

data.dir = "/Users/guindon/latex/ibm/results_rev/crossvalidation";


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

# plot.df = rbind(df.ibm[df.ibm$MSE<3000,],df.rrw[df.rrw$MSE<3000,],df.iou[df.iou$MSE<3000,]);
plot.df = rbind(df.ibm,df.rrw,df.iou);

n.points = 100;
n.dists = dim(df.ibm)[1];
rpoints <- st_sample(us_states, size = n.points, type = "random", crs = crs_lower48);
all.coord=st_coordinates(rpoints);
samp.coord.idx.a = sample(1:n.points,size=n.dists,replace=TRUE)
samp.coord.idx.b = sample(1:n.points,size=n.dists,replace=TRUE)
dist = geosphere::distHaversine(as.matrix(cbind(all.coord[samp.coord.idx.a,1],all.coord[samp.coord.idx.a,2]),ncol=2),as.matrix(cbind(all.coord[samp.coord.idx.b,1],all.coord[samp.coord.idx.b,2]),ncol=2),r=6371);
df.dum = matrix(nrow=n.dists,ncol=dim(df.ibm)[2]);
df.dum = data.frame(df.dum);
names(df.dum)=c("Dum","Name","MSE","CvLon","CvLat","TrueLon","TrueLat","LK","Model");
df.dum$MSE = dist;
df.dum$Model = rep("rand",length(df.dum$Name));
plot.df = rbind(plot.df,df.dum);


plot.df = cbind(plot.df,as.vector(sapply(plot.df$Model,get.model.col,all.col)));
colnames(plot.df)[10]<-"Col";

p = plot.df %>%
    mutate(Model = fct_relevel(Model,"rrw","iou","ibm","rand")) %>%
    ggplot(aes(x=Model,y=log(MSE),fill=Model)) +
    scale_fill_manual(values=c(get.model.col("rrw",all.col),get.model.col("iou",all.col),get.model.col("ibm",all.col),get.model.col("rand",all.col)))+
    geom_violin(trim=TRUE)+
    geom_boxplot(width=0.1, fill="grey")+
    theme_classic();

ggsave("/tmp/cv.pdf",plot=p);
