## Postprocessing of simulations with data generated under SLFV and processed using PIV models

library("tools");
library("ggplot2");
library("RColorBrewer")
library("stringr");
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
           return(all.col[3]),return(all.col[4]))));
}

b.prop = 0.1;
chain.len = 1000;

ibm.col = rgb(220/255,13/255,10/255,alpha=0.6);
iwn.col = rgb(177/255,198/255,209/255,alpha=0.6);
iou.col = rgb(250/255,210/255,1/255,alpha=0.6);
rrw.col = rgb(10/255,168/255,109/255,alpha=0.6);

model.names = c("ibm","rrw","iou");
## model.names = c("ibm","rrw","iou","iwn");
## model.names = c("rrw","ibm");
n.models = length(model.names);

# all.col = 1:n.models;
# for(i in 1:n.models)
# {
#     all.col[i] = as.vector(sapply(as.vector(sapply(model.names,qp,".col")),get))[i]
# }
all.col = c(ibm.col,iwn.col,iou.col,rrw.col);

data.dir = ".";

all.df = list();


                                        # Populate list all.df, each element of it corresponding to a model. Each of these
                                        # elements is itself a list of dataframes (as many dataframes as data
                                        # sets analysed)
for(i in 1:n.models)
{
    fnames = list.files(path=data.dir,pattern=qp("*_phyrex_stats_",model.names[i]),full.names=T);
    ## all.df[[i]] = lapply(fnames,read.table,header=T,check.names=F);
    all.df[[i]] = list();
    for(j in 1:length(fnames))
    {
        cat("Reading file",fnames[j],"\n");
        all.df[[i]][[j]] = read.table(fnames[j],header=T,check.names=F);
    }
}

cat("Completed reading data...\n");


                                        # number of datasets analysed under each model. Check that they are the same for all models
n.datasets = sapply(all.df,length);
for(i in 1:length(n.datasets)) stopifnot(n.datasets[[i]] == n.datasets[[1]]); 
n.datasets = as.numeric(n.datasets[[1]]);

cat("Number of datasets to process: ",n.datasets,"\n");


                                        # keep only datasets which anlysis has reached completion
completed = 1:n.datasets;
for(i in 1:n.datasets)
{
    completed[i] = 1;
    complete = 1;
    for(j in 1:n.models)
    {        
        if(dim(all.df[[j]][[i]])[1] < chain.len)
        {
            cat("!!!! model: ",model.names[j]," data set:",i,"len: ",dim(all.df[[j]][[i]])[1],"\n")
            complete = 0
            break;
        }
    }
    if(complete == 0) { completed[i] = 0; }
}

completed = which(completed > 0);
n.completed = length(completed);
cat("Complete runs: ",completed,"\n");

stopifnot(n.completed != 0);
                                        # remove burnin
for(i in 1:n.models)
{
    for(j in completed)
        {
            all.df[[i]][[j]] = all.df[[i]][[j]][(b.prop*chain.len):chain.len,];
        }
}

cat("Removed burnin...\n");

fnames = list.files(path=data.dir,pattern="*_sim_slfv_params",full.names=T);
truth = lapply(fnames,scan,sep="\n",what=character());
truth = lapply(truth,"[",4);
truth = lapply(truth,strsplit,":");
truth = lapply(truth,"[[",1);
truth = lapply(truth,"[[",2);
truth = as.numeric(truth);


mse.veloc = rep(completed,length(model.names));
est.speed = rep(completed,length(model.names));

for(k in 1:n.models)
{    
    for(i in 1:n.completed)
    {
        mse.veloc[(k-1)*length(completed)+i] = 0.0;
        d = all.df[[k]];
        
        if(str_detect(model.names[k],"rrw") == TRUE)
        {
            mse.veloc[(k-1)*length(completed)+i] =
                (median(d[[completed[i]]][,c(grep("displacement",colnames(d[[completed[i]]]),fixed=T))]) -
                truth[completed[i]])^2;
            
            est.speed[(k-1)*length(completed)+i] = median(d[[completed[i]]][,c(grep("displacement",colnames(d[[completed[i]]]),fixed=T))]);

        }
        else
        {
            mse.veloc[(k-1)*length(completed)+i] =
                (median(d[[completed[i]]][,c(grep("speedfromveloc",colnames(d[[completed[i]]]),fixed=T))]) -
                truth[completed[i]])^2;            

            est.speed[(k-1)*length(completed)+i] = median(d[[completed[i]]][,c(grep("speedfromveloc",colnames(d[[completed[i]]]),fixed=T))]);
        }        
    }    
}

cat("Finished MSE calculations...\n");


plot.df = data.frame(speed=rep(truth[completed],n.models), mse = mse.veloc, est.speed = est.speed, model = rep(model.names,each=n.completed));

p = ggplot(plot.df %>% arrange(desc(model)),aes(x=speed,y=mse,color=model)) +
    geom_point(size=4) +
    scale_colour_manual(values = sapply(levels(as.factor(plot.df$model)),get.model.col,all.col))+
    theme_classic();


q = ggplot(plot.df %>% arrange(desc(model)),aes(x=speed,y=est.speed,color=model)) +
    geom_point(size=4) +
    geom_abline(intercept=0,slope=1)+
    scale_colour_manual(values = sapply(levels(as.factor(plot.df$model)),get.model.col,all.col))+
    theme_classic();


