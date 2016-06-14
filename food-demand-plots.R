## Food demand plots
library(ggplot2)
library(reshape2)

make.demand.plot <- function(alldata,xdata,xlabel,max.yval)
{
  ## alldata: results of the food demand calculation
  ## xcolumn: name of the column for the X plot
  totaldmnd <- alldata$Qs + alldata$Qn
  ytop <- max(c(max.yval,totaldmnd))
  workset <- data.frame(staples=alldata$Qs, nonstaples=alldata$Qn, total=totaldmnd,
                        xval=xdata)
  ws.m <- melt(workset, id='xval')
  qplot(data=ws.m, x=xval, y=value, colour=variable) + xlab(xlabel) + ylab('Q') +
    geom_line() + geom_point() + ylim(0,ytop)
}
