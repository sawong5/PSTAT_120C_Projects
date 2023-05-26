#[PSTAT 120C] Data Analytic Report 1 
#Sam Wong
#Fall 2020

#load data
polls_data_2016=read.csv(paste0("C:/sers/aquas_jxptr00/OneDrive/Documents","/data/president_general_polls_sorted_end_date_2016.csv"))
polls_data_2020=read.csv(paste0("C:/sers/aquas_jxptr00/OneDrive/Documents","/data/president_polls_2020.csv"))

#1a)
#Minnesota
index_minn=which(president_general_polls_sorted_end_date_2016$state=="Minnesota")
n1=sum(president_general_polls_sorted_end_date_2016$total.clinton[index_minn])
n2=sum(president_general_polls_sorted_end_date_2016$total.trump[index_minn])
n1
n2
(n1-n2)/(n1+n2)

#Florida
index_flori=which(president_general_polls_sorted_end_date_2016$state=="Florida")
n3=sum(president_general_polls_sorted_end_date_2016$total.clinton[index_flori])
n4=sum(president_general_polls_sorted_end_date_2016$total.trump[index_flori])
n3
n4
(n3-n4)/(n3+n4)

#North Carolina
index_ncaro=which(president_general_polls_sorted_end_date_2016$state=="North Carolina")
n5=sum(president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro])
n6=sum(president_general_polls_sorted_end_date_2016$total.trump[index_ncaro])
n5
n6
(n5-n6)/(n5+n6)

#1b)
#Minnesota 
date_minn <- mdy(president_general_polls_sorted_end_date_2016$enddate[index_minn])

ylim_value=c(min(president_general_polls_sorted_end_date_2016$total.clinton[index_minn],president_general_polls_sorted_end_date_2016$total.trump[index_minn]),
             max(president_general_polls_sorted_end_date_2016$total.clinton[index_minn],president_general_polls_sorted_end_date_2016$total.trump[index_minn]))
plot(date_minn,president_general_polls_sorted_end_date_2016$total.clinton[index_minn],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Minnesota',ylim=ylim_value)
lines(date_minn,president_general_polls_sorted_end_date_2016$total.trump[index_minn],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#Florida
date_flori <- mdy(president_general_polls_sorted_end_date_2016$enddate[index_flori])

ylim_value=c(min(president_general_polls_sorted_end_date_2016$total.clinton[index_flori],president_general_polls_sorted_end_date_2016$total.trump[index_flori]),
             max(president_general_polls_sorted_end_date_2016$total.clinton[index_flori],president_general_polls_sorted_end_date_2016$total.trump[index_flori]))
plot(date_flori,president_general_polls_sorted_end_date_2016$total.clinton[index_flori],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Florida',ylim=ylim_value)
lines(date_flori,president_general_polls_sorted_end_date_2016$total.trump[index_flori],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#North Carolina
date_ncaro <- mdy(president_general_polls_sorted_end_date_2016$enddate[index_ncaro])

ylim_value=c(min(president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro],president_general_polls_sorted_end_date_2016$total.trump[index_ncaro]),
             max(president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro],president_general_polls_sorted_end_date_2016$total.trump[index_ncaro]))
plot(date_ncaro,president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro],
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Minnesota',ylim=ylim_value)
lines(date_ncaro,president_general_polls_sorted_end_date_2016$total.trump[index_ncaro],col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#1c)
#Minnesota 
counts_minn <- data.frame(
  data_date = c(date_minn,date_minn),
  counts=c(president_general_polls_sorted_end_date_2016$total.trump[index_minn],president_general_polls_sorted_end_date_2016$total.clinton[index_minn]),
  group=c(rep('Trump',length(date_minn)),rep('Clinton',length(date_minn)))
)
counts_minn_for_lm <- data.frame(
  data_date = date_minn,
  percentage_diff = (president_general_polls_sorted_end_date_2016$total.clinton[index_minn]-president_general_polls_sorted_end_date_2016$total.trump[index_minn])/(president_general_polls_sorted_end_date_2016$total.clinton[index_minn]+president_general_polls_sorted_end_date_2016$total.trump[index_minn])
)

as.numeric(date_minn)
lm_model_minn=lm(percentage_diff~(data_date),data=counts_minn_for_lm)
summary(lm_model_minn)

plot(counts_minn_for_lm$data_date,counts_minn_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
lines(counts_minn_for_lm$data_date,lm_model_minn$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Minnesota')

#Florida
counts_flori <- data.frame(
  data_date = c(date_flori,date_flori),
  counts=c(president_general_polls_sorted_end_date_2016$total.trump[index_flori],president_general_polls_sorted_end_date_2016$total.clinton[index_flori]),
  group=c(rep('Trump',length(date_flori)),rep('Clinton',length(date_flori)))
)
counts_flori_for_lm <- data.frame(
  data_date = date_flori,
  percentage_diff = (president_general_polls_sorted_end_date_2016$total.clinton[index_flori]-president_general_polls_sorted_end_date_2016$total.trump[index_flori])/(president_general_polls_sorted_end_date_2016$total.clinton[index_flori]+president_general_polls_sorted_end_date_2016$total.trump[index_flori])
)

as.numeric(date_flori)
lm_model_flori=lm(percentage_diff~(data_date),data=counts_flori_for_lm)
summary(lm_model_flori)

plot(counts_flori_for_lm$data_date,counts_flori_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
lines(counts_flori_for_lm$data_date,lm_model_flori$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Florida')

#North Carolina 
counts_ncaro <- data.frame(
  data_date = c(date_ncaro,date_ncaro),
  counts=c(president_general_polls_sorted_end_date_2016$total.trump[index_ncaro],president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro]),
  group=c(rep('Trump',length(date_ncaro)),rep('Clinton',length(date_ncaro)))
)
counts_ncaro_for_lm <- data.frame(
  data_date = date_ncaro,
  percentage_diff = (president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro]-president_general_polls_sorted_end_date_2016$total.trump[index_ncaro])/(president_general_polls_sorted_end_date_2016$total.clinton[index_ncaro]+president_general_polls_sorted_end_date_2016$total.trump[index_ncaro])
)

as.numeric(date_ncaro)
lm_model_ncaro=lm(percentage_diff~(data_date),data=counts_ncaro_for_lm)
summary(lm_model_ncaro)

plot(counts_ncaro_for_lm$data_date,counts_ncaro_for_lm$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_ncaro_for_lm$data_date,lm_model_ncaro$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')

#2a) 
#Minnesota
index_biden_minn_2020=which(president_polls_2020$answer=='Biden' & president_polls_2020$state=="Minnesota")
index_trump_minn_2020=which(president_polls_2020$answer=='Trump' & president_polls_2020$state=="Minnesota")
counts_biden_minn_2020=president_polls_2020$pct[index_biden_minn_2020]*president_polls_2020$sample_size[index_biden_minn_2020]
counts_trump_minn_2020=president_polls_2020$pct[index_trump_minn_2020]*president_polls_2020$sample_size[index_trump_minn_2020]
n1_2020_minn=sum(counts_biden_minn_2020)
n2_2020_minn=sum(counts_trump_minn_2020)
n1_2020_minn
n2_2020_minn
(n1_2020_minn-n2_2020_minn)/(n1_2020_minn+n2_2020_minn)

#Florida
index_biden_flori_2020=which(president_polls_2020$answer=='Biden' & president_polls_2020$state=="Florida")
index_trump_flori_2020=which(president_polls_2020$answer=='Trump' & president_polls_2020$state=="Florida")
counts_biden_flori_2020=president_polls_2020$pct[index_biden_flori_2020]*president_polls_2020$sample_size[index_biden_flori_2020]
counts_trump_flori_2020=president_polls_2020$pct[index_trump_flori_2020]*president_polls_2020$sample_size[index_trump_flori_2020]
n1_2020_flori=sum(counts_biden_flori_2020)
n2_2020_flori=sum(counts_trump_flori_2020)
n1_2020_flori
n2_2020_flori
(n1_2020_flori-n2_2020_flori)/(n1_2020_flori+n2_2020_flori)

#North Carolina
index_biden_ncaro_2020=which(president_polls_2020$answer=='Biden' & president_polls_2020$state=="North Carolina")
index_trump_ncaro_2020=which(president_polls_2020$answer=='Trump' & president_polls_2020$state=="North Carolina")
counts_biden_ncaro_2020=president_polls_2020$pct[index_biden_ncaro_2020]*president_polls_2020$sample_size[index_biden_ncaro_2020]
counts_trump_ncaro_2020=president_polls_2020$pct[index_trump_ncaro_2020]*president_polls_2020$sample_size[index_trump_ncaro_2020]
n1_2020_ncaro=sum(counts_biden_ncaro_2020)
n2_2020_ncaro=sum(counts_trump_ncaro_2020)
n1_2020_ncaro
n2_2020_ncaro
(n1_2020_ncaro-n2_2020_ncaro)/(n1_2020_ncaro+n2_2020_ncaro)

#2b)
#Minnesota
ylim_value=c(min(counts_biden_minn_2020,counts_trump_minn_2020),
             max(counts_biden_minn_2020,counts_trump_minn_2020))
plot(date_2020[index_biden_minn_2020],counts_biden_minn_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Minnesota',ylim=ylim_value)
lines(date_2020[index_trump_minn_2020],counts_trump_minn_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#Florida
ylim_value=c(min(counts_biden_flori_2020,counts_trump_flori_2020),
             max(counts_biden_flori_2020,counts_trump_flori_2020))
plot(date_2020[index_biden_flori_2020],counts_biden_flori_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='Florida',ylim=ylim_value)
lines(date_2020[index_trump_flori_2020],counts_trump_flori_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#North Carolina
ylim_value=c(min(counts_biden_ncaro_2020,counts_trump_ncaro_2020),
             max(counts_biden_ncaro_2020,counts_trump_ncaro_2020))
plot(date_2020[index_biden_ncaro_2020],counts_biden_ncaro_2020,
     col='blue',pch=18,cex=1,type='p',xlab='date',ylab='counts',main='North Carolina',ylim=ylim_value)
lines(date_2020[index_trump_ncaro_2020],counts_trump_ncaro_2020,col='red',pch=19,cex=.5,type='p')
legend("topleft",col=c('blue','red'),pch=c(18,19),legend=c('Clinton','Trump'))

#2c)
#Minnesota
counts_minn_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_minn_2020],
  percentage_diff = (counts_biden_minn_2020-counts_trump_minn_2020)/(counts_biden_minn_2020+counts_trump_minn_2020)
)

lm_model_minn_2020=lm(percentage_diff~(data_date),data=counts_minn_for_lm_2020)
summary(lm_model_minn_2020)

conf_interval_minn_fitted_2020= predict(lm_model_minn_2020, newdata=counts_minn_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_minn_for_lm_2020$data_date,counts_minn_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')
polygon(c(rev(counts_minn_for_lm_2020$data_date), counts_minn_for_lm_2020$data_date), 
        c(rev(conf_interval_minn_fitted_2020[,2]), conf_interval_minn_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_minn_for_lm_2020$data_date,lm_model_minn_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Minnesota')
lines(counts_minn_for_lm_2020$data_date,counts_minn_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Minnesota')

#Florida 
counts_flori_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_flori_2020],
  percentage_diff = (counts_biden_flori_2020-counts_trump_flori_2020)/(counts_biden_flori_2020+counts_trump_flori_2020)
)

lm_model_flori_2020=lm(percentage_diff~(data_date),data=counts_flori_for_lm_2020)
summary(lm_model_flori_2020)

conf_interval_flori_fitted_2020= predict(lm_model_flori_2020, newdata=counts_flori_for_lm_2020, interval="confidence",
                                        level = 0.95)

plot(counts_flori_for_lm_2020$data_date,counts_flori_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')
polygon(c(rev(counts_flori_for_lm_2020$data_date), counts_flori_for_lm_2020$data_date), 
        c(rev(conf_interval_flori_fitted_2020[,2]), conf_interval_flori_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_flori_for_lm_2020$data_date,lm_model_flori_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='Florida')
lines(counts_flori_for_lm_2020$data_date,counts_flori_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='Florida')

#North Carolina 
counts_ncaro_for_lm_2020 <- data.frame(
  data_date = date_2020[index_trump_ncaro_2020],
  percentage_diff = (counts_biden_flori_2020-counts_trump_ncaro_2020)/(counts_biden_ncaro_2020+counts_trump_ncaro_2020)
)

lm_model_ncaro_2020=lm(percentage_diff~(data_date),data=counts_ncaro_for_lm_2020)
summary(lm_model_ncaro_2020)

conf_interval_ncaro_fitted_2020= predict(lm_model_ncaro_2020, newdata=counts_ncaro_for_lm_2020, interval="confidence",
                                         level = 0.95)

plot(counts_ncaro_for_lm_2020$data_date,counts_ncaro_for_lm_2020$percentage_diff,
     col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')
polygon(c(rev(counts_ncaro_for_lm_2020$data_date), counts_ncaro_for_lm_2020$data_date), 
        c(rev(conf_interval_ncaro_fitted_2020[,2]), conf_interval_ncaro_fitted_2020[ ,3]), col = 'grey80', border = NA)
lines(counts_ncaro_for_lm_2020$data_date,lm_model_ncaro_2020$fitted.values,
      col='black',pch=20,type='l',xlab='date',ylab='difference in counts (%)',main='North Carolina')
lines(counts_ncaro_for_lm_2020$data_date,counts_ncaro_for_lm_2020$percentage_diff,
      col='black',pch=20,type='p',xlab='date',ylab='difference in counts (%)',main='North Carolina')

#3) #we want dates between Sep 26, 2016 and Oct 25, 2016
#Minnesota
index_selected_minn_2016=which(polls_data_minn_2016$enddate>='2016-09-26'& polls_data_penn_2016$enddate<="2016-10-26") 
polls_data_min_2016=polls_data_minn_2016[index_selected_minn_2016,]
(polls_data_2016$total.clinton[index_minn]-polls_data_2016$total.trump[index_minn])/(polls_data_2016$total.clinton[index_minn]+polls_data_2016$total.trump[index_minn])
percentage_diff_minn_2016=(polls_data_minn_2016$total.clinton-polls_data_minn_2016$total.trump)/(polls_data_minn_2016$total.clinton+polls_data_minn_2016$total.trump)
polls_data_minn_2016$enddate

year(polls_data_minnn_2016$enddate)=2020 

ylim_minn=c(min(percentage_diff_minn_2016,percentage_diff_minn_2020),max(percentage_diff_minn_2016,percentage_diff_minn_2020))
plot(polls_data_minn_2016$enddate,percentage_diff_minn_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Minnesota',ylim=ylim_minn)
lines(polls_date_minn_2020,percentage_diff_minn_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_minn_2016=lm(percentage_diff_minn_2016~(polls_data_minn_2016$enddate))
lm_model_minn_2020=lm(percentage_diff_minn_2020~(polls_date_minn_2020))

#Florida
index_selected_flori_2016=which(polls_data_flori_2016$enddate>='2016-09-26'& polls_data_flori_2016$enddate<="2016-10-26") 
polls_data_flori_2016=polls_data_flori_2016[index_selected_flori_2016,]
(polls_data_2016$total.clinton[index_flori]-polls_data_2016$total.trump[index_flori])/(polls_data_2016$total.clinton[index_flori]+polls_data_2016$total.trump[index_flori])
percentage_diff_flori_2016=(polls_data_flori_2016$total.clinton-polls_data_flori_2016$total.trump)/(polls_data_flori_2016$total.clinton+polls_data_flori_2016$total.trump)
polls_data_flori_2016$enddate

year(polls_data_flori_2016$enddate)=2020 

ylim_flori=c(min(percentage_diff_flori_2016,percentage_diff_flori_2020),max(percentage_diff_flori_2016,percentage_diff_flori_2020))
plot(polls_data_flori_2016$enddate,percentage_diff_flori_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='Florida',ylim=ylim_flori)
lines(polls_date_flori_2020,percentage_diff_flori_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_flori_2016=lm(percentage_diff_flori_2016~(polls_data_flori_2016$enddate))
lm_model_flori_2020=lm(percentage_diff_flori_2020~(polls_date_flori_2020))

#North Carolina
index_selected_ncaro_2016=which(polls_data_ncaro_2016$enddate>='2016-09-26'& polls_data_ncaro_2016$enddate<="2016-10-26") 
polls_data_ncaro_2016=polls_data_ncaro_2016[index_selected_ncaro_2016,]
(polls_data_2016$total.clinton[index_ncaro]-polls_data_2016$total.trump[index_ncaro])/(polls_data_2016$total.clinton[index_ncaro]+polls_data_2016$total.trump[index_ncaro])
percentage_diff_ncaro_2016=(polls_data_ncaro_2016$total.clinton-polls_data_ncaro_2016$total.trump)/(polls_data_ncaro_2016$total.clinton+polls_data_ncaro_2016$total.trump)
polls_data_ncaro_2016$enddate

year(polls_data_ncaro_2016$enddate)=2020 

ylim_ncaro=c(min(percentage_diff_ncaro_2016,percentage_diff_ncaro_2020),max(percentage_diff_ncaro_2016,percentage_diff_ncaro_2020))
plot(polls_data_ncaro_2016$enddate,percentage_diff_ncaro_2016,
     col='purple',pch=18,cex=1,type='p',xlab='date',
     ylab='percentage',main='North Carolina',ylim=ylim_ncaro)
lines(polls_date_ncaro_2020,percentage_diff_ncaro_2020,
      col='green',pch=20,cex=1,type='p')
legend("bottomleft",col=c('purple','green'),pch=c(18,20),legend=c('2016','2020'))

lm_model_ncaro_2016=lm(percentage_diff_ncaro_2016~(polls_data_ncaro_2016$enddate))
lm_model_ncaro_2020=lm(percentage_diff_ncaro_2020~(polls_date_ncaro_2020))

#4a)
#US Map
library(usmap)
library(ggplot2) 

president_general_polls_sorted_end_date_2016 <- data.frame(
  state =poll_state_diff_percentage[,1],
  diff_percentage=poll_state_diff_percentage[,2]
)

index_selected=which(date_2020>='2020-08-31') 
polls_data_2020_after_sep=president_polls_2020[index_selected,]  ###only work on the poll after Aug 31


polls_data_2020_after_sep=polls_data_2020_after_sep[which(president_polls_2020$answer=='Biden'|president_polls_2020$answer=='Trump'),]

index_biden_2020=which(polls_data_2020_after_sep$answer=='Biden')
index_trump_2020=which(polls_data_2020_after_sep$answer=='Trump' )

counts_biden_2020=president_polls_2020$pct[index_biden_2020]*president_polls_2020$sample_size[index_biden_2020]
counts_trump_2020=president_polls_2020$pct[index_trump_2020]*president_polls_2020$sample_size[index_trump_2020]

polls_data_2020$total.biden=rep(0,dim(president_polls_2020)[1])
polls_data_2020$total.trump=rep(0,dim(president_polls_2020)[1])

polls_data_2020$total.biden[index_biden_2020]=counts_biden_2020
polls_data_2020$total.trump[index_trump_2020]=counts_trump_2020

poll_state_sum_biden_2020=aggregate(president_polls_2020$total.biden, by=list(State=polls_data_2020$state),FUN=sum)
poll_state_sum_trump_2020=aggregate(president_polls_2020$total.trump, by=list(State=polls_data_2020$state),FUN=sum)

poll_state_sum_biden_2020=poll_state_sum_biden_2020[-1,]
poll_state_sum_trump_2020=poll_state_sum_trump_2020[-1,]

state_poll_2020 <- data.frame(
  state =poll_state_sum_biden_2020[,1],
  diff_percentage=(poll_state_sum_biden_2020[,2]-poll_state_sum_trump_2020[,2])/(poll_state_sum_biden_2020[,2]+poll_state_sum_trump_2020[,2])
)

limit_val=c(min(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage),
            max(state_poll_2016$diff_percentage,state_poll_2020$diff_percentage))
##2016
plot_usmap(data = state_poll_2016, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2016") 
##2020
plot_usmap(data = state_poll_2020, values = "diff_percentage", color = "black") +
  scale_fill_gradient2(name = "difference (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0,limits=limit_val)+
  theme(legend.position = "right")+
  ggtitle("2020") 

##difference between 2020 and 2016
##delete nebrask CD-1 and CD-3, as 2020 does not have it 
state_poll_2016$state
state_poll_2020$state
state_poll_2016=state_poll_2016[-c(31,33),]


state_poll_2020_2016_diff <- data.frame(
  state =state_poll_2020$state,
  diff=state_poll_2020$diff_percentage-state_poll_2016$diff_percentage
)

plot_usmap(data = state_poll_2020_2016_diff, values = "diff", color = "black") +
  scale_fill_gradient2(name = "difference between 2016 and 2020 (%)",   low= "red",
                       mid = "white",
                       high = "blue",
                       midpoint = 0)+
  theme(legend.position = "right")+
  ggtitle("difference between 2020 and 2016")