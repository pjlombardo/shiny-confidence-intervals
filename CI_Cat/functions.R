population<-data.frame(x=runif(1000,0,100),
                       y=runif(1000,0,100))
# population %>% dim()

get_labels<-function(p){
    nb<-round(p*1000,0)
    nr<-round((1-p)*1000,0)
    temp<-c(rep("blue",nb), rep("red",nr))
    # print(length(temp))
    temp<-sample(temp,1000,replace=F)
    temp
}


update_population<-function(p){
    temp_labels<-get_labels(p)
    # print(length(temp_labels))
    df<-data.frame(x=population$x,
                   y=population$y,
                   flag=temp_labels)
    return(df)
}

start_pop_l<-update_population(.3)
##### Generate a bunch of indices
get_index_list<-function(n){
    index_list<-list()
    for (j in 1:2000){
        index_list[[j]]<-sample(1:1000,n)
        
    }
    index_list
}

get_sample_details<-function(labeled_pop,index_list,p_val,ME){
    sample_props<-numeric(2000)
    ymins<-numeric(2000)
    ymaxes<-numeric(2000)
    flags<-character(2000)
    n<-length(index_list[[1]])
    for (j in 1:2000){
        p<-sum(labeled_pop[index_list[[j]],'flag']=="blue")/n
        sample_props[j]<-p
        ymins[j]<-max(0,p-ME)
        ymaxes[j]<-min(p+ME,1)
        # print( ((ymins[j]>p_val) | (ymaxes[j]<p_val)) )
        if (( ymins[j]>p_val) | (ymaxes[j]<p_val)) {
            flags[j]<-'red'
        } else {
            flags[j]<-'blue'
        }
    }
    
    data.frame(sample_props=sample_props,
               ymins = ymins,
               ymaxes = ymaxes,
               flag =flags)
}

start_index_list<-get_index_list(5)


start_sample_details<-get_sample_details(start_pop_l, start_index_list,.3,.1)
truth<-ggplot() + geom_point(aes(x=start_pop_l$x,
                                 y=start_pop_l$y,
                                 col=start_pop_l$flag),
                             alpha=.6)+
    scale_color_manual('',labels=c('Green','Other'),
                       values=c('green3','plum4'))


get_sample_plot<-function(data,index_list,sample_num){
    if (sample_num==0){
        ggplot() + geom_point(aes(x=data$x,
                                  y=data$y,
                                  col=data$flag),
                              alpha=.6,size=2)+
            scale_color_manual('',labels=c('Green','Other'),
                               values=c('green3','plum4'))+
            labs(x="",y="")+
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank())
    } else {
        N<-dim(data)[1]
        df_sample<- data[index_list[[sample_num]],]
        
        ggplot()+geom_point(aes(x=data$x,
                                y=data$y,
                                color=data$flag),
                            alpha=.5,size=2)+
            scale_color_manual('',labels=c('Green','Other'),
                               values=c('green3','plum4'))+
            geom_point(aes(x=df_sample$x,
                           y=df_sample$y),
                       shape=1,size=2,stroke=1.75)+
            labs(x='',y='')+
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank())
    }
}

start_sample_plot<-get_sample_plot(start_pop_l, start_index_list,0)

start_sample_plot

get_ci_start<-function(p){
    ggplot()+
        geom_hline(yintercept=p,
                   col='green4',
                   linewidth=1.25,
                   alpha=.75)+
        coord_cartesian(xlim=c(0,10),ylim=c(0,1))+
        geom_hline(yintercept=0)+geom_vline(xintercept=0)+
        labs(x="Sample ID",y="Sample Percentages")
}

get_conf_int<-function(pop_details, sample_num, p_val,showCI){
    if (sample_num==0){
        get_ci_start(p_val)
    } else {
        if (showCI==T){
            ggplot()+geom_hline(yintercept=p_val,col='green4',linewidth=1.25,
                                alpha=.75)+
                geom_hline(yintercept=0)+geom_vline(xintercept=0)+
                geom_errorbar(data=pop_details[1:sample_num,],
                              aes(x =1:sample_num, 
                                  ymin = ymins,
                                  ymax = ymaxes,
                                  col=flag),
                              linewidth=1,
                              width=.1+.2*(sample_num/(sample_num+10)))+
                geom_point(data=pop_details[1:sample_num,],
                           aes(x = 1:sample_num,
                               y = sample_props,
                               col=flag))+
                scale_color_manual("",values=c("blue","red"),guide="none")+
                coord_cartesian(xlim=c(0,max(10,sample_num+3)),
                                ylim=c(0,1))+
                labs(x="Sample ID",y="Sample Percentages")
        } else {
            ggplot()+geom_hline(yintercept=p_val,col='green4',linewidth=1.25,
                                alpha=.75)+
                geom_hline(yintercept=0)+geom_vline(xintercept=0)+
                geom_point(data=pop_details[1:sample_num,],
                           aes(x = 1:sample_num,
                               y = sample_props,
                               col=flag))+
                scale_color_manual("",values=c("blue","red"),guide="none")+
                coord_cartesian(xlim=c(0,max(10,sample_num+3)),
                                ylim=c(0,1))+
                labs(x="Sample ID",y="Sample Percentages") 
        }
    }
}
