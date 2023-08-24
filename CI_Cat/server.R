library(shiny)
# library(dplyr)
library(ggplot2)

source("functions.R",local = TRUE)

shinyServer(function(input, output) {
    
    # Plan:
    # reactive value for sample_num, based on get1 and get50
    # reactive value for index_list, based on sample size
    # reactive value for pop_lab, based on p
    # reactive value for samples_details, based on sample size, p, ME
    
    
    #input variables:  sample_size, p_val, reset, get_samp, get_50
    
    #output variables: success_rate, sampling, conf_int, p_hat
    
    sample_num<-reactiveValues(data=0)
    index_list<-reactiveValues(data = start_index_list)
    pop_lab<-reactiveValues(data = start_pop_l)
    sample_details<-reactiveValues(data = start_sample_details)
    sample_plot<-reactiveValues(data = start_sample_plot)
    conf_int_plot<-reactiveValues(data = get_ci_start(.3))
    
    output$sampling<-renderPlot({
        sample_plot$data
    })
    
    output$conf_int<-renderPlot({
        conf_int_plot$data
    })
    
    output$p_hat<-renderText(
        paste("Sample Percentage Green = ",
              round(sample_details$data[sample_num$data,
                                        'sample_props'],3))
    )
    
    output$success_rate<-renderText({
        sr<-100*(sum(sample_details$data[1:sample_num$data,'flag']=='blue')/sample_num$data)
        paste("Success Rate: ",sr,"%")
    })
    
    observeEvent(input$get_samp,{
        sample_num$data<-sample_num$data+1
        sample_plot$data<-get_sample_plot(
            pop_lab$data, index_list$data,sample_num$data
        )
        conf_int_plot$data<-get_conf_int(sample_details$data,
                                         sample_num$data,
                                         input$p_val,
                                         input$show_interval)
    })
    
    observeEvent(input$get_50,{
        sample_num$data<-sample_num$data + 50
        sample_plot$data<-get_sample_plot(
            pop_lab$data, index_list$data,sample_num$data
        )
        conf_int_plot$data<-get_conf_int(sample_details$data,
                                         sample_num$data,
                                         input$p_val,input$show_interval)
    })
    
    # get_sample_plot(start_pop_l, start_index_list[[5]])
    
    observeEvent(c(input$reset,input$sample_size, input$p_val), {
        sample_num$data<-0
        conf_int_plot$data<-get_ci_start(input$p_val)
        sample_plot$data<-get_sample_plot(pop_lab$data,
                                          index_list$data,
                                          sample_num$data)
        conf_int_plot$data<-get_conf_int(sample_details$data,
                                         sample_num$data,
                                         input$p_val,
                                         input$show_interval)
    })
    
    observeEvent(input$show_interval, {
        conf_int_plot$data<-get_conf_int(sample_details$data,
                                         sample_num$data,
                                         input$p_val,
                                         input$show_interval)
    })
    
    observeEvent(input$ME,{
        sample_details$data<-get_sample_details(pop_lab$data,
                                                index_list$data,
                                                input$p_val,
                                                input$ME)
        conf_int_plot$data<-get_conf_int(sample_details$data,
                                         sample_num$data,
                                         input$p_val,
                                         input$show_interval)
    })
    
    observeEvent(input$sample_size, {
        index_list$data<-get_index_list(input$sample_size)
        sample_details$data<-get_sample_details(pop_lab$data,
                                                index_list$data,
                                                input$p_val,
                                                input$ME)
    })
    
    observeEvent(input$p_val, {
        pop_lab$data<-update_population(input$p_val)
        sample_details$data<-get_sample_details(pop_lab$data,
                                                index_list$data,
                                                input$p_val,
                                                input$ME)
    })
    
    
}
)