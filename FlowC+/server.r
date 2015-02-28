library(gridExtra)
library(ggplot2)
library(flowCore)

flowplot_single <- function (file, x_channel, y_channel, xgate, ygate) {
        # file<- list.files()[1]
        # xgate <- 6
        # ygate <- 6
        # x_channel <- 3
        # y_channel <- 4
        
        ##lines <- stat_density2d(data = log10(flow.file[REF]), aes_string(x= names[x_channel], y= names[x_channel] ), colour = "purple")  #  SET ref CONTROL
        
        colPalette <- colorRampPalette(c("blue", "turquoise", "green", "yellow", "orange", "red"))
        
        gate1 <- annotate("segment", x=0, xend=8, y=ygate, yend=ygate, colour="gray", size=0.5)
        gate2 <- annotate("segment", x=xgate, xend=xgate, y=0, yend=8, colour="gray", size=0.5)
        
        flow.file <-as.data.frame(as(lapply(file,  read.FCS), "flowSet") [[1]]@ exprs)
        colnames(flow.file)  <- names <- c("FSC_A", "SSC_A", "FL1_A", "FL2_A", "FL3_A", "FL4_A", "FSC_H", "SSC_H", "FL1_H", "FL2_H", "FL3_H", "FL4_H", "Width", "Time")
        #         
        
        
        label1<-  annotate("text", label = paste (nrow(flow.file[log10(flow.file[,x_channel]) < xgate & log10(flow.file[,y_channel]) > ygate,])/ nrow(flow.file) *100, "%"), x = .5, y = 7.8,colour="black") 
        label2<-  annotate("text", label = paste (nrow(flow.file[log10(flow.file[,x_channel]) >= xgate & log10(flow.file[,y_channel]) >= ygate,])/ nrow(flow.file) *100, "%"), x = 6.5, y = 7.8,colour="black")          
        label3<-  annotate("text", label = paste (nrow(flow.file[log10(flow.file[,x_channel]) < xgate & log10(flow.file[,y_channel]) < ygate,])/ nrow(flow.file) *100, "%"), x = .5, y = .2,colour="black") 
        label4<-  annotate("text", label = paste (nrow(flow.file[log10(flow.file[,x_channel]) > xgate & log10(flow.file[,y_channel]) < ygate,])/ nrow(flow.file) *100, "%"), x = 6.5, y = .2,colour="black") 
        
        
        plot1 <-  ggplot(log10(flow.file), aes_string(x= names[x_channel], y= names[y_channel] )) +  
                geom_point(pch = ".", colour =densCols(log10(flow.file[, c(x_channel,y_channel)]), colramp = colPalette)) + 
                gate1 +gate2 + 
                label1 +label2 +label3 +label4 +
                #                        lines +  
                theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
plot1
}




shinyServer(
        function(input, output) {
                output$newPlot <- renderPlot({
                
                inFile1 <- input$file1
                inFile2 <- input$file2
                inFile3 <- input$file3
                inFile4 <- input$file4
                x_channel <- as.numeric (input$x_ch)
                y_channel <- as.numeric (input$y_ch)
                xgate <- input$xgate
                ygate <- input$ygate
                
### file1                
                if (is.null(inFile1))                  
                        file <- list.files()[1]   
                        else
                                file <- inFile1$datapath
                plot1 <- flowplot_single (file =file, x_channel, y_channel, xgate, ygate)
                                
### file2               
                if (is.null(inFile2))                  
                        file <- list.files()[2]   
                else
                        file <- inFile2$datapath
                plot2 <- flowplot_single (file =file, x_channel, y_channel, xgate, ygate)
                                                         
                                
### file3 
                if (is.null(inFile3))                  
                        file <- list.files()[3]   
                else
                        file <- inFile3$datapath
                plot3 <- flowplot_single (file =file, x_channel, y_channel, xgate, ygate)
                
### file4                
                if (is.null(inFile4))                  
                        file <- list.files()[4]   
                else
                        file <- inFile4$datapath
                plot4 <- flowplot_single (file =file, x_channel, y_channel, xgate, ygate)
                
                
                
                
#                 
#                 if ( input$title == T) 
#                                         plot1 <- plot1 + ggtitle(paste(inFile$name," Flow Cytometry Analysis") ) +
#                                                         xlab(paste(input$x_lab,  "(", names(x1)[x_ch], ")")) + 
#                                                         ylab(paste(input$y_lab,  "(", names(x1)[y_ch], ")")) 
#                 else
#                         plot1 <- plot1 +  ggtitle(input$title_in ) +
#                         xlab(input$x_lab) +  ylab(input$y_lab) 
                
                
                
                        myPlotList = list()
                myPlotList = c(myPlotList,list(plot1,plot2,plot3,plot4))        
                do.call(grid.arrange,  myPlotList)
                
                
                                        })                         
                                }
)









# options(shiny.maxRequestSize = 9*1024^2)
# library(flowCore)
# 
# shinyServer(function(input, output) {
#         output$contents <- renderTable({                      

#                 
#         })
# })