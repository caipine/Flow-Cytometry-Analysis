#library(gridExtra)
library(ggplot2)
library(flowCore)


shinyServer(
        function(input, output) {
                output$newPlot <- renderPlot({
                        colPalette <- colorRampPalette(c("blue", "turquoise", "green", "yellow", "orange", "red"))
                 
                inFile <- input$file1                             
                if (is.null(inFile)){                        
                        files <- list.files()
                        x1 <- as.data.frame(as(lapply(files[1],  read.FCS), "flowSet") [[1]]@ exprs)
                }           
                        else
                        x1 <- as.data.frame(as(lapply(inFile$datapath,  read.FCS), "flowSet") [[1]]@ exprs)
                
                colnames(x1)  <- names <- c("FSC_A", "SSC_A", "FL1_A", "FL2_A", "FL3_A", "FL4_A", "FSC_H", "SSC_H", "FL1_H", "FL2_H", "FL3_H", "FL4_H", "Width", "Time")
                        

                x_ch <- as.numeric (input$x_ch)
                y_ch <- as.numeric (input$y_ch)
                xgate <- input$xgate
                ygate <- input$ygate
                        
                
                        gate1 <- annotate("segment", x=0, xend=8, y=ygate, yend=ygate, colour="gray", size=0.5)
                        gate2 <- annotate("segment", x=xgate, xend=xgate, y=0, yend=8, colour="gray", size=0.5)
                                
                                label1<-  annotate("text", label = paste (nrow(x1[log10(x1[,x_ch]) < xgate & log10(x1[,y_ch]) > ygate,])/ nrow(x1) *100, "%"), x = .5, y = 7.8,colour="black") 
                                label2<-  annotate("text", label = paste (nrow(x1[log10(x1[,x_ch]) >= xgate & log10(x1[,y_ch]) >= ygate,])/ nrow(x1) *100, "%"), x = 6.5, y = 7.8,colour="black")          
                                label3<-  annotate("text", label = paste (nrow(x1[log10(x1[,x_ch]) < xgate & log10(x1[,y_ch]) < ygate,])/ nrow(x1) *100, "%"), x = .5, y = .2,colour="black") 
                                label4<-  annotate("text", label = paste (nrow(x1[log10(x1[,x_ch]) > xgate & log10(x1[,y_ch]) < ygate,])/ nrow(x1) *100, "%"), x = 6.5, y = .2,colour="black") 
                
                
                                plot1 <-  ggplot(log10(x1), aes_string(x= names[x_ch], y= names[y_ch] )) +  
                                          geom_point(pch = ".", colour =densCols(log10(x1[, c(x_ch,y_ch)]), colramp = colPalette)) + 
                                          gate1 +gate2 + label1 + label2 + label3 + label4 +                                            
                                          theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
                
                                                         
                                if ( input$title == T) 
                                        plot1 <- plot1 + ggtitle(paste(inFile$name," Flow Cytometry Analysis") ) +
                                                        xlab(paste(input$x_lab,  "(", names(x1)[x_ch], ")")) + 
                                                        ylab(paste(input$y_lab,  "(", names(x1)[y_ch], ")")) 
                                        else
                                                plot1 <- plot1 +  ggtitle(input$title_in ) +
                                                        xlab(input$x_lab) +  ylab(input$y_lab) 
                        plot1
                
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