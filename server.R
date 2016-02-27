####################################
#### DDP Assignment - server.R ###
####################################

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(corrplot)

load("procdata.RData") # load the dataframe

shinyServer(function(input, output, session){ # pass in a session argument
                                              # for extra functionality
    
    # keep control of the checkbox control
    
    observe({
        
        if(class(input$chartType) != "character"){
            
            updateCheckboxGroupInput(session, "chartType",
                                     choices = list("Violin Plot" = "VPlot",
                                                    "Box Plot" = "BPlot",
                                                    "Jitter Plot" = "JPlot"),
                                     selected = c("VPlot")
            )
            
        }
        
    })
    
    output$yearRange <- renderUI({
        minmax <- range(as.integer(year(procdata$Date)))
        sliderInput("range", "Years range to consider",
                    min = minmax[1], max = minmax[2], step = 1, value = minmax, sep = "")
    })
    
    # output$yZoom <- renderUI({
    #     minmax <- range(pData()$Performance)
    #     rng <- floor(minmax[1]) - ceiling(minmax[2])
    #     sliderInput("yzoom", "Y axis range",
    #                 min = floor(minmax[1]), max = ceiling(minmax[2]), step = round((minmax[2] - minmax[1])/100, 0), value = minmax, sep = "")
    # })
    # 
    output$procList <- renderUI({
        procList <- unique(procdata$Process)
        selectizeInput(
            'ggProcess', 'Processes to plot', choices = procList,
            multiple = TRUE, selected = procList
        )
    })
    
    # prep data once and then pass around the program
    oRank <- reactive({
        outlRank <- switch (input$ggOutl,
                            "No removal" = 0,
                            "Remove 1st pass" = 1,
                            "Remove 2nd pass" = 2,
                            "Remove 3rd pass" = 3
        )
        outlRank
    })

    pData <- reactive({
        procdata$OutlRank <- 0
        if(input$Transform == "Log") {
            df <- mutate(procdata, Performance = log(Performance))
        } else
            df <- procdata
        
        if(!is.null(input$range)) { # During application start we don't control whether this code is executed
                                    # before creating yersRange UI element of after.
            df <- filter(df, year(Date) >= input$range[1] & year(Date) <= input$range[2])
        }
        
        for(iter in 1:3) {
            for(outl in c("A", "B", "C", "D", "E", "F")) { # generate this list from data in the next revision
                bp1 <- boxplot(df$Performance[df$Process == outl & df$OutlRank == 0], plot = F)
                outliers <- which(df$Process == outl & df$Performance %in% bp1$out)
                df$Outlier[outliers] <- df$Iteration[outliers]
                df$OutlRank[outliers] <- iter
            }
        }

        df <- filter(df, OutlRank == 0 | OutlRank > oRank())
        df <- filter(df, Process %in% input$ggProcess)
        df
    })    
        
    pShapiro <- reactive({
        outdf <- data.frame()
        st <- data.frame(Process = character(1), Statistic = numeric(1), pvalue = numeric(1), Verdict = character(1), stringsAsFactors = F)
        for(proc in input$ggProcess){
            dat <- select(filter(pData(), Process == proc), Performance)[[1]]
            if(length(dat) > 3) {       # Shapiro test requires at least 3 data points
                stt <- shapiro.test(dat)
                st$Process[1] <- proc
                st$Statistic[1] <- round(stt$statistic,digits = 4)
                st$pvalue[1] <- formatC(stt$p.value, digits = 4, width = 6, flag = "-", format = "e")
                st$Verdict[1] <- ifelse(stt$p.value >= .05, "Normally distributed @ 0.05", "Not normally distributed @ 0.05")
                outdf <- rbind(outdf, st)
            }
        }
        if(nrow(outdf) > 0)
            names(outdf)[3] <- "p-value"
        outdf
    })
    
    pTtest <- reactive({
        len <- length(input$ggProcess)
        if(len == 0)
            len <- 1 # just to avoid error when defining matrix with 0 cols
        m <- matrix(rep(0, len * len), ncol = len)
        if(len > 1) {
            for(c in 1:(len - 1)) {
                d1 <- select(filter(pData(), Process == input$ggProcess[c]), Performance)[[1]]
                for(r in (c + 1):len) {
                    d2 <- select(filter(pData(), Process == input$ggProcess[r]), Performance)[[1]]
                    m[r,c] <- ifelse(t.test(d1, d2)$p.value < 0.05, 1, 0)
                }
            }
        }
        colnames(m) <- input$ggProcess
        rownames(m) <- input$ggProcess
        m
    })
    
    pLModel <- reactive({
        outlm <- data.frame()
        lmdat <- data.frame(Process = character(1), Slope = numeric(1), CI025 = numeric(1), CI975 = numeric(1), pvalue = numeric(1), Verdict = character(1), fit = list(1), stringsAsFactors = F)
        if(length(input$ggProcess) > 0) {       # at least one process should be selected
            for(proc in input$ggProcess){
                dat <- select(filter(pData(), Process == proc), Date, Performance)
                if(nrow(dat) > 0) {
                    fit <- lm(dat[2][[1]] ~ dat[1][[1]])
                    sumfit <- summary(fit)
                    lmdat$Process[1] <- proc
                    lmdat$Slope[1] <- round(sumfit$coefficients[2,1],digits = 4)
                    lmdat$CI025[1] <- round(confint(fit)[2,1],digits = 4)
                    lmdat$CI975[1] <- round(confint(fit)[2,2],digits = 4)
                    lmdat$pvalue[1] <- formatC(sumfit$coefficients[2,4], digits = 4, width = 6, flag = "-", format = "e")
                    lmdat$Verdict[1] <- ifelse(sumfit$coefficients[2,4] >= .05, "Trend is flat @ 0.05", "Trend is NOT flat @ 0.05")
                    lmdat$fit[1] <- list(fit)
                    outlm <- rbind(outlm, lmdat)
                } 
            }
        }else
            outlm <- lmdat
        if(nrow(outlm) > 0) {
            names(outlm)[3:5] <- c("CI 2.5%", "CI 97.5%", "p-value")
            rownames(outlm) <- input$ggProcessy
        }
        outlm
        
    })
    
    output$ggDistrib <- renderPlot({
        ga <- ggplot(pData(), aes(x = Process, y = Performance, fill = Process))
        if("VPlot" %in% input$chartType)
            ga <- ga + geom_violin()
        if("BPlot" %in% input$chartType)
            if(!("VPlot" %in% input$chartType)) {
                ga <- ga + geom_boxplot(aes(fill = Process), color = "red", outlier.colour = "red", outlier.size = 3, width = .5)
            } else {
                ga <- ga + geom_boxplot(fill = "grey", color = "red", outlier.colour = "red", outlier.size = 3, width = .2)
            }
        if("JPlot" %in% input$chartType)
            ga <- ga + geom_jitter(width = .2, alpha = .3, size = 3)
        if(input$labelOutl & ("BPlot" %in% input$chartType))
            ga <- ga + geom_text(aes(label = ifelse(pData()$OutlRank == oRank() + 1, Outlier, "")), hjust = 0, nudge_x = .1, size = 4)
        if(input$mean)
            ga <- ga + stat_summary(color = "black", fun.y=mean, geom="point", shape=21, size=4)
        ga <- ga + ggtitle("Process Performance")
        ga <- ga + xlab("Process type")
        if(input$Transform == "None") {
            ga <- ga + ylab("Performance metric")
        } else
            ga <- ga + ylab("Log of Performance metric")
        # if(!is.null(input$yzoom)) {
        #     rngoffset <- (input$yzoom[2] - input$yzoom[1])* .1
        #     ga <- ga + geom_hline(yintercept = input$yzoom[1], color = "blue", linetype = "dashed")
        #     ga <- ga + geom_hline(yintercept = input$yzoom[2], color = "blue")
        #     ga <- ga + ylim(input$yzoom[1] - rngoffset, input$yzoom[2] + rngoffset)
        # }
        ga <- ga + theme(plot.title = element_text(size = rel(2), colour = "darkblue", face = "bold"))
        ga <- ga + theme(axis.title.y = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.title.x = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.text.x = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.text.y = element_text(size = rel(1.5)))
        
        print(ga)

    }, height = 500)

    output$summary <- renderPrint({
        if(!is.null(input$ggProcess)) {
            aggregate(pData()$Performance, by = list(Process = pData()$Process), summary)
        } else {
            cat("No data to display")
        }
    })
    
    output$trendGraph <- renderPlot({

        g8 <- ggplot(pData(), aes(x = Date, y = Performance, color = Process))
        g8 <- g8 + geom_point(size = 3, alpha = .6)
        if(input$smoother)
            g8 <- g8 + geom_smooth(method = input$linearModel, color = "black")
        if(length(input$ggProcess > 0))
            g8 <- g8 + facet_wrap(~Process)
        g8 <- g8 + ggtitle("Performance trends per Process Type")
        g8 <- g8 + xlab("Process run date")
        if(input$Transform == "None") {
            g8 <- g8 + ylab("Performance metric")
        } else
            g8 <- g8 + ylab("Log of Performance metric")
        g8 <- g8 + theme(legend.position="none")
        g8 <- g8 + theme(axis.text.x = element_text(angle = 90))
        g8 <- g8 + theme(plot.title = element_text(size = rel(2), colour = "darkblue", face = "bold"))
        g8 <- g8 + theme(axis.title.y = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.title.x = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.text.x = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.text.y = element_text(size = rel(1.5)))
        
        print(g8)
    }, height = 500)

    output$ttest <- renderPlot({
        if(nrow(pTtest()) > 1)
            corrplot(pTtest(), col="darkgreen", type = "lower", cl.lim=c(0,1), tl.pos = "ld", diag = F, cl.pos = "n")
    })
    
    output$lmDiag <- renderPlot({
        op <- par(no.readonly = TRUE)
        par(mfrow = c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,1))
        s = input$lmodel_rows_selected
        if(length(input$ggProcess) > 0 & !is.null(s)) { # Protection for the situation when last process
            diagfit <- pLModel()$fit[s][[1]]            # has been removed from the list and not yet automatically added
            plot(diagfit, sub.caption = paste("Diagnostic plots for process", input$ggProcess[s]))
        }
        par(op)
    }, height = 500)
    
    # ### following sections define the code that makes graphs downloadable
    # To do: the code for plotting to file and to web page is the same, check
    #        if this can be moved outside reactive and renderPlot handlers to
    #        a function. The same with the download handler.

    pltDistr <- reactive({
        ga <- ggplot(pData(), aes(x = Process, y = Performance, fill = Process))
        if("VPlot" %in% input$chartType)
            ga <- ga + geom_violin()
        if("BPlot" %in% input$chartType) {
            if(!("VPlot" %in% input$chartType)) {
                ga <- ga + geom_boxplot(aes(fill = Process), color = "red", outlier.colour = "red", outlier.size = 3, width = .2)
            } else {
                ga <- ga + geom_boxplot(fill = "grey", color = "red", outlier.colour = "red", outlier.size = 3, width = .2)
            }
        }
        if("JPlot" %in% input$chartType)
            ga <- ga + geom_jitter(width = .2, alpha = .3, size = 3)
        if(input$labelOutl & ("BPlot" %in% input$chartType))
            ga <- ga + geom_text(aes(label = ifelse(pData()$OutlRank == oRank() + 1, Outlier, "")), hjust = 0, nudge_x = .1, size = 4)
        if(input$mean)
            ga <- ga + stat_summary(color = "black", fun.y=mean, geom="point", shape=21, size=4)
        ga <- ga + ggtitle("Process Performance")
        ga <- ga + xlab("Process type")
        if(input$Transform == "None") {
            ga <- ga + ylab("Performance metric")
        } else
            ga <- ga + ylab("Log of Performance metric")
        ga <- ga + theme(plot.title = element_text(size = rel(2), colour = "darkblue", face = "bold"))
        ga <- ga + theme(axis.title.y = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.title.x = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.text.x = element_text(size = rel(1.5)))
        ga <- ga + theme(axis.text.y = element_text(size = rel(1.5)))
        
        print(ga)
        
    })

    pltTrend <- reactive({
        
        g8 <- ggplot(pData(), aes(x = Date, y = Performance, color = Process))
        g8 <- g8 + geom_point(size = 3, alpha = .6)
        if(input$smoother)
            g8 <- g8 + geom_smooth(method = input$linearModel, color = "black")
        if(length(input$ggProcess > 0))
            g8 <- g8 + facet_wrap(~Process)
        g8 <- g8 + ggtitle("Performance trends per Process Type")
        g8 <- g8 + xlab("Process run date")
        if(input$Transform == "None") {
            g8 <- g8 + ylab("Performance metric")
        } else
            g8 <- g8 + ylab("Log of Performance metric")
        g8 <- g8 + theme(legend.position="none")
        g8 <- g8 + theme(axis.text.x = element_text(angle = 90))
        g8 <- g8 + theme(plot.title = element_text(size = rel(2), colour = "darkblue", face = "bold"))
        g8 <- g8 + theme(axis.title.y = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.title.x = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.text.x = element_text(size = rel(1.5)))
        g8 <- g8 + theme(axis.text.y = element_text(size = rel(1.5)))
        
        print(g8)
    })
    
    output$downloadDistribution <- downloadHandler(

        filename <- function() {
            paste("Process_distr_", format(Sys.time(), "%Y-%m-%d_%H-%M"),".png",sep="") },
        content <- function(file) {
            png(file, width = 800, height = 400,
                units = "px", pointsize = 12,
                bg = "white", res = NA)

            plot <- pltDistr()

            print(plot)

            dev.off()},
        contentType = 'image/png')

    output$downloadTrends <- downloadHandler(
        
        filename <- function() {
            paste("Process_trend_", format(Sys.time(), "%Y-%m-%d_%H-%M"),".png",sep="") },
        content <- function(file) {
            png(file, width = 800, height = 400,
                units = "px", pointsize = 12,
                bg = "white", res = NA)
            
            plot <- pltTrend()
            
            print(plot)
            
            dev.off()},
        contentType = 'image/png')
    
    output$shapiro <- DT::renderDataTable(DT::datatable(
        pShapiro(),
        options = list(autoWidth = T,
                       searching = FALSE,
                       paging = FALSE,
                       info = FALSE,
                       ordering = FALSE,
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#f4f4f4', 'color': '#0'});","}"
                       )
        ),
        style = "bootstrap",
        selection = "none",
        rownames = FALSE
    ))
    
    output$lmodel <- DT::renderDataTable({
        DT::datatable(
            pLModel()[,1:6],
            options = list(autoWidth = FALSE,
                           searching = FALSE,
                           paging = FALSE,
                           info = FALSE,
                           ordering = TRUE,
                           columnDefs = list(list(targets = c(1:5), orderable = FALSE)),
                           initComplete = JS(
                               "function(settings, json) {",
                               "$(this.api().table().header()).css({'background-color': '#f4f4f4', 'color': '#0'});","}"
                           )
            ),
            style = "bootstrap",
            selection = list(mode = 'single', selected = 1, target = 'row'),
            rownames = FALSE
        )
        },server = FALSE
    )
    
    # proxy = dataTableProxy('lmodel')
    # 
    # observeEvent(input$lmodel_rows_selected, {
    #     print(input$lmodel_rows_selected)
    #     if(is.null(input$lmodel_rows_selected))
    #         selectRows(proxy, 0)
    # })
    
    
    # output$ProcID  <- renderPrint({
    #     s = input$lmodel_rows_selected
    #     #if (length(s)) {
    #         cat('These rows were selected:\n\n')
    #         cat(s, sep = ', ')
    #     #}
    # })
    
})