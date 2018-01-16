

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(



)

# Define server logic required to draw a histogram
server <- function(input, output) {

  library("shiny")

  library("MALDIquant")
  library("MALDIquantForeign")

  options(shiny.maxRequestSize=200*1024^2)

  data("fiedler2009subset")

  convertYlim <- function(lim, fun) {
    fun <- match.fun(fun)
    lim <- fun(lim)
    lim[is.infinite(lim)] <- 0
    return(lim)
  }

  generateListNames <- function(x) {
    fn <- MALDIquantForeign:::.composeFilename(x, fileExtension="")
    fn <- sub(pattern="[[:punct:]]+$", replacement="", x=fn)
    return(setNames(x, fn))
  }

  massLimits <- function(x) {
    mL <- range(unlist(lapply(x, function(y)range(mass(y)))))
    mL <- trunc(mL)+c(0, 1)
    return(mL)
  }

  intensityLimits <- function(x) {
    return(c(0, ceiling(max(unlist(lapply(x, function(y)max(intensity(y))))))))
  }

  s <<- list()
  s <<- generateListNames(fiedler2009subset)
  xlim <- massLimits(s)
  ylim <- intensityLimits(s)

  shinyServer(function(input, output, session) {

    output$selectSpectra <- renderUI({
      input$ds
      input$dsFile

      selectInput(inputId="sel",
                  label="Plot Spectrum:",
                  choices=names(s), selected=names(s)[1], multiple=TRUE)
    })

    output$xlimSlider <- renderUI({
      input$ds
      input$dsFile

      xlim <- massLimits(s)
      sliderInput(inputId="xlim", label="Mass Range:",
                  min=xlim[1], max=xlim[2], value=xlim,
                  ticks=TRUE)
    })

    output$ylimSlider <- renderUI({
      input$ds
      input$dsFile

      ylim <- intensityLimits(s)
      sliderInput(inputId="ylim", label="Intensity Range:",
                  min=ylim[1], max=ylim[2],
                  value=ylim, ticks=TRUE)
    })

    dataset <- reactive({
      if (is.null(input$dsFile) || input$ds == "fiedler2009subset") {
        s <- fiedler2009subset
      } else {
        originalSize <- input$dsFile$size
        uploadedSize <- file.info(input$dsFile$datapath)$size
        filename <- file.path(dirname(input$dsFile$datapath), input$dsFile$name)
        file.rename(input$dsFile$datapath, filename)
        s <- import(filename)
        file.rename(filename, input$dsFile$datapath)
      }

      s <<- generateListNames(s)
      return(s)
    })

    currentSpectra <- reactive({
      dataset()

      if (is.null(input$sel)) {
        return(s[[1]])
      } else {
        return(s[input$sel])
      }
    })

    vsSpectra <- reactive({
      if (is.null(input$vs)) {
        method <- "sqrt"
      } else {
        method <- input$vs
      }
      return(transformIntensity(currentSpectra(), method=method))
    })

    smoothedSpectra <- reactive({
      if (is.null(input$sm)) {
        method <- "SavitzkyGolay"
        hws <- 10
      } else {
        method <- input$sm
        hws <- input$smHws
      }
      return(smoothIntensity(vsSpectra(), method=method, halfWindowSize=hws))
    })

    baselineCorrectedSpectra <- reactive({
      if (is.null(input$bc)) {
        method <- "SNIP"
        hws <- 100
      } else {
        method <- input$bc
        hws <- input$bcHws
      }

      return(lapply(smoothedSpectra(), function(y) {
        bl <- estimateBaseline(y, method=method, hws)
        intensity(y) <- intensity(y)-bl[, 2]
        return(y)
      }))
    })

    detectedPeaks <- reactive({
      return(detectPeaks(baselineCorrectedSpectra(), method=input$pdNoise,
                         halfWindowSize=input$pdHws, SNR=input$pdSNR))
    })

    # Generate a summary of the dataset
    checkSpectra <- reactive({
      s <- dataset()

      areEmpty <- sapply(s, isEmpty)
      areRegular <- sapply(s, isRegular)
      allLength <- sapply(s, length)

      anyEmpty <- any(areEmpty)
      anyIrregular <- any(!areRegular)
      anyLenghtDiffer <- any(length(s[[1]]) != allLength)

      return(list(anyEmpty=anyEmpty, anyIrregular=anyIrregular,
                  anyLenghtDiffer=anyLenghtDiffer,
                  table=data.frame(empty=areEmpty, irregular=!areRegular,
                                   length=allLength)))
    })

    output$spectraSummary <- renderPrint({
      cat("Is any spectrum empty? : ", checkSpectra()$anyEmpty, "\n")
      cat("Is any spectrum irregular? : ", checkSpectra()$anyIrregular, "\n")
      cat("Has any spectrum a different length? : ",
          checkSpectra()$anyLenghtDiffer, "\n")
    })

    output$spectraSummaryTable <- renderTable({
      checkSpectra()$table
    })

    ## taken from https://gist.github.com/wch/5436415
    listPlot <- function(x, additonalPlotFunction=NULL, prefix, xlim, ylim,
                         type="l") {
      plotOutputList <- lapply(seq_along(s), function(i) {
        plotname <- paste0(prefix, "plot", i)
        plotOutput(plotname)
      })

      for (i in seq_along(x)) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
          my_i <- i
          my_xlim <- xlim
          my_ylim <- ylim
          my_type <- type
          my_x <- x
          plotname <- paste0(prefix, "plot", my_i, sep="")
          output[[plotname]] <<- renderPlot({
            plot(my_x[[my_i]], xlim=my_xlim, ylim=my_ylim, type=my_type)
            if (!is.null(additonalPlotFunction)) {
              fun <- as.function(additonalPlotFunction)
              fun(my_x[[my_i]])
            }
          })
        })
      }

      return(plotOutputList)
    }

    output$plotRaw <- renderPlot({
      plot(s[[1]])
    })

    output$rawPlots <- renderUI({
      do.call(tagList, listPlot(currentSpectra(), prefix="raw",
                                xlim=input$xlim, ylim=input$ylim))
    })

    output$smoothedPlots <- renderUI({
      do.call(tagList,
              listPlot(vsSpectra(), function(y) {
                lines(smoothIntensity(y, method=input$sm,
                                      halfWindowSize=input$smHws), col=2)},
                prefix="smoothed", xlim=input$xlim,
                ylim=convertYlim(input$ylim, input$vs)))
    })

    output$baselinePlots <- renderUI({
      do.call(tagList,
              listPlot(smoothedSpectra(), function(y) {
                bl <- estimateBaseline(y, method=input$bc, input$bcHws)
                if (input$bcUS) {
                  lines(bl, col=2, lwd=2)
                }
                if (input$bcBC) {
                  lines(mass(y), intensity(y)-bl[,2], col=4)
                }
              },
              prefix="baseline", xlim=input$xlim,
              ylim=convertYlim(input$ylim, input$vs),
              type=ifelse(input$bcUS, "l", "n")))
    })

    output$peakPlots <- renderUI({
      do.call(tagList,
              listPlot(baselineCorrectedSpectra(), function(y) {
                n <- estimateNoise(y, method=input$pdNoise)
                lines(n[, 1], input$pdSNR*n[, 2], col=2, lwd=2)
                p <- detectPeaks(y, method=input$pdNoise,
                                 halfWindowSize=input$pdHws,
                                 SNR=input$pdSNR)
                points(p, col=4, pch=4, lwd=2)

                if (input$plTopN) {
                  top <- sort(intensity(p), decreasing=TRUE,
                              index.return=TRUE,
                              method="quick")$ix[1:input$plTopN]
                  if (input$plRotate) {
                    srt <- 90
                    adj <- c(-0.1, 0.5)
                  } else {
                    srt <- 0
                    adj <- c(0.5, 0)
                  }
                  labelPeaks(p[top], srt=srt, adj=adj)
                }
              },
              prefix="peaks", xlim=input$xlim,
              ylim=convertYlim(input$ylim, input$vs)))
    })

  })







}

# Run the application
shinyApp(ui = ui, server = server)

