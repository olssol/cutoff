shinyServer(function(input, output, session) {

    ##source("cutoff.R",    local=TRUE);
    source("cutoff_ui.R", local=TRUE);

    userLog          <- reactiveValues();
    userLog$data     <- NULL;

    ##--------------------------------------
    ##---------exit-------------------------
    ##--------------------------------------
    observeEvent(input$close, {stopApp()});

    ##------------------------------------
    ##---------main page------------------
    ##------------------------------------
    output$mainpage <- renderUI({
        tab.main();
    })


    ##--------------------------------------
    ##---------data upload------------------
    ##--------------------------------------

    ##--display data uploaded-----
    output$uiData <- DT::renderDataTable({
        if (input$displaydata) {
            userLog$data;
        }
        }, rownames=NULL,
        selection="none",
        options=list(pageLength = 50,
                     scrollX    = TRUE))

    ##--------------------------------------
    ##---------model specification----------
    ##--------------------------------------

    ##--model specification------
    output$uiPanelModel <- renderUI({
        if (is.null(userLog$data)) {
            msg.box("Please upload data first.", "warning");
        } else {
            panel.model();
        }
    })

    ##----specify covarates------
    output$uiModel <- renderUI({
        if (!is.null(userLog$data)) {
            do.call(fluidPage, gen.radiobtn(names(userLog$data)));
        }
    })

    ##--------------------------------------
    ##---------CUT oFF----------------------
    ##--------------------------------------
    output$htRst <- renderText({
        cur.rst <- sh.get.cutoff();
        HTML(cur.rst$result);
    })

    ##--------------------------------------
    ##---------PLOT-------------------------
    ##--------------------------------------
    output$outPlotOverview <- renderPlot({

        rst.cut <- sh.get.cutoff();
        if (is.null(rst.cut))
            return(NULL);

        t.plt <- input$inOP;

        type.variable <- unlist(strsplit(rst.cut$type,
                                         "_"))[1]


        if ("histogram" == t.plt) {
            if (rst.cut$type == "distribution") gauss <- TRUE
            else gauss <- FALSE
            plothistogram(marker=rst.cut$biomarker,
                           cutoff=rst.cut$cutoff,
                           gauss=gauss)
        }

        if ("OR" == t.plt & "outcome" == type.variable) {
            plotOR(rst.cut$OR,
                    marker=rst.cut$biomarker,
                    outcome=rst.cut$outcome,
                    cutoff=rst.cut$cutoff)
        }

        if ("ROC" == t.plt & "outcome" == type.variable) {
            plotROC(rst.cut$OR,
                     marker=rst.cut$biomarker,
                     outcome=rst.cut$outcome, cutoff=rst.cut$cutoff)
        }

        if ("HR" == t.plt & "survival" == type.variable) {
            plotHR(rst.cut$HR,
                    marker=rst.cut$biomarker,
                    time=rst.cut$time,
                    event=rst.cut$event,
                    cutoff=rst.cut$cutoff);
        }

        if ("time" == t.plt & "survival" == type.variable) {
            plottime(rst.cut$HR,
                      marker=rst.cut$biomarker,
                      time=rst.cut$time,
                      event=rst.cut$event,
                      cutoff=rst.cut$cutoff);
        }


    },  bg="transparent");


    output$outPlotCut <- renderPlot({

        rst.cut <- sh.get.cutoff();
        if (is.null(rst.cut))
            return(NULL);

        t.plt <- input$inACP;

        type.variable <- unlist(strsplit(rst.cut$type,
                                         "_"))[1]

        if ("waterfall" == t.plt & "outcome" == type.variable) {
            ind <- intersect(rst.cut$index$biomarker,
                             rst.cut$index$outcome);

            if (length(ind) >= 2*rst.cut$nmin)
                plotwaterfall(rst.cut$biomarker[ind],
                               rst.cut$outcome[ind],
                               rst.cut$cutoff);
        }

        if ("kaplanmeier" == t.plt & "survival" == type.variable) {
            ind <- intersect(rst.cut$index$biomarker,
                             rst.cut$index$survival);

            if (length(ind) >= 2*rst.cut$nmin)
                plotkaplanmeier(rst.cut$biomarker[ind],
                                 rst.cut$time[ind],
                                 rst.cut$event[ind],
                                 rst.cut$cutoff);
        }
    },  bg="transparent");
})
