##-------------------------------------------------------------
##           UI FUNCTIONS
##-------------------------------------------------------------

##show different type of messages
msg.box <- function(contents, type="info") {
    switch(type,
           info    = cls <- "cinfo",
           warning = cls <- "cwarning",
           success = cls <- "csuccess",
           error   = cls <- "cerror");
    rst <- '<div class="';
    rst <- paste(rst, cls, '">');
    rst <- paste(rst, contents);
    rst <- paste(rst, "</div>");
    HTML(rst);
}

##generate radio buttons for column variables
gen.radiobtn <- function(vnames) {
    rst <- list(HTML("<div class='row'>
                      <span class='smdl'></span>
                      <span class='smdl'>Biomarker</span>
                      <span class='smdl'>Outcome</span>
                      <span class='smdl'>Survival Time</span>
                      <span class='smdl'>Event</span>
                      <span class='smdl'>Ignore</span>
                      </div>"));


    for (i in 1:length(vnames)) {
        ##remove space
        vname <- gsub("\\s","", vnames[i]);
        bname <- paste("inRdo", i, sep="");
        rst[[i+1]] <- fluidRow(
            column(1, h5(vname), align="center"),
            column(11,
                   radioButtons(bname, "",
                                c(" " = "biomarker",
                                  " " = "outcome",
                                  " " = "time",
                                  " " = "event",
                                  " " = "ignore"),
                                selected="ignore",
                                inline=TRUE)
                   )

            );
    }
    rst
}

##tabset for data uploading
tab.upload <- function(){
    tabPanel("Upload Data",
             wellPanel(h4("Upload data"),
                       fluidRow(
                           column(3, h6("Choose File"),
                                  fileInput(inputId = 'userdata', label = '',
                                            accept=c('text/csv','text/comma-separated-values,text/plain'))),
                           column(2, h6("Separator"),
                                  radioButtons('sep', '',
                                               c(Comma=',',Semicolon=';',Tab='\t',Space=' '),
                                               '\t')),
                           column(2, h6("Quote"),
                                  radioButtons('quote', '',
                                               c(None='','Double Quote'='"','Single Quote'="'"),
                                               selected = '')),
                           column(2, h6("NA string"),
                                  radioButtons('nastrings', '', c('.'='.','NA'="NA"), selected = 'NA')),
                           column(2, h6("Other"),
                                  checkboxInput(inputId='header', label='Header', value=TRUE),
                                  checkboxInput(inputId="displaydata", label = "Show Data", value = TRUE)
                                  )
                       )),
             conditionalPanel(condition="input.displaydata == true",
                              wellPanel(h4("Review Data"),
                                        DT::dataTableOutput("uiData")))
             )
}

##specify model covariates
panel.model <- function(){
    list(
        wellPanel(id="panelmdl",
                  h4(""),
                  uiOutput("uiModel")
                  )
        );
}

panel.cutoff <- function() {
    list(
        wellPanel(
            fluidRow(column(4,
                            h4("Method for cutoff determination"),
                            radioButtons("inMtd", '',
                                         c("None" = "none",
                                           "Fit of mixture model" = "distribution",
                                           "Outcome: significance (Fisher's exact test)" = "outcome_significance",
                                           "Outcome: ROC curve (Euclidean distance)" = "outcome_euclidean",
                                           "Outcome: ROC curve (Manhattan distance)" = "outcome_manhattan",
                                           "Outcome: minimum sensitivity" = "outcome_sensitivity",
                                           "Outcome: minimum specificity" = "outcome_specificity",
                                           "Survival: significance (log-rank test)" = "survival_significance",
                                           "Enter cutoff value" = "manual")),
                            conditionalPanel(condition="input.inMtd == 'manual'
                                                        | input.inMtd == 'outcome_sensitivity'
                                                        | input.inMtd == 'outcome_specificity'",
                                             numericInput("inValue", "Please enter value", value = 0.05,
                                                          width = "150px"))
                            ),
                     column(6,
                            h4("Results"),
                            htmlOutput("htRst")
                            )
                     ),
            actionButton("btnSubmit", "Submit", styleclass="info")
        ),
        wellPanel(
            h4("Overview Plot"),
            fluidRow(
                column(3,
                       radioButtons("inOP", '',
                                     c("Histogram" = "histogram",
                                       "Odds Ratio (outcome information required)" = "OR",
                                       "ROC curve (outcome information required)" = "ROC",
                                       "Hazard Ratio (survival information required)" = "HR",
                                       "Difference in survival (survival information required)" = "time")
                                     )),
                column(9,plotOutput("outPlotOverview"))
            )
        ),
        wellPanel(
            h4("Plots at cutoff point"),
            fluidRow(
                column(3,
                       radioButtons("inACP", '',
                                    c("Waterfall plot (outcome information required)" = "waterfall",
                                      "Kaplan-Meier plot (survival information required)" = "kaplanmeier"))),
                column(9,plotOutput("outPlotCut"))
            )
        )
    )
}

##define the main tabset for beans
tab.main <- function() {
    tabsetPanel(type = "pills",
                id="mainpanel",
                selected="Upload Data",
                tab.upload(),
                tabPanel("Assign Variables", uiOutput("uiPanelModel")),
                tabPanel("Determine Cutoff", panel.cutoff())
                );
}


##-------------------------------------------------------------
##           SERVER FUNCTIONS
##-------------------------------------------------------------
observe ({
    inFile <- input$userdata;
    if (!is.null(inFile)){
        userLog$data  <- read.csv(inFile$datapath,
                                  header=input$header,
                                  sep=input$sep,
                                  quote=input$quote,
                                  na.strings=input$nastrings);
    }
})

sh.data <- reactive({

    cur.data  <- userLog$data;
    if (is.null(input$inRdo1) |
        is.null(cur.data))
        return(NULL);


    biomarker <- NULL;
    outcome   <- NULL;
    time      <- NULL;
    event     <- NULL;

    for (i in 1:ncol(cur.data)) {
        cur.t <- input[[paste("inRdo", i, sep="")]];
        switch(cur.t,
               biomarker = {biomarker <- cur.data[,i]},
               outcome   = {outcome   <- cur.data[,i]},
               time      = {time      <- cur.data[,i]},
               event     = {event     <- cur.data[,i]});
    }


    ## if (is.null(biomarker) |
    ##     is.null(outcome)   |
    ##     is.null(time)      |
    ##     is.null(event) )
    ##     return(NULL);

    list(biomarker = biomarker,
         outcome   = outcome,
         time      = time,
         event     = event);
})


sh.get.cutoff <- reactive({

    if (is.null(input$btnSubmit))
        return(NULL);

    isolate({
        cur.dta <- sh.data();
        type    <- input$inMtd;

        if (is.null(cur.dta) |
            is.null(type))
            return(NULL);

        cutoff    <- NULL;
        threshold <- NULL;
        if ("manual" == type) {
            cutoff <- as.numeric(input$inValue);
        } else if (type %in% c("outcome_sensitivity", "outcome_specificity")) {
            threshold <- as.numeric(input$inValue);
        }

        rst <- r.get.cutoff(type      = type,
                            biomarker = cur.dta$biomarker,
                            outcome   = cur.dta$outcome,
                            time      = cur.dta$time,
                            event     = cur.dta$event,
                            cutoff    = cutoff,
                            threshold = threshold);
    })
})



