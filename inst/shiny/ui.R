
library(shinythemes);

shinyUI(
    fluidPage(theme = shinytheme("cosmo"),
              includeScript('www/tools.js'),
              ##css
              tags$head(tags$title("Cutoff Finder"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Oswald"),
                        tags$link(rel = "stylesheet", type = "text/css", href = "//fonts.googleapis.com/css?family=Lora")
                        ),

              ##title box
              withTags({
                  div(class="cheader",
                      "Cutoff Finder",
                      tags$button(
                               id = 'close',
                               type = "button",
                               class = "btn action-button",
                               onclick = "setTimeout(function(){window.close();},500);",  # close browser
                               "Exit",
                               style="float: right;
                                      background-image: url(texturebg2.jpg);"
                           )
                      )
              }),

              ##main page
              uiOutput("mainpage"),

              ##foot
              withTags({
                  div(class="cfooter",
                      "Cutoff Finder")
              })
              )
)
