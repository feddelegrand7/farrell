#' farrell Data Envelopment Analysis Gadget
#'
#' @return A data frame of the efficiency results including efficiency scores and peers' determination.
#' @export
#'
#'

farrell <- function(){


  ui <- miniUI::miniPage(


    shiny::tags$style("

h1{color:#337AB7;font-weight:bold; font-family:Verdana;}

body{background-color:#E0DAD0}
                      "),


    miniUI::miniTabstripPanel(


      # Uploading data tab ------------------------------------


      miniUI::miniTabPanel(
        title = "Loading Data",

        icon = shiny::icon("accusoft"),


        miniUI::miniContentPanel(

          shiny::sidebarLayout(


            shiny::sidebarPanel(


              shiny::fileInput(inputId = "file1",
                               label = "Load a csv file",
                               accept = c("csv", "CSV",
                                          "csv/text",
                                          "Comma Separated Values")),
              shiny::tags$hr()

            ),


            shiny::mainPanel(

              shiny::tags$h1("Data Frame Overview"),

              shiny::tableOutput(outputId = "tbl_loading")


            )))),



      # Model Tuning  -------------------------------------------------


      miniUI::miniTabPanel(


        title = "Model Tuning",

        icon = shiny::icon("affiliatetheme"),

        miniUI::miniContentPanel(


        shiny::fluidRow(

          shiny::tags$h1("Model Tuning", align = "center"),

          shiny::tags$hr(),

          shiny::column(4,
                        shinyWidgets::awesomeCheckboxGroup(
                          inputId = "input_select",
                          label = "Select the Input Variables",
                          choices = "")),

          shiny::column(4,
                        shinyWidgets::awesomeCheckboxGroup(
                          inputId = "output_select",
                          label = "Select the Output Variables",
                          choices = "")),

          shiny::column(4,

                        shinyWidgets::pickerInput(
                          inputId = "ID_choose",
                          label = "Select the Identification column",
                          choices = "",
                          options = list(
                            style = "btn-danger")),

                        shinyWidgets::pickerInput(
                          inputId = "RTS_choose",
                          label = "Select the Returns to Scale assumption",
                          choices = c("crs", "vrs", "irs",  "drs", "add", "fdh"),
                          options = list(
                            style = "btn-danger")),

                        shiny::actionButton(inputId = "help", label = "Help"),

                        shiny::tags$br(),

                        shiny::tags$br(),

                        shinyWidgets::pickerInput(
                          inputId = "orientation_choose",
                          label = "Select the orientation",
                          choices = c("input", "output"),
                          options = list(
                            style = "btn-danger"))






          )),
        shiny::tags$br(),

        shiny::tags$br(),


        shiny::fluidRow(

          shiny::column(4, ""),

          shiny::column(4, ""),

          shiny::column(4, shiny::submitButton(text = "Calculate Efficiency")),



        ),

        shiny::tags$br(),

        shiny::tags$br(),

        shiny::tags$br(),

        shiny::tags$br()



      )),


      # Results -----------------------------------------------------------------

      miniUI::miniTabPanel(

        title = "Efficiency Results",

        icon = shiny::icon("audible"),

        miniUI::miniContentPanel(

          shiny::tags$h1("Efficiency Results", align = "center"),

          shiny::tags$hr(),

        shiny::sidebarLayout(


          shiny::sidebarPanel(

            shiny::helpText("Click on the download button to get a csv file of the results"),

            shiny::downloadButton(outputId = "dbtn1", label = "download"),
            shiny::tags$br(),
            shiny::tags$br(),
            shiny::tags$h3("DEA Summary"),
            shiny::verbatimTextOutput("summary")

          ),

          shiny::mainPanel(



            shinycssloaders::withSpinner(shiny::tableOutput("eff_results1"),
                                         color = "#324C63")


          )



        ))),




# lambdas -----------------------------------------------------------------

 miniUI::miniTabPanel(title = "Lambdas",

    icon = shiny::icon("avianex"),

    miniUI::miniContentPanel(

   shiny::tags$h1("Lambdas", align = "center"),

   shiny::tags$hr(),

   shiny::tags$br(),

   shiny::sidebarLayout(


     shiny::sidebarPanel(
       shiny::helpText("Click on the download button to get a csv file of the results"),

       shiny::downloadButton(outputId = "dbtn2", label = "download"),

     ),

     shiny::mainPanel(shiny::tableOutput(outputId = "lambdas") %>%
                 shinycssloaders::withSpinner(color = "#324C63")
)

   )


 ))



    ))



  ############################ SERVER #######################################




  server <- function(input, output, session) {


    df <- shiny::reactive({

      shiny::req(input$file1)

      data <- data.table::fread(input$file1$datapath)


      shinyWidgets::updateAwesomeCheckboxGroup(session = session,
                                               inputId = "input_select",
                                               label = "Select the Input Variables",
                                               choices = data %>% dplyr::select_if(is.numeric) %>% names(),
                                               selected = NULL,
                                               status = "danger",
                                               inline = F)

      shinyWidgets::updateAwesomeCheckboxGroup(session = session,
                                               inputId = "output_select",
                                               label = "Select the Output Variables",
                                               choices = data %>% dplyr::select_if(is.numeric) %>% names(),
                                               selected = NULL,
                                               status = "danger",
                                               inline = F)

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "ID_choose",
                                      label = "Select the Identification column",
                                      choices = data %>% names())






      return(data)

    })


    output$tbl_loading <- shiny::renderTable({




      utils::head(df())



    })


    shiny::observeEvent(input$help, {
      shiny::showModal(shiny::modalDialog(
        title = "Help",
        shiny::HTML("

<ul>

<li>

crs: Constant returns to scale, convexity and free disposability

</li>

<li>

vrs: Variable returns to scale, convexity and free disposability

</li>

<li>

irs: Increasing returns to scale (up-scaling, but not down-scaling), additivity, and free disposability

</li>

<li>

drs: Decreasing returns to scale, convexity, down-scaling and free disposability
</li>


<li>

add: Additivity (scaling up and down, but only with integers), and free disposability; also known af replicability and free disposability, the free disposability and replicability hull (frh) -- no convexity assumption

</li>


<li>

fdh: Free disposability hull, no convexity assumption

</li>


</ul>


<a href='https://cran.r-project.org/web/packages/Benchmarking/Benchmarking.pdf'> Source</a>



             ")
      ))
    })



    scores <- shiny::reactive({

      df <- df()


      inputs <- df %>% dplyr::select(input$input_select)
      outputs <- df %>% dplyr::select(input$output_select)

      orientation <- switch(input$orientation_choose,
                            "input" = "in" ,
                            "output" = "out")

      r_eff <- Benchmarking::dea(X = data.matrix(inputs),
                                 Y = data.matrix(outputs),
                                 RTS = input$RTS_choose,
                                 ORIENTATION = orientation)

      return(r_eff)


    })



    output$eff_results1 <- shiny::renderTable({


      shiny::req(input$input_select,
                 input$output_select,
                 input$orientation_choose,
                 input$RTS_choose,
                 input$ID_choose)

      df <- df()

      r_eff2 <- scores()

      id <- df %>% dplyr::select(input$ID_choose)

      id2 <- df[, input$ID_choose]

      results <- dplyr::tibble(score = r_eff2$eff)

      peers <- Benchmarking::peers(r_eff2, NAMES = df %>% dplyr::pull(input$ID_choose))

      peers <- as.data.frame(peers)

      results <- cbind(id, results, peers)

      results <- results %>% dplyr::arrange(dplyr::desc(score))


      print(results)




    })


   download_results <- shiny::reactive({


     shiny::req(input$input_select,
                input$output_select,
                input$orientation_choose,
                input$RTS_choose,
                input$ID_choose)

     df <- df()

     r_eff2 <- scores()

     id <- df %>% dplyr::select(input$ID_choose)

     id2 <- df[, input$ID_choose]

     results <- dplyr::tibble(score = r_eff2$eff)

     peers <- Benchmarking::peers(r_eff2, NAMES = df %>% dplyr::pull(input$ID_choose))

     peers <- as.data.frame(peers)

     results <- cbind(id, results, peers)

     results <- results %>% dplyr::arrange(dplyr::desc(score))


     return(results)



   })



    output$dbtn1 <- shiny::downloadHandler(
      filename = function() {
        paste('efficiency-', Sys.Date(), '.csv', sep='')
      },
      content = function(file) {
        readr::write_csv(download_results(), path = file)
      }


    )

    output$summary <- shiny::renderPrint({


      shiny::req(input$input_select,
                 input$output_select,
                 input$orientation_choose,
                 input$RTS_choose,
                 input$ID_choose)


      r_eff2 <- scores()

      summary(r_eff2)

    })



output$lambdas <- shiny::renderTable({

  shiny::req(input$input_select,
             input$output_select,
             input$orientation_choose,
             input$RTS_choose,
             input$ID_choose)

    r_eff2 <- scores()

    df <- df()

    id2 <- df[, input$ID_choose]


    lambdas1 <- r_eff2$lambda

    lambdas2 <- as.data.frame(lambdas1)

    lambdas2 <- rlang::set_names(lambdas2, as.character(id2))

    rownames(lambdas2) <- id2

    lambdas3 <- lambdas2 %>% dplyr::select_if(~sum(.) > 0)

    lambdas4 <- tibble::rownames_to_column(lambdas3, var = "names")

    print(lambdas4)


    })


download_lambdas <- shiny::reactive({


  r_eff2 <- scores()

  df <- df()

  id2 <- df[, input$ID_choose]


  lambdas1 <- r_eff2$lambda

  lambdas2 <- as.data.frame(lambdas1)

  lambdas2 <- rlang::set_names(lambdas2, as.character(id2))

  rownames(lambdas2) <- id2

  lambdas3 <- lambdas2 %>% dplyr::select_if(~sum(.) > 0)

  lambdas4 <- tibble::rownames_to_column(lambdas3, var = "names")

  return(lambdas4)



})


output$dbtn2 <- shiny::downloadHandler(
  filename = function() {
    paste('lambdas-', Sys.Date(), '.csv', sep='')
  },
  content = function(file) {
    readr::write_csv(download_lambdas(), path = file)
  })




  }


  shiny::runGadget(ui, server, viewer = shiny::paneViewer())



}



