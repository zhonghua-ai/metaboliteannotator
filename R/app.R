#'
#' @keywords internal
ui <- shiny::navbarPage(
    # 修改图标样式
    title = tags$img(src = "custom/icon.svg",  # 改为使用custom路径
                    height = "40px",
                    style = "margin-top: 0px; margin-right: 10px;"),
    id = "mainTabs",
    windowTitle = "MetaboliteAnnotator",
    header = tagList(
       shinyjs::useShinyjs(), # Moved useShinyjs here
       tags$head(
           tags$link(href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;600&display=swap",
                     rel = "stylesheet"),
           tags$link(href = "https://fonts.googleapis.com/css2?family=Orbitron:wght@500&display=swap",
                     rel = "stylesheet"),
           tags$style(HTML("
               /* Overall modern and clean style */
               body {
                   background-color: #ffffff;
                   color: #333333;
                   font-family: -apple-system, BlinkMacSystemFont, 'Helvetica Neue', Helvetica, Arial, sans-serif;
                   margin: 0;
                   padding: 0;
               }

               /* navbar height is 60px */
               .navbar {
                   min-height: 60px;
                   display: flex;
                   align-items: center;
               }

               .navbar-brand {
                   padding: 10px 15px;
                   height: 60px;
                   display: flex;
                   align-items: center;
               }

               /* navbar tab title bold and font size increase */
               .navbar-nav > li > a {
                   font-weight: bold;
                   font-size: 18px;
                   padding: 15px;
                   line-height: 30px;
                   height: 60px;
                   display: flex;
                   align-items: center;
               }

               /* navbar tab title bold and font size increase */
               .navbar-nav {
                   display: flex;
                   align-items: center;
                   height: 60px;
               }

               /* other existing styles, such as .well, button styles, etc. remain unchanged */
               .well {
                   background: #f8f8f8 !important;
                   border: 1px solid #e0e0e0;
                   border-radius: 12px;
                   padding: 20px;
                   box-shadow: none;
               }
               /* set the style of the console output: text color is white, font is Arial */
               #searchConsole, #aiConsole, #keggConsole, #endogenousConsole {
                   color: white;
                   font-family: 'JetBrains Mono', Consolas, monospace;
               }
           ")),
           tags$script(HTML("
                // Helper function to update notification positions
                function updateNotifications() {
                    var notifications = $('.shiny-notification');
                    var offset = 20;
                    notifications.each(function() {
                        $(this).animate({ top: offset + 'px' }, 300);
                        offset += $(this).outerHeight() + 10;
                    });
                }
                $(document).on('shiny:notificationShown shiny:notificationDismissed', function() {
                    setTimeout(updateNotifications, 100);
                });
           ")),
           tags$link(rel = "stylesheet", type = "text/css",
                     href = "https://cdn.datatables.net/buttons/2.2.2/css/buttons.dataTables.min.css"),
           tags$script(src = "https://cdn.datatables.net/buttons/2.2.2/js/dataTables.buttons.min.js"),
           tags$script(src = "https://cdn.datatables.net/buttons/2.2.2/js/buttons.colVis.min.js"),
           tags$link(rel = "stylesheet", type = "text/css",
                     href = "https://cdn.datatables.net/searchhighlight/1.0.1/dataTables.searchHighlight.css"),
           tags$script(src = "https://cdn.datatables.net/searchhighlight/1.0.1/dataTables.searchHighlight.min.js"),
           tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jquery-highlight/3.7.0/jquery.highlight.min.js"),
           tags$head(tags$script(HTML('
                Shiny.addCustomMessageHandler("setupConsoleScroll", function(message) {
                    var consoleId = message.consoleId;
                    var $console = $("#" + consoleId);
                    if ($console.length) {
                        function scrollToBottom() {
                            $console.animate({ scrollTop: $console[0].scrollHeight }, 300);
                        }
                        var observer = new MutationObserver(function(mutations) {
                            requestAnimationFrame(function() {
                                scrollToBottom();
                            });
                        });
                        observer.observe($console[0], {
                            childList: true,
                            subtree: true,
                            characterData: true
                        });
                        scrollToBottom();
                    }
                });
           ')))
       )
    ),

    tabPanel("0. Instructions",
         fluidPage(
           includeMarkdown("readme.md")
         )
    ),
    tabPanel("1. Compound Auto-search",
         shiny::sidebarLayout(
             sidebarPanel(
                 textAreaInput("compounds",
                      "Enter compounds for search",
                      rows = 10,
                      placeholder = "Prostaglandin E2\nProstaglandin D2\n(Enter one compound per line)"),
                 helpText("Please enter compound names (one per line), then click 'Start Search' to begin processing."),
                 helpText("The search progress will be shown on the right in the real-time console."),

                 div(style = "margin-top: 10px;",
                     checkboxInput("clearCache", "Clear cache before running", value = TRUE),
                     helpText("Uncheck this option to keep previous unfinished processing.")
                 ),

                 shiny::actionButton("runSearch", "Start Search", class = "btn-primary")
             ),
             mainPanel(
                 h4("Search Progress", style = "margin-bottom: 15px;"),
                 div(
                     id = "searchConsole",
                     style = "height:500px; overflow-y:auto; background: #000000;",
                     ""
                 )
             )
         )
    ),
    tabPanel("2. AI-assisted Identification",
         fluidRow(
             column(3,
                 wellPanel(
                     div(style = "margin-bottom: 20px;",
                         shiny::actionButton("loadLocalResults", "Load Local Results",
                                    class = "btn-info",
                                    style = "width: 100%;"),
                         helpText("Load previous results from Step 1 if available.")
                     ),

                     # AI model selection
                     radioButtons("model",
                         "Select AI Model (Not Recommend Reasoner Models!!)",
                         choices = c("deepseek", "gpt-4o-mini")),

                     # New textAreaInput for AI Identification Prompt (Optional)
                     textAreaInput("ai_prompt",
                                   "AI Identification Prompt (Optional)",
                                   value = "For non-lipid compounds, the following cases should be considered the same compound:\n1) Suffixes \"-ate\" and \"acid\" are equivalent (e.g., \"Methylmalonate\" and \"Methylmalonic acid\");\n2) Ignore naming variations (e.g., trans/E, allo/alpha, etc.);\n3) DL/D/L configurations are equivalent;\n4) Prefixes \"methyl\" or \"phosphoric acid\" are equivalent;\n5) \"alpha\" equals Greek letter alpha, \"beta\" equals Greek letter beta, \"gamma\" equals Greek letter gamma, but distinguish alpha/beta/gamma forms!;\n6) Tautomers are equivalent;\n7) Resonance structures are equivalent;\n8) Hydrates or solvates are equivalent (e.g., CuSO4 and CuSO4_5H2O);\n9) Different ionization states are equivalent;\n10) Different salt forms are equivalent (e.g., sodium and potassium salts);\n11) Isotope variants are equivalent (unless specified);\n20) Inorganic and organic forms of the same compound are equivalent.\n\nFor lipid compounds, carefully check structural differences. For example, LPC(13:0) and LPC 18:1 should be considered different compounds due to different carbon chain lengths and saturation.\n\nAlways carefully compare oxidation states and structural positions!\nProstaglandin compounds (e.g., PGE2, PGD2) should be strictly distinguished based on their core structure and functional groups!",
                                   rows = 8),

                     helpText("This optional prompt will be sent to the AI."),

                     textInput("custom_api_url",
                              "Custom API URL (Optional)",
                              placeholder = "e.g., https://api.deepseek.com"),

                     textInput("custom_model_name",
                              "Custom Model Name (Optional) (Not Recommend Reasoner Models!!)",
                              placeholder = "e.g., deepseek-chat"),

                     passwordInput("apikey",
                              "Enter API Key",
                              placeholder = "Your personal API key"),

                     div(style = "margin-bottom: 20px;",
                         checkboxInput("clearCurrentModel",
                                     "Delete current model results",
                                     value = FALSE),
                         helpText("Check to clear all current model results before starting.")
                     ),

                     shiny::actionButton("runAI", "Start AI Identification", class = "btn-primary")
                 )
             ),
             column(9,
                 div(
                     style = "background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                     h4("AI Identification Results", style = "margin-bottom: 15px;"),
                     helpText("Review the AI-identified candidate compounds below. You can adjust the selection for each compound."),
                     DT::DTOutput("aiResultsTable"),
                     div(
                         style = "margin-top: 20px; text-align: right;",
                         shiny::actionButton("confirmAIResults",
                                    "Confirm Selection and Continue",
                                    class = "btn-success btn-lg")
                     )
                 ),
                 div(
                     style = "margin-top: 20px; background: white; padding: 20px; border-radius: 10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                     h4("AI Identification Log", style = "margin-bottom: 15px;"),
                     div(
                         id = "aiConsole",
                         style = "height: 200px; overflow-y: auto; background: #000000; padding: 10px; border-radius: 5px;",
                         ""
                     )
                 )
             )
         )
    ),
    tabPanel("3. Source Check (Endogenous)",
         fluidRow(
             column(12,
                 div(
                     style = "background: #fff3cd; color: #856404; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                     icon("exclamation-triangle"),
                     "Warning: HMDB website connection may be unstable. Please use HMDB endogenous check with caution."
                 )
             )
         ),
         fluidRow(
             column(12,
                 wellPanel(
                     h4("Load Local Final Results"),
                     p("Load the final compound results saved locally."),
                     shiny::actionButton("loadFinalResults", "Load Local Final Results",
                                          class = "btn-info", style = "width: 100%;")
                 )
             )
         ),
         fluidRow(
             column(6,
                 wellPanel(
                     h4("HMDB Endogenous Check"),
                     p("Check endogenous status using HMDB database."),
                     shiny::actionButton("runHMDBCheck", "Start HMDB Check",
                                    class = "btn-warning",
                                    style = "width: 100%;")
                 )
             ),
             column(6,
                 wellPanel(
                     h4("mzCloud Endogenous Check"),
                     p("Check endogenous status using mzCloud database."),
                     shiny::actionButton("runMzCloudCheck", "Start mzCloud Check",
                                    class = "btn-info",
                                    style = "width: 100%;")
                 )
             )
         ),
         fluidRow(
             column(12,
                 div(
                     style = "margin-top: 20px;",
                     h4("Check Progress"),
                     div(
                         id = "endogenousConsole",
                         style = "height: 300px; overflow-y: auto; background: #000000; padding: 10px; border-radius: 5px;",
                         ""
                     ),
                     div(
                         style = "margin-top: 20px; text-align: right;",
                         shiny::actionButton("confirmEndogenous",
                                    "Confirm and Continue",
                                    class = "btn-success btn-lg")
                     )
                 )
             )
         )
    ),
    tabPanel("4. Processed Results View",
         fluidPage(
             h4("Final Processed Results", style = "margin-bottom: 15px;"),
             helpText("Review the final processed compound results. If you are satisfied with them, click 'Next' to proceed to KEGG Database & Processing."),
             div(
                 style = "margin-bottom: 20px;",
                 shiny::actionButton("refreshResults",
                            "Refresh Results",
                            icon = icon("sync"),
                            class = "btn-info")
             ),
             div(
                 style = "margin-bottom: 10px;",
                 tags$em("Last updated: "),
                 textOutput("lastUpdateTime", inline = TRUE)
             ),
             DT::DTOutput("finalResultsTable"),
             div(
                 style = "margin-top: 20px; text-align: right;",
                 shiny::actionButton("nextFromProcessed", "Next", class = "btn-success btn-lg")
             )
         )
    ),
    tabPanel("5. External Database Annotation",
        shiny::sidebarLayout(
            sidebarPanel(
                div(style = "margin-bottom: 20px;",
                    shiny::actionButton("loadKeggLocalResults", "Load Local Results",
                               class = "btn-info",
                               style = "width: 100%;"),
                    helpText("Load previously saved KEGG analysis results if available.")
                ),

                selectizeInput(
                    "keggSpecies",
                    "Select Species",
                    choices = NULL,
                    selected = NULL,
                    options = list(
                        placeholder = 'Select a species',
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                ),

                radioButtons("pathwayLevel",
                           "Select Pathway Analysis Level:",
                           choices = c(
                               "Pathway Name (81 KEGG pathways)" = "pathway_name",
                               "Pathway Class (13 pathway categories)" = "pathway_class"
                           ),
                           selected = "pathway_name"),

                div(
                    id = "speciesLoadingIndicator",
                    style = "display: none;",
                    tags$span(class = "loading-spinner"),
                    "Loading species data..."
                ),

                helpText("Select the species and pathway analysis level then click 'Start external database annotation'."),

                shiny::actionButton("runKegg", "Start external database annotation", class = "btn-primary"),
                shiny::actionButton("copyCID", "Copy CID", class = "btn-secondary", style = "width: 100%; margin-top: 10px;"),
                shiny::actionButton("copyKEGGHMDB", "Copy KEGG ID + HMDB ID)", class = "btn-secondary", style = "width: 100%; margin-top: 5px;"),
                tags$div(
                  tags$a(href = "https://csbg.cnb.csic.es/mbrole3/analysis.php", target = "_blank", "MBROLE3"),
                  tags$br(),
                  tags$a(href = "https://www.metaboanalyst.ca/MetaboAnalyst/upload/EnrichUploadView.xhtml", target = "_blank", "MetaboAnalyst")
                )
            ),
            mainPanel(
                h4("External Database Annotation Progress", style = "margin-bottom: 15px;"),
                div(
                    id = "keggConsole",
                    style = "height:300px; overflow-y:auto; background: #000000;",
                    ""
                ),
                h4("Pathway Enrichment Results", style = "margin-top: 20px;"),
                downloadButton("downloadKEGGBubblePlot", "Download Bubble Plot (PPTX)", class = "btn-success"),
                plotOutput("pathwayBubblePlot", height = "600px")
            )
        )
    ),
    tabPanel("6. Report Download",
         fluidPage(
             h4("Report Download", style = "margin-top: 20px;"),
             p("Click the button below to generate a Processing Report (PDF format), which contains the processing and results of each section, so that you can easily understand how the system works."),
             downloadButton("downloadReport", "Generate Processing Report", class = "btn-success")
         )
    )
)

#' Shiny Server Function
#'
#' The server logic for the Shiny app.
#'
#' @keywords internal
server <- function(input, output, session) {
    # Add: Record processing statistics (time, compound count)
    report_info <- shiny::reactiveValues(
        search_start = NULL,
        search_end = NULL,
        ai_start = NULL,
        ai_end = NULL,
        total_compounds = 0
    )

    # Add: Store status for each step
    step_status <- shiny::reactiveValues(
        search_complete = FALSE,
        ai_complete = FALSE,
        kegg_complete = FALSE
    )

    # Add reactive values to store AI results and compound results
    ai_results <- shiny::reactiveVal(NULL)
    compound_results <- shiny::reactiveVal(NULL)
    enrichment_results <- shiny::reactiveVal(NULL)

    # Initialize species data
    species_data <- shiny::reactiveVal(NULL)

    # Load species data when app starts
    shiny::observe({
        tryCatch({
            # Get organism list from KEGG
            org <- as.data.frame(KEGGREST::keggList('organism'))[,c(2:3)]
            species_data(org)

            # Update selectizeInput with server-side data
            updateSelectizeInput(
                session,
                "keggSpecies",
                choices = setNames(org$organism, org$species),
                selected = org$organism[1],  # Default to first species
                server = TRUE,  # Enable server-side processing
                options = list(
                    placeholder = 'Type to search species...',
                    maxOptions = 200,
                    searchField = c("value", "label"),
                    render = I("{
                        option: function(item, escape) {
                            return '<div>' + escape(item.label) + '</div>';
                        }
                    }")
                )
            )

            # Hide loading indicator
            shinyjs::hide("speciesLoadingIndicator")

        }, error = function(e) {
            log_message(sprintf("Error loading species data: %s", e$message), tag = "ERROR")
            showNotification("Failed to load species data", type = "error")
        })
    })

    # Compound Auto-search
    shiny::observeEvent(input$runSearch, {
        req(input$compounds)
        # Record the start time of the first step
        report_info$search_start <- Sys.time()

        # Start loading animation: Add loading state
        shinyjs::addClass("runSearch", "btn-loading")
        disable("runSearch")

        withConsoleRedirect("searchConsole", {
            tryCatch({
                compounds <- strsplit(input$compounds, "\n")[[1]]
                compounds <- trimws(compounds[nzchar(compounds)])
                compounds <- unique(compounds)

                if (length(compounds) == 0) {
                    stop("Please enter at least one valid compound name")
                }
                report_info$total_compounds <- length(compounds)

                results <- process_compounds(
                  compounds,
                  process_dir = APP_PATHS$process,
                  clear_cache = input$clearCache
                )

                if (!is.null(results) && length(results) > 0) {
                    step_status$search_complete <- TRUE
                    shiny::updateTabsetPanel(session, "mainTabs", selected = "2. AI-assisted Identification")
                }
            }, error = function(e) {
                insertUI(
                    "#searchConsole",
                    where = "beforeEnd",
                    ui = HTML(paste0(
                        "<span class='console-error'>",
                        format(Sys.time(), "[%H:%M:%S]"),
                        "[ERROR]",
                        e$message,
                        "</span><br/>"
                    ))
                )
            }, finally = {
                enable("runSearch")
                shinyjs::removeClass("runSearch", "btn-loading")
                report_info$search_end <- Sys.time()
                step1_info <- list(
                    start = report_info$search_start,
                    end = report_info$search_end,
                    duration = as.numeric(difftime(report_info$search_end, report_info$search_start, units = "secs"))
                )
                saveRDS(step1_info, file = file.path(APP_PATHS$process, "step1_info.rds"))
                saveRDS(report_info$total_compounds, file = file.path(APP_PATHS$process, "total_compounds.rds"))
            })
        })
    })

    # AI-assisted Identification
    shiny::observeEvent(input$runAI, {
        req(input$apikey)
        if (!step_status$search_complete) {
            showNotification("Please complete compound search step first", type = "warning")
            return()
        }

        report_info$ai_start <- Sys.time()

        shinyjs::addClass("runAI", "btn-loading")
        disable("runAI")

        withConsoleRedirect("aiConsole", {
            tryCatch({
                if (input$clearCurrentModel) {
                    clean_ai_logs(APP_PATHS$process, input$model)
                    log_message(sprintf("Cleared history for model %s", input$model))
                }

                match_table_path <- file.path(APP_PATHS$process, "match_table.csv")
                results_path <- file.path(APP_PATHS$process, "all_results.rds")

                if (!file.exists(match_table_path)) {
                    stop("Cannot find compound match table file, please complete compound search step first")
                }
                if (!file.exists(results_path)) {
                    stop("Cannot find search results file, please complete compound search step first")
                }

                match_table <- read.csv(match_table_path)
                results <- readRDS(results_path)

                ai_process_cpd_names <- match_table |>
                    dplyr::filter(exact_match != 'TRUE') |>
                    dplyr::pull(compound_name)

                if (length(ai_process_cpd_names) == 0) {
                    message("No compounds requiring AI identification")
                    step_status$ai_complete <- TRUE
                    # 为了防止后续操作报错，设置空的 AI identification 结果数据框
                    ai_results(data.frame(
                        compound_name = '',
                        result1 = '',
                        result2 = '',
                        result3 = '',
                        selected_result = '',
                        KEGG_ID = '',
                        HMDB_ID = '',
                        CHEBI_ID = '',
                        MolecularFormula = '',
                        MolecularWeight = '',
                        CTD = '',
                        Similarity = '',
                        stringsAsFactors = FALSE
                    ))

                    # --- 新增代码：生成 final_compound_results.csv ---
                    match_table_path <- file.path(APP_PATHS$process, "match_table.csv")
                    results_path <- file.path(APP_PATHS$process, "all_results.rds")
                    if (!file.exists(match_table_path)) {
                        stop("Cannot find compound match table file, please complete compound search step first")
                    }
                    if (!file.exists(results_path)) {
                        stop("Cannot find search results file, please complete compound search step first")
                    }
                    match_table <- read.csv(match_table_path)
                    results <- readRDS(results_path)

                    final_results <- data.frame(
                        compound_name = match_table$compound_name,
                        matched_name = NA_character_,
                        final_result = NA_character_,
                        CID = NA_integer_,
                        IUPACName = NA_character_,
                        MolecularFormula = NA_character_,
                        MolecularWeight = NA_real_,
                        KEGG_ID = NA_character_,
                        HMDB_ID = NA_character_,
                        CHEBI_ID = NA_character_,
                        CTD = NA_character_,
                        Similarity = NA_real_,
                        stringsAsFactors = FALSE
                    )

                    # 处理 exact 匹配的化合物
                    exact_matches <- match_table$compound_name[match_table$exact_match == 'TRUE']
                    for (cpd in exact_matches) {
                        idx <- which(final_results$compound_name == cpd)
                        if (length(idx) > 0 && !is.null(results[[cpd]]$all_results) && nrow(results[[cpd]]$all_results) > 0) {
                            result_row <- results[[cpd]]$all_results[1, , drop = FALSE]
                            final_results[idx, ] <- list(
                                compound_name = cpd,
                                matched_name = result_row$SearchName[1],
                                final_result = "TRUE",
                                CID = as.integer(result_row$CID[1]),
                                IUPACName = as.character(result_row$IUPACName[1]),
                                MolecularFormula = as.character(result_row$MolecularFormula[1]),
                                MolecularWeight = as.numeric(result_row$MolecularWeight[1]),
                                KEGG_ID = as.character(result_row$KEGG_ID[1]),
                                HMDB_ID = as.character(result_row$HMDB_ID[1]),
                                CHEBI_ID = as.character(result_row$CHEBI_ID[1]),
                                CTD = as.character(result_row$CTD[1]),
                                Similarity = 1
                            )
                        }
                    }

                    final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
                    write.csv(final_results, final_results_path, row.names = FALSE)
                    log_message(sprintf("No AI identification needed: final compound results saved to: %s", final_results_path))
                    # --- 新增代码结束 ---

                    shiny::updateTabsetPanel(session, "mainTabs", selected = "3. Source Check (Endogenous)")
                    showNotification("No AI identification needed, please do the endogenous check step", type = "message")
                    return()
                }

                message(sprintf("Found %d compounds requiring AI identification", length(ai_process_cpd_names)))

                api_config <- list(
                    base_url = if (!is.null(input$custom_api_url) && nchar(trimws(input$custom_api_url)) > 0) {
                        trimws(input$custom_api_url)
                    } else switch(input$model,
                        "deepseek" = "https://api.deepseek.com",
                        "gpt-4o-mini" = "https://api.chatfire.cn/v1"
                    ),
                    model = if (!is.null(input$custom_model_name) && nchar(trimws(input$custom_model_name))) {
                        trimws(input$custom_model_name)
                    } else switch(input$model,
                        "deepseek" = "deepseek-chat",
                        "gpt-4o-mini" = "gpt-4o-mini"
                    ),
                    api_key = input$apikey,
                    prompt = input$ai_prompt  # <-- new element with user prompt
                )

                message(sprintf("Using AI model: %s", api_config$model))
                message(sprintf("API URL: %s", api_config$base_url))
                message("Testing AI model connection...")
                chat <- chat_openai(
                  base_url = api_config$base_url,
                  model = api_config$model,
                  api_key = api_config$api_key,
                  echo = "text"
                )
                test_response <- chat$chat("What model are you?")
                message(sprintf("AI model connection successful! Model response: %s", test_response))

                message("Starting to process compounds requiring identification...")
                all_identified_compounds <- process_all_compounds(
                    ai_process_cpd_names,
                    api_config,
                    results = results,
                    process_dir = APP_PATHS$process,
                    model_name = input$model
                )

                all_results_df <- data.frame(
                    compound_name = names(all_identified_compounds),
                    result1 = sapply(all_identified_compounds, function(x) x[[1]]),
                    result2 = sapply(all_identified_compounds, function(x) x[[2]]),
                    result3 = sapply(all_identified_compounds, function(x) x[[3]]),
                    selected_result = sapply(all_identified_compounds, function(x) x[[1]])
                )

                compound_details <- lapply(all_results_df$compound_name, function(cpd_name) {
                    if (!is.null(results[[cpd_name]]) && !is.null(results[[cpd_name]]$all_results)) {
                        result_row <- results[[cpd_name]]$all_results[1, ]
                        return(data.frame(
                            compound_name = cpd_name,
                            KEGG_ID = ifelse(!is.null(result_row$KEGG_ID), result_row$KEGG_ID, NA),
                            HMDB_ID = ifelse(!is.null(result_row$HMDB_ID), result_row$HMDB_ID, NA),
                            CHEBI_ID = ifelse(!is.null(result_row$CHEBI_ID), result_row$CHEBI_ID, NA),
                            MolecularFormula = ifelse(!is.null(result_row$MolecularFormula), result_row$MolecularFormula, NA),
                            MolecularWeight = ifelse(!is.null(result_row$MolecularWeight), result_row$MolecularWeight, NA),
                            CTD = ifelse(!is.null(result_row$CTD), result_row$CTD, NA),
                            Similarity = ifelse(!is.null(result_row$Similarity), result_row$Similarity, NA)
                        ))
                    } else {
                        return(data.frame(
                            compound_name = cpd_name,
                            KEGG_ID = NA,
                            HMDB_ID = NA,
                            CHEBI_ID = NA,
                            MolecularFormula = NA,
                            MolecularWeight = NA,
                            CTD = NA,
                            Similarity = NA
                        ))
                    }
                }) %>% dplyr::bind_rows()

                all_results_df <- all_results_df %>%
                    dplyr::left_join(compound_details, by = "compound_name")

                ai_results(all_results_df)

                ai_results_base <- file.path(APP_PATHS$process, 'AI_identified_cleaned')
                ai_results_path <- sprintf("%s_%s.csv", ai_results_base, input$model)
                write.csv(all_results_df, ai_results_path, row.names = FALSE)

                latest_results_path <- file.path(APP_PATHS$process, 'AI_identified_cleaned_latest.csv')
                file.copy(ai_results_path, latest_results_path, overwrite = TRUE)

                log_message(sprintf("AI identification results saved to: %s", ai_results_path))
                log_message("Latest results file also updated")

                showNotification("AI identification complete, please check results", type = "message")

            }, error = function(e) {
                showNotification(sprintf("Error: %s", e$message), type = "error")
                log_message(e$message, tag = "ERROR")
            }, finally = {
                enable("runAI")
                shinyjs::removeClass("runAI", "btn-loading")
                report_info$ai_end <- Sys.time()
                step2_info <- list(
                    start = report_info$ai_start,
                    end = report_info$ai_end,
                    duration = as.numeric(difftime(report_info$ai_end, report_info$ai_start, units = "secs"))
                )
                saveRDS(step2_info, file = file.path(APP_PATHS$process, "step2_info.rds"))
            })
        })
    })

    output$aiResultsTable <- DT::renderDT({
      if (is.null(ai_results())) {
        log_message("No AI results available, returning empty table")
        return(DT::datatable(
          data.frame(
            `Compound Name` = character(),
            `First Result` = character(),
            `First Details` = character(),
            `Second Result` = character(),
            `Second Details` = character(),
            `Third Result` = character(),
            `Third Details` = character(),
            `Selected Result` = character(),
            stringsAsFactors = FALSE
          ),
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip'
          )
        ))
      }

      df <- ai_results()
      results <- compound_results()

      log_message("Starting to process AI results table")
      log_message(sprintf("Number of compounds in AI results: %d", nrow(df)))
      log_message(sprintf("Available columns in AI results: %s",
                          paste(colnames(df), collapse = ", ")))

      req(results)

      log_message("Checking and initializing result columns...")

      if (!"result1" %in% names(df)) {
        log_message("Warning: result1 column missing")
        df$result1 <- NA
      }
      if (!"result2" %in% names(df)) {
        log_message("Adding missing result2 column")
        df$result2 <- NA
      }
      if (!"result3" %in% names(df)) {
        log_message("Adding missing result3 column")
        df$result3 <- NA
      }
      if (!"selection" %in% names(df)) {
        log_message("Adding missing selection column")
        df$selection <- df$result1
      }

      log_message("Checking data in each column:")
      log_message(sprintf("result1 values: %s",
                          paste(head(df$result1, 3), collapse = ", ")))
      log_message(sprintf("result2 values: %s",
                          paste(head(df$result2, 3), collapse = ", ")))
      log_message(sprintf("result3 values: %s",
                          paste(head(df$result3, 3), collapse = ", ")))

      log_message("Creating table data...")

      tryCatch({
        table_data <- data.frame(
          `Compound Name` = df$compound_name,
          `First Result` = ifelse(is.na(df$result1), "FALSE", df$result1),
          `First Details` = sapply(seq_len(nrow(df)), function(i) {
            if (is.na(df$result1[i]) || df$result1[i] == "FALSE") return("NA")
            get_compound_details(df, i, df$result1[i], results)
          }),
          `Second Result` = ifelse(is.na(df$result2), "FALSE", df$result2),
          `Second Details` = sapply(seq_len(nrow(df)), function(i) {
            if (is.na(df$result2[i]) || df$result2[i] == "FALSE") return("NA")
            get_compound_details(df, i, df$result2[i], results)
          }),
          `Third Result` = ifelse(is.na(df$result3), "FALSE", df$result3),
          `Third Details` = sapply(seq_len(nrow(df)), function(i) {
            if (is.na(df$result3[i]) || df$result3[i] == "FALSE") return("NA")
            get_compound_details(df, i, df$result3[i], results)
          }),
          `Selected Result` = ifelse(is.na(df$selection), df$result1, df$selection),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        log_message("Table data created successfully")
        log_message(sprintf("Number of rows in table_data: %d", nrow(table_data)))

        log_message("Creating selection options...")
        selection_options <- lapply(seq_len(nrow(df)), function(i) {
          options <- c(df$result1[i], df$result2[i], df$result3[i])
          options <- options[!is.na(options)]
          options <- unique(c(options, 'FALSE'))
          log_message(sprintf("Row %d options: %s", i,
                              paste(options, collapse = ", ")))
          return(options)
        })

        log_message("Creating datatable...")
        dt <- DT::datatable(
          table_data,
          options = list(
            pageLength = 25,
            scrollX = TRUE,
            dom = 'Bfrtip',
            columnDefs = list(
              list(
                targets = 8,
                render = DT::JS("
                          function(data, type, row, meta) {
                              if (type === 'display') {
                                  // Get all possible options from the results columns
                                  var options = [];
                                  // Row[1] is First Result, Row[3] is Second Result, Row[5] is Third Result
                                  if (row[2]) options.push(row[2]);
                                  if (row[4]) options.push(row[4]);
                                  if (row[6]) options.push(row[6]);
                                  
                                  // Always include 'FALSE' as an option
                                  options.push('FALSE');
                                  
                                  // Remove duplicates and empty values
                                  options = [...new Set(options.filter(opt => opt !== null && opt !== ''))];
                                  
                                  // Use the cell data or the value in our 'Selected Result' column
                                  var currentValue = data || row[7] || options[0];
                                  if (!options.includes(currentValue) && currentValue) {
                                      options.push(currentValue);
                                  }

                                  var select = '<select class=\"result-select\" data-row=\"' + meta.row + '\">';
                                  options.forEach(function(option) {
                                      var selected = (option === currentValue) ? ' selected' : '';
                                      select += '<option value=\"' + option + '\"' + selected + '>' + option + '</option>';
                                  });
                                  select += '</select>';
                                  return select;
                              }
                              return data;
                          }
                      ")
              ),
              list(
                targets = c(2, 4, 6),
                render = DT::JS("
                                function(data, type, row, meta) {
                                    if (type === 'display') {
                                        return data.replace(/\\n/g, '<br>');
                                    }
                                    return data;
                                }"
                )
              )
            ),
            drawCallback = DT::JS("
                        function(settings) {
                            var api = this.api();
                            api.rows().every(function(rowIdx) {
                                var data = this.data();
                                var $select = $(this.node()).find('.result-select');
                                if ($select.length) {
                                    $select.val(data[8]);
                                    $select.off('change').on('change', function() {
                                        var value = $(this).val();
                                        data[8] = value;
                                        api.row(rowIdx).data(data);
                                        // Passing selection changes to the server via Shiny
                                        Shiny.setInputValue('result_selection', { row: rowIdx, value: value }, {priority: 'event'});
                                    });
                                }
                            });
                        }
                    "),
            stateSave = TRUE,
            stateDuration = -1
          ),
          selection = 'none',
          escape = FALSE
        ) %>%
        DT::formatStyle("Compound Name", backgroundColor = "#f0f4f8") %>%
        DT::formatStyle("First Result", backgroundColor = "#f3f0f8") %>%
        DT::formatStyle("First Details", backgroundColor = "#f8f0f0") %>%
        DT::formatStyle("Second Result", backgroundColor = "#f0f8f4") %>%
        DT::formatStyle("Second Details", backgroundColor = "#f8f7f0") %>%
        DT::formatStyle("Third Result", backgroundColor = "#f0f5f8") %>%
        DT::formatStyle("Third Details", backgroundColor = "#f8f0f5") %>%
        DT::formatStyle("Selected Result", backgroundColor = "#f0f8f8")

        log_message("Datatable created successfully with column background colors")
        return(dt)

      }, error = function(e) {
        log_message(sprintf("Error creating table: %s", e$message), tag = "ERROR")
        log_message(sprintf("Error occurred at: %s", e$call), tag = "ERROR")
        stop(e)
      })
    })

    shiny::observeEvent(input$result_selection, {
       req(ai_results())

       df <- ai_results()
       row <- input$result_selection$row
       value <- input$result_selection$value

       log_message(sprintf("Updating selection for row %d to value: %s", row + 1, value))

       if (!"selection" %in% names(df)) {
           df$selection <- df$result1
       }

       df$selection[row + 1] <- value
       ai_results(df)

       ai_results_path <- file.path(APP_PATHS$process, sprintf("AI_identified_cleaned_%s.csv", input$model))
       write.csv(df, ai_results_path, row.names = FALSE)

       latest_results_path <- file.path(APP_PATHS$process, 'AI_identified_cleaned_latest.csv')
       file.copy(ai_results_path, latest_results_path, overwrite = TRUE)

       log_message(sprintf("Selection saved to file: %s", ai_results_path))
       showNotification("Selection updated and saved", duration = 5)
   })

    get_compound_details <- function(df, row_idx, result_name, results) {
      compound_name <- df$compound_name[row_idx]
      if (is.null(compound_name)) {
        log_message(sprintf("Row %d: compound name is NULL", row_idx))
        return("NA")
      }

      if (result_name == "FALSE") {
        return("NA")
      }

      kegg_id <- df$KEGG_ID[row_idx]
      hmdb_id <- df$HMDB_ID[row_idx]
      chebi_id <- df$CHEBI_ID[row_idx]
      ctd <- df$CTD[row_idx]
      mf <- df$MolecularFormula[row_idx]
      mw <- df$MolecularWeight[row_idx]

      kegg_id <- ifelse(kegg_id == "Not_Access", "NA", as.character(kegg_id))
      hmdb_id <- ifelse(hmdb_id == "Not_Access", "NA", as.character(hmdb_id))
      chebi_id <- ifelse(chebi_id == "Not_Access", "NA", as.character(chebi_id))
      ctd <- ifelse(ctd == "Not_Access", "NA", as.character(ctd))
      sprintf(
        "KEGG: %s\nHMDB: %s\nCHEBI: %s\nMF: %s\nMW: %s",
        kegg_id,
        hmdb_id,
        chebi_id,
        ctd,
        ifelse(is.na(mf), "NA", as.character(mf)),
        ifelse(is.na(mw), "NA", format(as.numeric(mw), digits = 2, nsmall = 2)))
    }


    shiny::observeEvent(input$confirmAIResults, {
        # Add loading animation, add loading style, and disable button
        shinyjs::addClass("confirmAIResults", "btn-loading")  # Add loading animation style
        disable("confirmAIResults")                           # Disable button

        showNotification("Confirming information and generating results table", type = "message", duration = 10)
        withConsoleRedirect("aiConsole", {
            tryCatch({
                ai_results_df <- ai_results()
                if (is.null(ai_results_df)) {
                    stop("No AI identification results available")
                }
                if (!"selection" %in% names(ai_results_df)) {
                    log_message("Initializing selection column with default result1")
                    ai_results_df$selection <- ai_results_df$result1
                }

                results <- compound_results()
                if (is.null(results)) {
                  tryCatch(
                    {
                      results <- readRDS(file.path(APP_PATHS$process, "all_results.rds"))
                    },
                    error = function(e) {
                      stop("Compound results not found")
                    }
                  )
                }

                match_table_path <- file.path(APP_PATHS$process, "match_table.csv")
                if (!file.exists(match_table_path)) {
                    stop("Match table file not found")
                }
                match_table <- read.csv(match_table_path)

                final_results <- data.frame(
                    compound_name = match_table$compound_name,
                    matched_name = NA_character_,
                    final_result = NA_character_,
                    CID = NA_integer_,
                    IUPACName = NA_character_,
                    MolecularFormula = NA_character_,
                    MolecularWeight = NA_real_,
                    KEGG_ID = NA_character_,
                    HMDB_ID = NA_character_,
                    CHEBI_ID = NA_character_,
                    CTD = NA_character_,
                    Similarity = NA_real_,
                    stringsAsFactors = FALSE
                )

                exact_matches <- match_table$compound_name[match_table$exact_match == 'TRUE']
                log_message(sprintf("Processing %d exact matches", length(exact_matches)))

                i <- 0
                for (cpd_name in exact_matches) {
                    i <- i + 1
                    idx <- which(final_results$compound_name == cpd_name)
                    if (length(idx) > 0 && !is.null(results[[cpd_name]]$all_results) && nrow(results[[cpd_name]]$all_results) > 0) {
                        result_row <- results[[cpd_name]]$all_results[1, , drop = FALSE]
                        final_results[idx, ] <- list(
                            compound_name    = cpd_name,
                            matched_name     = if(length(result_row$SearchName) > 0) result_row$SearchName[1] else NA_character_,
                            final_result     = "TRUE",
                            CID              = if(length(result_row$CID) > 0) as.integer(result_row$CID[1]) else NA_integer_,
                IUPACName        = if(length(result_row$IUPACName) > 0) as.character(result_row$IUPACName[1]) else NA_character_,
                MolecularFormula = if(length(result_row$MolecularFormula) > 0) as.character(result_row$MolecularFormula[1]) else NA_character_,
                MolecularWeight  = if(length(result_row$MolecularWeight) > 0) as.numeric(result_row$MolecularWeight[1]) else NA_real_,
                KEGG_ID          = if(length(result_row$KEGG_ID) > 0) as.character(result_row$KEGG_ID[1]) else NA_character_,
                HMDB_ID          = if(length(result_row$HMDB_ID) > 0) as.character(result_row$HMDB_ID[1]) else NA_character_,
                            CHEBI_ID         = if(length(result_row$CHEBI_ID) > 0) as.character(result_row$CHEBI_ID[1]) else NA_character_,
                            CTD              = if(length(result_row$CTD) > 0) as.character(result_row$CTD[1]) else NA_character_,
                            Similarity       = 1
                        )
                    }
                }

                ai_compounds <- match_table$compound_name[match_table$exact_match != 'TRUE']
                log_message(sprintf("Processing %d compounds requiring AI identification", length(ai_compounds)))

                j <- 0
                for (cpd_name in ai_compounds) {
                    j <- j + 1
                    idx <- which(final_results$compound_name == cpd_name)
                    if (length(idx) == 0) {
                        log_message(sprintf("Warning: Could not find index for AI compound: %s", cpd_name))
                        next
                    }
                    if (cpd_name %in% ai_results_df$compound_name) {
                        compound_row <- ai_results_df[ai_results_df$compound_name == cpd_name, ]
                        selected_name <- compound_row$selection
                        if (!is.null(selected_name) && selected_name != "FALSE" && selected_name != "" && !is.na(selected_name)) {
                            result_data <- results[[cpd_name]]$all_results
                            if (!is.null(result_data) && nrow(result_data) > 0) {
                                selected_row <- result_data[result_data$SearchName == selected_name, ]
                                if (nrow(selected_row) > 0) {
                                    selected_row <- selected_row[1, , drop = FALSE]
                                    final_results[idx, ] <- list(
                                        compound_name = cpd_name,
                                        matched_name = selected_name,
                                        final_result = "TRUE",
                                        CID = if(length(selected_row$CID) > 0) as.integer(selected_row$CID[1]) else NA_integer_,
                                        IUPACName = if(length(selected_row$IUPACName) > 0) as.character(selected_row$IUPACName[1]) else NA_character_,
                                        MolecularFormula = if(length(selected_row$MolecularFormula) > 0) as.character(selected_row$MolecularFormula[1]) else NA_character_,
                                        MolecularWeight = if(length(selected_row$MolecularWeight) > 0) as.numeric(selected_row$MolecularWeight[1]) else NA_real_,
                                        KEGG_ID = if(length(selected_row$KEGG_ID) > 0) as.character(selected_row$KEGG_ID[1]) else NA_character_,
                                        HMDB_ID = if(length(selected_row$HMDB_ID) > 0) as.character(selected_row$HMDB_ID[1]) else NA_character_,
                                        CHEBI_ID = if(length(selected_row$CHEBI_ID) > 0) as.character(selected_row$CHEBI_ID[1]) else NA_character_,
                                        CTD = if(length(selected_row$CTD) > 0) as.character(selected_row$CTD[1]) else NA_character_,
                                        Similarity = if(length(selected_row$Similarity) > 0) as.numeric(selected_row$Similarity[1]) else NA_real_
                                    )
                                    next
                                }
                            }
                        }
                    }
                    final_results[idx, "final_result"] <- "FALSE"
                }

                final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
                write.csv(final_results, final_results_path, row.names = FALSE)

                log_message(sprintf("Results saved to: %s", final_results_path))
                exact_match_compound_info <- final_results |> dplyr::filter(compound_name %in% exact_matches)
                exact_match_compound_info_path <- file.path(APP_PATHS$process, 'Step2-Compound_results(exact match).csv')
                write.csv(exact_match_compound_info, exact_match_compound_info_path, row.names = FALSE)
                log_message(sprintf("Exact Match Results saved to: %s", exact_match_compound_info_path))
                ai_match_compound_info <- final_results |> dplyr::filter(compound_name %in% ai_compounds)
                ai_match_compound_info_path <- file.path(APP_PATHS$process, 'Step2-Compound_results(AI-assisted match).csv')
                write.csv(ai_match_compound_info, ai_match_compound_info_path, row.names = FALSE)
                log_message(sprintf("AI-assisted Results saved to: %s", ai_match_compound_info_path))

                ## --- Add: Process addition state compound information ---
                tryCatch({
                    cid_parent_file <- file.path(APP_PATHS$data, "pubchem_info", "CID-Parent.fst")
                    cid_parent_file_last_update_time <- file.path(APP_PATHS$data, "pubchem_info", "cid_parent_lastest_update_time.rds")

                    # if cid_parent_file or cid_parent_file_last_update_time is not exist, then update the latest data, if the file exists and the last update time is more than 7 days, then update the latest data
                    if (!file.exists(cid_parent_file) || !file.exists(cid_parent_file_last_update_time)) {
                       update_pubchem_data()
                    } else {
                       last_update <- readRDS(cid_parent_file_last_update_time)
                       if (difftime(Sys.Date(), last_update, units = "days") > 7) {
                         update_pubchem_data()
                       }
                    }

                    if (file.exists(cid_parent_file)) {
                        if (!requireNamespace("fst", quietly = TRUE)) {
                            log_message("Package 'fst' is not available. Please install it to process addition state compounds.", tag = "ERROR")
                        } else {
                            cid_parent_data <- fst::read_fst(cid_parent_file)
                            if (!all(c("CID", "Parent") %in% names(cid_parent_data))) {
                                log_message("CID-Parent.fst file does not contain required columns 'CID' and 'Parent'.", tag = "ERROR")
                            } else {
                                idx_addition <- which(grepl("[-+]", final_results$MolecularFormula))
                                if (length(idx_addition) > 0) {
                                    compound_need_address_additionstate <- final_results$CID[idx_addition]
                                    for (i in idx_addition) {
                                        compound_cid <- final_results$CID[i]
                                        parent_info <- cid_parent_data[cid_parent_data$CID == compound_cid, ]
                                        if (nrow(parent_info) > 0) {
                                            parent_cid <- parent_info$Parent[1]
                                            search_url <- sprintf(
                                              "https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/%s/property/IUPACName,MolecularFormula,MolecularWeight,CanonicalSMILES,IsomericSMILES,InChI,InChIKey/JSON",
                                              parent_cid
                                            )
                                            response <- tryCatch({
                                                httr::GET(search_url)
                                            }, error = function(e) {
                                                log_message(sprintf("Error during GET request for parent CID %s: %s", parent_cid, e$message), tag = "ERROR")
                                                NULL
                                            })

                                            if (!is.null(response) && httr::status_code(response) == 200) {
                                                result_json <- tryCatch({
                                                    jsonlite::fromJSON(rawToChar(response$content))
                                                }, error = function(e) {
                                                    log_message(sprintf("Error parsing JSON for parent CID %s: %s", parent_cid, e$message), tag = "ERROR")
                                                    NULL
                                                })

                                                if (!is.null(result_json) &&
                                                    !is.null(result_json$PropertyTable) &&
                                                    !is.null(result_json$PropertyTable$Properties)) {
                                                    result_data <- as.data.frame(result_json$PropertyTable$Properties, stringsAsFactors = FALSE)
                                                    if (nrow(result_data) > 0) {
                                                        result_row <- result_data[1, , drop = FALSE]
                                                        # source(file.path(APP_PATHS$R,'pubchem_cpd_search.R'))
                                                        cid_kegg_info <- get_kegg_id(parent_cid)
                                                        final_results[i, "matched_name"] <- as.character(result_row$IUPACName[1])
                                                        final_results[i, "final_result"] <- "TRUE"
                                                        final_results[i, "CID"] <- as.integer(result_row$CID[1])
                                                        final_results[i, "IUPACName"] <- as.character(result_row$IUPACName[1])
                                                        final_results[i, "MolecularFormula"] <- as.character(result_row$MolecularFormula[1])
                                                        final_results[i, "MolecularWeight"] <- as.numeric(result_row$MolecularWeight[1])

                                                        final_results[i, "KEGG_ID"] <- if(length(cid_kegg_info$KEGG) > 0) as.character(cid_kegg_info$KEGG[1]) else 'Not_Access'
                                                        final_results[i, "HMDB_ID"] <- if(length(cid_kegg_info$HMDB) > 0) as.character(cid_kegg_info$HMDB[1]) else 'Not_Access'
                                                        final_results[i, "CHEBI_ID"] <- if(length(cid_kegg_info$CHEBI) > 0) as.character(cid_kegg_info$CHEBI[1]) else 'Not_Access'
                                                        final_results[i, "CTD"] <- if(length(cid_kegg_info$CTD) > 0) as.character(cid_kegg_info$CTD[1]) else 'Not_Access'
                                                        final_results[i, "Similarity"] <- 1
                                                        log_message(sprintf("Updated addition state info for compound CID %s using parent CID %s", compound_cid, parent_cid))
                                                        Sys.sleep(0.1)
                                                    }
                                                } else {
                                                    log_message(sprintf("HTTP request failed for parent CID %s", parent_cid), tag = "ERROR")
                                                }
                                            } else {
                                                log_message(sprintf("HTTP request failed for parent CID %s", parent_cid), tag = "ERROR")
                                            }
                                        } else {
                                            log_message(sprintf("No parent info found for compound CID %s in CID-Parent.fst", compound_cid), tag = "WARNING")
                                        }
                                    }

                                    # 对有CID但是HMDB ID没有查询到的化合物再执行一次Get_kegg_id
                                    temp_final_results_external_id <- final_results[!final_results$CID %in% c('Not_Access','NA'),]
                                    
                                    # 找出temp_final_results_external_id中HMDB_ID缺失的行
                                    rows_hmdb_not_found <- which(temp_final_results_external_id$HMDB_ID %in% c('Not_Access','NA'))
                                    
                                    if (length(rows_hmdb_not_found) > 0) {
                                        for(i in rows_hmdb_not_found){
                                            # 获取当前CID
                                            current_cid <- temp_final_results_external_id$CID[i]
                                            
                                            # 获取该CID在原始final_results中的索引位置
                                            original_idx <- which(final_results$CID == current_cid)
                                            
                                            # 使用get_kegg_id获取信息
                                            cid_kegg_info <- get_kegg_id(current_cid)
                                            
                                            # 更新原始final_results中对应行的信息
                                            final_results$HMDB_ID[original_idx] <- if(length(cid_kegg_info$HMDB) > 0) as.character(cid_kegg_info$HMDB[1]) else 'Not_Access'
                                            final_results$KEGG_ID[original_idx] <- if(length(cid_kegg_info$KEGG) > 0) as.character(cid_kegg_info$KEGG[1]) else 'Not_Access'
                                            final_results$CHEBI_ID[original_idx] <- if(length(cid_kegg_info$CHEBI) > 0) as.character(cid_kegg_info$CHEBI[1]) else 'Not_Access'
                                            final_results$CTD[original_idx] <- if(length(cid_kegg_info$CTD) > 0) as.character(cid_kegg_info$CTD[1]) else 'Not_Access'
                                        }
                                    }

                                    write.csv(final_results, final_results_path, row.names = FALSE)
                                    log_message("Final_results updated with addition state compounds info!")

                                    exact_match_compound_info <- final_results |> dplyr::filter(compound_name %in% exact_matches)
                                    exact_match_compound_info_path <- file.path(APP_PATHS$process, 'Step2-Compound_results(exact match).csv')
                                    write.csv(exact_match_compound_info, exact_match_compound_info_path, row.names = FALSE)
                                    log_message(sprintf("(Update_Addition_Statu)Exact Match Results saved to: %s", exact_match_compound_info_path))
                                    ai_match_compound_info <- final_results |> dplyr::filter(compound_name %in% ai_compounds)
                                    ai_match_compound_info_path <- file.path(APP_PATHS$process, 'Step2-Compound_results(AI-assisted match).csv')
                                    write.csv(ai_match_compound_info, ai_match_compound_info_path, row.names = FALSE)
                                    log_message(sprintf("(Update_Addition_Statu)AI-assisted Results saved to: %s", ai_match_compound_info_path))
                                } else {
                                    log_message("No compounds with addition state found in MolecularFormula")
                                }
                            }
                        }
                    }
                    ## --- Add: End of new part ---

                    step_status$ai_complete <- TRUE
                    shiny::updateTabsetPanel(session, "mainTabs", selected = "3. Source Check (Endogenous)")

                    showNotification("Results saved successfully", type = "message")

                }, error = function(e) {
                    log_message(sprintf("Error processing results: %s", e$message), tag = "ERROR")
                    showNotification(sprintf("Error: %s", e$message), type = "error")
                    }, finally = {
                        # Restore button status after processing
                        enable("confirmAIResults")
                        shinyjs::removeClass("confirmAIResults", "btn-loading")
                    })
                })
        })
    })

    shiny::observeEvent(input$mainTabs, {
        if (input$mainTabs == "2. AI-assisted Identification" && is.null(compound_results())) {
            withConsoleRedirect("aiConsole", {
                tryCatch({
                    match_table <- read.csv(file.path(APP_PATHS$process, "match_table.csv"))
                    results <- readRDS(file.path(APP_PATHS$process, "all_results.rds"))

                    compound_results(results)

                    ai_process_cpd_names <- match_table |>
                        dplyr::filter(exact_match != 'TRUE') |>
                        dplyr::pull(compound_name)

                    log_message(sprintf("Loaded %d compound data", nrow(match_table)))

                    step_status$search_complete <- TRUE

                    showNotification("Successfully loaded compound results", type = "message")

                }, error = function(e) {
                    showNotification("Failed to load compound results. Please complete step 1 first or use 'Load Local Results'",
                                   type = "warning")
                    log_message(sprintf("Error: %s", e$message), tag = "ERROR")
                })
            })
        }
    })

    shiny::observeEvent(input$loadLocalResults, {
        if (!is.null(compound_results())) {
            showNotification("Results already loaded", type = "message")
            return()
        }

        withConsoleRedirect("aiConsole", {
            tryCatch({
                match_table <- read.csv(file.path(APP_PATHS$process, "match_table.csv"))
                results <- readRDS(file.path(APP_PATHS$process, "all_results.rds"))

                compound_results(results)

                ai_process_cpd_names <- match_table |>
                    dplyr::filter(exact_match != 'TRUE') |>
                    dplyr::pull(compound_name)

                log_message(sprintf("Loaded %d compound data", nrow(match_table)))

                step_status$search_complete <- TRUE

                showNotification("Successfully loaded local results")

            }, error = function(e) {
                showNotification("Failed to load local results", type = "error")
                log_message(sprintf("Error: %s", e$message), tag = "ERROR")
            })
        })
    })

    kegg_compound_data <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$loadKeggLocalResults, {
        if (!is.null(kegg_compound_data())) {
            showNotification("Results already loaded", type = "message")
            return()
        }

        withConsoleRedirect("keggConsole", {
            tryCatch({
                final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
                if (!file.exists(final_results_path)) {
                    stop("Cannot find final_compound_results.csv. Please complete step 2 first.")
                }

                final_results <- read.csv(final_results_path)
                kegg_compound_data(final_results)

                valid_kegg_ids <- sum(!is.na(final_results$KEGG_ID) & final_results$KEGG_ID != "")

                log_message(sprintf("Successfully loaded compound data: %d total compounds, %d with KEGG IDs",
                                  nrow(final_results),
                                  valid_kegg_ids))

                step_status$ai_complete <- TRUE

                showNotification(sprintf("Successfully loaded %d compounds", nrow(final_results)),
                               type = "message")

            }, error = function(e) {
                showNotification(sprintf("Error loading results: %s", e$message),
                               type = "error")
                log_message(sprintf("Error: %s", e$message), tag = "ERROR")
            })
        })
    })

    shiny::observeEvent(input$runKegg, {
        req(input$keggSpecies)

        if (is.null(kegg_compound_data())) {
            showNotification("Please load compound results first", type = "warning")
            return()
        }

        shinyjs::addClass("runKegg", "btn-loading")
        disable("runKegg")

        withConsoleRedirect("keggConsole", {
            tryCatch({
                organism <- input$keggSpecies
                log_message(sprintf("Processing KEGG data for organism: %s", organism))

                final_results <- kegg_compound_data()
                compounds_with_kegg <- final_results[!is.na(final_results$KEGG_ID) & final_results$KEGG_ID != "", ]

                log_message(sprintf("Found %d compounds with KEGG IDs out of %d total compounds",
                                  nrow(compounds_with_kegg),
                                  nrow(final_results)))

                kegg_dir <- file.path(APP_PATHS$data, "KEGG")
                rds_path <- file.path(kegg_dir, paste0('KEGG_pathways_compound_', organism, '.rds'))
                kegg_organism_last_update_time_path <- file.path(kegg_dir, paste0('kegg_organism_last_update_time_', organism,'.rds'))

                pathway_data <- if (!file.exists(rds_path) || !file.exists(kegg_organism_last_update_time_path)) {
                  log_message(sprintf('start download latest kegg files...'))
                    process_and_save_species(organism)
                } else {
                  kegg_organism_last_update_time_path <- file.path(kegg_dir, paste0('kegg_organism_last_update_time_', organism,'.rds'))
                  kegg_organism_last_update_time <- readRDS(kegg_organism_last_update_time_path)
                  current_time <- Sys.Date()
                  kegg_date_diff <- current_time - kegg_organism_last_update_time
                  if (kegg_date_diff > 14){
                    log_message(sprintf('start download latest kegg files(>14days)...'))
                    process_and_save_species(organism)
                  } else {
                    tryCatch({
                      data <- readRDS(rds_path)
                      showNotification("KEGG data is latest, loaded local files!")
                      data
                    }, error = function(e) {
                      log_message(sprintf("Error reading existing KEGG data: %s", e$message))
                      process_and_save_species(organism)
                    })
                  }

                }

                if (is.null(pathway_data)) {
                    stop("Failed to load or process KEGG pathway data")
                }

                results <- perform_pathway_enrichment(
                    compounds_with_kegg$KEGG_ID,
                    pathway_data,
                    analysis_level = input$pathwayLevel
                )

                enrichment_results(results)

                enrichment_results_path <- file.path(APP_PATHS$process, 'pathway_enrichment_results.csv')
                write.csv(results, enrichment_results_path, row.names = FALSE)

                final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')

                
                
                chem_gene_path <- file.path(APP_PATHS$data, 'ctd_info', 'chem-gene_data.fst')
                chem_go_path <- file.path(APP_PATHS$data, 'ctd_info', 'chem-go_data.fst')
                chem_phenotype_path <- file.path(APP_PATHS$data, 'ctd_info', 'chem-phenotype_data.fst')
                reactome_path <- file.path(APP_PATHS$data, 'reactome_info', 'reactome_chebi_mapping.fst')
                if (file.exists(final_results_path)) {  
                  final_results <- read.csv(final_results_path)
                  final_results_endogenous <- read.csv(file.path(APP_PATHS$process,'final_compound_results_with_endogenous.csv')) |> 
                    dplyr::rename(Compound_Name = compound_name) |> 
                    dplyr::select(-c(matched_name,final_result))
                  write.csv(final_results_endogenous, file.path(APP_PATHS$process, '0.MetaboliteAnnotator_Results.csv'), row.names = FALSE)
                  
                  final_results_kegg <- base::merge(final_results, pathway_data |> dplyr::select(-c(compound_name)), by.x = 'KEGG_ID', by.y = 'compound_KEGG_ID',all.x = T) |> 
                    dplyr::select(compound_name, CID, KEGG_ID, pathway_name, pathway_id) |> 
                    dplyr::rename(Compound_Name = compound_name,
                                  KEGG_Pathway_Name = pathway_name,
                                  KEGG_Pathway_ID = pathway_id)
                  
                  write.csv(final_results_kegg, file.path(APP_PATHS$process, '1.MetaboliteAnnotator_Results_KEGG.csv'), row.names = FALSE)
        
                  if(file.exists(chem_gene_path) && file.exists(chem_go_path) && file.exists(chem_phenotype_path) && file.exists(reactome_path)){
                    chem_gene_data <- fst::read_fst(chem_gene_path)
                    final_results_Gene <- base::merge(final_results, chem_gene_data, by.x = 'CTD', by.y = 'ChemicalID',all.x = T) |> 
                      dplyr::select(compound_name, CID, CTD,GeneSymbol, GeneForms, Interaction, InteractionActions, PubMedIDs) |> 
                      dplyr::rename(Compound_Name = compound_name,
                                    Chemical_Gene_Interaction = Interaction, 
                                    Chemical_Gene_InteractionActions = InteractionActions, 
                                    Chemical_Gene_PubMedIDs = PubMedIDs
                                    )
                    final_results_Gene$ctd_link <- ifelse(
                      !is.na(final_results_Gene$CTD) & final_results_Gene$CTD != "Not_Access",
                      paste0('https://ctdbase.org/detail.go?type=chem&acc=', final_results_Gene$CTD),
                      NA
                    )
                    rm(chem_gene_data)
                    gc()
                    write.csv(final_results_Gene, file.path(APP_PATHS$process, '2.MetaboliteAnnotator_Results_Gene.csv'), row.names = FALSE)
                    
                    chem_go_data <- fst::read_fst(chem_go_path)
                    final_results_GO <- base::merge(final_results, chem_go_data, by.x = 'CTD', by.y = 'ChemicalID',all.x = T) |> 
                      dplyr::select(compound_name, CID, CTD, Ontology, GOTermName, GOTermID, HighestGOLevel) |> 
                      dplyr::rename(Compound_Name = compound_name
                      )
                    final_results_GO$ctd_link <- ifelse(
                      !is.na(final_results_GO$CTD) & final_results_GO$CTD != "Not_Access",
                      paste0('https://ctdbase.org/detail.go?type=chem&acc=', final_results_GO$CTD),
                      NA
                    )
                    rm(chem_go_data)
                    gc()
                    write.csv(final_results_GO, file.path(APP_PATHS$process, '3.MetaboliteAnnotator_Results_GO.csv'), row.names = FALSE)
                    
                    chem_phenotype_data <- fst::read_fst(chem_phenotype_path)
                    final_results_phenptype <- base::merge(final_results, chem_phenotype_data, by.x = 'CTD', by.y = 'ChemicalID',all.x = T) |> 
                      dplyr::select(compound_name, CID, CTD, phenotypename, interaction, interactions, pubmedids) |> 
                      dplyr::rename(Compound_Name = compound_name,
                                    Chemical_Phenotype_Interaction = interaction, 
                                    Chemical_Phenotype_InteractionActions = interactions, 
                                    Chemical_Phenotype_PubMedIDs = pubmedids
                      )
                    final_results_phenptype$ctd_link <- ifelse(
                      !is.na(final_results_phenptype$CTD) & final_results_phenptype$CTD != "Not_Access",
                      paste0('https://ctdbase.org/detail.go?type=chem&acc=', final_results_phenptype$CTD),
                      NA
                    )
                    rm(chem_phenotype_data)
                    gc()
                    write.csv(final_results_phenptype, file.path(APP_PATHS$process, '4.MetaboliteAnnotator_Results_Phenotype.csv'), row.names = FALSE)
                    

                    
                    reactome_data <- fst::read_fst(reactome_path)
                    final_results_reactome <- base::merge(final_results, reactome_data, by.x = 'CHEBI_ID', by.y = 'CHEBI_ID',all.x = T) |> 
                      dplyr::select(compound_name, CID, Reactome_ID, Reactome_Link, Reaction) |> 
                      dplyr::rename(Compound_Name = compound_name)
                    rm(reactome_data)
                    gc()
                    write.csv(final_results_reactome, file.path(APP_PATHS$process, '5.MetaboliteAnnotator_Results_Reactome.csv'), row.names = FALSE)
                  }
                }

                log_message(sprintf("Found %d enriched pathways", nrow(results)))
            }, error = function(e) {
                log_message(sprintf("Error in KEGG analysis: %s", e$message), tag = "ERROR")
                showNotification("Error processing KEGG data", type = "error")
            }, finally = {
                enable("runKegg")
                shinyjs::removeClass("runKegg", "btn-loading")
            })
        })
    })

    output$pathwayBubblePlot <- shiny::renderPlot({
        req(enrichment_results())
        create_pathway_bubble_plot(enrichment_results())
    })

    shiny::observe({
        lapply(c("searchConsole", "aiConsole", "keggConsole"), function(consoleId) {
            session$sendCustomMessage(type = "setupConsoleScroll", list(
                consoleId = consoleId
            ))
        })
    })

    last_update_time <- shiny::reactiveVal(NULL)

    merge_and_format_results <- function() {
        final_results <- read.csv(file.path(APP_PATHS$process, 'final_compound_results.csv'))

        hmdb_path <- file.path(APP_PATHS$process, 'final_compound_results_with_hmdb.csv')
        if (file.exists(hmdb_path)) {
            hmdb_results <- read.csv(hmdb_path)
            final_results$HMDB_endogenous <- hmdb_results$HMDB_endogenous
        }

        mzcloud_path <- file.path(APP_PATHS$process, 'final_compound_results_with_mzcloud.csv')
        if (file.exists(mzcloud_path)) {
            mzcloud_results <- read.csv(mzcloud_path)
            final_results$mzCloud_endogenous <- mzcloud_results$mzCloud_endogenous
        }

        if ("HMDB_endogenous" %in% names(final_results)) {
            final_results$HMDB_endogenous <- ifelse(is.na(final_results$HMDB_endogenous),
                                                      "Unknown",
                                                      ifelse(final_results$HMDB_endogenous, "Yes", "No"))
        }

        final_results$endogenous_status <- "Unknown"
        final_results$endogenous_status <- sapply(1:nrow(final_results), function(i) {
            hmdb_status <- final_results$HMDB_endogenous[i]
            mzcloud_info <- final_results$mzCloud_endogenous[i]

            hmdb_yes <- !is.na(hmdb_status) && hmdb_status == "Yes"
            hmdb_no <- !is.na(hmdb_status) && hmdb_status == "No"
            mzcloud_valid <- !is.na(mzcloud_info)

            if (hmdb_yes || mzcloud_valid) {
                return("Yes")
            } else if (hmdb_no && !mzcloud_valid) {
                return("No")
            } else {
                return("Unknown")
            }
        })

        write.csv(final_results,
                 file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv'),
                 row.names = FALSE)

        last_update_time(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

        return(final_results)
    }

    shiny::observeEvent(input$refreshResults, {
        withProgress(message = 'Refreshing results...', {
            tryCatch({
                final_results_path <- file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv')
                compound_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')

                if (file.exists(final_results_path)) {
                    # The endogenous information file exists, read directly
                } else if (file.exists(compound_results_path)) {
                    showNotification("No endogenous information found, loading compound search results", type = "warning")
                } else {
                    showNotification("No results to display, please complete other steps and try again", type = "warning")
                }

                # Update the last refresh time to trigger DT re-rendering
                last_update_time(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

                showNotification("Results refreshed successfully", type = "message")
            }, error = function(e) {
                showNotification(sprintf("Error refreshing results: %s", e$message), type = "error")
            })
        })
    })

    output$lastUpdateTime <- renderText({
        if (is.null(last_update_time())) {
            "Not yet updated"
        } else {
            last_update_time()
        }
    })

    output$finalResultsTable <- DT::renderDT({
        # Add a virtual dependency to ensure the table will be re-rendered after refreshing
        dummy <- last_update_time()

        mzcloud_results_path <- file.path(APP_PATHS$process, 'final_compound_results_with_mzcloud.csv')
        hmdb_results_path <- file.path(APP_PATHS$process, 'final_compound_results_with_hmdb.csv')
        compound_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
        endogenous_results_path <- file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv')
        
        if (file.exists(endogenous_results_path)) {
            df <- read.csv(endogenous_results_path)
        } else if (file.exists(mzcloud_results_path) & file.exists(hmdb_results_path)) {
          hmdb_results <- read.csv(hmdb_results_path)
          mzcloud_results <- read.csv(mzcloud_results_path)
          Bind_endo_results <- hmdb_results
          # Bind_endo_results[,names(mzcloud_results)[13]] <- mzcloud_results$mzCloud_endogenous
          Bind_endo_results$HMDB_endogenous <- ifelse(is.na(Bind_endo_results$HMDB_endogenous),'NO','YES')
          df <- Bind_endo_results
        }else if (file.exists(mzcloud_results_path)){
            df <- read.csv(mzcloud_results_path)
        }else if (file.exists(hmdb_results_path)){
            df <- read.csv(hmdb_results_path)
        } else if (file.exists(compound_results_path)) {
          showNotification("No endogenous information found, loading compound search results", type = "warning")
          df <- read.csv(compound_results_path, stringsAsFactors = FALSE)
        } else {
          showNotification("No results to display, please complete other steps and try again", type = "warning")
          df <- data.frame()  # Empty data frame
        }


        selected_cols <- c("compound_name", "matched_name", "final_result",
                           "HMDB_endogenous",
                           "mzCloud_endogenous", "CID", "KEGG_ID",
                           "HMDB_ID", "CHEBI_ID", "CTD", "Similarity")

        col_names <- c("Compound Name", "Matched Name", "Match Result",
                       "HMDB Endogenous",
                       "mzcloud Endogenous", "PubChem CID", "KEGG ID",
                       "HMDB ID", "ChEBI ID", "CTD", "Similarity")

        # Ensure the column order and missing column processing in df
        available_cols <- selected_cols[selected_cols %in% names(df)]
        missing_cols <- setdiff(selected_cols, names(df))
        for (col in missing_cols) {
            df[[col]] <- NA
        }

        df_display <- df[, selected_cols]
        colnames(df_display) <- col_names

        DT::datatable(df_display,
                      options = list(pageLength = 10,
                                     autoWidth = TRUE,
                                     stateSave = TRUE,
                                     stateDuration = -1,
                                     order = list(list(2, 'desc')),
                                     dom = 'Blfrtip',
                                     buttons = c('colvis'),
                                     searchHighlight = TRUE,
                                     language = list(
                                         searchHighlight = TRUE
                                     )),
                  rownames = FALSE,
                  extensions = c('Buttons')) %>%
            DT::formatStyle(
                'Compound Name',
                backgroundColor = '#f0f4f8'
            ) %>%
            DT::formatStyle(
                'Matched Name',
                backgroundColor = '#f3f0f8'
            ) %>%
            DT::formatStyle(
                'Match Result',
                backgroundColor = '#f8f0f0',
                color = DT::styleEqual(
                    c("true", "false"),
                    c('#28a745', '#dc3545')
                )
            ) %>%
            DT::formatStyle(
                'HMDB Endogenous',
                backgroundColor = '#f0f8f4',
                color = DT::styleEqual(
                    c("Yes", "No", "Unknown"),
                    c('#28a745', '#dc3545', '#ffc107')
                )
            ) %>%
            DT::formatStyle(
                'mzcloud Endogenous',
                backgroundColor = '#f8f7f0'
            ) %>%
            DT::formatStyle(
                'PubChem CID',
                backgroundColor = '#f0f5f8'
            ) %>%
            DT::formatStyle(
                'KEGG ID',
                backgroundColor = '#f8f0f5'
            ) %>%
            DT::formatStyle(
                'HMDB ID',
                backgroundColor = '#f0f8f8'
            ) %>%
            DT::formatStyle(
                'ChEBI ID',
                backgroundColor = '#f8f4f0'
            ) %>%
            DT::formatStyle(
                'Similarity',
                backgroundColor = '#f4f8f0',
                color = DT::styleInterval(
                    c(0.7, 0.9),
                    c('#dc3545', '#ffc107', '#28a745')
                )
            )
    })

    shiny::observeEvent(input$nextFromProcessed, {
        shiny::updateTabsetPanel(session, "mainTabs", selected = "5. External Database Annotation")
    })

    # HMDB endogenous check
    shiny::observeEvent(input$runHMDBCheck, {
        req(file.exists(file.path(APP_PATHS$process, 'final_compound_results.csv')))

        shinyjs::addClass("runHMDBCheck", "btn-loading")
        disable("runHMDBCheck")

        withConsoleRedirect("endogenousConsole", {
            tryCatch({
                final_results <- read.csv(file.path(APP_PATHS$process, 'final_compound_results.csv'))

                log_message("Starting HMDB endogenous check...")

                total_compounds <- nrow(final_results)
                pb <- txtProgressBar(min = 0, max = total_compounds, style = 3)

                final_results$HMDB_endogenous <- NA

                for (i in seq_len(total_compounds)) {
                    hmdb_id <- final_results$HMDB_ID[i]
                    if (!is.na(hmdb_id) && hmdb_id != "" && hmdb_id != "NA" && hmdb_id != "Not_Access") {
                        final_results$HMDB_endogenous[i] <- check_hmdb_endogenous(hmdb_id)
                        log_message(sprintf("Processed HMDB ID %s: endogenous = %s",
                                          hmdb_id,
                                          ifelse(is.na(final_results$HMDB_endogenous[i]), "NA", final_results$HMDB_endogenous[i])))
                    }
                    setTxtProgressBar(pb, i)
                    Sys.sleep(0.5)
                }
                close(pb)

                write.csv(final_results,
                         file.path(APP_PATHS$process, 'final_compound_results_with_hmdb.csv'),
                         row.names = FALSE)

                log_message("HMDB endogenous check complete")

            }, error = function(e) {
                log_message(sprintf("Error in HMDB check: %s", e$message), tag = "ERROR")
                showNotification("Error in HMDB check", type = "error")
            }, finally = {
                enable("runHMDBCheck")
                shinyjs::removeClass("runHMDBCheck", "btn-loading")
            })
        })
    })

    # mzCloud endogenous check
    shiny::observeEvent(input$runMzCloudCheck, {
        req(file.exists(file.path(APP_PATHS$process, 'final_compound_results.csv')))

        shinyjs::addClass("runMzCloudCheck", "btn-loading")
        disable("runMzCloudCheck")

        withConsoleRedirect("endogenousConsole", {
            tryCatch({
                final_results <- read.csv(file.path(APP_PATHS$process, 'final_compound_results.csv'))

                log_message("Starting mzCloud endogenous check...")

                mzcloud_cache_dir <- file.path(APP_PATHS$cache, "mzcloud")
                if (!dir.exists(mzcloud_cache_dir)) {
                    dir.create(mzcloud_cache_dir, recursive = TRUE)
                }

                total_compounds <- nrow(final_results)
                pb <- txtProgressBar(min = 0, max = total_compounds, style = 3)

                # 初始化所有行为NA
                final_results$mzCloud_endogenous <- NA
                temp_results_file <- file.path(APP_PATHS$process, 'final_compound_results_mzcloud_temp.csv')

                for (i in seq_len(total_compounds)) {
                    compound_name <- final_results$matched_name[i]
                    if (!is.na(compound_name) && compound_name != "" && compound_name != "NA" && final_results$final_result[i] == "TRUE") {

                        search_url <- sprintf("https://www.mzcloud.org/compound/Search?Query=%s", URLencode(compound_name))

                        log_message(sprintf("Processing compound: %s", compound_name))

                        result <- tryCatch({
                            process_mzcloud_page(search_url, cache_dir = mzcloud_cache_dir, compound_name = compound_name)
                        }, error = function(e) {
                            log_message(sprintf("Error processing %s: %s", compound_name, e$message), tag = "WARNING")
                            return(NA)
                        })

                        final_results$mzCloud_endogenous[i] <- if (!is.null(result) && !is.na(result)) result else NA

                        log_message(sprintf("mzCloud info for %s: %s", compound_name,
                                          ifelse(is.na(final_results$mzCloud_endogenous[i]), "Not found", final_results$mzCloud_endogenous[i])))

                        if (i %% 5 == 0) {
                            write.csv(final_results, temp_results_file, row.names = FALSE)
                        }
                    }
                    setTxtProgressBar(pb, i)
                    Sys.sleep(0.5)
                }
                close(pb)

                if (file.exists(temp_results_file)) {
                    file.remove(temp_results_file)
                }

                results_path <- file.path(APP_PATHS$process, 'final_compound_results_with_mzcloud.csv')
                write.csv(final_results, results_path, row.names = FALSE)

                log_message("mzCloud endogenous check complete")
                log_message(sprintf("Results saved to: %s", results_path))

                # 合并HMDB结果（如果存在）
                if (file.exists(file.path(APP_PATHS$process, 'final_compound_results_with_hmdb.csv'))) {
                    hmdb_results <- read.csv(file.path(APP_PATHS$process, 'final_compound_results_with_hmdb.csv'))
                    
                    # 确保两个数据框有相同的行数和顺序
                    if (nrow(hmdb_results) == nrow(final_results)) {
                        # 将HMDB结果合并到最终结果中
                        final_results$HMDB_endogenous <- hmdb_results$HMDB_endogenous
                        
                        # 保存合并后的结果
                        write.csv(final_results, file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv'), row.names = FALSE)
                        log_message("Successfully merged HMDB and mzCloud results")
                    } else {
                        log_message("HMDB results have different number of rows, cannot merge", tag = "WARNING")
                        # 仍然保存mzCloud结果
                        write.csv(final_results, file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv'), row.names = FALSE)
                    }
                } else {
                    # 如果没有HMDB结果，直接保存mzCloud结果
                    write.csv(final_results, file.path(APP_PATHS$process, 'final_compound_results_with_endogenous.csv'), row.names = FALSE)
                }

            }, error = function(e) {
                log_message(sprintf("Error in mzCloud check: %s", e$message), tag = "ERROR")
                showNotification("Error in mzCloud check", type = "error")
            }, finally = {
                enable("runMzCloudCheck")
                shinyjs::removeClass("runMzCloudCheck", "btn-loading")
            })
        })
    })

    shiny::observeEvent(input$confirmEndogenous, {
        shiny::updateTabsetPanel(session, "mainTabs", selected = "4. Processed Results View")
    })

    output$downloadReport <- shiny::downloadHandler(
      filename = function() {
        paste("MetaboliteAnnotator_Report_", Sys.Date(), ".zip", sep = "")
      },
      content = function(file) {
        # Create a temporary directory to store all files
        temp_dir <- tempdir()
        report_dir <- file.path(temp_dir, "MetaboliteAnnotator_Report")
        dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
        
        # Generate the report
        step1_file <- file.path(APP_PATHS$process, "step1_info.rds")
        step2_file <- file.path(APP_PATHS$process, "step2_info.rds")
        total_file <- file.path(APP_PATHS$process, "total_compounds.rds")

        if(file.exists(step1_file) && file.exists(step2_file) && file.exists(total_file)){
            step1_info <- readRDS(step1_file)
            step2_info <- readRDS(step2_file)
            total_compounds <- readRDS(total_file)
        } else {
            total_compounds <- 0
            step1_info <- list(duration = NA)
            step2_info <- list(duration = NA)
        }

        params <- list(
          total_compounds = total_compounds,
          step1_time = if(!is.na(step1_info$duration)) paste0(round(step1_info$duration, 2), " sec") else "N/A",
          step2_time = if(!is.na(step2_info$duration)) paste0(round(step2_info$duration, 2), " sec") else "N/A",
          output_paths = list(
            process = APP_PATHS$process,
            data = APP_PATHS$data
          )
        )

        # Use the template from the package
        tempReport <- system.file("rmd", "report_template.Rmd", package = "MetaboliteAnnotator")
        if (tempReport == "") {
          stop("Report template not found in package!")
        }

        report_file <- file.path(report_dir, "MetaboliteAnnotator_Report.docx")
        
        out <- rmarkdown::render(
          tempReport,
          output_format = rmarkdown::word_document(
            toc = TRUE,
            toc_depth = 3
          ),
          output_file = report_file,
          params = params,
          envir = new.env(parent = globalenv())
        )
        
        # Copy result CSV files if they exist
        result_files <- c(
          "0.MetaboliteAnnotator_Results.csv",
          "1.MetaboliteAnnotator_Results_KEGG.csv",
          "2.MetaboliteAnnotator_Results_Gene.csv", 
          "3.MetaboliteAnnotator_Results_GO.csv",
          "4.MetaboliteAnnotator_Results_Phenotype.csv",
          "5.MetaboliteAnnotator_Results_Reactome.csv"
        )
        
        for (result_file in result_files) {
          source_path <- file.path(APP_PATHS$process, result_file)
          if (file.exists(source_path)) {
            file.copy(source_path, file.path(report_dir, result_file))
          }
        }
        
        # Create zip file with all contents
        files_to_zip <- list.files(report_dir, full.names = TRUE)
        utils::zip(file, files_to_zip, flags = "-j")  # -j flag to store just the file without the path
        
        # Clean up temporary files
        unlink(report_dir, recursive = TRUE)
      }
    )

    output$downloadKEGGBubblePlot <- shiny::downloadHandler(
      filename = function() {
        paste0("Pathway_Bubble_Plot_", Sys.Date(), ".pptx")
      },
      content = function(file) {
        req(enrichment_results())
        # Generate the ggplot object using the enrichment data
        plot_object <- create_pathway_bubble_plot(enrichment_results())
        # Use topptx function to export the ggplot object as a PPTX file.
        eoffice::topptx(plot_object, file = file,height = 7,width = 9)
      }
    )


    shiny::observeEvent(input$loadFinalResults, {
        withConsoleRedirect("endogenousConsole", {
            tryCatch({
                final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
                if (!file.exists(final_results_path)) {
                    stop("Final compound results file not found")
                }
                final_df <- read.csv(final_results_path, stringsAsFactors = FALSE)
                log_message(sprintf("Loaded local final compound results with %d rows", nrow(final_df)))
                showNotification("Local final compound results loaded successfully", type = "message")
            }, error = function(e) {
                showNotification(sprintf("Error loading final results: %s", e$message), type = "error")
                log_message(e$message, tag = "ERROR")
            })
        })
    })

    ## === 新增服务器端: 复制CID 和 KEGG ID+HMDB ID 功能 ===

    # 复制 final_compound_results.csv 中的 "CID" 列到剪贴板
    shiny::observeEvent(input$copyCID, {
      final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
      if(file.exists(final_results_path)){
        df_final <- read.csv(final_results_path, stringsAsFactors = FALSE)
        if("CID" %in% names(df_final)){
          cid_text <- paste(df_final$CID, collapse = "\n")
          #  use JSON.toString to ensure the text is correctly embedded in the JS string
          js_code <- sprintf('navigator.clipboard.writeText(%s);', jsonlite::toJSON(cid_text, auto_unbox = TRUE))
          shinyjs::runjs(js_code)
          showNotification("CID has been copied to the clipboard", type = "message")
        } else {
          showNotification("CID column not found in the final results", type = "error")
        }
      } else{
        showNotification("Final compound results file not found", type = "error")
      }
    })

    # 复制 final_compound_results.csv 中合并后的 "KEGG_ID" 与 "HMDB_ID"
    shiny::observeEvent(input$copyKEGGHMDB, {
      final_results_path <- file.path(APP_PATHS$process, 'final_compound_results.csv')
      if(file.exists(final_results_path)){
        df_final <- read.csv(final_results_path, stringsAsFactors = FALSE)
        if("KEGG_ID" %in% names(df_final) && "HMDB_ID" %in% names(df_final)){
          merged_ids <- sapply(1:nrow(df_final), function(i){
            ke <- as.character(df_final$KEGG_ID[i])
            hm <- as.character(df_final$HMDB_ID[i])
            if(!is.na(ke) && ke != "" && ke != "Not_Access"){
              return(ke)
            } else if(!is.na(hm) && hm != "" && hm != "Not_Access"){
              return(hm)
            } else {
              return(NA)
            }
          })
          merged_ids <- merged_ids[!is.na(merged_ids)]
          merged_text <- paste(merged_ids, collapse = "\n")
          js_code <- sprintf('navigator.clipboard.writeText(%s);', jsonlite::toJSON(merged_text, auto_unbox = TRUE))
          shinyjs::runjs(js_code)
          showNotification("KEGG ID + HMDB ID has been copied to the clipboard", type = "message")
        } else {
          showNotification("KEGG_ID or HMDB_ID column not found in the final results", type = "error")
        }
      } else{
        showNotification("Final compound results file not found", type = "error")
      }
    })
}

#' Run Shiny UI
#'
#' This function launches the Shiny application.
#'
#' @export
run_ui <- function() {
    if (!exists("APP_PATHS", envir = .GlobalEnv)) {
       stop("Please call init_app() first to initialize the working directory")
    }
    suppressWarnings(shiny::shinyApp(ui = ui, server = server))
}


