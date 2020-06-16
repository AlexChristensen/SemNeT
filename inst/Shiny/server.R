# Code for SemNeT----
server <- function(input, output, session)
{
  ###################
  #### HIDE TABS ####
  ###################
  
  hideTab(inputId = "tabs", target = "Network Estimation")
  hideTab(inputId = "tabs", target = "Random Network Analyses")
  hideTab(inputId = "tabs", target = "Bootstrap Network Analyses")
  hideTab(inputId = "tabs", target = "Random Walk Analyses")
  hideTab(inputId = "tabs", target = "Spreading Activation Analyses")
  
  #######################
  #### DATA EXAMPLES ####
  #######################
  
  # Data
  observeEvent(input$data_example,
               {
                 output$example_data_response <- renderTable({head(SemNeT::open.clean)},
                                                             rownames = TRUE,
                                                             caption = "Response Matrix (participants by typed responses)",
                                                             caption.placement = getOption("xtable.caption.placement", "top")
                 )
                 
                 output$example_data_binary <- renderTable({head(SemNeT::open.binary)},
                                                           digits = 0,
                                                           rownames = TRUE,
                                                           caption = "Binary Matrix (participants by alphabetical responses)",
                                                           caption.placement = getOption("xtable.caption.placement", "top")
                 )
               }
  )
  
  observeEvent(input$group_example,
               {
                 output$example_group <- renderTable({
                   group_example <- as.matrix(head(SemNeT::open.group))
                   row.names(group_example) <- row.names(head(SemNeT::open.binary))
                   colnames(group_example) <- "Group"
                   return(group_example)
                 },
                 rownames = TRUE,
                 caption = "Group Vector (ordered by participant)",
                 caption.placement = getOption("xtable.caption.placement", "top"),
                 width = 250
                 )
               }
  )
  
  #############################
  #### NETWORK ESTIMATION ####
  ############################
  
  # Keep previous environment
  prev.env <<- ls(envir = globalenv())
  
  # Check if anything exists in previous environment
  if(length(prev.env) != 0)
  {
    # Initialize textcleaner objects variable
    tc.object <<- vector(length = length(prev.env))
    
    # Check for textcleaner objects
    for(i in 1:length(prev.env))
    {tc.object[i] <- class(get(prev.env[i], envir = globalenv())) == "textcleaner"}
    
    # Set up environment objects
    if(sum(tc.object) != 0)
    {
      output$clean_ui <- renderUI({
        selectInput("clean_envir", label = "textcleaner Objects Detected in Environment. Use?",
                    choices = c("", prev.env[tc.object]), selected = 1)
      })
    }
    
    if(exists("group"))
    {
      output$group_ui <- renderUI({
        radioButtons("group_envir", label = "R Object 'group' Detected in Environment. Use?",
                     choices = c("Yes", "No"), inline = TRUE, selected = "Yes")
      })
    }
  }
  
  
  # Load Data panel
  observeEvent(input$load_data,
               {
                 # Let user know
                 showNotification("Loading data...")
                 
                 if(!is.null(input$clean_envir) || !is.null(input$group_envir))
                 {
                   # Load data from R environment
                   if(input$clean_envir != "")
                     dat <<- get(input$clean_envir, envir = globalenv())$responses$clean
                   
                   # Load group from R environment
                   if(input$group_envir == "Yes")
                   {group <<- group}
                 }
                 
                 # Load preprocessed data
                 if(!is.null(input$data))
                 {dat <<- SemNeT:::read.data(input$data$datapath)}
                 
                 # Load group data
                 if(!is.null(input$group))
                 {group <<- SemNeT:::read.data(input$group$datapath)}
                 
                 # Load data from SemNeT package
                 if(!exists("dat"))
                 {
                   dat <<- SemNeT::open.clean
                   group <<- SemNeT::open.group
                 }
                 
                 # Load group data from SemNeT package
                 if(!exists("group"))
                 {group <<- rep(1, nrow(data))}
                 
                 # Organize group data
                 group <<- unlist(group)
                 
                 # Show network estimation tab
                 showTab(inputId = "tabs", target = "Network Estimation")
                 
                 # Print waiting message
                 # FOR R PACKAGE AND WEB
                 shinyalert::shinyalert(title = "Data Loaded Successfully",
                                        type = "info",
                                        showConfirmButton = TRUE)
                 
                 # Move on to network estimation tab
                 updateTabsetPanel(session, "tabs",
                                   selected = "Network Estimation")
                 
               }
  )
  
  # Set up UI based on selection of network estimaton
  ## Option 1
  output$network_options_1 <- renderUI({
    
    network <- input$estimation
    
    if(network == "Community Network (CN)")
    {numericInput("window", label = "Window Size", value = 2, min = 1, max = Inf)
    }else if(network == "Naive Random Walk (NRW)")
    {numericInput("threshold", label = "Threshold (Minimum Number of Co-occurrences)", value = 3, min = 1, max = Inf)
    }else if(network == "Triangulated Maximally Filtered Graph (TMFG)")
    {selectInput("assoc", label = "Association Measure", choices = c("Angular", "Cosine",
                                                                     "Euclidean Distance",
                                                                     "Faith", "Jaccard Index",
                                                                     "Pearson's Correlation",
                                                                     "RR"), selected = "Cosine"
    )}
    
  })
  
  ## Option 2
  output$network_options_2 <- renderUI({
    
    network <- input$estimation
    
    if(network == "Community Network (CN)")
    {selectInput("alpha", label = paste("Significance Level"), choices = c(.05, .01, .001))
    }else if(network == "Triangulated Maximally Filtered Graph (TMFG)")
    {numericInput("minCase", label = "Minimum Number of Responses", value = 2, min = 1, max = Inf)}
    
  })
  
  # Network Estimation panel
  
  ## Hide clear results button
  shinyjs::hide("reset")
  
  observeEvent(input$run_est,
               {
                 # Let user know
                 showNotification("Estimating networks...")
                 
                 ## Identify unique groups
                 uniq <<- unique(group)
                 
                 ## Grab network
                 network <<- switch(input$estimation,
                                    "Triangulated Maximally Filtered Graph (TMFG)" = "TMFG",
                                    "Community Network (CN)" = "CN",
                                    "Naive Random Walk (NRW)" = "NRW",
                                    "Pathfinder Network (PN)" = "PN"
                 )
                 
                 ## Change responses to binary matrix
                 if(network == "TMFG")
                   if(all(apply(dat, 2, is.character)))
                   {bin_dat <<- SemNeT:::resp2bin(dat)}
                 
                 ## Create new data
                 for(i in 1:length(uniq))
                 {
                   assign(paste(uniq[i]),
                          dat[which(group == uniq[i]),],
                          envir = globalenv())
                 }
                 
                 ## Estimate networks
                 if(network == "CN")
                 {
                   ## Estimate networks
                   nets <<- lapply(mget(paste(uniq), envir = globalenv()),
                                   function(x){CN(x, window = input$window, alpha = as.numeric(input$alpha))})
                   
                 }else if(network == "NRW")
                 {
                   ## Estimate networks
                   nets <<- lapply(mget(paste(uniq), envir = globalenv()),
                                   function(x){NRW(x, threshold = input$threshold)})
                   
                 }else if(network == "PN")
                 {
                   # Print waiting message
                   # FOR R PACKAGE
                   #shinyalert::shinyalert(title = "Running...",
                   #                        text = "Check R Console for the Pathfinder Network Estimation Progress",
                   #                        type = "info")
                   
                   # FOR WEB
                   shinyalert::shinyalert(title = "Running...",
                                          text = "Results will appear when the Pathfinder Network estimations are completed (do not exit browser)",
                                          type = "info")
                   
                   ## Estimate networks
                   nets <<- lapply(mget(paste(uniq), envir = globalenv()),
                                   function(x){PF(x)})
                   
                 }else if(network == "TMFG")
                 {
                   ## Create new data
                   for(i in 1:length(uniq))
                   {
                     assign(paste(uniq[i]),
                            bin_dat[which(group == uniq[i]),],
                            envir = globalenv())
                   }
                   
                   ## Store binary groups
                   for(i in 1:length(uniq))
                   {assign(paste(uniq[i]),
                           SemNeT::finalize(get(paste(uniq[i]), envir = globalenv()),
                                            minCase = as.numeric(input$minCase)),
                           envir = globalenv())}
                   
                   ## Equate groups
                   eq <<- SemNeT:::equateShiny(mget(paste(uniq), envir = globalenv()))
                   
                   ## Grab proper association label
                   sim <<- switch(input$assoc,
                                  "Angular" = "angular",
                                  "Cosine" = "cosine",
                                  "Euclidean Distance" = "euclid",
                                  "Faith" = "faith",
                                  "Jaccard Index" = "jaccard",
                                  "Pearson's Correlation" = "cor",
                                  "RR" = "rr",
                   )
                   
                   ## Compute associations
                   assoc <<- lapply(SemNeT:::equateShiny(mget(paste(uniq), envir = globalenv())),
                                    SemNeT::similarity, method = sim)
                   
                   ## Estimate networks
                   nets <<- lapply(assoc, function(x){NetworkToolbox::TMFG(x)$A})
                 }
                 
                 ## Compute network measures
                 meas <<- lapply(nets, SemNeT::semnetmeas)
                 
                 ## Organized output
                 meas.mat <<- sapply(meas, c)
                 
                 ## Generate plot
                 plots <<- SemNeT:::compare_netShiny(nets, config = "spring", weighted = TRUE)
                 
                 ## Render semantic networks plot
                 output$viz <- renderPlot({
                   
                   ### Manipulate Shiny plot window
                   if(length(plots$datalist) == 2)
                   {layout(t(1:2))
                   }else if(length(plots$datalist) > 2)
                   {
                     #Find square root
                     len <- floor(sqrt(length(plots$datalist)))
                     
                     #Remainder
                     remain <- length(plots$datalist)%%len
                     
                     #Change layout accordingly
                     layout(t(matrix(1:(length(plots$datalist)+remain),ncol=len)))
                   }
                   
                   ### Generate plot
                   SemNeT:::plot.compareShiny(plots)
                 })
                 
                 ## Render network measures table
                 output$measures <- renderTable(meas.mat,
                                                rownames = TRUE,
                                                caption = "Network Measures",
                                                caption.placement = getOption("xtable.caption.placement", "top"))
                 
                 ## Change later input for bootstrap networks
                 output$type <- renderUI({
                   
                   if(network == "TMFG")
                   {checkboxGroupInput("percent", label = "Proportion of Nodes Remaining",
                                       choiceNames = sprintf("%1.2f",seq(.50,.90,.10)),
                                       choiceValues = seq(.50,.90,.10), inline = TRUE,
                                       selected = seq(.50,.90,.10))
                   }else{
                     removeUI("type", immediate = TRUE)
                   }
                   
                 })
                 
                 # Show analysis tabs
                 showTab(inputId = "tabs", target = "Random Network Analyses")
                 showTab(inputId = "tabs", target = "Bootstrap Network Analyses")
                 showTab(inputId = "tabs", target = "Random Walk Analyses")
                 showTab(inputId = "tabs", target = "Spreading Activation Analyses")
                 
                 ## Hide clear results button
                 shinyjs::show("reset")
                 
               }
  )
  
  # Reset
  observeEvent(input$reset,
               {
                 
                 shinyalert::shinyalert(title = "Are you sure?",
                                        text = "You are about to erase your output\n(Data will not be erased)",
                                        type = "error",
                                        showConfirmButton = TRUE,
                                        showCancelButton = TRUE,
                                        callbackR = function(x)
                                        {
                                          if(x)
                                          {
                                            showNotification("Results cleared")
                                            
                                            # Refresh tables and plots
                                            output$viz <- renderPlot({})
                                            output$measures <- renderTable({})
                                            output$randnet <- renderTable({})
                                            output$aspl <- renderTable({})
                                            output$cc <- renderTable({})
                                            output$q <- renderTable({})
                                            output$tab <- renderTable({})
                                            output$asplPlot <- renderPlot({})
                                            output$ccPlot <- renderPlot({})
                                            output$qPlot <- renderPlot({})
                                            output$walk_rand <- renderTable({})
                                            output$spreadr_animate <- renderPlot({})
                                            
                                            # Network Estimation tab
                                            updateSelectInput(session = session,
                                                              inputId = "estimation",
                                                              label = "Network Estimation Method",
                                                              choices = c("Community Network (CN)",
                                                                          "Naive Random Walk (NRW)",
                                                                          "Pathfinder Network (PN)",
                                                                          "Triangulated Maximally Filtered Graph (TMFG)")
                                            )
                                            
                                            # Hide tabs
                                            hideTab(inputId = "tabs", target = "Random Network Analyses")
                                            hideTab(inputId = "tabs", target = "Bootstrap Network Analyses")
                                            hideTab(inputId = "tabs", target = "Random Walk Analyses")
                                            hideTab(inputId = "tabs", target = "Spreading Activation Analyses")
                                            
                                            # Random Network Analyses tab
                                            updateNumericInput(session = session,
                                                               inputId = "iters_rand",
                                                               label = "Number of Iterations",
                                                               value = 1000, min = 0, step = 100)
                                            
                                            
                                            if(exists("core_rand", envir = globalenv()))
                                            {
                                              updateSelectInput(session = session,
                                                                inputId = "cores_rand",
                                                                label = "Number of Processing Cores",
                                                                choices = core_rand,
                                                                selected = ceiling(length(core_rand) / 2)
                                              )
                                            }
                                            
                                            # Bootstrap Network Analyses tab
                                            updateNumericInput(session = session,
                                                               inputId = "iters_boot",
                                                               label = "Number of Iterations",
                                                               value = 1000, min = 0, step = 100)
                                            
                                            if(exists("core_boot", envir = globalenv()))
                                            {
                                              updateSelectInput(session = session,
                                                                inputId = "cores_boot",
                                                                label = "Number of Processing Cores",
                                                                choices = core_boot,
                                                                selected = ceiling(length(core_boot) / 2)
                                              )
                                            }
                                            
                                            if(network == "TMFG")
                                            {
                                              updateCheckboxGroupInput(session = session,
                                                                       "percent", label = "Proportion of Nodes Remaining",
                                                                       choiceNames = sprintf("%1.2f",seq(.50,.90,.10)),
                                                                       choiceValues = seq(.50,.90,.10), inline = TRUE,
                                                                       selected = seq(.50,.90,.10)
                                              )
                                            }
                                            
                                            ## Hide plot button
                                            shinyjs::hide("run_plot")
                                            
                                            # Random Network Analyses tab
                                            updateNumericInput(session = session,
                                                               inputId = "reps",
                                                               label = "Number of Repetitions",
                                                               value = 20, min = 0, max = Inf, step = 5)
                                            
                                            updateNumericInput(session = session,
                                                               inputId = "steps",
                                                               label = "Number of Steps",
                                                               value = 10, min = 0, max = Inf, step = 1)
                                            
                                            updateNumericInput(session = session,
                                                               inputId = "iters_walk",
                                                               label = "Number of Iterations",
                                                               value = 10000, min = 0, max = Inf, step = 1000)
                                            
                                            if(exists("core_walk", envir = globalenv()))
                                            {
                                              updateSelectInput(session = session,
                                                                inputId = "cores_walk",
                                                                label = "Number of Processing Cores",
                                                                choices = core_walk,
                                                                selected = ceiling(length(core_walk) / 2)
                                              )
                                            }
                                            
                                            # Spreading Activation Analyses tab
                                            updateNumericInput(session = session,
                                                               inputId = "retention",
                                                               label = "Retention (proportion of activation that remains in spreading node)",
                                                               value = 0.5, min = 0, max = 1, step = .10)
                                            
                                            updateNumericInput(session = session,
                                                               inputId = "time",
                                                               label = "Number of Time Steps",
                                                               value = 10, min = 0, max = Inf, step = 1)
                                            
                                            updateNumericInput(session = session,
                                                               inputId = "decay",
                                                               label = "Decay (activation lost at each time step)",
                                                               value = 0, min = 0, max = 1, step = .10)
                                            
                                            updateNumericInput(session = session,
                                                               inputId = "suppress",
                                                               label = "Suppress (activation less than value is set to zero)",
                                                               value = 0, min = 0, max = Inf, step = 1)
                                            
                                            updateSelectInput(session = session,
                                                              inputId = "animate_size",
                                                              label = "Plot Size",
                                                              choices = c("Small (500 x 500)", "Medium (900 x 900)", "Large (1400 x 1400)"),
                                                              selected = "Medium (900 x 900)"
                                            )
                                            
                                            ## Show inputs
                                            shinyjs::show("network_select")
                                            shinyjs::show("retention")
                                            shinyjs::show("time")
                                            shinyjs::show("decay")
                                            shinyjs::show("suppress")
                                            shinyjs::show("set_act")
                                            
                                            ## Hide inputs
                                            shinyjs::hide("animate")
                                            shinyjs::hide("animate_size")
                                            shinyjs::hide("animate_slider")
                                            shinyjs::hide("reset_act")
                                            shinyjs::hide("reset")
                                            shinyjs::hide("node_select")
                                            shinyjs::hide("run_spr_act")
                                            
                                            if(exists("clean"))
                                            {rm(list = ls(envir = globalenv())[-match(c("prev.env", "clean", "dat", "group"), ls(globalenv()))], envir = globalenv())
                                            }else if(exists("group") && exists("dat"))
                                            {rm(list = ls(envir = globalenv())[-match(c("prev.env", "dat", "group"), ls(globalenv()))], envir = globalenv())
                                            }else{rm(list = ls(envir = globalenv())[-match(c("prev.env", "dat", "group"), ls(globalenv()))], envir = globalenv())}
                                            
                                          }
                                        })
               })
  
  #############################
  #### RANDOM NETWORK TEST ####
  #############################
  
  # Determine the number of cores
  ## Random Networks
  output$cores_rand <- renderUI({
    
    core_rand <<- seq(1,parallel::detectCores()-1,1)
    names(core_rand) <- paste(core_rand)
    
    selectInput("cores_rand", label = "Number of Processing Cores",
                choices = core_rand,
                selected = ceiling(length(core_rand) / 2)
    )
  })
  
  # Random Networks panel
  observeEvent(input$run_rand,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Print waiting message
                 # FOR R PACKAGE
                 #shinyalert::shinyalert(title = "Running...",
                 #                        text = "Check R Console for the Random Network Analyses Progress",
                 #                        type = "info")
                 
                 # FOR WEB
                 shinyalert::shinyalert(title = "Running...",
                                        text = "Results will appear when the Random Network Analyses are completed (do not exit browser)",
                                        type = "info")
                 
                 # Run random networks
                 rand_res <- reactive({
                   
                   randres <<- SemNeT:::randnet.testShiny(nets, iter = as.numeric(input$iters_rand), cores = as.numeric(input$cores_rand))
                   
                   # Convert into matrix
                   for(i in 1:length(randres))
                   {
                     if(i == 1)
                     {randresmat <- randres[[i]]
                     }else{randresmat <- cbind(randresmat, randres[[i]])}
                   }
                   
                   return(randresmat)
                   
                 })
                 
                 # Render random networks table
                 output$randnet <- renderTable({rand.res <<- rand_res(); rand.res}, rownames = TRUE,
                                               caption = "Random Network Results",
                                               caption.placement = getOption("xtable.caption.placement", "top")
                 )
                 
               }
  )
  
  ################################
  #### BOOTSTRAP NETWORK TEST ####
  ################################
  
  ## Bootstrap Networks
  output$cores_boot <- renderUI({
    
    core_boot <<- seq(1,parallel::detectCores()-1,1)
    names(core_boot) <- paste(core_boot)
    
    selectInput("cores_boot", label = "Number of Processing Cores",
                choices = core_boot,
                selected = ceiling(length(core_boot) / 2)
    )
  })
  
  ## Hide plot button
  shinyjs::hide("run_plot")
  
  # Partial Bootstrap Networks panel
  observeEvent(input$run_boot,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Run bootstrap networks
                 boot <- reactive({
                   
                   if(network == "TMFG")
                   {
                     ## Obtain percentages
                     percents <- as.numeric(input$percent)
                     
                     # Show progress
                     withProgress({
                       
                       ## Run partial bootstrap networks
                       for(i in 1:length(percents))
                       {
                         if(exists(paste(percents[i]), envir = globalenv()))
                         {next
                         }else{
                           
                           # Print waiting message
                           # FOR R PACKAGE
                           #shinyalert::shinyalert(title = paste("Running...\n","(Proportion of nodes remaining: ",sprintf("%1.2f", percents[i]),")",sep=""),
                           #                      text = "Check R Console for the Bootstrap Network Analyses Progress",
                           #                      type = "info")
                           
                           # FOR WEB
                           shinyalert::shinyalert(title = paste("Running...\n","(Proportion of nodes remaining: ",sprintf("%1.2f", percents[i]),")",sep=""),
                                                  text = "Results will appear when the Bootstrap Network Analyses are completed (do not exit browser)",
                                                  type = "info")
                           
                           # Increase progress
                           setProgress(value = i)
                           
                           assign(paste(percents[i]),
                                  SemNeT:::bootSemNeTShiny(eq,
                                                           prop = percents[i],
                                                           sim = sim,
                                                           weighted = FALSE,
                                                           iter = as.numeric(input$iters_boot),
                                                           cores = as.numeric(input$cores_boot),
                                                           type = "node",
                                                           method = network),
                                  envir = globalenv())
                           
                         }
                       }
                       
                     }, message = "Computing bootstraps...", value = 0, min = 1, max = length(percents))
                     
                   }else{
                     
                     # Print waiting message
                     # FOR R PACKAGE
                     #shinyalert::shinyalert(title = "Running...",
                     #                       text = "Check R Console for the Bootstrap Network Analyses Progress",
                     #                       type = "info")
                     
                     # FOR WEB
                     shinyalert::shinyalert(title = "Running...",
                                            text = "Results will appear when the Bootstrap Network Analyses are completed (do not exit browser)",
                                            type = "info")
                     
                     
                     ## Only one
                     percents <- as.numeric(100)
                     
                     assign(paste(percents),
                            SemNeT:::bootSemNeTShiny(mget(paste(uniq), envir = globalenv()),
                                                     weighted = FALSE,
                                                     iter = as.numeric(input$iters_boot),
                                                     cores = as.numeric(input$cores_boot),
                                                     type = "case",
                                                     method = network),
                            envir = globalenv())
                     
                   }
                   
                   return(list(mget(paste(percents), envir = globalenv())))
                 })
                 
                 # Render Tables
                 if(network == "TMFG")
                 {
                   
                   if(length(eq) == 2)
                   {
                     
                     res_boot <<- boot()
                     
                     ## Average Shortest Path Length
                     output$aspl <- renderTable({
                       
                       bootTest <<- list()
                       
                       bootTest$ASPL <<- SemNeT:::test.bootSemNeTShiny(unlist(res_boot, recursive = FALSE))$ASPL; bootTest$ASPL
                     }, rownames = TRUE,
                     caption = "Average Shortest Path Lengths (ASPL)",
                     caption.placement = getOption("xtable.caption.placement", "top")
                     )
                     
                     ## Clustering Coefficient
                     output$cc <- renderTable({
                       bootTest$CC <<- SemNeT:::test.bootSemNeTShiny(unlist(res_boot, recursive = FALSE))$CC; bootTest$CC
                     }, rownames = TRUE,
                     caption = "Clustering Coefficient (CC)",
                     caption.placement = getOption("xtable.caption.placement", "top")
                     )
                     
                     ## Modularity
                     output$q <- renderTable({
                       bootTest$Q <<- SemNeT:::test.bootSemNeTShiny(unlist(res_boot, recursive = FALSE))$Q; bootTest$Q
                     }, rownames = TRUE,
                     caption = "Modularity",
                     caption.placement = getOption("xtable.caption.placement", "top")
                     )
                   }
                   
                 }else{
                   
                   res_boot <<- boot()
                   
                   if(length(uniq) == 2)
                   {
                     output$tab <- renderTable({
                       bootTest <<- list()
                       
                       bootTest <<- SemNeT:::test.bootSemNeTShiny(unlist(res_boot, recursive = FALSE)); bootTest
                     }, rownames = TRUE,
                     caption = "Bootstrap Network Results",
                     caption.placement = getOption("xtable.caption.placement", "top")
                     )
                   }
                   
                 }
                 
                 ## Show plot button
                 shinyjs::show("run_plot")
                 
               }
  )
  
  # Plots panel
  observeEvent(input$run_plot,
               {
                 # Let user know
                 showNotification("Generating plots...")
                 
                 # Generate plots
                 pbplot <<- SemNeT:::plotbootSemNeTShiny(unlist(res_boot, recursive = FALSE))
                 
                 ## Average Shortest Path Length
                 output$asplPlot <- renderPlot({pbplot$aspl}, height = 400, width = 700)
                 
                 ## Clustering Coefficient
                 output$ccPlot <- renderPlot({pbplot$cc}, height = 400, width = 700)
                 
                 ## Modularity
                 output$qPlot <- renderPlot({pbplot$q}, height = 400, width = 700)
                 
               }
               
  )
  
  ##############################
  #### RANDOM WALK ANALYSIS ####
  ##############################
  
  # Determine the number of cores
  ## Random Walk
  output$cores_walk <- renderUI({
    
    core_walk <<- seq(1,parallel::detectCores()-1,1)
    names(core_walk) <- paste(core_walk)
    
    selectInput("cores_walk", label = "Number of Processing Cores",
                choices = core_walk,
                selected = ceiling(length(core_walk) / 2)
    )
  })
  
  # Determine networks
  output$A <- renderUI({
    selectInput("A", label = "Select Network A",
                choices = names(nets))
  })
  
  output$B <- renderUI({
    selectInput("B", label = "Select Network B",
                choices = names(nets)[-which(names(nets) == input$A)])
  })
  
  # Random Walks panel
  observeEvent(input$run_walk,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Print waiting message
                 # FOR R PACKAGE
                 #shinyalert::shinyalert(title = "Running...",
                 #                        text = "Check R Console for the Random Walk Analyses Progress",
                 #                        type = "info")
                 
                 # FOR WEB
                 shinyalert::shinyalert(title = "Running...",
                                        text = "Results will appear when the Random Walk Analyses are completed (do not exit browser)",
                                        type = "info")
                 
                 # Run random networks
                 rand_walk <- reactive({
                   
                   rw <<- SemNeT:::randwalkShiny(nets, input$A, input$B,
                                                 reps = as.numeric(input$reps),
                                                 steps = as.numeric(input$steps),
                                                 iter = as.numeric(input$iters_walk),
                                                 cores = as.numeric(input$cores_walk))
                   
                   return(rw)
                   
                 })
                 
                 # Render random networks table
                 output$walk_rand <- renderTable({rw <<- rand_walk(); rw$short}, rownames = FALSE,
                                                 caption = "Random Walk Results",
                                                 caption.placement = getOption("xtable.caption.placement", "top")
                 )
                 
               }
  )
  
  #######################################
  #### SPREADING ACTIVATION ANALYSIS ####
  #######################################
  
  ## Hide animate button
  shinyjs::hide("run_spr_act")
  
  # Determine networks
  output$network_select <- renderUI({
    selectInput("network_select", label = "Select a Network",
                choices = names(nets),
                selected = NULL)
  })
  
  observeEvent(input$set_act,
               {
                 ## Show node select
                 shinyjs::show("node_select")
                 
                 # Determine nodes
                 output$node_select <- renderUI({
                   
                   # Gets rid of NULL index bug
                   req(input$network_select)
                   
                   # Name of selected network
                   net_name <<- input$network_select
                   
                   # Nodes of the selected network
                   nodes <<- colnames(nets[[net_name]])
                   # Create matrix of nodes with blank activations
                   mat <<- cbind(nodes, rep("", length(nodes)))
                   
                   # Create Shiny matrix of nodes and activations
                   shinyMatrix::matrixInput("node_activation",
                                            cols = list(
                                              names = TRUE,
                                              editableNames = FALSE
                                            ),
                                            value = matrix(mat, ncol = 2,
                                                           dimnames = list(NULL, c("Node", "Activation")))
                   )
                   
                 })
                 
                 ## Hide set activation
                 shinyjs::hide("set_act")
                 
                 ## Show set activation
                 shinyjs::show("run_spr_act")
               }
  )
  
  ## Hide animate button
  shinyjs::hide("animate")
  
  ## Hide plot size
  shinyjs::hide("animate_size")
  
  ## Hide animation slider
  shinyjs::hide("animate_slider")
  
  ## Hide reset activation
  shinyjs::hide("reset_act")
  
  # Random Walks panel
  observeEvent(input$run_spr_act,
               {
                 # Let user know
                 showNotification("Computing statistics...")
                 
                 # Set up start_run data frame
                 act_mat <<- input$node_activation
                 
                 # Convert nodes to numbers
                 act_mat[,1] <<- 1:nrow(act_mat)
                 
                 # Keep activation rows
                 keep.row <<- ifelse(is.na(act_mat[,2]) | act_mat[,2] == "" | act_mat[,2] == "0", FALSE, TRUE)
                 
                 # Keep only those rows
                 act_mat <<- as.matrix(act_mat[keep.row,])
                 
                 # Make sure two columns
                 if(ncol(act_mat) != 2)
                 {act_mat <<- t(act_mat)}
                 
                 # Make sure values are numeric
                 act_mat <<- apply(act_mat, 2, as.numeric)
                 
                 # Keep rows
                 act_df <<- as.data.frame(act_mat)
                 
                 # Make sure two columns (and data frame)
                 if(ncol(act_df) != 2)
                 {act_df <<- as.data.frame(t(act_df))}
                 
                 # Make sure column names are correct
                 colnames(act_df) <<- c("node", "activation")
                 
                 # Run spreading activation
                 sa <<- spreadr::spreadr(network = nets[[net_name]],
                                         start_run = act_df,
                                         retention = input$retention,
                                         time = input$time,
                                         decay = input$decay,
                                         suppress = input$suppress)
                 
                 ## Show animate button
                 shinyjs::show("animate")
                 
                 ## Show plot size
                 shinyjs::show("animate_size")
                 
                 ## Hide matrix input
                 shinyjs::hide("node_select")
                 
                 ## Hide inputs
                 shinyjs::hide("network_select")
                 shinyjs::hide("run_spr_act")
                 shinyjs::hide("retention")
                 shinyjs::hide("time")
                 shinyjs::hide("decay")
                 shinyjs::hide("suppress")
                 
               }
  )
  
  observeEvent(input$animate,
               {
                 
                 # Initialize plot list
                 if(!exists("plot_list", envir = globalenv()))
                 {
                   plot_list <<- vector("list", length = length(nets))
                   names(plot_list) <<- names(nets)
                 }
                 
                 # Plot size
                 plot_size <<- switch(input$animate_size,
                                      "Small (500 x 500)" = 500,
                                      "Medium (900 x 900)" = 900,
                                      "Large (1400 x 1400)" = 1400
                 )
                 
                 # Initialize time list
                 time_list <<- vector("list", length = max(sa$time))
                 
                 # Progress through plots
                 withProgress({
                   
                   # Generate animation
                   for(i in 1:max(sa$time))
                   {
                     
                     # Increase progress
                     setProgress(value = i)
                     
                     # Plots
                     SemNeT:::spreadrShinyPlot(network = nets[[net_name]], spreadr.output = sa, time = i, size = plot_size)
                     time_list[[i]] <<- recordPlot()
                   }
                   
                 }, message = "Generating animation...", value = 0, min = 1, max = max(sa$time))
                 
                 # Set class of time list
                 class(time_list) <- "animateShiny"
                 
                 # Insert into plot list
                 plot_list[[net_name]] <<- time_list
                 
                 # Render plot
                 output$spreadr_animate <- renderPlot({
                   
                   # Gets rid of NULL index bug
                   req(input$animate_slider2)
                   
                   plot_list[[net_name]][[input$animate_slider2]]
                   
                 }, width = plot_size, height = plot_size)
                 
                 # Slider for animation
                 output$animate_slider <- renderUI({
                   sliderInput("animate_slider2", "Time Step",
                               min = 1, max = max(sa$time), value = 1, step = 1,
                               animate = TRUE)
                 })
                 
                 ## Show animation slider
                 shinyjs::show("animate_slider")
                 
                 ## Show spreadr animate
                 shinyjs::show("spreadr_animate")
                 
                 ## Show reset activation
                 shinyjs::show("reset_act")
               }
  )
  
  observeEvent(input$reset_act,
               {
                 ## Show inputs
                 shinyjs::show("network_select")
                 shinyjs::show("retention")
                 shinyjs::show("time")
                 shinyjs::show("decay")
                 shinyjs::show("suppress")
                 shinyjs::show("set_act")
                 
                 ## Hide animate button
                 shinyjs::hide("animate")
                 
                 ## Hide reset activation
                 shinyjs::hide("reset_act")
                 
                 ## Hide plot size
                 shinyjs::hide("animate_size")
                 
                 ## Hide animate slider
                 shinyjs::hide("animate_slider")
                 
                 ## Hide animate slider
                 shinyjs::hide("spreadr_animate")
                 
               }
  )
  
  
  
  onStop(function(x)
  {
    # Save results into condensed list
    resultShiny <<- list()
    
    if(exists("dat", envir = globalenv()))
    {resultShiny$data <<- dat}
    
    if(exists("group", envir = globalenv()))
    {resultShiny$group <<- group}
    
    if(exists("nets", envir = globalenv()))
    {resultShiny$network <<- nets}
    
    if(exists("meas.mat", envir = globalenv()))
    {resultShiny$measures <<- meas.mat}
    
    if(exists("plots", envir = globalenv()))
    {resultShiny$comparePlot <<- plots}
    
    if(exists("rand.res", envir = globalenv()))
    {resultShiny$randomTest <<- rand.res}
    
    if(exists("res_boot", envir = globalenv()))
    {resultShiny$bootstrap <<- unlist(res_boot, recursive = FALSE)}
    
    if(exists("bootTest", envir = globalenv()))
    {resultShiny$bootstrapTest <<- bootTest}
    
    if(exists("pbplot", envir = globalenv()))
    {resultShiny$bootstrapPlot <<- pbplot}
    
    if(exists("rw", envir = globalenv()))
    {resultShiny$randomWalk <<- rw}
    
    if(exists("sa", envir = globalenv()))
    {resultShiny$spreadingActivation <<- sa}
    
    if(any(!is.null(unlist(plot_list))))
    {resultShiny$spreadingActivationPlot <<- plot_list}
    
    # Remove all other variables from global environment
    rm(list = ls(envir = globalenv())[-match(c("resultShiny", prev.env), ls(globalenv()))], envir = globalenv())
    
    # Remove plots from user view
    dev.off()
  }
  )
  
}