# Code for SemNeT
server <- function(input, output, session)
{
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
  
  # Semantic Networks panel
  observeEvent(input$run_est,
               {
                 # Let user know
                 showNotification("Estimating networks...")
                 
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
                 {dat <<- SemNeT::open.clean}
                 
                 # Load group data from SemNeT package
                 if(!exists("group"))
                 {group <<- SemNeT::open.group}
                 
                 # Organize group data
                 group <<- unlist(group)
                 
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
                   {dat <<- SemNeT:::resp2bin(dat)}
                 
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
                   shinyalert::shinyalert(title = "Running...",
                                          text = "Check R Console for the Pathfinder Network Estimation Progress",
                                          type = "info")
                   
                   # FOR WEB
                   #shinyalert::shinyalert(title = "Running...",
                   #                      text = "Results will appear when the Pathfinder Network estimations are completed (do not exit browser)",
                   #                      type = "info")
                   
                   ## Estimate networks
                   nets <<- lapply(mget(paste(uniq), envir = globalenv()),
                                   function(x){PF(x)})
                   
                 }else if(network == "TMFG")
                 {
                   
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
                 plots <<- SemNeT:::compare_netShiny(nets, config = "spring", weighted = FALSE)
                 
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
                                            showNotification("Results Cleared")
                                            
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
                                            updateSelectInput(session = session,
                                                              inputId = "estimation",
                                                              label = "Network Estimation Method",
                                                              choices = c("",
                                                                          "Community Network (CN)",
                                                                          "Naive Random Walk (NRW)",
                                                                          "Pathfinder Network (PN)",
                                                                          "Triangulated Maximally Filtered Graph (TMFG)")
                                            )
                                            
                                            if(exists("clean"))
                                            {rm(list = ls(envir = globalenv())[-match(c("clean", "dat", "group"), ls(globalenv()))], envir = globalenv())
                                            }else{rm(list = ls(envir = globalenv())[-match(c("dat", "group"), ls(globalenv()))], envir = globalenv())}
                                            
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
    
    selectInput("cores_rand", label = "Number of processing cores",
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
                 shinyalert::shinyalert(title = "Running...",
                                        text = "Check R Console for the Random Network Analyses Progress",
                                        type = "info")
                 
                 # FOR WEB
                 #shinyalert::shinyalert(title = "Running...",
                 #                       text = "Results will appear when the Random Network Analyses are completed (do not exit browser)",
                 #                       type = "info")
                 
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
    
    selectInput("cores_boot", label = "Number of processing cores",
                choices = core_boot,
                selected = ceiling(length(core_boot) / 2)
    )
  })
  
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
                     
                     ## Run partial bootstrap networks
                     for(i in 1:length(percents))
                     {
                       if(exists(paste(percents[i]), envir = globalenv()))
                       {next
                       }else{
                         
                         # Print waiting message
                         # FOR R PACKAGE
                         shinyalert::shinyalert(title = paste("Running...\n","(Proportion of nodes remaining: ",sprintf("%1.2f", percents[i]),")",sep=""),
                                                text = "Check R Console for the Bootstrap Network Analyses Progress",
                                                type = "info")
                         
                         # FOR WEB
                         #shinyalert::shinyalert(title = paste("Running...\n","(Proportion of nodes remaining: ",sprintf("%1.2f", percents[i]),")",sep=""),
                         #                       text = "Results will appear when the Bootstrap Network Analyses are completed (do not exit browser)",
                         #                       type = "info")
                         
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
                     
                   }else{
                     
                     # Print waiting message
                     # FOR R PACKAGE
                     shinyalert::shinyalert(title = "Running...",
                                            text = "Check R Console for the Bootstrap Network Analyses Progress",
                                            type = "info")
                     
                     # FOR WEB
                     #shinyalert::shinyalert(title = "Running...",
                     #                       text = "Results will appear when the Bootstrap Network Analyses are completed (do not exit browser)",
                     #                       type = "info")
                     
                     
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
    
    # Remove all other variables from global environment
    rm(list = ls(envir = globalenv())[-match(c("resultShiny", prev.env), ls(globalenv()))], envir = globalenv())
    
  }
  )
  
}