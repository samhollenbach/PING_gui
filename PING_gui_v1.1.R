#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFiles)
library(ggplot2)

#SOURCE R SCRIPTS#
setwd("/home/common_arse/PING")
source("PING_extractor_v1.0.R")
source("PING_gc_caller_v1.1.R")
source("PING_allele_caller_v1.0.R")

#--------------------#
#--GLOBAL VARIABLES--#
#--------------------#

loci.choices <- c("2DL1", "2DL23", "2DL4", "2DL5", "2DS3", "2DS4", "2DS5", "2DP1", "3DL1", "3DS1", "3DL2", "3DL3")
count_table_present <- FALSE
plots_show<<-FALSE
finished_plots <<- FALSE
threshold_table <- data.frame(matrix(NA, nrow=15, ncol=11))
plot.counter <- 1
threshold_number <- 1
save_current_plot <<- FALSE
results_dir <- "Everything/"


##############################################################################
#--------------------------------UI BLOCK------------------------------------#
##############################################################################

ui <- shinyUI(fluidPage(
  includeCSS("PING_styles.css"),
  
  #--Title area--#
  headerPanel(h1("Pushing Immunogenetics to the Next Generation (PING)")),
  tags$div(class="topbar"),
  tags$div(class="topbar2"),
   
  
  #-----------------------------------#
  #         Main Content Area         #
  #-----------------------------------#
  
  fluidRow(
    
    column(12,class="head",
      
      style="overflow-y:scroll; max-height: 70vh",
      
      
      #-----------------------------------#
      #         Extractor Panel           #
      #-----------------------------------#
      
      column(3,
        wellPanel(class="box",
                      
          #--Select sequences--#
          h3("Read Extractor"),
          tags$div(class="intro-divider"),
          shinyDirButton('directory', label = "Choose Sequences Directory", title="Select the directory where your sequence files are stored"),
          textOutput('grab_dir'),
          hr(),
          
          #--Run Extractor--#
          actionButton("run","Run Extractor"),
          hr(),
          tags$b("Tip: Make sure you deselect the 'Automatically run' checkboxes before running the Read Extractor if you want to execute each script manually!"),
          
          #--Busy panel--#
          conditionalPanel(
            condition="output.extractor_busy == 'show'",
            tags$div(class="intro-divider"),
            tags$b("Extractor script is running..."),
            tags$div(class="holder",
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"))
          )
        )
      ),
      
      #-----------------------------------#
      #    Gene Content Caller Panel      #
      #-----------------------------------#
      
      column(3, offset=1,
        wellPanel(class="box",
                  
          #--Auto gene content caller panel--#
          h3("Gene Content Caller"),
          tags$div(class="intro-divider"),
          checkboxInput("auto_gc","Automatically run Gene Content Caller from Read Extractor Sequences", value = TRUE),
          tags$br(),
          
          #--Manual run gc panel--#
          conditionalPanel(
            condition = "input.auto_gc == false",
            tags$div(class="intro-divider"),
            shinyDirButton('ping_sequence_dir', label = "Choose Sequences Directory", title="Select directory containing the PING_sequence files from the Read Extractor"),
            textOutput('gc_dir'),
            conditionalPanel(
              condition = "input.ping_sequence_dir != null",
              hr(),
              
              actionButton("run_gc","Run PING_gc")
            ),
            
            
            
            conditionalPanel(
              condition = "output.count_table_present == true",
              tags$div(class="intro-divider"),
              tags$b("There is already a MIRA Count Table present in your working directory, would you like to re-run the Gene Content Caller from this count table?"),
              hr(),
              actionButton("recalc", "Recalculate")
            )
          ),
          
          #--Plot panel--#
          conditionalPanel(
            condition = "output.show_plots",
            tags$div(class="intro-divider"),
            actionButton("next_plot","Next Plot"),
            hr(),
            textOutput('plot_info'),
            plotOutput('plots',click="plot_click", width = "400px", height = "400px")
          ),
          #--Busy panel--#
          conditionalPanel(
            condition="output.gc_busy == 'show'",
            tags$div(class="intro-divider"),
            tags$b("Gene Content script is running..."),
            tags$div(class="holder",
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"))
          )
        )
      ),
      
      #-----------------------------------#
      #       Allele Caller Panel         #
      #-----------------------------------#
      
      column(3, offset=1,
        wellPanel(class="box",
                  
          #--Auto allele-caller panel--#
          h3("Allele Caller"),
          tags$div(class="intro-divider"),
          checkboxInput("auto_caller","Automatically run Allele Caller from Gene Content output",value = TRUE),
          #--Manual caller panel--#
          conditionalPanel(
            condition = "input.auto_caller == false",
            tags$div(class="intro-divider"),
            shinyFilesButton('combined_res', 'Choose File','Select a CombinedResults file', multiple=F),
            actionButton("run_caller","Run Allele Caller")
          ),
          
          #--Busy panel--#
          conditionalPanel(
            condition="output.caller_busy == 'show'",
            tags$div(class="intro-divider"),
            tags$b("Allele Caller script is running..."),
            tags$div(class="holder",
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"),
                     tags$div(class="circle"))
          ),
          
          conditionalPanel(
            condition = "output.show_finished == 'show'",
            tags$div(class="intro-divider"),
            tags$b("Finished running! Check your results directory to see the script output.")
          ),
          
          #--Accepted loci panel--#
          tags$div(class="intro-divider"),
          checkboxGroupInput("loci",h4("Select Loci to Analyze"),choices=loci.choices,selected=loci.choices),
          actionButton('selectAll', "Select All"),
          actionButton('deselectAll', "Deselect All")
          
        )
      )
    )
  ),
  
  #--Bottom divider--#
  tags$div(class="topbar")
  
  
))


##############################################################################
#----------------------------SERVER BLOCK------------------------------------#
##############################################################################

server <- shinyServer(function(input, output, session) {
  
  #----------------------------------------------------------------------------#
  #               SET WORKING DIRECTORIES FOR FILE CHOOSERS                    #
  #----------------------------------------------------------------------------#
  #setwd("/home/github/PING_sam")
  roots = c(wd="..")
  shinyDirChoose(input,id='directory',session=session,roots=roots)
  roots2 = c(wd="..")
  shinyDirChoose(input,id='ping_sequence_dir',session=session,roots=roots2)
  roots3 = c(wd="..")
  shinyFileChoose(input,id='combined_res',session=session,roots=roots3)
  
  
  
  #----------------------------------------------------------------------------#
  #                             REACTIVE VALUES                                #
  #----------------------------------------------------------------------------#
  #Holds all reactive values for conditional panels
  
  values <- reactiveValues()
  isolate({
    values$show_plots <- F
    values$running <- F
    values$update_plot <- F
    
    values$ex_dir <- "Pick Sequences Folder"
    
    values$running_extractor <- F
    values$running_gc <- F
    values$running_caller <- F
    
    values$all_finished <- F
  })
  
  #----------------------------------------------------------------------------#
  #                         OBSERVE BUTTON EVEN METHODS                        #
  #----------------------------------------------------------------------------#
  #Run Extractor Button
  observeEvent(input$run, {
    if(isolate(values$running_extractor) || isolate(values$running_gc) || isolate(values$running_caller)){
      cat("You cannot run this script while another script is running")
      return()
    }
    run_extractor()
  })
  
  #Run Gene Content Button
  observeEvent(input$run_gc, {
    if(isolate(values$running_extractor) || isolate(values$running_gc) || isolate(values$running_caller)){
      cat("You cannot run this script while another script is running")
      return()
    }
    gc_dir <- paste0(parseDirPath(roots2,input$ping_sequence_dir),'/')
    run_gc(gc_dir)
    
  })
  
  #Recalc Gene Content Button
  observeEvent(input$recalc,{
    if(isolate(values$running_extractor) || isolate(values$running_gc) || isolate(values$running_caller)){
      cat("You cannot run this script while another script is running")
      return()
    }
    recalc_gc()
  })
  
  #Run Allele Caller Button
  observeEvent(input$run_caller,{
    if(isolate(values$running_extractor) || isolate(values$running_gc) || isolate(values$running_caller)){
      cat("You cannot run this script while another script is running")
      return()
    }
    run_caller()
  })
  
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session = session, "loci", selected = loci.choices)
  })
  
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session = session, "loci", selected = c(""))
  })
  
  #----------------------------------------------------------------------------#
  #                       MAIN RUN SCRIPT METHODS                              #
  #----------------------------------------------------------------------------#
  
  #Run PING_extractor script
  run_extractor <- function(){
    isolate(values$running_extractor <- T)
    isolate(values$all_finished <- F)
    observe({
      session$onFlushed(extractor, once=TRUE)
    })
  }
  
  #Runs after session is flushed once animation on extractor panel is shown
  extractor <- function(){
    isolate(dir <- parseDirPath(roots,input$directory))
    dir <- paste0(dir,"/")
    if(is.null(dir)){
      return(NULL)
    }
    cat("\n\n---Running PING Extractor---\n")
    #Run PING_extractor script
    b <<- F
    tryCatch({
      ping_extractor(sample.location=dir)
    }, error = function(err){
      cat(err)
      cat("No valid sequences found. Please choose a valid sequence folder and try again.")
      isolate(values$ex_dir <- "No valid sequences found. Please choose a valid sequence folder and try again.")
      b <<- T
      isolate(values$running_extractor <- F)
      
    })
    if(b){
      return(NULL)
    }
    
    isolate(values$running_extractor <- F)
    #values$running_extractor <- F
    
    agc <- isolate(input$auto_gc)
    #Automatically run GC if option is selected
    if(agc){
      run_gc(ping.sequences = "./PING_sequences/")
    }
    
    
  }
  
  #Run PING_gc_caller main function
  run_gc <- function(ping.sequences="./PING_sequences/"){
    isolate(values$running_gc <- T)
    isolate(values$all_finished <- F)
    gc_sequence_dir <<- ping.sequences
    observe({
      session$onFlushed(gc, once=TRUE)
    })
    
  }
  
  #Runs after session is flushed once animation on gc panel is shown
  gc <- function(ping.sequences=gc_sequence_dir){
    cat("\n\n---Running PING Gene Content Caller---\n")
    
    reset_global_variables()
    #Run PING_gc_caller script
    tryCatch({
      ping_gc_caller(sample.location=ping.sequences,make.graphs = F,results.directory=results_dir)
    }, error = function(err){
      cat(ping.sequences)
      cat("No valid PING sequences found. Please choose a valid PING sequence folder and try again.")
      isolate(values$gc_dir <- "No valid PING sequences found. Please choose a valid PING sequence folder and try again.")
      b <<- T
      isolate(values$running_gc <- F)
    })
    if(b){
      return(NULL)
    }
    
    plots_show<<-TRUE
    isolate(values$show_plots <- T)
    
    cat("Creating graphs...")
    dir.create("plots/")
    
    values$running_gc <- F
  }
  
  #Run PING_gc_caller recalc function
  recalc_gc <- function(){
    cat("\n\n---Recalculating Gene Content---\n")
    isolate(values$all_finished <- F)
    #Run recalc in PING_gc_caller script
    ping_recalc(make.graphs = F,results.directory=results_dir)
    
    reset_global_variables()
    
    ac <- isolate(input$auto_caller)
    if(ac){
      observe({
        run_caller(sequences = "PING_sequences/",results.dir = results_dir)
      })
      
    }
  }
  
  #Run PING_allele_caller
  run_caller <- function(sequences="PING_sequences/", results.dir=results_dir){
    values$running_caller <- T
    isolate(values$all_finished <- F)
    caller_sequence_dir <<- sequences
    observe({
      session$onFlushed(caller, once=TRUE)
    })
  }
  
  #Runs after session is flushed once animation on caller panel is shown
  caller <- function(sequences=caller_sequence_dir, results.dir=results_dir){
    cat("\n\n---Running PING Allele Caller---\n")
    isolate({
      loci <- input$loci
      combined_res <- input$combined_results
    })
    
    
    if(!is.null(combined_res)){
      ping_allele_caller(sample.location=sequences,supported.loci=loci,ping.gc.output = file.path(combined_res),results=results.dir)
    }else{
      ping_allele_caller(sample.location=sequences,supported.loci=loci,results=results.dir, ping.gc.output = paste0(results.dir,"Combined_results.csv"))
    }
    values$running_caller <- F
    
    isolate(values$all_finished <- T)
    
    #Resets global variables to their defaults so you can re-run the script
    reset_global_variables()
  }
  
 
  
  
  #----------------------------------------------------------------------------#
  #                          PING GENE CALLER METHODS                          #
  #----------------------------------------------------------------------------#
  
  #Calls to check if MIRA_count_table is present after running GC
  output$count_table_present <- reactive({
    input$auto_gc
    input$recalc
    check_mira_table()
  })
  outputOptions(output, "count_table_present", suspendWhenHidden=FALSE)
  
  #Checs if a MIRA table exists
  check_mira_table <- function(){
    if(file.exists("MIRA_count_table.csv")){
      count_table_present <<- TRUE
    }else{
      count_table_present <<- FALSE
    }
    return(count_table_present)
  }
  
  #Normalizes MIRA_count_table counts against the 3DL3 Locus (See README for more detail)
  normalize_mira_reads <- function(sequence.list, mira.count.table) {
    for(i in 1:length(mira.count.table[1,])) {
      mira.count.table[sequence.list[i]] <- mira.count.table[sequence.list[i]]/mira.count.table[grep("3DL3", rownames(mira.count.table)),sequence.list[i]]
    }
    return(mira.count.table)
  }
  
  check_kff_results <- function(){
    if(file.exists(paste0(results_dir, "kff_results.csv"))){
      kff_results <- read.csv(paste0(results_dir, "kff_results.csv"), check.names = F)
    }
    return(kff_results)
  }
  
  #Gets KFF negative points to plot and stores them
  get_kff_neg_points <- function(results,normalized.mira.counts,plot.num){
    
    if(file.exists(paste0(results, "kff_results.csv"))){
      kff_results <- check_kff_results(results)
      # Finding the ordered colnames of mira results
      kff_sample_order <- colnames(normalized.mira.counts)[order(normalized.mira.counts[plot.num,])]
      
      # Matching mira and kff locus
      kff_locus <- rownames(normalized.mira.counts)[plot.num]
      kff_locus <- unlist(strsplit(kff_locus, "_"))[1]
      
      # Putting kff results in the same order
      kff_locus_results <- kff_results[grep(kff_locus, kff_results[,1]), kff_sample_order]
      
      # Finding all KFF neg results
      kff_neg_samples <- colnames(kff_locus_results)[grepl(0, kff_locus_results)]
      
      if(length(kff_neg_samples) != 0){
        # Converting these back into MIRA points
        mira_kff_neg_points <- normalized.mira.counts[i, kff_neg_samples]
        
        # Graphing
        return(mira_kff_neg_points)
      }
    }
  }
  
  check_kff_results <- function(){
    if(file.exists(paste0(results_dir, "kff_results.csv"))){
      kff_results <- read.csv(paste0(results_dir, "kff_results.csv"))
    }
    return(kff_results)
  }
  
  
  #----------------------------------------------------------------------------#
  #                               PLOT METHODS                                 #
  #----------------------------------------------------------------------------#
  
  #Creates the plots from the MIRA_count_table results
  create_graphs <- function(results=results_dir, threshold.file = "Resources/gc_resources/defaultThresholds.txt") {
    
    #Moved location of MIRA_count_table.csv
    mira_count_table <- read.csv(paste0(results,"MIRA_count_table.csv"),check.names = F,stringsAsFactors = F)
    
    mira_count_table <- mira_count_table[1:15,]
    
    row.names(mira_count_table) <- mira_count_table[,1]
    mira_count_table <- mira_count_table[,-1,drop=F]
    
    mira_count_table[,1] <- as.numeric(as.character(mira_count_table[,1]))
    
    sequence_list <- colnames(mira_count_table)
    
    
    #cat("\n\nNormalizing MIRA reads to 3DL3\n")
    normalized.mira.counts <- normalize_mira_reads(sequence_list, mira_count_table)
    #cat(length(normalized.mira.counts[,1]))
    return(normalized.mira.counts)
    
  }
  
  #Builds the plot
  plotInput <- function(){
    
    normalized.mira.counts <- create_graphs(results = results_dir)
    
    if(file.exists(paste0(results_dir, "kff_results.csv"))){
      kff_results <- check_kff_results()
    }
    #print(head(temp))
    x_lim <- length(normalized.mira.counts[plot.counter,])
    
    y_lim <- max(normalized.mira.counts[plot.counter,]) + 0.2
    
    plot.window(xlim = c(1, x_lim), ylim = c(0, y_lim))
    plot(as.numeric(normalized.mira.counts[plot.counter, order(normalized.mira.counts[plot.counter,])]), main = rownames(normalized.mira.counts)[plot.counter], xlab = "Sample",ylab = "Locus Ratio",pch ='o')
    
    if(file.exists(paste0(results_dir, "kff_results.csv"))){
      # Finding the ordered colnames of mira results
      kff_sample_order <- colnames(normalized.mira.counts)[order(normalized.mira.counts[plot.counter,])]
      
      # Matching mira and kff locus
      kff_locus <- rownames(normalized.mira.counts)[plot.counter]
      kff_locus <- unlist(strsplit(kff_locus, "_"))[1]
      
      #THIS LINE IS BREAKING BECAUSE OF "UNDEFINED COLUMNS" - maybe run whole script again to get a new kff_results file (could have more columns than the mira reads?)
      # Putting kff results in the same order
      kff_locus_results <- kff_results[grep(kff_locus, kff_results[,1]), kff_sample_order]
      
      # Finding all KFF neg results
      kff_neg_samples <- colnames(kff_locus_results)[grepl(0, kff_locus_results)]
      
      # Mira values
      mira_values <- as.numeric(normalized.mira.counts[plot.counter, order(normalized.mira.counts[plot.counter,])])
      
      
      if(length(kff_neg_samples) != 0){
        # Converting these back into MIRA points
        mira_kff_neg_points <- as.numeric(normalized.mira.counts[plot.counter, kff_neg_samples])
        
        # X axis values
        x_vals <- match(mira_kff_neg_points, mira_values)
        
        # Graphing
        par(new = T)
        plot(x_vals, mira_kff_neg_points, ylab = NA, xlab = NA, axes = F, col = "red", pch = 4, xlim = c(1, x_lim), ylim = c(0, y_lim))
      }
    }
    #mira_kff_neg_points <- get_kff_neg_points(results = results_dir,normalized.mira.counts = temp, plot.num = plot.counter)
    #par(new = T)
    #plot((1:length(mira_kff_neg_points)), mira_kff_neg_points, ylab = NA, xlab = NA, axes = F, col = "red", pch = 4, xlim = c(1, x_lim), ylim = c(0, y_lim))
    
    for(i in 1:length(threshold_table[plot.counter,])){
      abline(h = threshold_table[plot.counter,i], col = "red")
    }
    
    
  }
  
  #---Main plot output---#
  output$plots <- renderPlot({
    values$update_plot
    #cat("update\n")
    if(save_current_plot){
      
      save_current_plot <<- F
      #invalidateLater(100)
    }
    isolate(plotInput())
  })

  #Saves the current plot to ./plots/
  save_plot <- function(){
    if(plot.counter < 10){
      file <- paste0("plots/plot0",plot.counter,".png")
    }else{
      file <- paste0("plots/plot",plot.counter,".png")
    }
    
    isolate(ggsave(filename = file,plot = plotInput()))
    
  }
  
  #Output controlling if plots are shown
  output$show_plots <- reactive({
    
    input$next_plot
    
    #Close plots
    if(!values$show_plots && finished_plots){
      plots_show<<-FALSE
      write_thresh_file(results_dir)
      return(FALSE)
    }
    #Stop showing plots after all have been shown
    if(plot.counter>=15){
      save_plot()
      cat(paste0("Saved Plot ", plot.counter, " to /plots/\n"))
      plot.counter<<- 1
      finished_plots <<- TRUE
      plots_show<<-FALSE
      isolate(values$show_plots <- FALSE)
      return(FALSE)
    }else if(!plots_show){
      return(FALSE)
    }else{
      isolate(values$update_plot <- !values$update_plot)
      return(TRUE)
    }
  })
  #Set show plots to high priority so it will always run before set_run_recalc since they have same dependencies
  outputOptions(output,"show_plots",priority = 10)
  
  #Queue Recalc to run on next reactive flush after plots have been hidden
  #Sort of a workaround that could possibly cause errors but seems to be working fine for now
  set_run_recalc <- observe(priority = 0,{
    if(!values$show_plots && finished_plots){
      cat("Finished clicking plots, running Recalc")
      session$onFlushed(recalc_gc, once=TRUE)
      finished_plots <<- FALSE
    }
  })
  
  #Controls Next Plot click
  output$update_plot_num <- reactive({
    
    if((input$next_plot != 0) && plots_show){
      save_plot()
      cat(paste0("Saved Plot ", plot.counter, " to /plots/\n"))
      plot.counter <<- plot.counter+1
      threshold_number <<- 1
      save_current_plot <<- TRUE
      isolate(values$update_plot <- !values$update_plot)
    }
  })
  
  
  #Adds a threshold point to desired plot
  add.threshold <- observe({
    y <- input$plot_click$y
    if(is.null(y)){
      return()
    }
    threshold <- round(y,digits=3)
    threshold_table[plot.counter, threshold_number] <<- threshold
    threshold_number <<- threshold_number+1
    isolate(values$update_plot <- !values$update_plot)
  })
  
  #Compiles all marked threshold points into threshold results file
  write_thresh_file <- function(results){
    #Write thresholds to file
    thresh_columns <- c("KIR", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    cat("Writing set thresholds to New_thresholds.csv\n\n")
    write.table(threshold_table, file = "New_thresholds.txt", quote = F, sep = "\t", na = "", row.names = F, col.names = thresh_columns)
    write.table(threshold_table, file = paste0(results, "New_thresholds.txt"), quote = F, sep = "\t", na = "", row.names = F, col.names = thresh_columns)
  }
  
  #Output plot info
  output$plot_info <- renderText({
    input$next_plot
    input$plot_click
    
    return(paste0("Plot ",plot.counter,"/15"))
  })
  #Forces show_plots to update even when not shown so conditional panel can update
  outputOptions(output, "show_plots", suspendWhenHidden=FALSE)
  outputOptions(output, "update_plot_num", suspendWhenHidden=FALSE)
  
  
  #----------------------------------------------------------------------------#
  #                        TEXT OUTPUT METHODS                                 #
  #----------------------------------------------------------------------------#
  
  update_ex_dir <- observe({
    input$directory
    values$ex_dir <- substring(parseDirPath(roots,input$directory),1)
  })
  
  output$grab_dir <- renderText({
    return(values$ex_dir)
  })
  update_gc_dir <- observe({
    input$ping_sequence_dir
    values$gc_dir <- substring(parseDirPath(roots2,input$ping_sequence_dir),1)
    mira_run = FALSE
  })
  output$gc_dir <- renderText({
    return(values$gc_dir)
  })
  
  
  
  reset_global_variables <- function(){
    count_table_present <- FALSE
    plots_show<<-FALSE
    finished_plots <<- FALSE
    threshold_table <- data.frame(matrix(NA, nrow=15, ncol=11))
    plot.counter <- 1
    threshold_number <- 1
    save_current_plot <<- FALSE
  }
  
  #----------------------------------------------------------------------------#
  #                         BUSY ANIMATION OUTPUT METHODS                      #
  #----------------------------------------------------------------------------#
  
  #Sends output to conditional panel controlling Busy Animation
  output$extractor_busy <- renderText({
    if(!values$running_extractor){
      return("")
    }
    return("show")
  })
  
  output$gc_busy <- renderText({
    if(!values$running_gc){
      return("")
    }
    return("show")
  })
  
  output$caller_busy <- renderText({
    if(!values$running_caller){
      return("")
    }
    return("show")
  })
  
  output$show_finished <- reactive({
    if(!values$running_caller){
      return("")
    }
    return("show")
  })
  
  outputOptions(output, "extractor_busy", suspendWhenHidden=FALSE)
  outputOptions(output, "gc_busy", suspendWhenHidden=FALSE)
  outputOptions(output, "caller_busy", suspendWhenHidden=FALSE)
  outputOptions(output, "extractor_busy", priority=5)
  outputOptions(output, "gc_busy", priority=5)
  outputOptions(output, "caller_busy", priority=5)
  
  
})


# Run the application 
shinyApp(ui = ui, server = server)

