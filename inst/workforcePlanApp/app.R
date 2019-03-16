
## Shiny Workforce Planning Worksheet
## created by Dale Kube (dkube@uwalumni.com)
## part of the hR package available in CRAN

suppressMessages(library(shiny))
suppressMessages(library(rhandsontable))
suppressMessages(library(data.table))
suppressMessages(library(knitr))

# Initialize dummy global variables to pass CRAN tests
value=NULL
variable=NULL
Total=NULL

# Define initial date attributes
current = as.POSIXlt(Sys.Date())
first.month = current$mon+1
first.month = sprintf("%02d",first.month)
first.year = format(Sys.Date(),"%Y")
first.date = paste(first.year,first.month,"01",sep="-")
mns = seq(as.Date(first.date),by="month",length=13)
mns = format(mns,"%b-%Y")

shinyApp(
  
  # UI Client
  ui = fluidPage(
    
    # Boilerplate Code
    tags$meta(name="viewport",content="width=750"),
    tags$title("hR: Workforce Planning"),
    tags$head(HTML("<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.6.3/css/all.css' integrity='sha384-UHRtZLI+pbxtHCWp1t77Bi1L4ZtiqrqD80Kn4Z8NTSRyMA2Fd33n5dQ8lWUE00s/' crossorigin='anonymous'>")),
    tags$style(HTML(
      ".fixedWidth {width:750px;}
      .rhandsontable {overflow:visible;}
      body {min-height:1200px;margin:25px;}
      td {padding-right:15px;width:auto;white-space:nowrap !important;}
      .colHeader {white-space:nowrap !important;}
      .col-sm-3 {width:auto;}
      #Calculate {margin-bottom:10px;}
      .metricsBox1 {width:auto;height:auto;display:inline-block;border-radius:5px;margin:10px;}
      .metricsBox2 {color:white;width:auto;font-size:20px;}
      .smallPad {padding:5px;}"
    )),
    
    # Header
    h2("Workforce Planning Worksheet"),
    
    # Description paragraph
    p(
      class="fixedWidth",
      "This simple, interactive workforce planning worksheet allows people managers and team leaders to execute basic
      workforce planning tasks that support recruitment, team strategy, and business forecasting. Users must indicate the roles 
      within their team and monthly desired headcounts. This leads to pragmatic calculations which provide insight into hiring needs, 
      expected turnover, and other factors that contribute to the successful management of a high-performing team."
    ),
    br(),

    fileInput("loadWorksheet",label="Load Existing Worksheet",accept=".rds",multiple=F),
    hr(),
    
    # Step 1: Define team roles
    h3("Step 1: Define Team Roles"),
    p(
      class="fixedWidth",
      "Type-in (1) the roles that exist in your team now and 
      (2) the roles that will exist in the next 12 months. This ensures that you are
      planning ahead for all roles in your team. Each entry will create a new row in the table in Step 2."
    ),
    uiOutput("TypeRolesUI"),
    hr(),
    
    # Step 2: Add Desired Headcounts
    h3("Step 2: Add Desired Headcounts"),
    p(
      class="fixedWidth",
      "Type-in the desired headcounts, which should reflect the number of employees,
      or FTEs, in each role at the start of the month (i.e. I need three associates working
      in my team at the start of August in order to properly manage the portfolio).",
      strong("Don't forget to periodically save your worksheet!")
    ),
    br(),
    rHandsontableOutput("hot"),
    uiOutput("downloadButtonUI"),
    hr(),
    
    # Step 3: Calculate Change Metrics
    h3("Step 3: Calculate Change Metrics"),
    p(
      class="fixedWidth",
      "Press the 'Calculate' button to calculate relevant change metrics which help to plan ahead for
      the next 12 months. Use this data to develop an effective talent stratey.
      The calculations consider the headcounts in the previous table."
    ),
    uiOutput("calculateUI"),
    div(
      style="display:flex;align-items:top;margin-top:10px;",
      uiOutput("NoChangeAlert"),
      uiOutput("hires"),
      uiOutput("turnover"),
      uiOutput("headChange")
    )
    
  ),
  
  # Server client
  server = function(input,output,session){
    
    # Define reactive variables and shinyFile objects
    x = reactiveValues()
    
    # Render the handsontable
    getTable = reactive({
      
      # Render the table
      output$hot = renderRHandsontable({
        
        m = x$n-1
        if(nrow(x$df)>1){
          
          renderer = paste("function (instance, td, row, col, prop, value, cellProperties) {
                           Handsontable.renderers.TextRenderer.apply(this, arguments);
                           if (col==0 & row!=",m,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';cellProperties.readOnly='true';} 
                           else if (row==",m,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='center';td.style.fontWeight='bold';cellProperties.readOnly='true';} 
                           else {td.align='center';}}
                           ")
          }else{
  
              renderer = paste("function (instance, td, row, col, prop, value, cellProperties) {
                         Handsontable.renderers.TextRenderer.apply(this, arguments);
                         if (col==0) {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';cellProperties.readOnly='true';}
                         else {td.align='center';}}
                         ")
              }
        
        # Render the rhandsontable object
        rhandsontable(x$df) %>% hot_cols(renderer=renderer)
        
        })
      
      # Present the Calculate button if records exist in the table
      if(nrow(x$df)>0){
        
        output$calculateUI = renderUI({
          
          actionButton(
            inputId="Calculate",
            label="Calculate",
            icon=icon("calculator")
          )
          
        })
        
        output$downloadButtonUI = renderUI({
          
          downloadButton('saveWorksheet','Save Worksheet',style="background-color:green;color:white;margin-top:10px;")
          
        })
        
        }else{
          
          output$calculateUI = renderUI({""})
          output$downloadButtonUI = renderUI({""})
          
        }
      
      })
    
    # Load Existing Worksheet
    observeEvent(input$loadWorksheet,{
        
      # Load the worksheet file (".rds")
      load(file=as.character(input$loadWorksheet$datapath))
      
      # Remove old months
      DF = DF[c(1,which(colnames(DF) %in% mns))]
      
      # Update reactive variables
      x$df = DF
      x$selected = DF$Role[DF$Role!="Total"]
      x$n = nrow(x$df)
      
      # Update the role input
      updateSelectizeInput(
        session=session,
        inputId="TypeRoles",
        choices=x$selected,
        selected=x$selected
      )
      
      # Render the handsontable
      getTable()
      
    })
    
    # Save the worksheet
    output$saveWorksheet = downloadHandler(
      
      filename = "workforce-plan.rds",
      content = function(file) {
        
        DF = hot_to_r(input$hot)
        save(DF,file=file)
        
        }
      
    )
    
    # Role Input UI
    observe({
      
      output$TypeRolesUI = renderUI({
        
        selectizeInput(
          inputId="TypeRoles",
          label="Type-In Roles",
          multiple=T,
          choices=x$selected,
          selected=x$selected,
          options=list(
            create=T,
            plugins=list('remove_button','drag_drop')
          )
        )
        
      })
      
    })
    
    # Account for role additions and removals
    observeEvent(input$TypeRoles,{
      
      x$roles = input$TypeRoles
      x$roles = sort(x$roles)
      
      # Account for added roles
      x$roles_add = x$roles[!(x$roles %in% x$df$Role)]
      if(length(x$roles_add)>0){
        
        df.add = data.frame(Role=x$roles_add,stringsAsFactors=F)
        df.add[mns] = 0
        x$df = rbind(x$df,df.add)
        
      }
      
      # Account for removed roles
      x$df = x$df[x$df$Role %in% x$roles,]
      
      # Add totals if there are multiple roles
      if(nrow(x$df)>1){
        
        totals = sapply(x$df[-1],as.numeric)
        totals = c("Role"="Total",colSums(totals))
        x$df = rbind(x$df,totals)
        x$n = nrow(x$df)
        row.names(x$df) = NULL
        row.names(x$df)[x$n] = ""
        
      }
      
      # Render the handsontable
      x$n = nrow(x$df)
      getTable()
      
    })
    
    # Account for changes to the hot table
    observeEvent(input$hot,{
      
      # Add totals if there are multiple roles
      x$new = hot_to_r(input$hot)
      x$new = x$new[x$new$Role!="Total",]
      if(nrow(x$new)>1){
        
        totals = sapply(x$new[-1],as.numeric)
        totals = c("Role"="Total",colSums(totals))
        x$df = rbind(x$new,totals)
        x$n = nrow(x$df)
        row.names(x$df) = NULL
        row.names(x$df)[x$n] = ""
        
      }
      
      # Render the handsontable
      getTable()
      
    })
    
    # Calculate descriptive metrics when the 'Calculate' button is pressed
    observeEvent(input$Calculate,{
      
      m = hot_to_r(input$hot)
      cols = m$Role
      m = t(m[-1])
      colnames(m) = cols
      m = data.table(m,keep.rownames=T)
      if("Total" %in% cols){
        m[,Total:=NULL]
      }
      h = m[c(1,nrow(m)),-1]
      cols = colnames(m)[-1]
      m[,(cols):=lapply(.SD,as.numeric),.SDcols=cols]
      m[,(cols):=lapply(.SD,function(z) z-c(NA,z[-.N])),.SDcols=cols]
      m = melt.data.table(m,na.rm=T,id.vars="rn")
      m = m[value!=0]
      
      if(nrow(m)>0){
        
        output$NoChangeAlert = renderUI({""})
        m[,variable:=paste0("<span title='",variable,"'>",variable,"</span>")]
        
        # expected hires
        hires = m[value>0]
        if(nrow(hires)>0){
          
          output$hires = renderUI({
            
            div(
              class="metricsBox1",
              style="border: thin #52BE80 solid;",
              div(
                class="metricsBox2 smallPad",
                style="background-color:#52BE80;",
                "Expected Hires"
              ),
              div(
                class="smallPad",
                HTML(kable(hires,col.names=NULL,format="html",escape=F))
              )
              
            )
            
          })
          
        }else{
          
          output$hires = renderUI({""})
          
        }
        
        # expected turnover
        turnover = m[value<0]
        
        if(nrow(turnover)>0){
          
          output$turnover = renderUI({
            
            div(
              class="metricsBox1",
              style="border: thin #F5B041 solid;",
              div(
                class="metricsBox2 smallPad",
                style="background-color:#F5B041;",
                "Expected Turnover"
              ),
              div(
                class="smallPad",
                HTML(kable(turnover,col.names=NULL,format="html",escape=F))
              )
              
            )
            
          })
          
        }else{
          
          output$turnover = renderUI({""})
          
        }
        
        # Headcount change
        h = as.data.frame(t(h),stringsAsFactors=F)
        h$V1 = as.numeric(h$V1)
        h$V2 = as.numeric(h$V2)
        h$Change = h$V2-h$V1
        h$Percent = 100*(h$Change/h$V1)
        h$Role = row.names(h)
        h = h[h$Change!=0,]
        
        if(nrow(h)>0){
          
          h$Percent = sprintf("%1.1f%%",h$Percent)
          h$Sign = ifelse(h$Change>0,"+","")
          h$Percent = paste0("(",h$Sign,h$Percent,")")
          h = h[c("Role","Change","Percent")]
          
          output$headChange = renderUI({
            
            div(
              class="metricsBox1",
              style="border: thin #85C1E9 solid;",
              div(
                class="metricsBox2 smallPad",
                style="background-color:#85C1E9;",
                "Expected 12-Month Headcount Change"
              ),
              div(class="smallPad",HTML(kable(h,col.names=NULL,row.names=F,format="html",escape=F)))
              
            )
            
          })
          
        }else{
          
          output$headChange = renderUI({""})
          
        }
        
      }else{
        
        output$hires = renderUI({""})
        output$turnover = renderUI({""})
        output$headChange = renderUI({""})
        output$NoChangeAlert = renderUI({
          
          div(style="color:red;","There aren't any changes in headcounts. There is nothing to analyze.")
          
        })
        
      }
      
    })
    
  }
  
)
