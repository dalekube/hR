
#' @title workforcePlan
#' @description Launch an interactive workforce planning worksheet that helps
#' team leaders and managers to plan for hiring and other changes to the workforce.
#' @param file The name of an existing data set from a workforcePlan. If an existing data set does
#' not exist, a new workforcePlan will be created from scratch.
#' @import shiny
#' @export
#' @examples
#' workforcePlan(plan="test.rds")

workforcePlan = function(plan){
  
  # Load necessary packages
  require(shiny)
  require(shinyjs)
  require(rhandsontable)
  require(data.table)
  require(shinyWidgets)
  require(knitr)
  
  # Define initial data frame to store all sheet attributes
  plan=paste0(plan,".rds")
  if(file.exists(plan)){
    load(plan)
    selected=sort(DF$Role[DF$Role!="Total"])
    
  }else{
    selected=NULL
  }
  current = as.POSIXlt(Sys.Date())
  first.month = current$mon+1
  first.month = sprintf("%02d",first.month)
  first.year = format(Sys.Date(),"%Y")
  first.date = paste(first.year,first.month,"01",sep="-")
  mns = seq(as.Date(first.date),by="month",length=13)
  mns = format(mns,"%b-%Y")
  
  # Start Shiny app
  shinyApp(
    
    # UI client
    ui = fluidPage(
      
      # Boilerplate Code
      useShinyjs(),
      useShinydashboard(),
      tags$title("hR: Workforce Planning Worksheet"),
      tags$head(HTML("<link rel='stylesheet' href='https://use.fontawesome.com/releases/v5.6.3/css/all.css' integrity='sha384-UHRtZLI+pbxtHCWp1t77Bi1L4ZtiqrqD80Kn4Z8NTSRyMA2Fd33n5dQ8lWUE00s/' crossorigin='anonymous'>")),
      tags$style(HTML(
        ".fa-check {color:green;}
        .fixedWidth {width:750px;}
        .rhandsontable {overflow:visible}
        body {min-height:1500px;}
        td {padding-right:15px;width:auto;}
        .col-sm-3 {width:auto;}"
      )),
      
      # Header
      h2("Workforce Planning Worksheet"),

      # Description paragraph
      p(
        class="fixedWidth",
        "This interactive, open-source workforce planning worksheet is designed for people managers and team leaders
        who are responsible for recruitment, team strategy, and business forecasting. Users are able to indicate the roles 
        within their team and fill in desired headcounts. This leads to pragmatic and useful calculations that provide insight into hiring needs, 
        expected turnover, and other factors that contribute to the successful management of a high-performing team."
        ),
      
      # Button to save the worksheet
      div("Don't forget to periodically save your work!",style="color:red;"),
      actionButton(
        inputId="save",
        label="Save Worksheet",
        icon=icon("save")
      ),
      hidden(
        tags$i(
          id='saveIcon',
          class='fas fa-spinner fa-spin'
        ),
        span(
          id="savePlan",
          HTML(paste0("Worksheet saved as <b>",plan,"</b>"))
        )
      ),
      
      hr(),
      
      # Step 1: Define team roles
      h3("Step 1: Define Team Roles"),
      p(
        class="fixedWidth",
        "Type-in (1) the roles that exist in your team now and 
        (2) the roles that will exist in the next 12 months. This ensures that you are
        planning ahead for all roles in your team. Each entry will create a new row in the table in Step 2."
      ),
      selectizeInput(
        inputId="TypeRoles",
        label="Type-In Roles",
        multiple=T,
        choices=selected,
        selected=selected,
        options=list(
          create=T,
          plugins=list('remove_button','drag_drop')
          )
      ),
      hr(),
      
      # Step 2: Add Desired Headcounts
      h3("Step 2: Add Desired Headcounts"),
      p(
        class="fixedWidth",
        "Type-in the desired headcounts, which should reflect the number of employees,
        or FTEs, in each role at the start of the month (i.e. I need three associates working
        in my team at the start of August in order to properly manage the portfolio)."
      ),
      br(),
      rHandsontableOutput("hot"),
      br(),
      br(),
      
      hr(),
      
      # Step 3: Calculate Hiring & Expected Turnover
      h4("Step 3: Calculate Expected Hiring & Turnover"),
      p(
        class="fixedWidth",
        "Press the 'Calculate' button to calculate hiring needs and expected turnover for
        the next 12 months. Use this data to plan ahead and develop an effective talent stratey.
        The calculations consider the headcounts in the previous table."
      ),
      actionButton(
        inputId="Calculate",
        label="Calculate",
        icon=icon("calculator")
      ),
      br(),
      br(),
      uiOutput("hires",inline=T),
      uiOutput("turnover",inline=T)
      
      ),
    
    # Server client
    server = function(input, output) {
      
      x = reactiveValues()
      
      # IF DF exists, save as x$df to initialize first table
      if(exists("DF")){
        x$df = DF
      }
      
      observeEvent(input$save,{
        
        if(!is.null(input$TypeRoles)){
          
          DF = hot_to_r(input$hot)
          show("saveIcon")
          save(DF,file=plan)
          Sys.sleep(1)
          show("savePlan")
          removeClass("saveIcon","fa-spinner fa-spin")
          addClass("saveIcon","fa-check")
          Sys.sleep(3)
          hide("savePlan")
          hide("saveIcon")
          removeClass("saveIcon","fa-check")
          addClass("saveIcon","fa-spinner fa-spin")
          
        }
        
      })
      
      # Render the table and add roles when entered
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
        
        totals = lapply(x$df[-1],as.numeric)
        totals = c("Role"="Total",colSums(as.data.frame(totals)))
        x$df = rbind(x$df,totals)
        n = nrow(x$df)
        row.names(x$df) = NULL
        row.names(x$df)[n] = ""
        
        output$hot = renderRHandsontable({
          
          rhandsontable(x$df) %>%
            hot_col("Role",readOnly=TRUE) %>%
            hot_row(row=n,readOnly=TRUE) %>%
            hot_cols(renderer = paste("
                                      function (instance, td, row, col, prop, value, cellProperties) {
                                      Handsontable.renderers.TextRenderer.apply(this, arguments);
                                      if (col==0 & row!=",n-1,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';} 
                                      else if (row==",n-1,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='center';td.style.fontWeight='bold';} 
                                      else {td.align='center';}}
                                      "))
          
        })
        
      })
      
      observeEvent(input$hot,{
        
        # Add totals
        x$new = hot_to_r(input$hot)
        x$new = x$new[x$new$Role!="Total",]
        totals = lapply(x$new[-1],as.numeric)
        totals = c("Role"="Total",colSums(as.data.frame(totals)))
        x$df = rbind(x$new,totals)
        n = nrow(x$df)
        row.names(x$df) = NULL
        row.names(x$df)[n] = ""
        
        output$hot = renderRHandsontable({
          
          rhandsontable(x$df) %>%
            hot_col("Role",readOnly=TRUE) %>%
            hot_row(row=n,readOnly=TRUE) %>%
            hot_cols(renderer = paste("
                                      function (instance, td, row, col, prop, value, cellProperties) {
                                      Handsontable.renderers.TextRenderer.apply(this, arguments);
                                      if (col==0 & row!=",n-1,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';} 
                                      else if (row==",n-1,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='center';td.style.fontWeight='bold';} 
                                      else {td.align='center';}}
                                      "))
          
        })
        
      })
      
      # Calculate descriptive metrics when the 'Calculate' button is pressed
      observeEvent(input$Calculate,{
        
        m = hot_to_r(input$hot)
        cols = m$Role
        m = t(m[-1])
        colnames(m) = cols
        m = data.table(m,keep.rownames=T)
        m[,Total:=NULL]
        cols = colnames(m)[-1]
        m[,(cols):=lapply(.SD,as.numeric),.SDcols=cols]
        m[,(cols):=lapply(.SD,function(z) z-c(NA,z[-.N])),.SDcols=cols]
        m = melt.data.table(m,na.rm=T,id.vars="rn")
        m = m[value!=0]
        m[,variable:=paste0("<span title='",variable,"'>",variable,"</span>")]
        
        # expected hires
        hires = m[value>0]
        if(nrow(hires)>0){
          
          output$hires = renderUI({

              box(
                solidHeader=T,
                title="Expected Hires",
                status="success",
                width=3,
                HTML(kable(hires,col.names=NULL,format="html",escape=F))
              )
            
          })
          
        }else{
          
          output$hires = renderUI({""})
          
        }
        
        # expected turnover
        turnover = m[value<0]
        
        if(nrow(turnover)>0){
          
          output$turnover = renderUI({

              box(
                solidHeader=T,
                status="warning",
                title="Expected Turnover",
                width=3,
                HTML(kable(turnover,col.names=NULL,format="html",escape=F))
              )
            
          })
          
        }else{
          
          output$turnover = renderUI({""})
          
        }
        
      })
      
    }
    
  )
  
}

setwd("C:/Users/Dale/Downloads")
workforcePlan(plan="dales-team-2019")
