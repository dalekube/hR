
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
mns = seq(as.Date(first.date),by="month",length=12)
mns = format(mns,"%b-%Y")

shinyApp(

  # UI Client
  ui = fluidPage(

    # Boilerplate Code
    tags$meta(name="viewport",content="width=750"),
    tags$title("hR: Workforce Planning"),
    tags$script(src="https://kit.fontawesome.com/a43c80910b.js", crossorigin="anonymous"),
    tags$style(HTML(
      "
      .rhandsontable {overflow:visible;}
      body {min-height:1600px;margin:25px;}
      td {padding-right:15px;width:auto;white-space:nowrap !important;}
      .colHeader {white-space:nowrap !important;}
      .col-sm-3 {width:auto;}
      #Calculate {margin-bottom:10px;background-color:green;color:white;}
      .smallPad {padding:5px;}
      .metric {
        width:300px;
        height:100px;
        margin:5px;
        border-radius:5px;
        border:thin black solid;
        background-color:#EAEAEA;
        display:inline-block;
        }
      .metricHeader {font-weight:bold; padding:3px;}
      .box {max-width:1000px;border-radius:5px;border:thin lightgrey solid;padding:15px;margin:20px;}
      .textBox {max-width:750px;}
      .fa-seedling {font-size:50px;}
      
      "
    )),

    div(
      
      class="box",
      
      # Header
      div(
        span(style="font-size:32px;margin-right:15px;","Workforce Planning Worksheet"),
        icon("seedling")
      ),
      
      # Description paragraph
      p(
      class="textBox",
      "This simple, interactive workforce planning worksheet allows people managers and team leaders to execute basic
      workforce planning tasks that support recruitment, team strategy, and business forecasting. Users indicate the roles
      within their team, cost per role estimates, and monthly desired headcounts. This leads to pragmatic calculations
      which provide insight into hiring needs, expected turnover, and other factors that contribute to the successful
      management of a high-performing team."
      ),
      br(),
      
      fileInput("loadWorksheet",label="Load Existing Worksheet",accept=".rds",multiple=F)
      
    ),

    div(
      class="box",
      
      # Step 1: Define team roles
      h3("Step 1: Define Team Roles"),
      p(
      class="textBox",
      "Indicate the roles that exist in your team now and the roles that
      will exist in the next 12 months. This ensures that you are planning ahead
      for all roles in your team. For example, type 'Data Analyst'."
      ),
      uiOutput("TypeRolesUI"),
      br(),
      
      # Step 2: Specify Typical Cost Per Role
      h3("Step 2: Annual Cost Per Role"),
      p(
      class="textBox",
      "Specify the typical annual cost ($) per employee in each role. This info is used
      to estimate spending over time on labor. For example, an analyst might cost the business
      about $55,000 per year."
      ),
      br(),
      rHandsontableOutput("spendHot"),
      br(),
      
      # Step 3: Add Desired Headcounts
      h3("Step 3: Add Desired Headcounts"),
      p(
      class="textBox",
      "Type-in the desired headcounts, which should reflect the number of employees,
      or FTEs, in each role at the start of the month (i.e. I need three associates working
      in my team at the start of August in order to properly manage the portfolio).",
      strong("Don't forget to periodically save your worksheet!")
      ),
      br(),
      rHandsontableOutput("hot"),
      uiOutput("downloadButtonUI"),
      br(),
      
      # Step 4: Calculate Change Metrics
      h3("Step 4: Calculate Change Metrics"),
      p(
      class="textBox",
      "Finally, press the 'Calculate' button to calculate relevant change metrics
      which help to plan ahead for the next 12 months. Use this data to develop an effective
      talent stratey and communicate your plans to others. The calculations consider the
      cost estimates and headcounts in the previous steps."
      ),
      uiOutput("calculateUI"),
      br(),
      
      div(
        style="display:inline-block;",
        
        uiOutput("NoChangeAlert",inline=T),
        uiOutput("hires",inline=T),
        uiOutput("turnover",inline=T),
        uiOutput("headChange",inline=T),
        uiOutput("totalSpending",inline=T)
        
      )
      
    )

  ),

  # Server client
  server = function(input,output,session){

    # Define reactive variables and shinyFile objects
    x = reactiveValues(
      aggs=c("Total Headcount","Total Compensation"),
      totalSpend=0
    )

    # Reactive variable to render the spend handsontable
    getSpendTable = reactive({

      # Update Cost Estimate Role UI
      new.roles = x$roles[!(x$roles %in% x$spend$Role)]
      if(length(new.roles)>0){

        new.spend = data.frame(Role=new.roles,Cost=0,stringsAsFactors=F)
        x$spend = rbind(x$spend,new.spend)

      }

      # Account for removed roles
      x$spend = x$spend[x$spend$Role %in% x$roles,]

      renderer = paste("function (instance, td, row, col, prop, value, cellProperties) {
                       Handsontable.renderers.NumericRenderer.apply(this, arguments);
                       if (col==0) {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';cellProperties.readOnly='true';}
                       else {td.align='center';}}")

      # Render the cost per role handsontable
      output$spendHot = renderRHandsontable({

        rhandsontable(x$spend,rowHeaders=NULL) %>% 
          hot_cols(renderer=renderer) %>% 
          hot_col("Cost",format="$0,000")

      })

    })

    # Reactive variable to render the headcount handsontable
    getTable = reactive({

      # Add headcount totals if there are multiple roles
      x$df = x$df[!(x$df$Role %in% x$aggs),]

      # Add headcount totals
      head.totals = sapply(x$df[-1],as.numeric)
      if(nrow(x$df)>1){
        head.totals = colSums(head.totals)
      }
      head.totals = c("Role"="Total Headcount",head.totals)

      # Add spending totals based on cost estimates and headcounts
      spend.totals = sapply(x$df[-1],as.numeric)
      spend.totals = crossprod(x$spend$Cost,spend.totals)
      spend.totals = round(spend.totals/12)

      # Save grand total for the annual spending
      x$totalSpend = sum(spend.totals)

      spend.totals = format(spend.totals,big.mark=",",format='f')
      spend.totals = as.data.frame(spend.totals)
      spend.totals = cbind("Role"="Total Compensation",spend.totals)
      colnames(spend.totals) = colnames(x$df)

      # Combine aggregates and recount table
      x$df = rbind(x$df,head.totals,spend.totals)
      x$n = nrow(x$df)
      row.names(x$df) = NULL

      # Decide which style to render the handsontable with
      m = x$n-2
      renderer = paste("function (instance, td, row, col, prop, value, cellProperties) {
                       Handsontable.renderers.NumericRenderer.apply(this, arguments);
                       if (col==0 & row<",m,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='left';cellProperties.readOnly='true';}
                       else if (row>=",m,") {td.style.background = '#F0F0F0';td.style.color = 'black';td.align='right';td.style.fontWeight='bold';cellProperties.readOnly='true';}
                       else {td.align='center';}}
                       ")

      # Render the rhandsontable object
      output$hot = renderRHandsontable({rhandsontable(x$df,rowHeaders=NULL) %>% hot_cols(renderer=renderer)})

      })

    # Observe headcount table to see if Calculate button should be visible
    observe({

      # Present the Calculate button if records exist in the table
      if(!is.null(x$df)){

        if(nrow(x$df)>0){

          output$calculateUI = renderUI({

            actionButton(inputId="Calculate",label="Calculate",icon=icon("calculator"))

          })

          output$downloadButtonUI = renderUI({

            downloadButton('saveWorksheet','Save Worksheet',style="background-color:green;color:white;margin-top:10px;")

          })

        }else{

          output$calculateUI = renderUI({""})
          output$downloadButtonUI = renderUI({""})

        }

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
      x$selected = DF$Role[!(DF$Role %in% x$aggs)]
      x$n = nrow(x$df)
      x$roles = x$selected
      x$spend = SPEND

      # Update the role input
      updateSelectizeInput(
        session=session,
        inputId="TypeRoles",
        choices=x$selected,
        selected=x$selected
      )

      # Render the handsontable
      getSpendTable()
      getTable()

    })

    # Save the worksheet
    output$saveWorksheet = downloadHandler(

      filename = "workforce-plan.rds",
      content = function(file) {

        DF = hot_to_r(input$hot)
        SPEND = hot_to_r(input$spendHot)
        save(DF,SPEND,file=file)

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

      x$roles = sort(input$TypeRoles)

      # Account for added roles
      x$roles_add = x$roles[!(x$roles %in% x$df$Role)]
      if(length(x$roles_add)>0){

        df.add = data.frame(Role=x$roles_add,stringsAsFactors=F)
        df.add[mns] = 0
        x$df = rbind(x$df,df.add)

      }

      # Account for removed roles
      x$df = x$df[x$df$Role %in% x$roles,]

      # Render the cost estimate handsontable
      getSpendTable()

      # Render the headcount handsontable
      getTable()

    })

    # Account for changes to the hot table
    observeEvent(input$hot,{

      x$df = hot_to_r(input$hot)
      getTable()

    })

    # Account for changes to the spend Hot table
    observeEvent(input$spendHot,{

      x$spend = hot_to_r(input$spendHot)
      getSpendTable()
      getTable()

    })

    # Calculate descriptive metrics when the 'Calculate' button is pressed
    observeEvent(input$Calculate,{

      m = hot_to_r(input$hot)
      cols = m$Role
      m = t(m[-1])
      colnames(m) = cols
      m = data.table(m,keep.rownames=T)
      m = m[,!(colnames(m) %in% x$aggs),with=F]
      h = m[c(1,nrow(m)),-1]
      cols = colnames(m)[-1]
      m[,(cols):=lapply(.SD,as.numeric),.SDcols=cols]
      m[,(cols):=lapply(.SD,function(z) z-c(NA,z[-.N])),.SDcols=cols]
      m = melt.data.table(m,na.rm=T,id.vars="rn")
      m = m[value!=0]

      if(nrow(m)>0){

        output$NoChangeAlert = renderUI({""})
        m[,valuePrint:=paste0("<strong>",value,"</strong>")]

        # expected hires
        hires = m[value>0]
        if(nrow(hires)>0){

          hires[,value:=NULL]

          output$hires = renderUI({

            div(
              class="metric",
              div(
                class="metricHeader",
                "Expected Hires Per Month:"
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

          turnover[,value:=NULL]

          output$turnover = renderUI({

            div(
              class="metric",
              div(
                class="metricHeader",
                "Expected Turnover Per Month:"
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
          h$Change = paste0("<strong>",h$Change,"</strong>")
          h$Percent = paste0("<strong>",h$Percent,"</strong>")

          output$headChange = renderUI({

            div(
              class="metric",
              div(
                class="metricHeader",
                "Expected 12-Month Headcount Change:"
              ),
              div(
                class="smallPad",
                HTML(kable(h,col.names=NULL,row.names=F,format="html",escape=F))
                )

            )

          })

        }else{

          output$headChange = renderUI({""})

        }

        # Total Compensation
        if(x$totalSpend>0){

          total = paste0("$",format(x$totalSpend,big.mark=","))

          output$totalSpending = renderUI({

            div(
              class="metric",
              div(
                class="metricHeader",
                "Expected Total Annual Compensation:"
              ),
              div(
                class="smallPad",
                HTML(paste0("<span style='font-weight:bold'>",total,"</span"))
              )

            )

          })

        }else{

          output$totalSpending = renderUI({""})

        }

      }else{

        output$hires = renderUI({""})
        output$turnover = renderUI({""})
        output$headChange = renderUI({""})
        output$totalSpending = renderUI({""})
        output$NoChangeAlert = renderUI({

          div(style="color:red;","There aren't any changes in headcounts. There is nothing to analyze.")

        })

      }

    })

  }

)
