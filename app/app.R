#### To do:
# <done> Add Remove all button 
# <done> Add Use Date checkbox
# Handle students who dropped (right now, if there is no payment, the student does not show up)
#     - put dropped students at end?
# <almost> Keep cells to two rows maximum 
#   <set the max-width of last column to handle large notes>
# <done> Reorder the dropdown row 
# <done> Order table by column
# Add in all students who completed course (need Bayes, Res, MLE)
# PAFBC has 1 seat but has never used it -- so they don;t show up

library(shiny);
library(shinyjs);
library(DT);
library(package="readxl");

# https://qfcatmsu.github.io/Online-Class-Info/data/Partner_Info.xlsx
#a = read.delim("../data/a.txt");
classEnroll = readRDS(file = "../data/QFC_ClassEnroll.rds");
partnerInfo = read_excel(path="../data/Partner_Info.xlsx");
classInfo = readRDS(file = "../data/QFC_ClassInfo.rds");
freeSpotDF = readRDS(file = "../data/QFC_FreeSpotsDF.rds");
partnerSpots = readRDS(file = "../data/QFC_PartnerSpots.rds");


# Define UI for application that draws a histogram
ui = fluidPage(

  ### Can you treat panels as variables?
  ### Print out the number of seats given to each partner each year 
  
  # Application title
  titlePanel("QFC Online Classes"),
  
  tags$head(
    tags$link(rel="stylesheet", type="text/css", 
              href="styles.css")  # in www folder
  ),
  
  shinyjs::useShinyjs(),   ## need this to use shinyjs on server side...
  
  #### Is a main panel needed? Doubt it..
  ## Each row has 12 columns...
#  mainPanel(width=13,
    # Set up tabs
  tabsetPanel(type = "tabs",
    ### First Tab: Classes
    tabPanel("Classes",  
      ### Row 1 in Classes -- dropdown menus
      fluidRow(
        ### This row has 3 objects...
        column(width=3, 
               selectInput("className", label = "QFC Classes", multiple = TRUE,
                           selected = "Select an option",
                           choices = names(classEnroll))),
        column(width=1,
               actionButton(inputId ="clearList",
                            label=HTML("<span style='font-size: 10px;'>Remove<br>All</span>"))),
        column(width=1,
               checkboxInput(inputId ="useDate", 
                            label="Filter by dates",
                            value=FALSE)),
        column(width=3,
               dateInput(inputId = "startDate1",
                         label= "Start",
                         min = "2006-02-01",
                         max=Sys.Date(), 
                         value="2016-06-28")),
        column(width=3,
               dateInput(inputId = "endDate1",
                         label= "End",
                         min = "2006-02-01",
                         max=Sys.Date(), 
                         value=Sys.Date()))
      ),
      ### Row 2 in Classes -- table for classes -- and other random things
      fluidRow(width=12,
              textOutput("totalNum"),
              # tableOutput("table")
              # DT::dataTableOutput("table", options=list(pageLength = 100))
              DT::dataTableOutput("table")
      ),
    ),
    ### Second Tab: Partners
    tabPanel("Partners",   
      ### Row 1 in Partners -- dropdown menus 
      fluidRow(
        column(width=5, 
             selectInput("partnerInfo", label = "Free Class Info", 
                         choices = c("Partner Info"="partnerInfo",
                                     "Spot Given"="freeSpotDF", 
                                     "Spots Used"="partnerSpots"))),
        ### Can these date select be copied????
        column(width=3,
               dateInput(inputId = "startDate2",
                         label= "Start",
                         min = "2006-02-01",
                         max=Sys.Date(), 
                         value="2021-06-28")),
        column(width=3,
               dateInput(inputId = "endDate2",
                         label= "End",
                         min = "2006-02-01",
                         max=Sys.Date(), 
                         value=Sys.Date())) 
      ),      
      ### Row 2 in Classes -- table for partner info
      fluidRow(width=12,
               DT::dataTableOutput("table2")
            #   tableOutput("table2"),
      ),
    ),
    ### Second Tab: Partners
    tabPanel("Info",
             mainPanel(p("Non-MSU students need to get an ", 
                         HTML("<a target='_blank' href='https://tech.msu.edu/msu-guest-account/'>MSU Guest Account</a>"),
                         "The Guest Account ID is the same as the email they
                used to get the account."),
             p("To give non-MSU students online MSU library (LER) access, you need to email the MSU ID Office
                IDOffice@msu.edu with the MSU Guest Account of the student.  An example email:"),
             p("Can you give library access the following student taking our non-credit classes starting today and ending in 4 months:
                 
                 Can you give library access the following people taking our non-credit classes starting 12/12 and ending 4/14:

Name                                    MSU Guest Account:
Tamblyn, Jillian                  Jillian.Tamblyn@gov.bc.ca
Lake, Colin                           colin.lake@ontario.ca
")
             )),    
  ),
)
#)

server = function(input, output, session) {
  
 # shinyjs::disable("startDate1"); shinyjs::disable("endDate1");
  
  observeEvent(input$clearList, {
    updateSelectizeInput(session, "className", selected=character(0));
  });
  
   observeEvent(input$useDate, {
     if (input$useDate == TRUE)
     {
       shinyjs::show("startDate1");
       shinyjs::show("endDate1");
     }
     else
     {
       shinyjs::hide("startDate1");
       shinyjs::hide("endDate1");       
     }
   });
  
  listenClassSelect = reactive({
    list(input$className, input$startDate1, input$endDate1, input$useDate)
  });
  
  listenPartnerSelect = reactive({
    list(input$partnerInfo, input$startDate2, input$endDate2);
  })
  
  observeEvent(listenClassSelect(),
  { 
    numSelected = length(input$className);
    if(numSelected == 0)
    {
      ### Need to make sure everything is cleared -- otherwise you get an error
      output$table = DT::renderDataTable(NULL);
      output$totalNum = renderText("Total: 0");
    }
    else
    {
      classTable = classEnroll[[input$className[1]]];
      if(numSelected > 1)  # need to add class name to table
      {
        ### Add column giving the name of the class
        classTable = cbind(input$className[1], classTable);
        colnames(classTable)[1] = "Class Name";
        
        ### Append each class to the table
        for(i in 2:numSelected)
        {
          newClass = cbind(input$className[i],  classEnroll[[input$className[i]]]);
          colnames(newClass)[1] = "Class Name";
          classTable = rbind(classTable, newClass);
        }
      }
      
      if (input$useDate == TRUE)
      {
        ### Get start and end date:
        startDate = as.Date(input$startDate1); 
        endDate = as.Date(input$endDate1);
  
        ### Get date column from table
        dates = as.Date(classTable$Date_SignUp);
        range = which(dates >= startDate & dates <= endDate);
  
        ### Subset table based on date
        classTable = classTable[range,];
      }
      
      numStudents = nrow(classTable);
      numPartner = length(which(!is.na(classTable$QFC_Partner)));
      payment = sum(as.numeric(classTable$Amount), na.rm=TRUE);
      numFree = length(grep("Free", classTable$Payment_Type));
                       
      # output$table = renderTable(classTable);
      output$table = DT::renderDataTable(classTable, options=list(paging=FALSE));
      output$totalNum = renderText(paste("#Student:", numStudents,
                                         "#Partners:", numPartner,
                                         "Total Payment:", payment,
                                         "Number Free:", numFree));
    }
  })
  
  observeEvent(listenPartnerSelect(),
  { 
    ### Get start and end date
    startDate = as.Date(input$startDate2); 
    endDate = as.Date(input$endDate2);
    
    partnerInfo = get(input$partnerInfo);
    if("Date" %in% colnames(partnerInfo))
    {
      dates = as.Date(partnerInfo$Date);
      range = which(dates >= startDate & dates <= endDate);
      shinyjs::show("startDate2");
      shinyjs::show("endDate2");
      
      reducedTable = partnerInfo[range,];
   #   output$table2 = renderTable(reducedTable[order(reducedTable$Date, decreasing = TRUE),]);
      output$table2 = DT::renderDataTable(reducedTable[order(reducedTable$Date, decreasing = TRUE),],
                                          options=list(paging=FALSE));
    }
    else
    {
      shinyjs::hide("startDate2");
      shinyjs::hide("endDate2");
      
 #     output$table2 = renderTable(partnerInfo, rownames = TRUE);
      output$table2 = DT::renderDataTable(partnerInfo, rownames = TRUE,
                                          options=list(paging=FALSE));
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
