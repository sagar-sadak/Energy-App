library(shiny)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(dplyr)
library(sf)
library(tmap)

state_energy_prod <- read_csv("../Energy/state_data1.csv")
shapes <- read_sf("../shapes/cb_2018_us_state_500k.shp")
elec_net_gen = read_csv("../Energy/Electricity_net_generation_total(all sectors).csv")
elec_end_use = read_csv("../Energy/Electricity_end_use.csv")
state_emissions = read_csv("../Energy/state_data.csv")
state_energy_prod = read_csv("../Energy/state_data1.csv")
elec_overview = read_csv("../Energy/Electricity_overview.csv", col_types = cols(Value = col_double()))

ui <- fluidPage(
  tabsetPanel(
  tabPanel(title = "State-wise Energy Production",
             sidebarPanel(
               selectInput("source", "Choose the Energy Source",
                           selected = "coal",
                           choices = c(colnames(state_energy_prod)[4:13], "All")),
           ),
           
           mainPanel(
             tmapOutput("plot1")
           )
           ),
  
  tabPanel(title = "Electricity",
           sidebarPanel(
             h4("Electricity Production"),
             selectInput("type", "Choose the plot to be displayed",
                         choices = c(unique(elec_net_gen$Description), "All"),
                         selected = "Electricity Net Generation Total (including from sources not shown), All Sectors"),
             
             br(),
             br(),
             
             h4("Electricity Consumption"),
             checkboxGroupInput(
               "checkbox", "Choose the plots you want to include",
               choices = unique(elec_end_use$Description), 
               selected = "Electricity End Use, Total"
             )),
           
           mainPanel(
             h4("Production Plot"),
             plotlyOutput("plot2"),
             
             h4("Consumption Plot"),
             plotlyOutput("plot3")
           )
           ),
  tabPanel(title = "Emissions",
           sidebarPanel(
             selectInput("emissionType", "Choose Type of Emission",
                         selected = "CO2e",
                         choices = colnames(state_emissions)[2:8])
           ),
           mainPanel(
             tmapOutput("plot4")
           )
           ),
  tabPanel(title = "Energy Comparison Tool",
           sidebarPanel(
             helpText("Select the lower and upper years you want to compare"),
             sliderInput("slider", "Choose years", min = 1973, 
                         max = 2021, value = c(1980, 2000), sep = "")
           ),
           mainPanel(
             plotlyOutput("plot5")
           ))
  )
)

server <- function(input, output){
  output$plot1 <- renderTmap({
    
    temp = na.omit(state_energy_prod)
    start = temp[,c(1,3)]
    end = as.data.frame(sapply(temp[,seq(4,14)], function(x) ((as.double(substr(x, 1, nchar(x)-1)))/100) * start$`Net Generation (MWh)`))
    temp = cbind.data.frame(start, end)
    
    filter_out = c('15', '02', '72', '66', '69', '60', '78')
    df = shapes[!shapes$STATEFP %in% filter_out,]
    joined = inner_join(df, temp, by = c("STUSPS"="State"))
    colors <- c("Blues", "Reds","Greens","Oranges","Purples")
    title <- paste('2020', input$source,'Energy Production by State')
    if (input$source != "All") {
      joined = joined %>% select(input$source, everything())
      out = c("Geothermal", "Other Fossil")
      if (!input$source %in% out){
        tm_shape(joined) + tm_fill(input$source, style ="quantile", 
      title="Energy in MWh", palette = sample(colors,1)) + tm_borders(col="black", lwd=1.5) + 
        tm_layout(legend.outside = F, main.title= title, main.title.position = "center")
      }
      else
      {
        tm_shape(joined) + tm_fill(input$source, title="Energy in MWh", palette = sample(colors,1)) + 
          tm_borders(col="black", lwd=1.5) + 
          tm_layout(legend.outside = F, main.title= title, main.title.position = "center")
      }
    }
    else {
      joined = joined %>% select(`Net Generation (MWh)`, everything())
      tm_shape(joined) + tm_fill("Net Generation (MWh)", style = "quantile", title="Energy in MWh", palette = "Blues") + 
        tm_borders(col="black", lwd=1.5) + tm_layout(legend.outside = F, 
        main.title= '2020 Net Energy Production by State', main.title.position = "center")
    }
  })
  
  output$plot2 <- renderPlotly({
    if (input$type != "All"){
    temp = separate(elec_net_gen, YYYYMM, c("YYYY", "MM"), sep=4, convert=T) %>% 
      filter(MM == 13, Description == input$type)
    temp$Value = as.double(temp$Value)
    ggplot(temp, aes(YYYY, Value)) + geom_line(col="red") + 
      ylab("Electricity (Million Kilowatthours)") + xlab("Year") + 
      ggtitle(input$type)}
    else{
      temp = separate(elec_net_gen, YYYYMM, c("YYYY", "MM"), sep=4, convert=T) %>% 
        filter(MM == 13, Description != "Electricity Net Generation Total (including from sources not shown), All Sectors")
      temp$Value = as.double(temp$Value)
      temp$Description = str_sub(temp$Description, 33, 100)
      ggplot(temp, aes(YYYY, Value, group=Description, color=Description)) + geom_line() + 
        ylab("Electricity (Million Kilowatthours)") + xlab("Year") + 
        ggtitle("Electricity Production grouped by Sector")
    }
  })
  
  output$plot3 <- renderPlotly({
    temp = separate(na.omit(elec_end_use), YYYYMM, c("YYYY", "MM"), sep=4, convert=T) %>% 
      filter(MM == 13, Description %in% input$checkbox)
    temp$Value = as.double(temp$Value)
    ggplot(temp, aes(YYYY, Value, group=Description, color=Description)) + 
      geom_line() + ylab("Electricity (Million Kilowatthours)") + xlab("Year") + 
      ggtitle("Electricity End-Use by Sector")
  })
  
  output$plot4 <- renderTmap({
    userInput <- input$emissionType
    temp = na.omit(state_emissions)
    a = inner_join(temp, state_energy_prod)
    filter_out = c('15', '02', '72', '66', '69', '60', '78')
    df = shapes[!shapes$STATEFP %in% filter_out,]
    a$total_emission = (a[[userInput]]) * a$`Net Generation (MWh)`
    emissions = inner_join(df, a, by = c("STUSPS"="State"))
    emissions <- emissions %>% select(total_emission, everything())
    title <- paste("2020", userInput, "Emissions by State")
    tm_shape(emissions) + tm_fill("total_emission", style = "quantile", 
    title="Emissions (in pounds)", palette = "Reds") + tm_borders(col="black", lwd=1) + 
      tm_layout(legend.outside = F, main.title= title,
                main.title.position = "center")
  })
  
  output$plot5 <- renderPlotly({
    inside = c("Electricity Net Generation, Total", "Electricity End Use, Total")
    lab <- paste("Months of",input$slider[1],"and",input$slider[2])
    title <- paste("Energy Production and Consumption in the years", input$slider[1],
                   "and",input$slider[2])
    temp1 = separate(elec_overview, YYYYMM, c("YYYY", "MM"), sep=4, convert=T) %>% 
      filter(Description %in% inside, MM != 13, YYYY == input$slider[1])
    y1 = unite(temp1, YYYYMM, c("YYYY", "MM"), sep="") %>% mutate(Date = ym(YYYYMM)) %>% 
      mutate(month = format(Date, "%m"))
    y1$Description = paste(input$slider[1], y1$Description)
    
    temp2 = separate(elec_overview, YYYYMM, c("YYYY", "MM"), sep=4, convert=T) %>% 
      filter(Description %in% inside, MM != 13, YYYY == input$slider[2])
    y2 = unite(temp2, YYYYMM, c("YYYY", "MM"), sep="") %>% mutate(Date = ym(YYYYMM)) %>%
      mutate(month = format(Date, "%m"))
    y2$Description = paste(input$slider[2], y2$Description)
    
    ggplot() + geom_line(data=y1, mapping=aes(x=month, y=Value, group=Description, color=Description)) + 
      ylab("Electricity (Billion Kilowatthours)") + xlab(lab) +
      geom_line(data=y2, mapping=aes(x=month, y=Value, group=Description, color=Description), lwd = 1) +
      ggtitle(title)
    
  })
}

shinyApp(ui, server)
