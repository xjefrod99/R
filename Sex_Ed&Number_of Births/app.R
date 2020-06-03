library(stringr)
library(tidytext)
library(tidyverse)
library(data.table)
library(rvest)
library(readxl)
library(ggplot2)
library(readr)
library(leaflet)
library(geojsonio)
library(shiny)


sex.ed<-read_excel('~/desktop/math216/Project/sexed.xlsx')

states<-geojson_read('~/desktop/math216/Project/states.json',
                     what = "sp")

states.data<-states

sex.ed2<-sex.ed

with.colors<-sex.ed2%>%
    mutate(colors=ifelse(is.na(`sex ed mandated`) & is.na(`hiv ed mandated`),
                         "neither sex education nor HIV education is mandated",
                         ifelse(is.na(`sex ed mandated`) & !is.na(`hiv ed mandated`),
                                "only HIV education is mandated",
                                ifelse(!is.na(`sex ed mandated`) & is.na(`hiv ed mandated`),
                                       "only sex education is mandated",
                                       "both sex education and HIV education is mandated"))))
with.colors2<-with.colors

with.colors2$`Be Medically Accurate`<-with.colors2$`Be Medically Accurate`%>%
    str_replace_all("X", "yes")%>%
    str_replace_all("HIV", "yes for HIV")

with.colors2$`Be Age Appropriate`<-with.colors2$`Be Age Appropriate`%>%
    str_replace_all("X", "yes")%>%
    str_replace_all("HIV", "yes for HIV")    

with.colors2$`Be Culturally Appropriate and Unbiased`<-with.colors2$`Be Culturally Appropriate and Unbiased`%>%
    str_replace_all("X", "yes")%>%
    str_replace_all("HIV", "yes for HIV")

with.colors2$`Cannot Promote Religion`<-with.colors2$`Cannot Promote Religion`%>%
    str_replace_all("X", "yes")%>%
    str_replace_all("HIV", "yes for HIV")

#View(with.colors2)


states.data@data<-left_join(states.data@data,
                            with.colors2,
                            by= c("NAME"="STATE"))

states.data@data<-states.data@data%>%
    mutate(colors2=ifelse(is.na(colors),
                          "no data available",
                          colors))

url <- 'https://inkplant.com/code/state-latitudes-longitudes'
data <- url %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="main_content_div"]/table') %>%
    .[[1]] %>%
    html_table()

#View(data)
names(data) = data[1,]
data <- data[-c(1),]
#more data
a1 <- read_csv("~/desktop/math216/CDCmarriedbirth.csv")

a1[5,2] <- "Births to unmarried women; all races"
a1[5,3] <- "Births to unmarried women; white"
a1[5,4] <- "Births to unmarried women; black"
a1[5,5] <- "Births to unmarried women; hispanic"
a1[5,6] <- "Percent of births to unmarried; all races"
a1[5,7] <- "Percent of births to unmarried; white"
a1[5,8] <- "Percent of births to unmarried; black"
a1[5,9] <- "Percent of births to unmarried; hispanic"

a2 <- a1[6:57, 1:9]
colnames(a2) <- a1[5, ]
a2[1,1] <- "United States"


data <- right_join(data, a2, by = c('State' = 'Area'))

data$`Births to unmarried women; all races` <- as.numeric(gsub(",", "", data$`Births to unmarried women; all races`, fixed = T))

data$`Births to unmarried women; white` <- as.numeric(gsub(",", "", data$`Births to unmarried women; white`, fixed = T))

data$`Births to unmarried women; black` <- as.numeric(gsub(",","", data$`Births to unmarried women; black`, fixed = T))
data$`Births to unmarried women; hispanic` <- as.numeric(gsub(",","", data$`Births to unmarried women; hispanic`, fixed = T))

#View(data)

data$Latitude <- as.numeric(data$Latitude)
data$Longitude <- as.numeric(data$Longitude)
#replace NA or make it numeric

#View(data)
states.data@data <- left_join(states.data@data, data, by = c('NAME' = "State"))
#View(states.data@data) #
ui <- fluidPage(
    selectInput(inputId = "stats", # for 
                label = "Racial and Ethnic Diversity of Births in the United States",
                choices = colnames(states.data@data[19:22])),
    leafletOutput(outputId = "graph2"),
    textOutput(outputId = "description")
)
#Shiny App begins
server <- function(input, output, session) {
    
    output$description <- renderText({ 
        paste("This map shows the demographics of births to unmarried mothers by race and ethnicity. It is noted, that
        this map cannot support a statistically significant correlation between sex education laws by state and the number of births to unmarried mothers.
        However, I think it is still interesting to explore the visualization between sex education laws and number of births in unmarried mothers.
        The states are colored in such a way to juxtapose the different categories of mandated sex education laws in the United States. 
        The size of the circles represents the number of births to unmarried women, and hovering over the circle is the exact number.
        The data utilized was downloaded from CDC website. ") 
        })
    output$graph2<- renderLeaflet({
        colors244 <- colorFactor(palette = "Set1",
                                 domain = states.data@data$colors2) 
        
        states.data%>%
            leaflet()%>%
            addTiles()%>%
            addPolygons(fillColor = ~colors244(colors2),
                        weight = 2,
                        color = "white",
                        dashArray = "3",
                        opacity = 1,
                        fillOpacity = .7)%>%
            setView(-96, 37.8,3)%>%
            addLegend(pal = colors244,
                       values = ~colors2) %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude,
                             radius = ~states.data@data[, input$stats] / (states.data@data[, 19]) *8, 
                             color = 'yellow',
                             label = ~states.data@data[, input$stats] )

    })

}
shinyApp(ui, server)

