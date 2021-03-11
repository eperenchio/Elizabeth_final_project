#library(ggmap)
#library(shiny)
#library(sf)
#library(RColorBrewer)
#library(tidyverse)

huge_df<- read.csv("data/combined_countries/combined.csv")
world_df<- map_data("world")

#rename world column to prepare for merging
world_df<- fortify(world_df)
world_df<- world_df %>%
  rename(REGION_NAME = region)

#change formatting of huge_df to prepare for joining
huge_df$DATE<- as.Date(huge_df$DATE, format = "%m/%d/%y")
huge_df = huge_df[-1,]
new_huge_df<- full_join(world_df, huge_df, by ="REGION_NAME", copy=TRUE)

#make sure all lat & long are there
joined_df<- new_huge_df %>%
  full_join(world_df, by=c("long", "lat", "group"))

#create a smaller df with the columns needed for map
co2_2020_df<- joined_df %>%
  select(REGION_NAME.x, DATE, long, lat, group, subregion.x, TOTAL_CO2_MED)

co2_2020_df$TOTAL_CO2_MED = as.numeric(as.character(co2_2020_df$TOTAL_CO2_MED))
co2_2020_df<-fortify(co2_2020_df)

#new column with color category...
co2_2020_df$percent<- ifelse(co2_2020_df$TOTAL_CO2_MED <=-0.01 & co2_2020_df$TOTAL_CO2_MED > -1, "Less than 1%",
                             ifelse(co2_2020_df$TOTAL_CO2_MED <=-1 & co2_2020_df$TOTAL_CO2_MED > -2, "1-2%",
                                    ifelse(co2_2020_df$TOTAL_CO2_MED <= -2 & co2_2020_df$TOTAL_CO2_MED > -3, "2-3%",
                                           ifelse(co2_2020_df$TOTAL_CO2_MED <= -3 & co2_2020_df$TOTAL_CO2_MED > -4, "3-4%",
                                                  ifelse(co2_2020_df$TOTAL_CO2_MED <= -4 & co2_2020_df$TOTAL_CO2_MED > -5, "4-5%", 
                                                         ifelse(co2_2020_df$TOTAL_CO2_MED <= -5 & co2_2020_df$TOTAL_CO2_MED > -6, "Above 5%", no = NA))))))

#create a WIDE df by turing columns to dates & fill with percent range
wide_co2_2020_df<- co2_2020_df%>%
  select(REGION_NAME.x, DATE, long, lat, group, percent) %>%
  pivot_wider(names_from = DATE, 
              values_from = percent,
              values_fill = list(value="NA"),
              values_fn = list)

#function for setting the aesthetics of the plot
my_theme<- function (){
  theme_bw() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          strip.text = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          legend.position = "right",
          panel.border = element_blank(), 
          strip.background = element_rect(fill = 'white', colour = 'white'))
}

elizabeth_server<- function(input, output){
  sliderValues<- reactive({
    co2_2020_df(
      name = "Date:",
      value = input$DATE
    )
  })
  output$map<- renderPlot({
    map_subset<- co2_2020_df[co2_2020_df$DATE == input$date, ]
    map<- ggplot()+
      geom_polygon(data=map_subset, mapping=aes(x = long, y = lat, group = group, fill = as.character(percent)))+
      geom_path(data=world_df, mapping=aes(x = long, y = lat, group = group), color = "grey", size = .3)+
      coord_map(xlim=c(-180,180)) +
      labs(
        x = NULL,
        y = NULL,
        title = NULL,
        fill= "Percent Change in Global CO2 Levels (Metric Tons))"
      )+
      my_theme()+
      scale_fill_brewer(palette="Reds", direction = -1)
    return(map)
  })
}
  
  
  
colors = c("Less than 1%"="lightpink", "1-2%"="lightcoral", "2-3%"="brown1", "3-4%"="red2", "4-5%"="firebrick", "Above 5%"="darkred")  
  
  
  output$max_country_text<- renderText({
    paste("This map shows the change in CO2 emissions on", input$date, ". 
            Since lockdown dates and restrictions varied from country to country, 
            my goal was to evaluate which countries had the biggest reduction in CO2 
            emissions on different dates in 2020. From interacting with this map, one
            can tell that CO2 emissions declined globally during late March & most of April,
            but emissions eventually rose once countries came out of lockdown.")
  })
  width = 800
  height = 550
  return(map)
} 
