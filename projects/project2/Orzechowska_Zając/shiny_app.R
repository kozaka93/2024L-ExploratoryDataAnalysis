ui <- navbarPage("bikes in NYC",
                 theme = shinytheme("flatly"),
                 tags$style(HTML("
    body {
      background-color: #fcf3e3;
    }
  ")),
                 tabPanel("User preferences",
                              navlistPanel(
                                tabPanel("Distribution of trip duration",
                                         radioButtons("plot_choice1", "Distribution of trip duration by", 
                                                      choices = list("Gender" = "gender_plot1",
                                                                     "Season" = "season_plot1",
                                                                     "Age Group" = "age_group_plot1"),
                                                      selected = "gender_plot1",
                                                      inline = TRUE),
                                         mainPanel(
                                           plotOutput("selected_plot1")
                                         ),
                                         h4("insights"),
                                         conditionalPanel(
                                           condition = "input.plot_choice1 == 'gender_plot1'",
                                           p("The distribution of travel duration for both genders is quite similar."),
                                           p("The average travel duration appears to be comparable for both genders, suggesting no significant differences in travel time preferences between women and men."),
                                           p("The distribution is slightly more dispersed for men, which may indicate greater variability in travel duration.")
                                         ),
                                         conditionalPanel(
                                           condition = "input.plot_choice1 == 'season_plot1'",
                                           p("Travel duration varies across different seasons."),
                                           p("Spring and summer have wider distributions, indicating longer travel times in these seasons, likely due to favorable weather conditions and more holidays."),
                                           p("Winter and autumn have narrower distributions, suggesting shorter travel times during these seasons.")
                                         ),
                                         conditionalPanel(
                                           condition = "input.plot_choice1 == 'age_group_plot1'",
                                           p("Travel duration varies across different age groups."),
                                           p("Younger age groups (20-39) have more varied travel durations, while older age groups (60-69 and above) have more consistent travel durations."),
                                           p("The 50-59 age group also shows some variation, but not as much as the younger groups.")
                                         )
                                ),
                                         tabPanel("Averege trip duration",
                                         selectInput("plot_choice2", "Average trip duration by", 
                                                     choices = list("Gender" = "gender_bar_plot",
                                                                    "Age Group" = "age_group_bar_plot",
                                                                    "User Type" = "usertype_bar_plot",
                                                                    "Season" = "season_bar_plot"),
                                                     selected = "gender_bar_plot"),
                                         
                                         mainPanel(
                                           plotOutput("selected_plot2")
                                         ),
                                         h4("insights"),
                                         conditionalPanel(
                                           condition = "input.plot_choice2 == 'gender_bar_plot'",
                                           p("The average travel duration is higher for women than for men."),
                                           p("This may suggest that women tend to take longer trips compared to men."),
                                           p("These differences might stem from varying travel behaviors or transportation preferences between genders.")
                                           ),
                                         conditionalPanel(
                                           condition = "input.plot_choice2 == 'age_group_bar_plot'",
                                           p("The longest average travel duration is observed in individuals aged 20-29."),
                                           p("Travel duration decreases in the 30-39, 40-49, and 50-59 age groups, then stabilizes for older age groups."),
                                           p("Individuals aged 70-79 and those over 80 have the shortest average travel durations."),
                                           p("Younger individuals may tend to take longer trips, possibly due to greater mobility and physical activity.")
                                           ),
                                         conditionalPanel(
                                           condition = "input.plot_choice2 == 'usertype_bar_plot'",
                                           p("Customers have a longer average travel duration compared to subscribers."),
                                           p("This may suggest that occasional users tend to take longer trips than regular users, who might have shorter but more frequent trips."),
                                           p("Subscribers may use the services more systematically and efficiently, leading to shorter average travel durations.")
                                         ),
                                         conditionalPanel(
                                           condition = "input.plot_choice2 == 'season_bar_plot'",
                                           p("The longest average travel duration is in spring, followed by summer."),
                                           p("Autumn has a shorter average travel duration than spring and summer, but longer than winter."),
                                           p("Winter has the shortest average travel duration."),
                                           p("This may be due to weather conditions, where warmer months (spring and summer) encourage longer trips, while colder months (winter) limit travel duration.")
                                         )
                                         ),
                                tabPanel("Usage throught the day",
                                         selectInput("filter_choice", "Choose a filter:", 
                                                     choices = list("Gender" = "gender", 
                                                                    "User Type" = "usertype", 
                                                                    "Age Group" = "age_group")),
                                         uiOutput("dynamic_ui"),
                                mainPanel(
                                  plotOutput("filtered_plot")
                                ),
                                h4("insights"),
                                conditionalPanel(
                                  condition = "input.filter_choice == 'gender'",
                                  p("In both cases, for women and men, two distinct peaks can be observed in the morning and evening hours."),
                                  p("The highest traffic is visible around 8:00 AM and 5:00-6:00 PM, suggesting that most users ride bikes to commute to work or school and then back home."),
                                  p("Both men and women exhibit similar bike usage patterns; however, the number of rides by men is generally higher than that of women during most hours."),
                                  p("In the afternoon hours, between the morning and evening peaks, the number of rides is stable but lower than during peak hours.")
                                ),
                                conditionalPanel(
                                  condition = "input.filter_choice == 'usertype'",
                                  p("Subscribers show a clear pattern with peak hours in the morning around 8:00 AM and in the evening around 5:00-6:00 PM, similar to the overall pattern observed in the gender-based analysis."),
                                  p("Customers also show a peak in the evening around 5:00 PM, but the morning peak is less pronounced compared to subscribers."),
                                  p("The overall number of rides by subscribers is higher than that of customers, suggesting that subscribers are more consistent in their usage of the bike service."),
                                  p("	Both customers and subscribers show lower activity levels during nighttime hours (approximately 11:00 PM to 5:00 AM), consistent with typical sleep patterns.")
                                ),
                                conditionalPanel(
                                  condition = "input.filter_choice == 'age_group'",
                                  p("For age group 20-30, the pattern shows clear peaks during morning hours (around 8:00 AM) and evening hours (around 5:00-6:00 PM), indicating commuting times."),
                                  p("This age group has a high volume of rides, suggesting that individuals in their 20s are frequent users of bike-sharing services, likely for commuting purposes."),
                                  p("For age group 30-40, similar to the 20-30 age group, there are distinct peaks in the morning and evening."),
                                  p("The overall number of rides is slightly lower compared to the 20-30 age group, but still significant, indicating regular usage for commuting."),
                                  p("For age group 40-50, the peaks in the morning and evening are still present but less pronounced than in younger age groups."),
                                  p("This suggests a moderate use of bike-sharing services, possibly due to different commuting habits or other transportation preferences."),
                                  p("For age group 50-60, there is a noticeable reduction in the number of rides compared to younger age groups."),
                                  p("Peaks are still observed, but they are much smaller, indicating lower overall usage of bike-sharing services."),
                                  p("For age group 60-70 the usage pattern flattens significantly, with very few rides throughout the day."),
                                  p("This age group shows minimal engagement with bike-sharing services, possibly due to physical limitations or a preference for other modes of transport."),
                                  p("For people older than 70, the number of rides is very low, with almost negligible activity throughout the day")
                                  )),
                                tabPanel("Number of rentals",
                                         radioButtons("plot_choice4", "Number of rentals by", 
                                                      choices = list("Gender" = "gender_plot4",
                                                                     "Season" = "season_plot4",
                                                                     "Age Group" = "age_group_plot4",
                                                                     "User Type" = "usertype_plot4"),
                                                      selected = "gender_plot4",
                                                      inline = TRUE),
                                         
                                         mainPanel(
                                           plotOutput("selected_plot4")
                                         ),
                                         h4("insights"),
                                         conditionalPanel(
                                           condition = "input.plot_choice4 == 'gender_plot4'",
                                           p("The number of bike rentals by males is significantly higher than that by females."),
                                           p("Males account for more than double the number of rentals compared to females. This could indicate that bike-sharing services are more popular or more frequently used by males."),
                                           p("To balance usage, bike-sharing companies might consider targeted marketing strategies to encourage more female riders."),
                                           p("Understanding the reasons behind the lower number of female users can help in making service improvements. For instance, enhancing safety features, offering women-centric promotions, or improving bike station locations could potentially increase female ridership.")
                                           ),
                                         conditionalPanel(
                                           condition = "input.plot_choice4 == 'season_plot4'",
                                           p("The highest number of rentals is observed in autumn"),
                                           p("Summer also sees a high volume of bike rentals, likely because of the warm weather and longer daylight hours."),
                                           p("Although lower than in autumn and summer, winter still has a significant number of rentals, indicating that a considerable number of users continue to use bike-sharing services despite colder weather."),
                                           p("Spring has the least bike-sharing activity among the four seasons, which might be due to variable weather conditions or other factors.")
                                         ),
                                         conditionalPanel(
                                           condition = "input.plot_choice4 == 'age_group_plot4'",
                                           p("Individuals in their 20s are the most frequent users of bike-sharing services."),
                                           p("People in their 30s also actively use bike-sharing services, though less than those in their 20s."),
                                           p("There is a noticeable decline in usage as age increases beyond 30."),
                                           p("The decline in usage suggests that older age groups are less inclined to use bike-sharing services."),
                                           p("To increase engagement across different age groups, especially older demographics, bike-sharing services might need to implement targeted improvements such as better safety features, more comfortable bikes, or incentives for older users.")
                                           ),
                                         conditionalPanel(
                                           condition = "input.plot_choice4 == 'usertype_plot4'",
                                           p("Subscribers account for the vast majority of bike rentals."),
                                           p("This indicates that regular users who have subscriptions are the primary users of the bike-sharing service."),
                                           p("Customers, or occasional users, have a significantly lower number of rentals"),
                                           p("This shows that while there are a substantial number of occasional users, they contribute much less to the overall usage compared to subscribers.")
                                         ))
                          )
                 ),
                 tabPanel("Station preferences",
                          titlePanel("Station Analysis"),
                          
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("usertype_choice", "Select User Type:", 
                                           choices = list("Customer" = "Customer", "Subscriber" = "Subscriber")),
                              sliderInput("hour_choice", "Select Hour:", 
                                          min = 0, max = 23, value = 0, step = 1, animate = TRUE)
                            ),
                            mainPanel(
                              fluidRow(
                                column(6, h4("Start Station"),plotOutput("start_station_plot")),
                                column(6, h4("End Station"), plotOutput("end_station_plot"))
                              )
                            )
                          )
                 )
)

# Definiowanie logiki serwera
server <- function(input, output) {
  output$selected_plot1 <- renderPlot({
    if (input$plot_choice1 == "gender_plot1") {
      ggplot(data1.1.1, aes(x = gender, y = tripduration)) + 
        geom_violin(fill = "#013d5a", color = "#013d5a") + 
        labs(x = "Gender", y = "Trip Duration in minutes") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        )
    } else if (input$plot_choice1 == "season_plot1") {
      ggplot(data1.1.2, aes(x = factor(season, levels = c("autumn", "winter", "spring", "summer")), y = tripduration)) + 
        geom_violin(fill = "#013d5a", color = "#013d5a") + 
        labs(x = "Season", y = "Trip Duration in minutes") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        )
    } else if (input$plot_choice1 == "age_group_plot1") {
      ggplot(data1.1.3, aes(x = age_group, y = tripduration)) + 
        geom_violin(fill = "#013d5a", color = "#013d5a") + 
        labs(x = "Age Group", y = "Trip Duration in minutes") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        )
    }
  })
  output$selected_plot2 <- renderPlot({
    if (input$plot_choice2 == "gender_bar_plot") {
      ggplot(data1.2.1, aes(x = gender, y = mean_td)) + 
        geom_col(fill = "#bdd3ce") + 
        labs(x = "Gender", y = "Mean Trip Duration (in minutes)") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        ) 
    } else if (input$plot_choice2 == "age_group_bar_plot") {
      ggplot(data1.2.2, aes(x = age_group, y = mean_td)) + 
        geom_col(fill = "#bdd3ce") + 
        labs(x = "Age Group", y = "Mean Trip Duration (in minutes)") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        ) 
    } else if (input$plot_choice2 == "usertype_bar_plot") {
      ggplot(data1.2.3, aes(x = usertype, y = mean_td)) + 
        geom_col(fill = "#bdd3ce") + 
        labs(x = "User Type", y = "Mean Trip Duration") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        ) 
    } else if (input$plot_choice2 == "season_bar_plot") {
      ggplot(data1.2.4, aes(x = factor(season, levels = c("autumn", "winter", "spring", "summer")), y = mean_td)) + 
        geom_col(fill = "#bdd3ce") + 
        labs(x = "Season", y = "Mean Trip Duration (in minutes)") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        ) 
    }
  })
  output$selected_plot4 <- renderPlot({
    if (input$plot_choice4 == "gender_plot4") {
      ggplot(data1.4.1, aes(x=gender, y=n)) +
        geom_segment( aes(x=gender, xend=gender, y=0, yend=n), 
                      linewidth = 1, color = "#f4a258") +
        geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
        geom_text(aes(label=n), vjust=2.5, hjust=0.5, 
                  color="#013d5a", size = 5, fontface = "bold") +
        labs(x = "gender", y = "number of rentals") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "#fcf3e3"),
              panel.background = element_rect(fill = "#fcf3e3"),
              axis.text.x = element_text(size = 5.5, face = "bold"))+
        scale_y_continuous(labels = comma)
    } else if (input$plot_choice4 == "season_plot4") {
      ggplot(data1.4.4, aes(x=factor(season, levels = c("autumn", "winter", "spring", "summer")), y=n)) +
        geom_segment( aes(x=season, xend=season, y=0, yend=n), 
                      linewidth = 1, color = "#f4a258") +
        geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
        geom_text(aes(label=n), vjust=2.5, hjust=0.5, 
                  color="#013d5a", size = 5, fontface = "bold") +
        labs(x = "season", y = "number of rentals") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "#fcf3e3"),
              panel.background = element_rect(fill = "#fcf3e3"))+
        scale_y_continuous(labels = comma)
    } else if (input$plot_choice4 == "age_group_plot4") {
      ggplot(data1.4.2, aes(x=age_group, y=n)) +
        geom_segment( aes(x=age_group, xend=age_group, y=0, yend=n), 
                      linewidth = 1, color = "#f4a258") +
        geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
        geom_text(aes(label=n), vjust=0, hjust=0.5, 
                  color="#013d5a", size = 5, fontface = "bold") +
        labs(x = "age group", y = "number of rentals") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "#fcf3e3"),
              panel.background = element_rect(fill = "#fcf3e3"))+
        scale_y_continuous(labels = comma)
    } else if (input$plot_choice4 == "usertype_plot4") {
      ggplot(data1.4.3, aes(x=usertype, y=n)) +
        geom_segment( aes(x=usertype, xend=usertype, y=0, yend=n), 
                      linewidth = 1, color = "#f4a258") +
        geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
        geom_text(aes(label=n), vjust=2.5, hjust=0.5, 
                  color="#013d5a", size = 5, fontface = "bold") +
        labs(x = "user type", y = "number of rentals") +
        theme_classic() +
        theme(plot.background = element_rect(fill = "#fcf3e3"),
              panel.background = element_rect(fill = "#fcf3e3"))+
        scale_y_continuous(labels = comma)
    }
  })
  output$dynamic_ui <- renderUI({
    if (input$filter_choice == "gender") {
      radioButtons("gender_choice", "Select Gender:", 
                   choices = list("Female" = "female", "Male" = "male"), inline = TRUE)
    } else if (input$filter_choice == "usertype") {
      radioButtons("usertype_choice", "Select User Type:", 
                   choices = list("Customer" = "Customer", "Subscriber" = "Subscriber"), inline = TRUE)
    } else if (input$filter_choice == "age_group") {
      sliderInput("age_group_choice", "Select Age Group:", 
                  min = 20, max = 80, value = 20, step = 10, animate = TRUE)
    }
  })
  
  output$filtered_plot <- renderPlot({
    if (input$filter_choice == "gender") {
      req(input$gender_choice)
      filtered_data <- data1.3.1[data1.3.1$gender == input$gender_choice, ]
      ggplot(filtered_data, aes(x = hour, y = n)) + 
        geom_col(fill = "#708c69") + 
        geom_line(color = "#43543f", linewidth = 1) + 
        labs(x = "Hour", y = "Number of rentals") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        ) +
        scale_y_continuous(labels = comma)
    } else if (input$filter_choice == "usertype") {
      req(input$usertype_choice)
      filtered_data <- data1.3.3[data1.3.3$usertype == input$usertype_choice, ]
      ggplot(filtered_data, aes(x = hour, y = n)) + 
        geom_col(fill = "#708c69") + 
        geom_line(color = "#43543f", linewidth = 1) + 
        labs(x = "Hour", y = "Number of rentals") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        )+
        scale_y_continuous(labels = comma)
    } else if (input$filter_choice == "age_group") {
      req(input$age_group_choice)
      age_group_selected <- ifelse(input$age_group_choice == 80, "80+", 
                                   paste0(input$age_group_choice, "-", input$age_group_choice + 9))
      filtered_data <- data1.3.2[data1.3.2$age_group == age_group_selected, ]
      ggplot(filtered_data, aes(x = hour, y = n, group = 1)) + 
        geom_col(fill = "#708c69") + 
        geom_line(color = "#43543f", linewidth = 1) + 
        labs(x = "Hour", y = "Number of rentals") + 
        theme_bw() + 
        theme(
          plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#fcf3e3", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#fcf3e3", color = NA)
        )+ 
        scale_y_continuous(labels = comma, limits = c(0, 700000))
    }
  })
  
  output$start_station_plot <- renderPlot({
    data_selected <- switch(input$usertype_choice,
                            "Customer" = data2.2.1,
                            "Subscriber" = data2.1.1)
    
    filtered_data <- data_selected %>% filter(hour == input$hour_choice) %>% head(10)
    
    ggdraw(ny_base + 
             geom_point(data = filtered_data, 
                        aes(x = start.station.longitude, y = start.station.latitude, group = hour),
                        color = "#013d5a", size = 2) +
             theme(panel.background = element_rect(fill = "#fcf3e3", color = NA),
                   plot.background = element_rect(fill = "#fcf3e3", color = NA),
                   panel.grid.major = element_line(color = "#fcf3e3"),
                   panel.grid.minor = element_line(color = "#fcf3e3"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank())) + 
      theme(panel.background = element_rect(fill = "#fcf3e3", colour = "#fcf3e3"))
  })
  
  output$end_station_plot <- renderPlot({
    data_selected <- switch(input$usertype_choice,
                            "Customer" = data3.2.1,
                            "Subscriber" = data3.1.1)
    
    filtered_data <- data_selected %>% filter(hour == input$hour_choice) %>% head(10)
    
    ggdraw(ny_base + 
             geom_point(data = filtered_data, 
                        aes(x = end.station.longitude, y = end.station.latitude, group = hour),
                        color = "#013d5a", size = 2) +
             theme(panel.background = element_rect(fill = "#fcf3e3", color = NA),
                   plot.background = element_rect(fill = "#fcf3e3", color = NA),
                   panel.grid.major = element_line(color = "#fcf3e3"),
                   panel.grid.minor = element_line(color = "#fcf3e3"),
                   axis.title.x = element_blank(),
                   axis.title.y = element_blank(),
                   axis.text.x = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks = element_blank())) + 
      theme(panel.background = element_rect(fill = "#fcf3e3", colour = "#fcf3e3"))
  })
}

# Uruchomienie aplikacji Shiny
shinyApp(ui = ui, server = server)