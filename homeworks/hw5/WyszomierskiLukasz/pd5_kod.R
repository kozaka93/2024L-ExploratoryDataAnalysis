library(xml2)
library(rvest)
library(dplyr)
library(plotly)
url <- "https://www.cso.ie/en/releasesandpublications/er/raa/regionalaccountsforagriculture2020/" 
page <- read_html(url) 
table <- html_table(page, fill = TRUE) 
border <- as.data.frame(table[2])[-1,]
names(border) <- c("category", "border")
west <- as.data.frame(table[3])[-1,]
names(west) <- c("category", "west")
mid_west <- as.data.frame(table[4])[-1,]
names(mid_west) <- c("category", "mid_west")
south_east <- as.data.frame(table[5])[-1,]
names(south_east) <- c("category", "south_east")
south_west <- as.data.frame(table[6])[-1,]
names(south_west) <- c("category", "south_west")
dublin <- as.data.frame(table[7])[-1,]
names(dublin) <- c("category", "dublin")
midland <- as.data.frame(table[8])[-1,]
names(midland) <- c("category", "midland")
df <- border %>% 
  inner_join(west, by = "category") %>% 
  inner_join(mid_west, by = "category") %>% 
  inner_join(south_east, by = "category") %>% 
  inner_join(south_west, by = "category") %>% 
  inner_join(dublin, by = "category") %>% 
  inner_join(midland, by = "category")

fig <- plot_ly(df, x = ~category, type = "bar", visible = FALSE) %>%
  add_trace(y = ~border, name = "Border", visible = TRUE,
            text = paste("region: Border" ,"<br>category: ", df$category, 
                         "<br>income share: ", df$border, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~west, name = "West", visible = FALSE,
            text = paste("region: West", "<br>category: ", df$category, 
                         "<br>income share: ", df$west, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~mid_west, name = "Mid-West", visible = FALSE,
            text = paste("region: Mid-West", "<br>category: ", df$category, 
                         "<br>income share: ", df$mid_west, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~south_east, name = "South-East", visible = FALSE,
            text = paste("region: South-East", "<br>category: ", df$category, 
                         "<br>income share: ", df$south_east, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~south_west, name = "South-West", visible = FALSE,
            text = paste("region: South-West", "<br>category: ", df$category, 
                         "<br>income share: ", df$south_west, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~dublin, name = "Dublin & Mid-East", visible = FALSE,
            text = paste("region: Dublin & Mid-East", "<br>category: ", df$category, 
                         "<br>income share: ", df$dublin, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  add_trace(y = ~midland, name = "Midland", visible = FALSE,
            text = paste("region: Midland", "<br>category: ", df$category, 
                         "<br>income share: ", df$midland, "%"),
            hoverinfo = 'text',
            textposition = "none",
            marker = list(color = 'lightblue')) %>%
  layout(
    showlegend = FALSE,
    xaxis = list(title = "category",
                 tickfont = list(size = 14, family = "Arial"),
                 titlefont = list(size = 16, family = "Arial"),
                 tickangle =30
    ),
    yaxis = list(title = "income share (in %)",
                 tickfont = list(size = 14, family = "Arial"),
                 titlefont = list(size = 16, family = "Arial")),
    title = "<b>Structure of agricultural production income</b>",
    titlefont = list(size = 20, family = "Arial"),
    annotations = list(
      list(
        text = "in each region of Ireland in 2020",
        font = list(size = 16, family = "Arial"),
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        x = 0.41,
        y = 1.05,
        xanchor = "center",
        yanchor = "bottom")),
    margin = list(
      l = 100, 
      r = 100,
      t = 100, 
      b = 100  
    ),
    height = 600, 
    width = 800,
    updatemenus = list(
      list(
        y = 1,
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = "Border"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
               label = "West"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE)),
               label = "Mid-West"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE)),
               label = "South-East"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
               label = "South-West"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)),
               label = "Dublin & Mid-East"),
          list(method = "restyle",
               args = list("visible", list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
               label = "Midland")
        )
      )
    )
  )

fig 