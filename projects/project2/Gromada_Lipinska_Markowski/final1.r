# wczytanie potrzebnych pakietów
library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(gganimate)
library(gifski)
library(shinycssloaders)


# dane  -------------------------------------------------------------------

dane_autobusy <- read_excel("dane_transportowe_2019-2023.xlsx", sheet = "autobusy")



dane_autobusy <- dane_autobusy %>%
  mutate(
    Miesiąc = case_when(
      Miesiąc == '1' ~ 'Styczeń',
      Miesiąc == '2' ~ 'Luty',
      Miesiąc == '3' ~ 'Marzec',
      Miesiąc == '4' ~ 'Kwiecień',
      Miesiąc == '5' ~ 'Maj',
      Miesiąc == '6' ~ 'Czerwiec',
      Miesiąc == '7' ~ 'Lipiec',
      Miesiąc == '8' ~ 'Sierpień',
      Miesiąc == '9' ~ 'Wrzesień',
      Miesiąc == '10' ~ 'Październik',
      Miesiąc == '11' ~ 'Listopad',
      Miesiąc == '12' ~ 'Grudzień'
    )
  )

tygodniowe <- autobusy_data %>%
  mutate(Week = cut(Data, breaks = "week", start.on.monday = TRUE)) %>%
  group_by(Week) %>%
  summarise(Total_Boardings = sum(`Liczba osób które wsiadły`)) %>%
  mutate(Week = as.Date(Week))

highlight_dates <-
  as.Date(c(
    "2019-12-30",
    "2020-02-01",
    "2020-07-01",
    "2020-09-01",
    "2020-12-01"
  ))
highlight_labels <-
  c("grudzień 2019",
    "luty 2020",
    "czerwiec 2020",
    "wrzesień 2020",
    "grudzień 2020")

colnames(dane_autobusy)[9] <- "Liczbaosobktorewsiadly"
colnames(dane_autobusy)[8] <- "Godzina"
colnames(dane_autobusy)[3] <- "Dzien"


dane_autobusy <- dane_autobusy %>%
  mutate(
    Dzien = case_when(
      Dzien == '1' ~ 'Poniedziałek',
      Dzien == '2' ~ 'Wtorek',
      Dzien == '3' ~ 'Środa',
      Dzien == '4' ~ 'Czwartek',
      Dzien == '5' ~ 'Piątek',
      Dzien == '6' ~ 'Sobota',
      Dzien == '7' ~ 'Niedziela'
      
    )
  ) %>% mutate(Dzien = fct_relevel(
    Dzien,
    c(
      'Poniedziałek',
      'Wtorek',
      'Środa',
      'Czwartek',
      'Piątek',
      'Sobota',
      'Niedziela'
    )
  ))


# Wczytaj swój zbiór danych
df <- dane_autobusy

# Utwórz nową kolumnę łączącą Rok i Miesiąc do wyboru w dropdown
df$YearMonth <- paste(df$Rok, df$Miesiąc)

df <- df %>% group_by(Dzien, YearMonth) %>% summarize(suma_osob = sum(Liczbaosobktorewsiadly)) %>% ungroup()


# Znajdź minimalną i maksymalną liczbę osób w zbiorze danych
min_people <- 0  # lub min(df$number_of_people)
max_people <- 5000000


# Ui i server -------------------------------------------------------------

ui <- navbarPage(
  "Projekt transportu",
  tabPanel(
    "Wstęp",
    fluidRow(column(
      12,
      h1(
        "Analiza Warszawskiego Transportu Publicznego",
        align = "center",
        style = "white-space: nowrap;"
      )
    )),
    fluidRow(column(
      12, align = "center", div(style = "text-align: center;", imageOutput("myImage"))
    )),
    fluidRow(column(
      12,
      h3(
        "Nasza analiza powstała we współpracy z WTP, który udostępnił nam dane. Naszym celem było zbadanie transportu w Warszawie. W naszym projekcie skupiliśmy się na liczbie pasażerów, jak i ruchu turystycznym.",
        align = "justify"
      )
    ))
  ),
  tabPanel("Bilety", sidebarLayout(
    sidebarPanel(
      style = "background-color: white;",
      HTML("<h4>Do wykresu</h4>"),
      radioButtons(
        "option_rodzaj_biletu",
        "Rodzaje biletów:",
        choices = c("krótkookresowy", "długookresowy", "jednorazowy"),
        selected = NULL
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("bilety_plot"),
        type = getOption("spinner.type", default = 5),
        color = getOption("spinner.color", default = "#CC3533"),
        size = getOption("spinner.size", default = 1.5)
      ),
      fluidRow(column(
        12,
        h4(
          "Wykres przedstawia średnią liczbę zakupionych biletów wybranego rodzaju według miesięcy. Możemy przeanalizować jak wygląda ruch turystyczny na podstawie kupowanych biletów. Warto zwrócić też uwagę kiedy kupowane są bilety długookresowe.",
          align = "justify"
        )
      ))
    )
  )),
  tabPanel("COVID", fluidPage(fluidRow(column(
    12,
    h1(
      "Wpływ pandemii na Warszawski Transport Publiczny",
      align = "center",
      style = "white-space: nowrap;"
    )
  )), fluidRow(
    column(12, mainPanel(
      imageOutput("animPlot", width = "100%", height = "auto"),
      shinycssloaders::withSpinner(
        plotOutput("linePlot"),
        type = getOption("spinner.type", default = 5),
        color = getOption("spinner.color", default = "#CC3533"),
        size = getOption("spinner.size", default = 1.5)
      )
    ))
  )), fluidRow(column(
    12,
    h4(
      "Animacja wizualizuje tygodniową liczbę osób wsiadających do autobusów w Warszawie od 2019 do 2023 roku. Czerwone przerywane linie wskazują ważne daty podczas pandemii. Można zauważyć znaczące spadki liczby pasażerów w kluczowych okresach pandemii.",
      align = "justify"
    )
  ))),
  tabPanel("Ruch", sidebarLayout(
    sidebarPanel(
      style = "background-color: white;",
      HTML("<h4>Do obu wykresów</h4>"),
      radioButtons(
        "srodek_transportu",
        "Środki transportu:",
        choices = c("autobus", "metro"),
        selected = NULL
      ),
      HTML("<h4>Do pierwszego wykresu</h4>"),
      selectInput(
        "dzien_specjalny",
        "Święta:",
        choices = c(
          "Boże Ciało",
          "Boże Narodzenie",
          "Dzień Wszystkich Świętych",
          "Międzynarodowy Dzień Bez Samochodu",
          "Niedziela Palmowa",
          "Nowy Rok",
          "Poniedziałek Wielkanocny",
          "Święto Konstytucji 3 Maja",
          "Święto Niepodległości",
          "Święto Pracy",
          "Święto Wojska Polskiego",
          "Święto Zmarłych",
          "Sylwester",
          "Trzech Króli",
          "Wigilia",
          "Wielka Sobota",
          "Wielkanoc",
          "Wielki Piątek",
          "Zesłanie Ducha Świętego"
        ),
        multiple = TRUE
      )
    ),
    mainPanel(
      shinycssloaders::withSpinner(
        plotOutput("autobusy_i_metro_dni_specjalne_v2"),
        type = getOption("spinner.type", default = 5),
        color = getOption("spinner.color", default = "#CC3533"),
        size = getOption("spinner.size", default = 1.5)
      ),
      fluidRow(column(
        12,
        h4(
          "Wykres pozwala porównać rozkład liczby pasażerów w dni świąteczne w zarówno w metrze, jak i w autobusach. Zachęcamy do wyboru kilku świąt jednocześnie.",
          align = "justify"
        )
      )),
      shinycssloaders::withSpinner(
        plotOutput("uzywalnosc_od_pory_dnia"),
        type = getOption("spinner.type", default = 5),
        color = getOption("spinner.color", default = "#CC3533"),
        size = getOption("spinner.size", default = 1.5)
      ),
      fluidRow(column(
        12,
        h4(
          "Wykres przedstawia rozkład ruchu w różnych porach dnia. Można przyjrzeć się osobno środkom transportu, które nas interesują. Skala na osi Y jest logarytmiczna w celu lepszego pokazania różnic wśród dużych wartości.",
          align = "justify"
        )
      ))
    )
  ))
)






server <- function(input, output, session) {
  # Renderowanie obrazu
  output$myImage <- renderImage({
    list(
      src = "www/zdj1.png",
      contentType = 'image/png',
      width = 600,
      height = 350,
      alt = "To jest obraz"
    )
  }, deleteFile = FALSE)
  
  # wczytanie danych jeżeli będzie to potrzebne można już tu przefiltrować
  # kopiując ścieżkę proszę pamiętać o tym żeby zamienić \ na /
  
  dane_autobusy <- reactive({
    dane_autobusy <- read_excel("dane_transportowe_2019-2023.xlsx", sheet = "autobusy")
    
    return(dane_autobusy)
  })
  
  dane_metro <- reactive({
    dane_metro <- read_excel("dane_transportowe_2019-2023.xlsx", sheet = "metro")
    
    return(dane_metro)
  })
  
  
  dane_bilety <- reactive({
    dane_bilety <- read_excel("dane_transportowe_2019-2023.xlsx", sheet = "bilety")
    dane_bilety %>%
      filter(`Okres ważności` != 'Nie dotyczy') %>%
      mutate(
        rodzaj_biletu = case_when(
          `Okres ważności` == '30 dni' |
            `Okres ważności` == '90 dni' |
            `Okres ważności` == 'Roczny' ~ "długookresowy",
          `Okres ważności` == 'Weekendowy' |
            `Okres ważności` == '72 h' |
            `Okres ważności` == '24 h' ~ "krótkookresowy",
          TRUE ~ "jednorazowy"
        ),
        Miesiąc = case_when(
          Miesiąc == '1' ~ 'Styczeń',
          Miesiąc == '2' ~ 'Luty',
          Miesiąc == '3' ~ 'Marzec',
          Miesiąc == '4' ~ 'Kwiecień',
          Miesiąc == '5' ~ 'Maj',
          Miesiąc == '6' ~ 'Czerwiec',
          Miesiąc == '7' ~ 'Lipiec',
          Miesiąc == '8' ~ 'Sierpień',
          Miesiąc == '9' ~ 'Wrzesień',
          Miesiąc == '10' ~ 'Październik',
          Miesiąc == '11' ~ 'Listopad',
          Miesiąc == '12' ~ 'Grudzień'
        )
      ) %>%
      mutate(Miesiąc = fct_relevel(
        Miesiąc,
        c(
          'Styczeń',
          'Luty',
          'Marzec',
          'Kwiecień',
          'Maj',
          'Czerwiec',
          'Lipiec',
          'Sierpień',
          'Wrzesień',
          'Październik',
          'Listopad',
          'Grudzień'
        )
      )) -> dane_bilety
    
    return(dane_bilety)
  })
  
  
  # wykres 1
  observeEvent(input$option_rodzaj_biletu, {
    filtered_data_1 <- dane_bilety() %>%
      filter(rodzaj_biletu == input$option_rodzaj_biletu) %>%
      group_by(Miesiąc) %>%
      summarise(Liczba = sum(`Liczba sztuk sprzedanych biletów`) / (5 * 10 ^
                                                                      4))
    
    output$bilety_plot <- renderPlot({
      ggplot(filtered_data_1, aes(x = Miesiąc, y = Liczba)) +
        geom_bar(stat = "identity", fill = '#CC3533') +
        geom_text(aes(
          label = round(Liczba, 2),
          vjust = -0.5,
          size = 2
        )) +
        labs(x = "Miesiąc", title = "Średnia liczba sprzedanych biletów według miesięcy") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5),
              legend.position = "none")
    })
  })
  
  # wykres 3
  observeEvent(c(input$srodek_transportu, input$dzien_specjalny), {
    filtered_data_2 <- merge(dane_autobusy(), dane_metro(), by = "Data")
    colnames(filtered_data_2)[colnames(filtered_data_2) == "Średnie wykorzystanie stacji"] <- "metro"
    colnames(filtered_data_2)[colnames(filtered_data_2) == "Liczba osób które wsiadły"] <- "autobus"
    
    filtered_data_2 <- filtered_data_2 %>%
      filter(`Nazwa dnia` %in% input$dzien_specjalny)
    
    nazwa <- as.character(input$srodek_transportu)
    
    output$autobusy_i_metro_dni_specjalne_v2 <- renderPlot({
      ggplot(filtered_data_2, aes(
        x = `Nazwa dnia`,
        y = !!sym(input$srodek_transportu)
      )) +
        geom_boxplot(fill = '#CC3533') +
        labs(title = "Rozkład ilości osób korzystających z wybranego środka komunikacji", y = '', x = 'Dzień świąteczny') +
        theme_bw()
    })
    
  })
  
  # wykres 5
  observeEvent(c(input$srodek_transportu), {
    filtered_data <- merge(dane_autobusy(), dane_metro(), by = c("Pora dnia", "Data"))
    
    
    filtered_data_2 <- filtered_data %>%
      mutate(
        metro = case_when(
          `Średnie wykorzystanie stacji` == 0 ~ 1,
          `Średnie wykorzystanie stacji` != 0 ~ `Średnie wykorzystanie stacji`
        )
      ) %>%
      mutate(
        autobus = case_when(
          `Liczba osób które wsiadły` == 0 ~ 1,
          `Liczba osób które wsiadły` != 0 ~ `Liczba osób które wsiadły`
        )
      ) %>%
      mutate(metro = log(metro), autobus = log(autobus)) %>%
      mutate(`Pora dnia` = fct_relevel(
        `Pora dnia`,
        c(
          "szczyt poranny 6-10",
          "między szczyt 10-14",
          "szczyt popołudniowy 14-18",
          "wieczór 18-22",
          "noc 22-6"
        )
      ))
    
    
    
    dane <- filtered_data_2 %>%
      group_by(`Pora dnia`)
    
    
    
    output$uzywalnosc_od_pory_dnia <- renderPlot({
      ggplot(dane, aes(
        x = `Pora dnia`,
        y = !!sym(input$srodek_transportu)
      )) +
        geom_violin(fill = '#CC3533') +
        labs(x = "Pora dnia", y = '', title = "Gęstość ilości osób korzystających z wybranego środka transportu") +
        theme_bw()
    })
    
  })
  
  
  
  
  # druga strona wykres 1 ---------------------------------------------------
  
  # Aktualizacja wyborów w dropdown na podstawie danych
  output$animPlot <- renderImage({
    # Temporary file for the animation
    anim_file <- tempfile(fileext = ".gif")
    
    # Create the plot
    p <-
      ggplot(tygodniowe, aes(x = Week, y = Total_Boardings, group = 1)) +
      geom_line(linewidth = 0.8, color = "blue") +
      labs(title = "Wykres liczby pasażerów autobusów w tygodniu", x = "Data", y = "Liczba pasażerów") +
      geom_vline(
        xintercept = as.numeric(highlight_dates),
        linetype = "dashed",
        color = "red",
        size = 0.5
      ) +
      scale_x_date(breaks = seq(min(tygodniowe$Week), max(tygodniowe$Week), by = "6 month"),
                   date_labels = "%b %Y") +
      theme_minimal() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          size = 16,
          face = "bold"
        ),
        axis.title.x = element_text(hjust = 0.5, size = 14),
        axis.title.y = element_text(hjust = 0.5, size = 14),
        plot.margin = margin(
          t = 10,
          r = 10,
          b = 10,
          l = 10,
          unit = "pt"
        )
      ) +
      transition_reveal(Week, keep_last = TRUE)
    
    
    #annotate("text", x = highlight_dates, y = Inf, label = highlight_labels, angle = 90, vjust = 1.5, hjust = 1.2, color = "red", size = 3) +
    
    for (i in seq_along(highlight_dates)) {
      vjust_value <-
        ifelse(highlight_dates[i] == as.Date("2019-12-30"), -0.5, 1.2)  # Ustalanie vjust dla konkretnej daty
      p <-
        p + annotate(
          "text",
          x = highlight_dates[i],
          y = Inf,
          label = highlight_labels[i],
          angle = 90,
          vjust = vjust_value,
          hjust = 1.2,
          color = "red",
          size = 3
        )
    }
    
    # Animate and save the plot
    anim <-
      animate(
        p,
        nframes = 500,
        fps = 30,
        width = 800,
        height = 600,
        renderer = gifski_renderer(anim_file)
      )
    
    # Return the animation
    list(src = anim_file, contentType = "image/gif")
  }, deleteFile = TRUE)
  
}

# Run the application
shinyApp(ui = ui, server = server)
