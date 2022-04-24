# install.packages("shiny") and install.package("DT") and install.packages("ggsci")--> color

source('data_cleaning.R')
# UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Country", label = "Country", 
                  choices = unique(df_country$Country), selected = 'Singapore')  # the input
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(title = 'Overiview', p('
This dashboard shows the attitudes towards democracy and science in different countries based on World Value Study. 
The attitude information is sourced from social media (internet, newspaper) and private connection (Email, phone, talk with friend or colleagues). 
The purpose is to explore the attitudes regarding democracy and science among each country ,also general global attitudes, meanwhile it also shows the frequency and popularity of people using social media to gather the information. 
It would also be interesting to see the difference among various countries and among specific question.'),

p('On the left side, you could select the country; on the right side, you could see four sections.'),

p('Below is a brief guidance.'),
br(),
p('Overview:') ,p('  - The general guidance of the website.'),
br(),
p('Democracy：'),p('  - Survey questions to evaluate democracy level.'),

p('  - The chart indicates the average score of each quesiton of the selected country.'),

p('  - The first table illustates the average score of each question of the selected country. '),
p('  - The second table illustrates the average score of each quesiton of all country.'),
br(),
p('News:') ,
br(),
p('  - It shows the frequency and the propottion of each information source used.'),
p('  - The chart indicates the proportion of how people get information.'),
p('  - The first table illustates the proportion of each question of the selected country.'),
p('  - The second table illustrates the proportion of each quesiton of all country.'),
br(),
p('Science:'),
p('  - Survey questions to evaluate science attitude level.'),

p('  - The chart indicates the average score of each quesiton of the selected country. '),
p('  - The first table illustates the average score of each question of the selected country.'),
p('  - The second table illustrates the average score of each quesiton of all country'), 
br(),
p('We were hoping this dashboard would suit your interests!')),
        tabPanel(title = 'Democracy', plotlyOutput('figb1'), dataTableOutput('dfb1'),  dataTableOutput('dfb2')),  # the layout for section b
        tabPanel(title = 'News', plotlyOutput('figc1'), dataTableOutput('dfc1'),  dataTableOutput('dfc2')),  # the layout for section b
        tabPanel(title = 'Science', plotlyOutput('figd1'), dataTableOutput('dfd1'),  dataTableOutput('dfd2'))  # the layout for section b
      )
    )
  )
)




# SERVER
server <- function(input, output){
  # parper the data
  df_b_temp <- reactive({
   df_b %>% filter(Country == input$Country) %>%
      ungroup() %>%
      select(-Country) 
 })  
 
  df_c_temp <- reactive({
    df_c %>% filter(Country == input$Country) %>% 
      ungroup() %>%
      select(-Country) 
  })  
  
  df_d_temp <- reactive({
    df_d %>% filter(Country == input$Country) %>% 
      ungroup() %>%
      select(-Country) 
  })  
  
  # the out for section 2
  output$figb1 <- renderPlotly({
    p1 <- ggplot(df_b_temp(), aes(x = Question, y = `Average Score`)) +
      geom_bar(stat = 'identity', fill = 'seagreen') +
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(hjust = 0.5)) + 
      labs(title = "How often in country's elections") +
      theme_bw() +
      scale_x_discrete(limits = c('Votes are counted fairly', 'Opposition candidates are prevented from running',
                                  'TV news favors the governing party', 'Voters are bribed',
                                  'Journalists provide fair coverage of elections', 'Election officials are fair',
                                  'Rich people buy elections', 'Voters are threatened with violence at the polls', 
                                  'Voters are offered a genuine choice in the elections'),
                       labels = paste0('Question', 1:9))
    ggplotly(p1)
  })
  output$dfb1 <- renderDataTable(df_b_temp(), options = list(lengthChange = FALSE))
  output$dfb2 <- renderDataTable(df_b1, options = list(lengthChange = FALSE))
  
  # the out for section 3
  output$figc1 <- renderPlotly({
    p1 <- ggplot(df_c_temp(), aes(x = Question, fill = Frequency, y = Proportion)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            legend.position = 'top') +
      labs(title = 'Information source')+
      scale_fill_d3()  +
      theme_bw() +
      scale_x_discrete(limits = c('Daily newspaper', 'Printed magazines', 'TV news', 
                                  'Radio news', 'Mobile phone', 'Email', 'Internet', 
                                  'Talk with friends or colleagues'),
                       labels = paste('Question', 1:8))
    ggplotly(p1) %>% hide_guides()
  })
  output$dfc1 <- renderDataTable(df_c_temp() %>%
                            pivot_wider(names_from = Frequency, values_from = Proportion), 
                          options = list(lengthChange = FALSE))
  output$dfc2 <- renderDataTable(df_c1 %>%ungroup() %>%
                            pivot_wider(names_from = Frequency, values_from = Proportion), 
                          options = list(lengthChange = FALSE))
  
  # the out for section 4
  output$figd1 <- renderPlotly({
    p1 <- ggplot(df_d_temp(), aes(x = Question, y = `Average Score`)) +
      geom_bar(stat = 'identity', fill = 'seagreen') +
      theme(axis.text.x = element_blank(), 
            plot.title = element_text(hjust = 0.5)) +
      theme_bw() +
      scale_x_discrete(limits = c('Science and technology are making our lives healthier, easier, and more comfortable',
                                  'Because of science and technology, there will be more opportunities for the next generation',
                                  'We depend too much on science and not enough on faith', 
                                  'One of the bad effects of science is that it breaks down people’s ideas of right and wrong',
                                  'It is not important for me to know about science in my daily life',
                                  'The world is better off, or worse off, because of science and technology'),
                       labels = paste0('Question', 1:7))
    ggplotly(p1, dynamicTicks = FALSE)
  })
  output$dfd1 <- renderDataTable(df_d_temp(), options = list(lengthChange = FALSE))
  output$dfd2 <- renderDataTable(df_d1, options = list(lengthChange = FALSE))
  
}

shinyApp(ui, server)

