#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(tidyquant)
library(rtweet)
library(syuzhet)
library(e1071)

tech_tickers <- c("AAPL", "GOOG", "META", "AMZN", "MSFT", "TSLA", "NFLX")
reg <- c("linear", "SVM", "loess")
prices <- tq_get(tech_tickers, 
                 get  = "stock.prices",
                 from = today()-months(12),
                 to   = today(),
                 complete_cases = F) %>%
  select(symbol,date,close, open, high, low, adjusted, volume)

auth_setup_default()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tech Stock Dashboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        h3("Plot Settings:"),
        
        checkboxGroupInput("ticker", label = "Select the stocks to view", 
                           choices = list("AAPL" = tech_tickers[1], "GOOG" = tech_tickers[2], "META" = tech_tickers[3], "AMZN" = tech_tickers[4], "MSFT" = tech_tickers[5], "TSLA" = tech_tickers[6], "NFLX" = tech_tickers[7]),
                           selected = tech_tickers),
        
        pickerInput(inputId = "timeframe",
                    label = "Time Period",
                    choices = c("1 month", "3 months", "6 months", "12 months", "Year to Date"),
                    selected = "12 months"),
        
        selectInput("price",
                    "Which Price would you like to plot:",
                    choices = c("Open", "Close", "Adjusted Close", "High", "Low")),
        
        h3("Analysis Settings:"),
        h4("Sentiment, Statistics & Regression"),
        
        selectInput("analysis",
                    "Select a Stock to Analyze",
                    choices = list("AAPL" = tech_tickers[1], "GOOG" = tech_tickers[2], "META" = tech_tickers[3], "AMZN" = tech_tickers[4], "MSFT" = tech_tickers[5], "TSLA" = tech_tickers[6], "NFLX" = tech_tickers[7]),
                    selected = tech_tickers[1]),

        selectInput("regressionType",
                    "Select the Regression Type to Apply:", choices = list("Linear" = reg[1], "Support Vector" = reg[2], "Local Polynomial" = reg[3]), selected = reg[3])
      ),
      
        # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Price Plot", plotlyOutput("pricePlot", width = 800)),
          tabPanel("Sentiment Analysis", br(), textOutput("stockSelected1"), br(), plotOutput("sentAnalysisPlot")),
          tabPanel("Statistics Summary", br(), textOutput("stockSelected2"), br(), verbatimTextOutput("statsTable")),
          tabPanel("Regression", br(), textOutput("stockSelected3"), br(), plotlyOutput("regressionPlot")),
          tabPanel("About", br(), h3(strong("Data Collected")), p('The data used in this dashboard is stock price data for seven popular technology stocks - Microsoft (MSFT), Apple (APPL), Alphabet (GOOG), Meta (META), Amazon (AMZN), Tesla (TSLA) and 
                                Netflix (NFLX). The data is pulled at application launch from Yahoo Finance using the tq_get function from the tidyquant package. When pulling finance data from Yahoo finance, data is pulled from the past 12 
                                months to the last full day of trading. This date range is typical on many finance dashboards. Additionally, the user can view the open, closing, daily high, daily low or adjusted close price.'), hr(), 
                                h3(strong("Motivation")),
                                p('Wall Street drives the financial world. Business, operations and financial decisions on the individual and corporate level are often influenced by the current and predicted state of the stock market. Creating 
                                a dashboard to monitor and analyze current stock prices and trends allows the user to compare performance of multiple stocks through sentiment analysis, a statistical summary and regression plots. In this case, 
                                technology stocks over time. This tool could be easily adapted for a user to monitor stocks in their portfolio. The intent of this dashboard is to provide a user-friendly interface to look at multiple stocks on a 
                                  single plot and then analyze these stocks individually.'), hr(),
                                h3(strong("Price Plot")), p('Most financial dashboards offer a plot of the stock price over time, but typically only show a single stock in each plot. For this plot, the user can select view all 7 tech stocks listed on the
                                                    in the Plot Settings checkboxes or a subset of them. Additionally, the user can chose to look at the data for the past month, three months, 6 months, 12 months, or year to date. The user also has the 
                                                    option to select which price they would like to view for each day: open, closing, high, low or adjusted price. The color of each line represents each stock currently plotted.'), hr(),
                                h3(strong("Analysis")), p('Each stock can individually be analyzed by selecting the stock from the "Select a Stock to Analyze dropdown" menu.'), 
                                h4(strong('Sentiment Analysis')), p('When analyzing stocks, individuals will often look for news stories reporting on current company news or the consumer or investor attitude related to the stock. Understanding how the consumers
                                                            or investors feel towards a company may indicate promising outlook for the stock or perhaps impending doom. To get insight from the public (investors, consumers and news outlets), tweets were 
                                                            imported using the  rtweet package and then their tone was analyzed to determine the current sentiment at the time of analysis. When a stock is selected for analysis, 500 tweets containing the 
                                                            stock ticker name were gathered and used to perform sentiment analysis using the syuzhet package which assigns a NRC sentiment score. The sentiment scores are presented as a bar chart. The
                                                            greater the score, the higher indication of that sentiment being observed in the data set'), 
                                h4(strong('Statistics')), p('The statistics tab presents a statistical summary of the selected stocks prices (close, open, high, low and adjusted) as well as the associated dates and trading volumes. This tab serves as a 
                                                            synopsis of the stocks basic information to help a user quickly gauge basic information about the stock such as the mean closing price or lowest opening price in the last year.'),
                                h4(strong('Regression')), p('To model the relationship between the stock price over time, regression models were trained. Regression models are used to find the relationship between a continuous target variable (price) 
                                                            given an input vector of data (time). The user has the option to select the type of regression they would like to use: Linear, Support Vector Machine or Local Polynomial. The intent of
                                                            allowing the three different regression types allows for the user to compare three common regression techniques and determine which type of regression is best suited for their analysis.
                                                            Note, when training the regression models, all available data were used to train the model.'),
                                h5(strong('Linear Regression')), p('The most basic type of regression is linear. In this form of regression, the "line of best fit" - slope and intercept of a line - is computed based on the input vector using the lm function. 
                                                           The simple linear regression uses ordinary least squares criteria to minimizes the sum of squared error between the training and test data. For stock price data, this type of model fails
                                                           to capture the local minima and maxima observed in the target vector.'),
                                h5(strong('Support Vector Machine')), p('A more robust regressor is based on Support Vector Machines (SVM). The SVM regressor uses kernel functions to map from the original data space to a linear feature space. Once the data have 
                                                                been mapped, the model looks to find the fit that will maximize the margin of the fit. Once the margin has been maximized, the data is returned to the input space. For this regression, the 
                                                                default kernel of the e1071 package, the Radial Basis Function (RBF) kernel is used. The SVM regressor does a better job fitting to the stock data as it is better able to capture the trends 
                                                                found in the data.'),
                                h5(strong('Local Polynomial')), p('Local polynomial regression is also implemented for the stock data using the loess function. The local polynomial regression fits a local polynomial surface to the training data using weighted
                                                          least squares. The local polynomial regression finds the best fit of smaller regions of the training data rather than the entire training data set as is done in the other two regression models
                                                          implemented.'), hr(),
                                h3(strong('Discussion & Conclusions')), p('Understanding trends in stock price data as well as public opinion of a companys stock is critical when studying and analyzing stock data. In this dashboard, both historical and current
                                                                          data are available for the user to analyze through statistics, sentiment analysis, time domain plotting, and regression. The time domain plotting shows all stocks on the same set of axis 
                                                                          which allows the user to see trends over time and compare trends of multiple stocks at the same time. Additionally, the user can deselect stocks to reduce the number of information plotted
                                                                          at a single time. This can be useful when trying to look directly at one or a combination of the seven stocks selected. From the time domain plot, it is clear that all of the stocks are
                                                                          currently (as of November 28th 2022) sitting below their highest price over the last year. '),
                                                                        p("The sentiment analysis tab allows the user to look at the current sentiment of 500 tweets pulled from Twitter. For all stocks, the tweets showed 'Positive' as the sentiment with the highest
                                                                          score. Other highly scored sentiments include anticipation, negative and trust. These additional sentiments may help an investor make decisions about their current portfolio. As an investor, 
                                                                          the sentiment analysis can indicate a favorable outlook and may help one determine whether or not they should buy more, hold or sell their shares."),
                                                                        p('The statistics summary tab provides important information about the stock, such as the highest price or lowest price across all of the data available. These metrics can help an investor 
                                                                          gauge information about the stocks performance over time and potential for rebounds or pull back. The regression tab shows three types of models that were used to find a fit between the 
                                                                          input vector (time) and target vector (price). Of the 3 types of regression, the Support Vector Machine and Local Polynomial showed to better model the relationships of the stock price 
                                                                          over time than the linear model. The linear model fits a line through the data and fails to capture the dynamics. When analyzing stock data, using a SVM or local polynomial is recommended
                                                                          to capture the ups and downs of the stock price over time.'),
                                                                        p('Further analysis may include creation of more robust models that combine the sentiment and regression analyses to create a more powerful model that could relate the sentiments learned 
                                                                          from the Twitter data to stock price. This could help an investor understand what type of news and public opinion can influence stock price or how the stock reacted previously. Additional
                                                                          training data (news headlines, company performance information, etc.) could be used to develop prediction focused models aimed towards predicting how a stock price may change in the future
                                                                          and provide indication of whether or not the stock should be bought, held, or sold. '), hr(),
                                h3(strong('References')), p('Bishop, Christopher M. Pattern Recognition and Machine Learning. Springer, 2006.'),
                                                          p('Christensen, Peer. “Monitoring Stock Performance Made Easy with R and Shiny.” Medium, Towards Data Science, 5 Jan. 2021, 
                                                            https://towardsdatascience.com/monitoring-stock-performance-made-easy-with-r-and-shiny-b6ab5fb02085. '),
                                                          p('Kearney, Michael W. “Package Rtweet - Cran.r-Project.org.” Cran.R-Project, 21 July 2022, https://cran.r-project.org/web/packages/rtweet/rtweet.pdf. '),
                                                          p('“Lesson 6 Use Reactive Expressions.” Shiny, https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/. '),
                                                          p('Porras, Eladio Montero. “R Linear Regression Tutorial: LM Function in R with Code Examples.” DataCamp, DataCamp, 18 July 2018, https://www.datacamp.com/tutorial/linear-regression-R. '),
                                                          p('Ripley, B D. “Local Polynomial Regression Fitting.” R: Local Polynomial Regression Fitting, R Documentation, https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loess.html. '),
                                                          p('Sagar, Chaitanya. “Building Regression Models in R Using Support Vector Regression.” KDnuggets, https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html. '),
                                                          p('“Shiny HTML Tags Glossary.” Shiny, https://shiny.rstudio.com/articles/tag-glossary.html. '),
                                                          p('Ven den Rul, Celine. “A Guide to Mining and Analysing Tweets with R.” Medium, Towards Data Science, 30 Sept. 2019, https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16. '),
                                                          p('“Widget Gallery.” Shiny, https://shiny.rstudio.com/gallery/widget-gallery.html. '),
                                                          p('“Yahoo Finance - Stock Market Live, Quotes, Business & Finance News.” Yahoo! Finance, Yahoo!, https://finance.yahoo.com/. '),
                                                          p('Zach. “How to Plot a Linear Regression Line in GGPLOT2 (with Examples) .” Statology, Statology.org, 14 Oct. 2020, https://www.statology.org/ggplot2-linear-regression/. ')),
        )
      )
    )
)

# Define server logic required to draw interactive plots 
server <- function(input, output) {
  observeEvent(c(input$timeframe, input$ticker, input$price), {
    
    prices <- prices %>%
       filter(symbol %in% input$ticker) 

    if (input$timeframe == "1 month") {
      prices <- prices %>%
        filter(date >= today()-months(1)) }
    if (input$timeframe == "3 months") {
      prices <- prices %>%
        filter(date >= today()-months(3)) }
    if (input$timeframe == "6 months") {
      prices <- prices %>%
        filter(date >= today()-months(6)) }
    if (input$timeframe == "12 months") {
      prices <- prices %>%
        filter(date >= today()-months(12)) }
    if (input$timeframe == "Year to Date") {
      prices <- prices %>%
        filter(year(date) == year(today())) }
    if (input$price == "Open") {
      output$pricePlot <- renderPlotly({ ( ggplotly(prices %>% 
                                                      group_by(symbol) %>%
                                                      mutate(init = if_else(date == min(date), open ,NA_real_)) %>%
                                                      mutate(value = round(100 * open / sum(init,na.rm=T),1)) %>%
                                                      ungroup() %>%
                                                      ggplot(aes(date, value, colour = symbol)) +
                                                      geom_line(size = 0.75, alpha = .9) +
                                                      # uncomment the line below to show area under curves
                                                      #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                                                      theme_minimal(base_size=16) +
                                                      theme(axis.title=element_blank(),
                                                            plot.background = element_rect(fill = "white"),
                                                            panel.background = element_rect(fill="white"),
                                                            legend.text = element_text(colour="black"))))})} 
    else if (input$price == "Close") {
      output$pricePlot <- renderPlotly({ ( ggplotly(prices %>% 
                                                      group_by(symbol) %>%
                                                      mutate(init = if_else(date == min(date), close ,NA_real_)) %>%
                                                      mutate(value = round(100 * close / sum(init,na.rm=T),1)) %>%
                                                      ungroup() %>%
                                                      ggplot(aes(date, value, colour = symbol)) +
                                                      geom_line(size = 0.75, alpha = .9) +
                                                      # uncomment the line below to show area under curves
                                                      #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                                                      theme_minimal(base_size=16) +
                                                      theme(axis.title=element_blank(),
                                                            plot.background = element_rect(fill = "white"),
                                                            panel.background = element_rect(fill="white"),
                                                            legend.text = element_text(colour="black"))))})} 
    else if (input$price == "Adjusted Close") {
      output$pricePlot <- renderPlotly({ ( ggplotly(prices %>% 
                                                      group_by(symbol) %>%
                                                      mutate(init = if_else(date == min(date), adjusted ,NA_real_)) %>%
                                                      mutate(value = round(100 * adjusted / sum(init,na.rm=T),1)) %>%
                                                      ungroup() %>%
                                                      ggplot(aes(date, value, colour = symbol)) +
                                                      geom_line(size = 0.75, alpha = .9) +
                                                      # uncomment the line below to show area under curves
                                                      #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                                                      theme_minimal(base_size=16) +
                                                      theme(axis.title=element_blank(),
                                                            plot.background = element_rect(fill = "white"),
                                                            panel.background = element_rect(fill="white"),
                                                            legend.text = element_text(colour="black"))))})}
    else if (input$price == "High") {
      output$pricePlot <- renderPlotly({ ( ggplotly(prices %>% 
                                                      group_by(symbol) %>%
                                                      mutate(init = if_else(date == min(date), high,NA_real_)) %>%
                                                      mutate(value = round(100 * high / sum(init,na.rm=T),1)) %>%
                                                      ungroup() %>%
                                                      ggplot(aes(date, value, colour = symbol)) +
                                                      geom_line(size = 0.75, alpha = .9) +
                                                      # uncomment the line below to show area under curves
                                                      #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                                                      theme_minimal(base_size=16) +
                                                      theme(axis.title=element_blank(),
                                                            plot.background = element_rect(fill = "white"),
                                                            panel.background = element_rect(fill="white"),
                                                            legend.text = element_text(colour="black"))))})}
    else if (input$price == "Low") {
      output$pricePlot <- renderPlotly({ ( ggplotly(prices %>% 
                                                      group_by(symbol) %>%
                                                      mutate(init = if_else(date == min(date), low ,NA_real_)) %>%
                                                      mutate(value = round(100 * low / sum(init,na.rm=T),1)) %>%
                                                      ungroup() %>%
                                                      ggplot(aes(date, value, colour = symbol)) +
                                                      geom_line(size = 0.75, alpha = .9) +
                                                      # uncomment the line below to show area under curves
                                                      #geom_area(aes(fill=symbol),position="identity",alpha=.2) +
                                                      theme_minimal(base_size=16) +
                                                      theme(axis.title=element_blank(), 
                                                            plot.background = element_rect(fill = "white"),
                                                            panel.background = element_rect(fill="white"),
                                                            legend.text = element_text(colour="black"))))})}
                                                     

  })
  observeEvent(c(input$analysis, input$regressionType), {
     n = 500
     tweet_search_term = input$analysis
     tweets <- search_tweets(tweet_search_term, n)
     tweets_df <- as.data.frame(tweets)
     
     tweet_sentiment <- get_nrc_sentiment(tweets_df$text)
     sentiment_score <- data.frame(colSums(tweet_sentiment[,]))
     names(sentiment_score) <- "Score"
     sentiment_score <- cbind("sentiment"=rownames(sentiment_score),sentiment_score)
     rownames(sentiment_score) <- NULL
     output$sentAnalysisPlot <- renderPlot({ ggplot(data = sentiment_score,aes(x=sentiment, y=Score)) + geom_bar(aes(fill=sentiment),stat = "identity") + 
       theme(legend.position="none") + xlab("Sentiments") + ylab("Scores") + 
       ggtitle("Total sentiment based on scores from Twitter Data") + theme_minimal() })
     
     priceStats <- prices %>% filter(symbol == input$analysis)
     output$statsTable <- renderPrint({summary(priceStats)})
     
     
     if (input$regressionType == "SVM") {
     output$regressionPlot <- renderPlotly({
      x <- priceStats$date
      y <- priceStats$close
      modelsvm = svm(priceStats$close~x, priceStats)
      predYsvm <- predict(modelsvm, priceStats)
      
      colors <- c("Training Data" = "#E69F00", "SVM Regression" = "#9999CC")
      ggplotly( ggplot(priceStats, aes(x = date, y = close, color = "Training Data")) + geom_line(size = 0.75, alpha = .9) +
                geom_point(aes(x = date, y = predYsvm, color = "SVM Regression")) +
                labs(x = "Date", y = "Closing Price", color = "Legend") + scale_color_manual(values = colors) + 
                  theme_minimal(base_size=16) +
                  theme(axis.title=element_blank(),
                        plot.background = element_rect(fill = "white"),
                        panel.background = element_rect(fill="white"),
                        legend.text = element_text(colour="black"))) 
     }) }
     else if (input$regressionType == "linear") {
       output$regressionPlot <- renderPlotly({
         x <- priceStats$date
         y <- priceStats$close
         modelLinear = lm(priceStats$close~x, priceStats)

         colors <- c("Training Data" = "#E69F00", "Linear Regression" = "#9999CC")
         ggplotly( ggplot(priceStats, aes(x = date, y = close, color = "Training Data")) + geom_line(size = 0.75, alpha = .9) +
                     stat_smooth(method = "lm", aes(color="Linear Regression", show_guide=TRUE)) + 
                     labs(x = "Date", y = "Closing Price", color = "Legend") + scale_color_manual(values = colors) + 
                     theme_minimal(base_size=16) +
                     theme(axis.title=element_blank(),
                           plot.background = element_rect(fill = "white"),
                           panel.background = element_rect(fill="white"),
                           legend.text = element_text(colour="black"))) 
       }) }
     else if (input$regressionType == "loess") {
       output$regressionPlot <- renderPlotly({
         x <- priceStats$date
         y <- priceStats$close
         #modelLogistic = glm(priceStats$close~x, priceStats)

         colors <- c("Training Data" = "#E69F00", "Local Polynomical Regression" = "#9999CC")
         ggplotly( ggplot(priceStats, aes(x = date, y = close, color = "Training Data")) + geom_line(size = 0.75, alpha = .9) + stat_smooth(aes(color="Local Polynomical Regression", show_guide=TRUE)) +
                     labs(x = "Date", y = "Closing Price", color = "Legend") + scale_color_manual(values = colors) + 
                     theme_minimal(base_size=16) +
                     theme(axis.title=element_blank(),
                           plot.background = element_rect(fill = "white"),
                           panel.background = element_rect(fill="white"),
                           legend.text = element_text(colour="black"))) 
       }) }
     
     output$stockSelected1 <- renderText({ paste("Analyzing ", input$analysis)})
     output$stockSelected2 <- renderText({ paste("Analyzing ", input$analysis)})
     output$stockSelected3 <- renderText({ paste("Analyzing ", input$analysis)})
  })
  
}
# Run the application 
#shinyApp(ui = ui, server = server)
runGadget(ui, server, viewer = browserViewer(browser = getOption("browser")))
