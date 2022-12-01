# DS501 Case Study 3 - My Tech Stock Dashboard

## Developed using
RStudio 2022.07.1 Build 554

## Packages Used 
- shiny (version 1.7.2)
- shinyWidgets (version 0.7.4)
- plotly (version 4.10.0)
- tidyverse (version 1.3.2)
- tidyquant (version 1.0.6)
- rtweet (version 1.0.2)
- syuzhet (version 1.0.6)
- e1071 (version 1.7-12)

## Data Collected
The data used in this dashboard is stock price data for seven popular technology stocks - Microsoft (MSFT), Apple (APPL), Alphabet (GOOG),
Meta (META), Amazon (AMZN), Tesla (TSLA) and  Netflix (NFLX). The data is pulled at application launch from Yahoo Finance using the tq_get function from
the tidyquant package. When pulling finance data from Yahoo finance, data is pulled from the past 12 months to the last full day of trading. This date 
range is typical on many finance dashboards. Additionally, the user can view the open, closing, daily high, daily low or adjusted close price.

## Motivation
Wall Street drives the financial world. Business, operations and financial decisions on the individual and corporate level are often influenced by the current 
and predicted state of the stock market. Creating a dashboard to monitor and analyze current stock prices and trends allows the user to compare performance of 
multiple stocks through sentiment analysis, a statistical summary and regression plots. In this case, technology stocks over time. This tool could be easily 
adapted for a user to monitor stocks in their portfolio. The intent of this dashboard is to provide a user-friendly interface to look at multiple stocks on a 
single plot and then analyze these stocks individually.

## Price Plot
Most financial dashboards offer a plot of the stock price over time, but typically only show a single stock in each plot. For this plot, the user can select view all 7 tech stocks listed on the
in the Plot Settings checkboxes or a subset of them. Additionally, the user can chose to look at the data for the past month, three months, 6 months, 12 months, or year to date. The user also has the 
option to select which price they would like to view for each day: open, closing, high, low or adjusted price. The color of each line represents each stock currently plotted.

## Analysis
Each stock can individually be analyzed by selecting the stock from the "Select a Stock to Analyze dropdown" menu.

### Sentiment Analysis
When analyzing stocks, individuals will often look for news stories reporting on current company news or the consumer or investor attitude related to the stock. Understanding how the consumers
or investors feel towards a company may indicate promising outlook for the stock or perhaps impending doom. To get insight from the public (investors, consumers and news outlets), tweets were 
imported using the  rtweet package and then their tone was analyzed to determine the current sentiment at the time of analysis. When a stock is selected for analysis, 500 tweets containing the 
stock ticker name were gathered and used to perform sentiment analysis using the syuzhet package which assigns a NRC sentiment score. The sentiment scores are presented as a bar chart. The
greater the score, the higher indication of that sentiment being observed in the data set

### Statistics
The statistics tab presents a statistical summary of the selected stocks prices (close, open, high, low and adjusted) as well as the associated dates and trading volumes. This tab serves as a 
synopsis of the stocks basic information to help a user quickly gauge basic information about the stock such as the mean closing price or lowest opening price in the last year.

### Regression 
To model the relationship between the stock price over time, regression models were trained. Regression models are used to find the relationship between a continuous target variable (price) 
given an input vector of data (time). The user has the option to select the type of regression they would like to use: Linear, Support Vector Machine or Local Polynomial. The intent of
allowing the three different regression types allows for the user to compare three common regression techniques and determine which type of regression is best suited for their analysis.
Note, when training the regression models, all available data were used to train the model.

**Linear Regression** 

The most basic type of regression is linear. In this form of regression, the "line of best fit" - slope and intercept of a line - is computed based on the input vector using the lm function. 
The simple linear regression uses ordinary least squares criteria to minimizes the sum of squared error between the training and test data. For stock price data, this type of model fails
to capture the local minima and maxima observed in the target vector.

**Support Vector Machine Regression**

A more robust regressor is based on Support Vector Machines (SVM). The SVM regressor uses kernel functions to map from the original data space to a linear feature space. Once the data have 
been mapped, the model looks to find the fit that will maximize the margin of the fit. Once the margin has been maximized, the data is returned to the input space. For this regression, the 
default kernel of the e1071 package, the Radial Basis Function (RBF) kernel is used. The SVM regressor does a better job fitting to the stock data as it is better able to capture the trends 
found in the data.

**Local Polynomial Regression**

Local polynomial regression is also implemented for the stock data using the loess function. The local polynomial regression fits a local polynomial surface to the training data using weighted
least squares. The local polynomial regression finds the best fit of smaller regions of the training data rather than the entire training data set as is done in the other two regression models
implemented.

## Discussion & Conclusion
Understanding trends in stock price data as well as public opinion of a companys stock is critical when studying and analyzing stock data. In this dashboard, both historical and current
data are available for the user to analyze through statistics, sentiment analysis, time domain plotting, and regression. The time domain plotting shows all stocks on the same set of axis 
which allows the user to see trends over time and compare trends of multiple stocks at the same time. Additionally, the user can deselect stocks to reduce the number of information plotted
at a single time. This can be useful when trying to look directly at one or a combination of the seven stocks selected. From the time domain plot, it is clear that all of the stocks are
currently (as of November 28th 2022) sitting below their highest price over the last year. 

The sentiment analysis tab allows the user to look at the current sentiment of 500 tweets pulled from Twitter. For all stocks, the tweets showed 'Positive' as the sentiment with the highest
score. Other highly scored sentiments include anticipation, negative and trust. These additional sentiments may help an investor make decisions about their current portfolio. As an investor, 
the sentiment analysis can indicate a favorable outlook and may help one determine whether or not they should buy more, hold or sell their shares.

The statistics summary tab provides important information about the stock, such as the highest price or lowest price across all of the data available. These metrics can help an investor 
gauge information about the stocks performance over time and potential for rebounds or pull back. The regression tab shows three types of models that were used to find a fit between the 
input vector (time) and target vector (price). Of the 3 types of regression, the Support Vector Machine and Local Polynomial showed to better model the relationships of the stock price 
over time than the linear model. The linear model fits a line through the data and fails to capture the dynamics. When analyzing stock data, using a SVM or local polynomial is recommended
to capture the ups and downs of the stock price over time.

Further analysis may include creation of more robust models that combine the sentiment and regression analyses to create a more powerful model that could relate the sentiments learned 
from the Twitter data to stock price. This could help an investor understand what type of news and public opinion can influence stock price or how the stock reacted previously. Additional
training data (news headlines, company performance information, etc.) could be used to develop prediction focused models aimed towards predicting how a stock price may change in the future
and provide indication of whether or not the stock should be bought, held, or sold.

## References
Bishop, Christopher M. Pattern Recognition and Machine Learning. Springer, 2006. 

Christensen, Peer. “Monitoring Stock Performance Made Easy with R and Shiny.” Medium, Towards Data Science, 5 Jan. 2021, https://towardsdatascience.com/monitoring-stock-performance-made-easy-with-r-and-shiny-b6ab5fb02085. 

Kearney, Michael W. “Package Rtweet - Cran.r-Project.org.” Cran.R-Project, 21 July 2022, https://cran.r-project.org/web/packages/rtweet/rtweet.pdf. 

“Lesson 6 Use Reactive Expressions.” Shiny, https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/. 

Porras, Eladio Montero. “R Linear Regression Tutorial: LM Function in R with Code Examples.” DataCamp, DataCamp, 18 July 2018, https://www.datacamp.com/tutorial/linear-regression-R. 

Ripley, B D. “Local Polynomial Regression Fitting.” R: Local Polynomial Regression Fitting, R Documentation, https://stat.ethz.ch/R-manual/R-devel/library/stats/html/loess.html. 

Sagar, Chaitanya. “Building Regression Models in R Using Support Vector Regression.” KDnuggets, https://www.kdnuggets.com/2017/03/building-regression-models-support-vector-regression.html. 

“Shiny HTML Tags Glossary.” Shiny, https://shiny.rstudio.com/articles/tag-glossary.html. 

Ven den Rul, Celine. “A Guide to Mining and Analysing Tweets with R.” Medium, Towards Data Science, 30 Sept. 2019, https://towardsdatascience.com/a-guide-to-mining-and-analysing-tweets-with-r-2f56818fdd16. 

“Widget Gallery.” Shiny, https://shiny.rstudio.com/gallery/widget-gallery.html. 

“Yahoo Finance - Stock Market Live, Quotes, Business & Finance News.” Yahoo! Finance, Yahoo!, https://finance.yahoo.com/. 

Zach. “How to Plot a Linear Regression Line in GGPLOT2 (with Examples) .” Statology, Statology.org, 14 Oct. 2020, https://www.statology.org/ggplot2-linear-regression/. 
