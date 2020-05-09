library(shiny)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(plotly)
library(rvest)
library(janitor)
library(countrycode)
library(gganimate)
library(infer)

# Reading in data directly from GitHub Repo. This will automatically update
# Shiny IF we run a CRONR job for the script that creates the RDS files.

# For spread:

covidGlobal <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/covidGlobal.RDS?raw=true"))
covidUS <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/covidUS.RDS?raw=true"))
worldometer_data <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/worldometer.RDS?raw=true"))
tests_per_state <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/53a643de83c9c4d5bf0d438b0ddc2a6e8816a59b/covid19_data_gov1005/tests_per_state.RDS?raw=true"))

# For policy:
policy <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/policy.RDS?raw=true"))

# For economics:
stock_cases <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/stock_cases.RDS?raw=true"))
gdp_cases <- readRDS(url("https://github.com/nishu-lahoti/covid19_data_gov1005/blob/master/covid19_data_gov1005/gdp.RDS?raw=true"))



########## Shiny App Starts Here ##########

# Define UI
ui <- navbarPage("The COVID-19 Data Project",
                 theme = shinytheme("flatly"),
                 
                 tabPanel("About",
                          fluidPage(
                              br(),
                              br(),
                              
                              # imageOutput("covid_image", width = "100%", height = "100%"),
                              
                              fluidRow(column(1), column(10,
                                                         
                                                         h1(tags$b("The Impact of COVID-19"), align = "center"),
                                                         p(tags$b("Analyzing how the spread of COVID-19 has impacted governments and economies"), align = "center"),
                                                         
                                                         br(),
                                                         
                                                         p("Over the past months, the spread of COVID-19 has upended the world,
                                                         with almost 3 million confirmed cases and 200,000 deaths reported to date. 
                                                         Cities, states, and entire nations have shut down, as travel-restrictions
                                                         and stay-at-home orders have come into effect. In the U.S. alone, more than
                                                         26 million citizens have filed for unemployment, with jobless claim filings
                                                         growing at a historically unprecedented pace. As politicians, researchers, 
                                                         lawyers, and other experts scramble for answers, uncertainty about when the
                                                           world can reopen and begin on a road-to-recovery has only continued to grow."),
                                                         
                                                         br(),
                                                         
                                                         p("The purpose of this project is to analyze the spread and impact of the 
                                                         coronavirus pandemic and to understand the efficacy of various policies 
                                                         enacted around the globe in mitigating the crisis. We seek to shed light 
                                                         on many different aspects of the pandemic using data visualization and 
                                                         statistical analysis, as well as provide commentary on the data we have. 
                                                           This site is organized into three parts:"),
                                                         
                                                         br(),
                                                         
                                                         h3(em("Spread")),
                                                         
                                                         p("COVID-19 has spread more rapidly than most politicians and people, in general, expected.
                                                         In some countries, the total number of confirmed cases and deaths exponentially increased
                                                         over the early months of 2020. In other countries, the total number of confirmed cases is uncertain
                                                         due to a lack of testing. In “Spread”, we explore the rate at which COVID-19 spread in countries across the world and
                                                           how this increase correlates with each country's ability to test."),
                                                         
                                                         br(),
                                                         
                                                         h3(em("Policy")),
                                                         
                                                         p("Governments around the world have implemented a variety of measures to respond to 
                                                         the threat and spread of COVID-19, ranging from mild to severe. Such measures include schools and workplaces closing, 
                                                         international travel controls, and emergency investment in healthcare and vaccines. 
                                                         In “Policy”, we explore the stringency of common policy responses governments have taken 
                                                           and compare these responses across countries and regions."),
                                                         
                                                         br(),
                                                         
                                                         h3(em("Economic Impact")),
                                                         
                                                         p("Markets around the world have suffered in the face of COVID-19, with stay-at-home orders and quarantine 
                                                         measures affecting non-essential businesses and productivity across the board. With the 
                                                         focus on combatting the illness coming first and foremost, we hope to see the impact that 
                                                           the number of cases, deaths, and recovered individuals have on indices."))))
                 ),
                
                 tabPanel("Spread",
                                   tabsetPanel(
                                       tabPanel("Rate of Spread",
                                                
                                                fluidRow(
                                                    column(2),
                                                    column(8,
                                                           
                                                           br(),
                                                           
                                                           h2("Visualizing the rate of spread across countries", style = "text-align: center"),
                                                           
                                                           br(),
                                                           
                                                           p("In the early months of 2020, most societies believed the Coronavirus to be a problem distant to their daily life, 
                                                           sequestered to a manufacturing city in the heart of China. As the virus began spreading outside of Wuhan, society-at-large continued
                                                           to believe that it could be contained. The aim of this section is 
                                                           to dispel that belief, showing just how quickly Coronavirus has spread in countries across the world,
                                                             its devastation, and the eventual pathway to recovery."))),
                                                
                                                fluidRow(
                                                    column(2),
                                                    column(8,
                                                           h4("Select a country to see how the arc of COVID-19 cases and deaths"),
                                                           selectInput("country_region", NULL,
                                                                       choices = c("Afghanistan",				
                                                                                   "Angola",			
                                                                                   "Albania",
                                                                                   "Algeria",	
                                                                                   "Andorra",				
                                                                                   "Argentina",		
                                                                                   "Australia",		
                                                                                   "Austria",				
                                                                                   "Azerbaijan",			
                                                                                   "Burundi",
                                                                                   "Belgium",				
                                                                                   "Burkina Faso",
                                                                                   "Burma",		
                                                                                   "Bangladesh",			
                                                                                   "Bulgaria",			
                                                                                   "Bahrain",				
                                                                                   "Bosnia and Herzegovina",				
                                                                                   "Belize",			
                                                                                   "Bolivia",				
                                                                                   "Brazil",				
                                                                                   "Barbados",
                                                                                   "Brunei",			
                                                                                   "Botswana",				
                                                                                   "Canada",			
                                                                                   "Chile",				
                                                                                   "China",				
                                                                                   "Cameroon",				
                                                                                   "Congo (Kinshasa)",				
                                                                                   "Colombia",				
                                                                                   "Costa Rica",
                                                                                   "Cuba",				
                                                                                   "Cyprus",				
                                                                                   "Czechia",				
                                                                                   "Germany",				
                                                                                   "Djibouti",				
                                                                                   "Dominica",				
                                                                                   "Denmark",				
                                                                                   "Dominican Republic",				
                                                                                   "Ecuador",
                                                                                   "Egypt",				
                                                                                   "Spain",				
                                                                                   "Estonia",				
                                                                                   "Ethiopia",				
                                                                                   "Finland",				
                                                                                   "France",				
                                                                                   "Gabon",				
                                                                                   "Ghana",				
                                                                                   "Gambia",
                                                                                   "Greece",				
                                                                                   "Guatemala",				
                                                                                   "Guyana",				
                                                                                   "Honduras",				
                                                                                   "Croatia",				
                                                                                   "Hungary",				
                                                                                   "Indonesia",				
                                                                                   "India",				
                                                                                   "Ireland",				
                                                                                   "Iran",
                                                                                   "Iraq",				
                                                                                   "Iceland",			
                                                                                   "Israel",				
                                                                                   "Italy",				
                                                                                   "Jamaica",				
                                                                                   "Jordan",				
                                                                                   "Japan",				
                                                                                   "Kazakhstan",				
                                                                                   "Kenya",				
                                                                                   "Kyrgyzstan",
                                                                                   "Korea, South",				
                                                                                   "Kuwait",				
                                                                                   "Laos",				
                                                                                   "Lebanon",				
                                                                                   "Libya",				
                                                                                   "Sri Lanka",				
                                                                                   "Luxembourg",			
                                                                                   "Morocco",				
                                                                                   "Moldova",				
                                                                                   "Madagascar",
                                                                                   "Mexico",				
                                                                                   "Mali",				
                                                                                   "Mongolia",				
                                                                                   "Mozambique",			
                                                                                   "Mauritania",			
                                                                                   "Mauritius",				
                                                                                   "Malawi",				
                                                                                   "Malaysia",				
                                                                                   "Namibia",
                                                                                   "Niger",				
                                                                                   "Nigeria",				
                                                                                   "Nicaragua",				
                                                                                   "Netherlands",				
                                                                                   "Norway",				
                                                                                   "New Zealand",				
                                                                                   "Oman",				
                                                                                   "Pakistan",			
                                                                                   "Panama",			
                                                                                   "Peru",
                                                                                   "Philippines",				
                                                                                   "Papua New Guinea",			
                                                                                   "Poland",				
                                                                                   "Portugal",				
                                                                                   "Paraguay",				
                                                                                   "West Bank and Gaza",			
                                                                                   "Qatar",				
                                                                                   "Romania",				
                                                                                   "Russia",				
                                                                                   "Rwanda",
                                                                                   "Saudi Arabia",			
                                                                                   "Sudan",				
                                                                                   "Singapore",				
                                                                                   "Sierra Leone",				
                                                                                   "El Salvador",				
                                                                                   "San Marino",				
                                                                                   "Serbia",				
                                                                                   "South Sudan",				
                                                                                   "Slovakia",				
                                                                                   "Slovenia",
                                                                                   "Sweden",
                                                                                   "Switzerland",	
                                                                                   "Eswatini",				
                                                                                   "Seychelles",				
                                                                                   "Syria",				
                                                                                   "Chad",				
                                                                                   "Thailand",				
                                                                                   "Trinidad and Tobago",				
                                                                                   "Tunisia",				
                                                                                   "Turkey",				
                                                                                   "Tanzania",				
                                                                                   "Uganda",				
                                                                                   "Ukraine",	
                                                                                   "United Kingdom",
                                                                                   "United Arab Emirates",
                                                                                   "Uruguay",			
                                                                                   "US",			
                                                                                   "Uzbekistan",				
                                                                                   "Venezuela",				
                                                                                   "Vietnam",				
                                                                                   "South Africa",				
                                                                                   "Zambia",				
                                                                                   "Zimbabwe",
                                                                                   "Taiwan")),
                                                           textOutput("selected_var"),
                                                           plotlyOutput("covidSpread"),
                                                           plotlyOutput("covidDeaths")),
                                                )),
                                       
                                       tabPanel("Testing",
                                            
                                                fluidRow(
                                                    column(2),
                                                    column(8,
                                                           
                                                           h2("Cases versus Testing", style = "text-align: center"),
                                                           
                                                           br(),
                                                           
                                                           p("Suspecting that countries with more testing infrastructure will report higher case rates, 
                                                           we created models to visualize the relationship between testing and cases. 
                                                           The series of visualizations below aim to test whether a relationship exists between tests 
                                                           per million and cases per million in countries with reported cases."))),
                                                
                                                br(),
                                                
                                                          p("The visualizations below are based on heavy data processing. Please wait a few moments for them to load!", 
                                                            style = "text-align:center"),
                                                
                                                br(),
                                                
                                                sidebarLayout(
                                                    sidebarPanel(column(4),
                                                                 
                                                                 h3("Raw Comparison"),          
                                                                 
                                                                 p("We started by plotting raw figures. The graph to the right shows total tests per 1M people compared against
                                                                   total cases for countries with over 15,000 reported cases.")),
                                                    
                                                    mainPanel( 
                                                      plotlyOutput("covidMTests")
                                                    )),
                                                
                                                sidebarLayout(
                                                    sidebarPanel(column(4),
                                                                 
                                                                 h3("Logarithmic Comparison"),
                                                                    
                                                                  p("Given the data in the first graph is clustered and challenging to visualize, 
                                                                    we plotted it on a logarithmic scale to better visualize any correlation.")),
                                                    
                                                    mainPanel(
                                                      plotlyOutput("covidLogMTests")
                                                    )),
                                                
                                                sidebarLayout(
                                                    sidebarPanel(column(4),
                                                                 
                                                                 h3("Error Rate"),
                                                                    
                                                                    p("Finally, seeing that a correlation seemed plausible, we wanted to test for uncertainty. By replicating
                                                                      the data we have available 500 times, we have 500 samples on which to assess a potential correlation between
                                                                      testing and cases. We segmented the data by total case rate and conducted a linear regression. Our models showed us
                                                                      that 95% of the time we can expect a low correlation between testing and case rates.")),
                                                                 
                                                    mainPanel(
                                                      plotOutput("covidCorrelation")
                                                    )),
                                                )
                                       )),
                 
                 tabPanel("Policy",
                          tabsetPanel(
                              tabPanel("Policy Response",
                                       fluidRow(
                                           column(2),
                                           column(8,
                                                  
                                                  h2("How Countries Have Responded with Policy", style = "text-align:center"),
                                                  br(),
                                                  p("Governments have taken a wide range of seemingly drastic, yet necessary measures in response to 
                                                  the pandemic, from prohibiting international travel early on to more recently enforcing lockdown and quarantine
                                                    procedures that have interrupted education, economies, and daily life."),
                                                  
                                                  br(),
                                                  
                                                  p("This visualization examines how, in a given country, the number of confirmed cases, 
                                                  the number of deaths, and the number of recovered cases have changed over time. 
                                                  The graph is divided into time segments based on the stringency level of the policy 
                                                  measure enacted during that time period. Stringency is generally measured by indicators for 
                                                  mild response, moderate response, and severe response, depending on the policy response in question.
                                                  The policy responses measured include school and workplace disruptions, public event cancellations and 
                                                  public transportations closures, the presence of public info campaigns, restrictions on internal movement 
                                                  and international travel, and testing policy and contact tracing. By comparing the change in cases with 
                                                  the stringency of the policy measure enacted over time, we attempt to examine the efficacy of government 
                                                  policy in curbing the spread of the virus. We suspect that there is little universal correlation but 
                                                    hope to draw insights by comparing policy responses among specific countries."))),
                                       
                                       br(),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText("Look at country-specific policy"),
                                           selectInput("indexInput", "Select a policy response",
                                                       choices = c("School closings", "Workplace closings", "Cancellation of public events", "Restrictions on gatherings",
                                                                   "Public transport closings", "Stay at home requirements", "Restrictions on internal movement",
                                                                   "International travel controls",
                                                                   # "Income support", "Debt contract relief", Fiscal measures", "International support", 
                                                                   # "Emergency investment in healthcare", "Investment in vaccines", 
                                                                   "Public information campaigns", "Testing policy", "Contact tracing")),
                                           selectInput("countryInput", "Select a country",
                                                       choices = c("Afghanistan",
                                                                   "Angola",			
                                                                   "Albania",
                                                                   "Algeria",	
                                                                   "Andorra",				
                                                                   "Argentina",		
                                                                   "Australia",		
                                                                   "Austria",				
                                                                   "Azerbaijan",			
                                                                   "Burundi",
                                                                   "Belgium",				
                                                                   "Burkina Faso",
                                                                   "Burma",		
                                                                   "Bangladesh",			
                                                                   "Bulgaria",			
                                                                   "Bahrain",				
                                                                   "Bosnia and Herzegovina",				
                                                                   "Belize",			
                                                                   "Bolivia",				
                                                                   "Brazil",				
                                                                   "Barbados",
                                                                   "Brunei",			
                                                                   "Botswana",				
                                                                   "Canada",			
                                                                   "Chile",				
                                                                   "China",				
                                                                   "Cameroon",				
                                                                   "Congo (Kinshasa)",				
                                                                   "Colombia",				
                                                                   "Costa Rica",
                                                                   "Cuba",				
                                                                   "Cyprus",				
                                                                   "Czechia",				
                                                                   "Germany",				
                                                                   "Djibouti",				
                                                                   "Dominica",				
                                                                   "Denmark",				
                                                                   "Dominican Republic",				
                                                                   "Ecuador",
                                                                   "Egypt",				
                                                                   "Spain",				
                                                                   "Estonia",				
                                                                   "Ethiopia",				
                                                                   "Finland",				
                                                                   "France",				
                                                                   "Gabon",				
                                                                   "Ghana",				
                                                                   "Gambia",
                                                                   "Greece",				
                                                                   "Guatemala",				
                                                                   "Guyana",				
                                                                   "Honduras",				
                                                                   "Croatia",				
                                                                   "Hungary",				
                                                                   "Indonesia",				
                                                                   "India",				
                                                                   "Ireland",				
                                                                   "Iran",
                                                                   "Iraq",				
                                                                   "Iceland",			
                                                                   "Israel",				
                                                                   "Italy",				
                                                                   "Jamaica",				
                                                                   "Jordan",				
                                                                   "Japan",				
                                                                   "Kazakhstan",				
                                                                   "Kenya",				
                                                                   "Kyrgyzstan",
                                                                   "Korea, South",				
                                                                   "Kuwait",				
                                                                   "Laos",				
                                                                   "Lebanon",				
                                                                   "Libya",				
                                                                   "Sri Lanka",				
                                                                   "Luxembourg",			
                                                                   "Morocco",				
                                                                   "Moldova",				
                                                                   "Madagascar",
                                                                   "Mexico",				
                                                                   "Mali",				
                                                                   "Mongolia",				
                                                                   "Mozambique",			
                                                                   "Mauritania",			
                                                                   "Mauritius",				
                                                                   "Malawi",				
                                                                   "Malaysia",				
                                                                   "Namibia",
                                                                   "Niger",				
                                                                   "Nigeria",				
                                                                   "Nicaragua",				
                                                                   "Netherlands",				
                                                                   "Norway",				
                                                                   "New Zealand",				
                                                                   "Oman",				
                                                                   "Pakistan",			
                                                                   "Panama",			
                                                                   "Peru",
                                                                   "Philippines",				
                                                                   "Papua New Guinea",			
                                                                   "Poland",				
                                                                   "Portugal",				
                                                                   "Paraguay",				
                                                                   "West Bank and Gaza",			
                                                                   "Qatar",				
                                                                   "Romania",				
                                                                   "Russia",				
                                                                   "Rwanda",
                                                                   "Saudi Arabia",			
                                                                   "Sudan",				
                                                                   "Singapore",				
                                                                   "Sierra Leone",				
                                                                   "El Salvador",				
                                                                   "San Marino",				
                                                                   "Serbia",				
                                                                   "South Sudan",				
                                                                   "Slovakia",				
                                                                   "Slovenia",
                                                                   "Sweden",
                                                                   "Switzerland",	
                                                                   "Eswatini",				
                                                                   "Seychelles",				
                                                                   "Syria",				
                                                                   "Chad",				
                                                                   "Thailand",				
                                                                   "Trinidad and Tobago",				
                                                                   "Tunisia",				
                                                                   "Turkey",				
                                                                   "Tanzania",				
                                                                   "Uganda",				
                                                                   "Ukraine",	
                                                                   "United Kingdom",
                                                                   "United Arab Emirates",
                                                                   "Uruguay",			
                                                                   "US",			
                                                                   "Uzbekistan",				
                                                                   "Venezuela",				
                                                                   "Vietnam",				
                                                                   "South Africa",				
                                                                   "Zambia",				
                                                                   "Zimbabwe",
                                                                   "Taiwan*"	)),
                                           sliderInput("dateRange",
                                                       "Select a date range:",
                                                       min = as.Date("2020-01-22","%Y-%m-%d"),
                                                       max = Sys.Date(),
                                                       value = c(as.Date("2020-01-22"), Sys.Date()),
                                                       timeFormat = "%Y-%m-%d"),
                                           
                                           p("Solid line represents Confirmed Cases, \n Dashed line represents Deaths,\n Dotted line represents Recovered.")),
                                         
                                         mainPanel(plotOutput("countryPolicy")))
                                       
                                       ),
                              
                              tabPanel("Stringency Index",
                                       
                                       fluidRow(column(2),
                                                column(8,
                                                       
                                                       h2("Stringency Level of Country Policy", style = "text-align:center"),
                                                       br(),
                                                       br(),
                                                       
                                                       p("This visualization plots the stringency index of the aggregated policy 
                                                       measures against the number of confirmed cases, recovered cases, or deaths over time, on a global level. 
                                                       We can compare the extent to which the number of cases and the
                                                       stringency of government policy responses differs among countries around the globe 
                                                       for any given date. The data we have shows that some countries, such the US, are not as stringent in government
                                                       response as one might anticipate given the number of cases. This observation is unfortunately underscored when one 
                                                         looks at deaths rather than confirmed cases or recoveries."),
                                                       
                                                       br(),
                                                       
                                                       p("The stringency index encompasses recorded scores for (1) school closing, (2) workplace closing, 
                                                       (3) cancelling public events, (4) restrictions on gatherings, (5) closing public transport, (6) stay at home requirements,
                                                       (7) restrictions on internal movement, (8) international travel controls, and (9) public information campaigns. The index is 
                                                       calculated by taking the average of the nine scores pertaining to these individual policy indicators, normalized to a value
                                                       between 0 and 100. Note that the stringency index simply records the number and strictness of 
                                                       government policies and should not be interpreted as ‘scoring’ the appropriateness 
                                                       or effectiveness of a country’s response. A higher stringency index does not necessarily 
                                                       mean that a country's response is ‘better’ than those with lower stringency indices."),
                                                       
                                                       br(),
                                                       
                                                       p("For more information on the Stringency Index, visit", 
                                                         a(href = "https://www.bsg.ox.ac.uk/sites/default/files/2020-05/BSG-WP-2020-032-v5.0_0.pdf",
                                                         "this working paper by Oxford.")
                                                       
                                                       ))),
                                       
                                       br(),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText("Compare global policy"),
                                           radioButtons("caseInput", "Choose a case type",
                                                        choices = c("Confirmed", "Deaths", "Recovered"),
                                                        selected = "Confirmed"),
                                           sliderInput("dateInput",
                                                       "Select a date:",
                                                       min = as.Date("2020-01-22","%Y-%m-%d"),
                                                       max = Sys.Date(),
                                                       value = as.Date("2020-01-22"),
                                                       timeFormat = "%Y-%m-%d")
                                         ),
                                         
                                         mainPanel(plotlyOutput("globalPolicy"))),
                                       
                                       # Regressions
                                       
                                       br(),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           h3("Stringency versus Cases"),
                                           p("Finally, seeing that a correlation seemed plausible, we wanted to test for uncertainty. By replicating
                                             the data we have available 500 times, we have 500 samples on which to assess a potential correlation between
                                             testing and cases. We segmented the data by total case rate and conducted a linear regression. Our models showed us
                                             that 95% of the time we can expect a low correlation between testing and case rates."),
                                           
                                           # radioButtons("caseInput", "Choose a case type",
                                           #              choices = c("Confirmed", "Deaths", "Recovered"),
                                           #              selected = "Confirmed"),
                                           # sliderInput("dateInput",
                                           #             "Select a date:",
                                           #             min = as.Date("2020-03-01","%Y-%m-%d"),
                                           #             max = Sys.Date(),
                                           #             value = as.Date("2020-03-01"),
                                           #             timeFormat = "%Y-%m-%d")
                                         ),
                                         
                                         mainPanel(
                                           plotOutput("stringencyCorrelation")
                                         ))
                                       
                                       ))),
                 
                 tabPanel("Economic Impact",
                          
                          tabsetPanel(
                              tabPanel("COVID & Stock Indices",
                                       
                                       fluidRow(column(2),
                                                column(8,
                                                       
                                                       h2("Economic Implications of COVID-19", style = "text-align: center"),
                                                       
                                                       br(),
                                                       
                                                       p("This visualization plots the number of confirmed cases, the number of deaths, 
                                                       or the number of recoveries against a specific country’s major stock index. 
                                                       This provides a glimpse into how the pandemic has impacted corporations in their day-to-day interactions, 
                                                       as well as how consumers and market analysts have responded in turn. 
                                                       The visualization tracks the logged cases and the closing price of the stock indices alongside 
                                                       the number of cases in order to gauge this impact on the economy. The countries tracked are: 
                                                       China, where the virus originated; South Korea, one of the countries originally hit hardest by the virus; 
                                                       Germany, a country with the fourth highest number of cases, but one with comparatively few deaths; 
                                                       Italy, a country that had extensive shutdowns in response to rapid spread of the virus, 
                                                       and the country with the third most cases worldwide; Spain, another country that quickly closed borders and 
                                                       put shutdowns in place, and the country with the second most cases worldwide; 
                                                         and the United States, currently the world leader in cases."))),
                                       
                                       br(),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText("Look at COVID-19's impact on stock prices for select countries"),
                                           selectInput("countryInputs", "Select a Country: ",
                                                       choices = c("China", 
                                                                   "Germany", 
                                                                   "Italy", 
                                                                   "Korea, South", 
                                                                   "Spain", 
                                                                   "US")),
                                           radioButtons("caseInputs", "Choose a case type",
                                                        choices = c("Confirmed", "Deaths", "Recovered"),
                                                        selected = "Confirmed")
                                           
                                           #,
                                           # sliderInput("dateRange",
                                           #             "Select a date range:",
                                           #             min = as.Date("2020-01-22","%Y-%m-%d"),
                                           #             max = Sys.Date(),
                                           #             value = c(as.Date("2020-01-22"), Sys.Date()),
                                           #             timeFormat = "%Y-%m-%d")
                                         ),
                                         mainPanel(plotOutput("stock_impact"))
                                       )),
                              
                              tabPanel("COVID & GDP",
                                       
                                       fluidRow(column(2),
                                                column(8,
                                                       
                                                       h2("Measuring COVID against country GDP", style = "text-align:center"),
                                                       
                                                       br(),
                                                       
                                                       p("This visualization displays the relationship between the GDP per capita as of 2018 
                                                       against the number of cases (confirmed, deaths, or recovered), on a global level, on any date after January 22nd. 
                                                       It aims to show the relationship between the pre-existing wealth of a country and the number of cases 
                                                       in order to track a few variables: first, whether wealth has any play in the spread of the virus; 
                                                       second, if wealth affects the number of deaths; and third, if wealth affects the likelihood of 
                                                         recovering from the virus."))),
                                       
                                       br(),
                                       
                                       sidebarLayout(
                                         sidebarPanel(
                                           helpText("Compare COVID-19's impact among countries of different GDP levels"),
                                           radioButtons("caseInput", "Choose a case type",
                                                        choices = c("Confirmed", "Deaths", "Recovered"),
                                                        selected = "Confirmed"),
                                           sliderInput("dateInput",
                                                       "Select a date:",
                                                       min = as.Date("2020-01-22","%Y-%m-%d"),
                                                       max = Sys.Date(),
                                                       value = as.Date("2020-01-22"),
                                                       timeFormat = "%Y-%m-%d")
                                         ),
                                         
                                         mainPanel(plotlyOutput("gdp_cases")))
                                       ))),
                 
                 tabPanel("Team",
                          column(6,
                                 h2("The Team"),
                                 br(),
                                 h4("Rebecca Xi"),
                                 p("is a sophomore at Harvard concentrating 
                                   in Applied Math & Economics with a secondary in Government. 
                                   Her github can be found", a(href =  "https://github.com/beccaxi",
                                                               "here.")),
                                 h4("Katelyn Li"),
                                 p("is a junior at Harvard concentrating 
                                   in Neuroscience with a secondary in Government. 
                                   Her github can be found", a(href =  "https://github.com/katelynxli",
                                                               "here.")),
                                 h4("Nishu Lahoti"),
                                 p("is a Masters student between Harvard's
                                   Graduate School of Design and Engineering. His github
                                   can be found", a(href = "https://github.com/nishu-lahoti", "here.")),
                                 
                                 h4("Jun-Yong Kim"),
                                 p("is a first-year at Harvard prospectively 
                                   concentrating in Statistics and Sociology. His github can
                                   be found", a(href = "https://github.com/juniyong247", "here.")),
                                 br(),
                                 p("The github repo for this project can be found", a(href = "https://github.com/nishu-lahoti/covid19_data_gov1005",
                                                                                      "here."))),
                          column(6,
                                 h1("Data Sources"),
                                 br(),
                                 h4(a(href = "https://github.com/CSSEGISandData/COVID-19",
                                      "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE")),
                                 p("This is the data repository for the 2019 Novel Coronavirus Visual Dashboard operated
                                 by the Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)."),
                                 h4(a(href = "https://github.com/nytimes/covid-19-data",
                                      "NYTimes Coronavirus (Covid-19) Data in the United States")),
                                 p("The New York Times is releasing a series of data files with cumulative counts of 
                                   coronavirus cases in the United States, at the state and county level, over time."),
                                 h4(a(href = "https://www.worldometers.info/coronavirus/",
                                      "Worldometer COVID-19 Data")),
                                 p("Worldometer manually analyzes, validates, and aggregates data from thousands of sources 
                                 in real time and provides global COVID-19 live statistics for a wide audience of caring people around the world."),
                                 h4(a(href = "https://finance.yahoo.com",
                                      "Yahoo Finance Daily Stock Indices Database")),
                                 p("Yahoo Finance tracks the daily prices of different stock indices from all around the world."), 
                                 h4(a(href = "https://github.com/OxCGRT/covid-policy-tracker",
                                      "Oxford Covid-19 Government Response Tracker")),
                                 p("The OxCGRT collects systematic information on which governments have taken which measures, and when, 
                                 scoring the stringency of such measures and aggregating these scores into a common Stringency Index."),
                                 h4(a(href = "https://data.worldbank.org/",
                                      "World Bank Data")),
                                 p("The World Bank collects data from member countries in areas like population totals and GDP figures."),
                                 h4(a(href = "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv",
                                      "ISO-3166 Country and Dependent Territories Lists with UN Regional Codes")),
                                 p("These lists are the result of merging data from two sources, the Wikipedia ISO 3166-1 article for alpha and numeric
                                   country codes, and the UN Statistics site for countries' regional, and sub-regional codes.")
                          )
                 )
)


# Define server logic
server <- function(input, output) {
    
    ######### Images #########
    #########        #########  
    
    output$covid_image <- renderImage({
        list(src = './covid19_data_gov1005/covid+global+map.jpg',
             height = 300,
             width = 600,
             style = "display: block, margin-left: auto, margin-right:auto;")},
        deleteFile = FALSE)
    
    ######### Spread #########
    #########        #########  
    
    output$selected_var <- renderText({
        paste(input$country_region)
    })
    
    
    output$covidSpread <- renderPlotly({
        
        confirmed_plot <- covidGlobal %>%
            filter(country_region == input$country_region,
                   increment_confirmed >= 0) %>%
            ggplot(aes(x = new_date, y = increment_confirmed, text = paste("</br> Date:", new_date,
                                                                           "</br> New Cases:", increment_confirmed))) +
            geom_col(fill = "#0D47A1") +
            geom_smooth(method = "lm", se = FALSE, color = "black") + 
            labs(
                title = "Incremental Cases of COVID-19 measured over time",
                x = "Date",
                y = "New Daily Cases"
            )
        
        confirmed_plot <- ggplotly(confirmed_plot, tooltip = "text")
        
        confirmed_plot
        
    })
    
    output$covidDeaths <- renderPlotly({
        
        deaths_plot <- covidGlobal %>%
            filter(country_region == input$country_region,
                   increment_confirmed >= 0) %>%
            ggplot(aes(x = new_date, y = increment_deaths, text = paste("</br> Date:", new_date,
                                                                        "</br> New Deaths:", increment_deaths))) +
            geom_col(fill = "#E64A19") +
            geom_smooth(method = "lm", se = FALSE, color = "black") + 
            labs(
                title = "Incremental Deaths of COVID-19 measured over time",
                x = "Date",
                y = "New Daily Deaths"
            )
        
        deaths_plot <- ggplotly(deaths_plot, tooltip = "text")
        
        deaths_plot
    })
    
    options(scipen = 999)
    
    # Normal
    
    worldometer_tests <- worldometer_data %>%
        filter(total_cases >= 15000,
               !is.na(total_tests))
    
    # Logarithmic
    
    worldometer_log_data <- worldometer_data %>%
        mutate(log_cases = log(total_cases),
               log_deaths = log(total_deaths),
               log_recovered = log(total_recovered),
               log_tests = log(total_tests),
               log_tests_1m = log(tests_1m_pop))
    
    output$covidMTests <- renderPlotly({
        
        covidMTests <- ggplot(worldometer_tests, aes(tests_1m_pop, total_cases, color = country_other, text = paste("</br> Country:", country_other,
                                                                                                                    "</br> Tests per 1 million:", tests_1m_pop,
                                                                                                                    "</br> Total Cases:", total_cases))) +
            geom_point() +
            geom_jitter() +
            theme_classic() +
            theme(legend.position = "none") +
            labs(
                title = "COVID-19 Country Testing Capacity",
                subtitle = "Visualizing a country's testing rate against case rate\nfor countries with over 15,000 reported cases.",
                x = "Tests per 1M",
                y = "Total Cases",
                color = "Country"
            )
        
        covidMTests <- ggplotly(covidMTests, tooltip = "text")
        
        covidMTests
        
    })
    
    output$covidLogMTests <- renderPlotly({
        
        # Logarithmic plot of tests per 1m
        
        covidLogMTests <- ggplot(worldometer_log_data, aes(log_tests_1m, log_cases, color = country_other, text = paste("</br> Country:", country_other,
                                                                                                                        "</br> Tests per 1 million (log transformed):", log_tests_1m,
                                                                                                                        "</br> Total Cases (log transformed):", log_cases))) +
            geom_point() +
            geom_smooth() +
            theme_classic() +
            theme(legend.position = "none") +
            labs(
                title = "Logarithmic comparison of cases to tests",
                x = "Tests per 1M \n(x10,000)",
                y = "Cases \n(x10,000)"
            )
        
        covidLogMTests <- ggplotly(covidLogMTests, tooltip = "text")
        
        covidLogMTests
        
    })
    
    output$covidCorrelation <- renderPlot({
        
        worldometer_model <- worldometer_data %>%
            filter(! is.na(total_tests)) %>%
            select(country_other, incidence, total_cases, total_tests) %>%
            rep_sample_n(size = nrow(worldometer_data), replace = TRUE, reps = 500) %>%
            group_by(replicate, incidence) %>%
            nest() %>%
            mutate(mod = map(data, ~ lm(total_cases ~ total_tests, data = .)),
                   reg_results = map(mod, ~ tidy(., conf.int = TRUE)),
                   disp_coef = map_dbl(reg_results, ~ filter(., term == "total_tests") %>% pull(estimate)))
        # lower_bound = map_dbl(reg_results, ~ filter(., term == "total_tests") %>% pull(conf.low)),
        # upper_bound = map_dbl(reg_results, ~ filter(., term == "total_tests") %>% pull(conf.high)))
        
        world_high <- worldometer_model %>%
            select(replicate, incidence, disp_coef) %>%
            filter(incidence == "100,000+ Cases") %>%
            pull(disp_coef) %>%
            quantile(c(0.025, 0.5, 0.975))
        
        world_med <- worldometer_model %>%
            select(replicate, incidence, disp_coef) %>%
            filter(incidence == "10,000+ Cases") %>%
            pull(disp_coef) %>%
            quantile(c(0.025, 0.5, 0.975))
        
        world_low <- worldometer_model %>%
            select(replicate, incidence, disp_coef) %>%
            filter(incidence == "1,000+ Cases") %>%
            pull(disp_coef) %>%
            quantile(c(0.025, 0.5, 0.975))
        
        world_bottom <- worldometer_model %>%
            select(replicate, incidence, disp_coef) %>%
            filter(incidence == "Less than 1000+ Cases") %>%
            pull(disp_coef) %>%
            quantile(c(0.025, 0.5, 0.975))
        
        updated_world_tibble <- tibble(index = "100,000+ Cases", conf_low = world_high[1], point_estimate = world_high[2], conf_high = world_high[3]) %>%
            add_row(index = "10,000+ Cases", conf_low = world_med[1], point_estimate = world_med[2], conf_high = world_med[3]) %>%
            add_row(index = "1,000+ Cases", conf_low = world_low[1], point_estimate = world_low[2], conf_high = world_low[3]) %>%
            add_row(index = "Less than 1,000 Cases", conf_low = world_bottom[1], point_estimate = world_bottom[2], conf_high = world_bottom[3])
        
        ggplot(updated_world_tibble, aes(y = point_estimate)) +
            geom_errorbar(aes(x = index, ymin = conf_low, ymax = conf_high), width = 0.1, color = "#0D47A1") +
            theme_classic() +
            ylim(-0.05, .2) %>%
            labs(
                title = "Correlation between Total Tests and Total Cases",
                subtitle = "Modeled by running a linear regression \ncomparing case and test rates 1000 times",
                x = "Cases by Country",
                y = "Correlation"
            )
        
    })
    
    ####### End Spread #######
    #########        #########
    
    ######### Policy #########
    #########        #########
    
    output$countryPolicy <- renderPlot({
      
        if(input$indexInput == "School closings") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C1_School.closing)
            subtitle <- "Policy response: Closings of schools and universities"
            breaks <- c("0", "1", "2", "3")
            labels <- c("No measures", "Recommend closing", "Require closing\n (only some levels)", "Require closing\n all levels")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1")
        } 
        else if(input$indexInput == "Workplace closings") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C2_Workplace.closing)
            subtitle <- "Policy response: Closings of workplaces"
            breaks <- c("0", "1", "2", "3")
            labels <- c("No measures", "Recommend closing", "Require closing\n for some sectors and\n categories of workers", "Require closing\n all-but-essential")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1")
        } 
        else if(input$indexInput == "Cancellation of public events") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C3_Cancel.public.events)
            subtitle <- "Policy response: Cancelling public events"
            breaks <- c("0", "1", "2")
            labels <- c("No measures", "Recommend cancelling", "Require cancelling")
            values <- c("midnightblue", "mediumblue", "dodgerblue")
        }
        else if(input$indexInput == "Restrictions on gatherings") {
            color <- policy %>% 
              filter(Country == input$countryInput) %>% 
              filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
              pull(C4_Restrictions.on.gatherings)
            subtitle <- "Policy response: Restrictions on gatherings"
            breaks <- c("0", "1", "2", "3", "4")
            labels <- c("No restrictions", "Restrictions on very\n large gatherings\n (above 1000 people)", 
                        "Restrictions on gatherings \n between 100-1000 people", "Restrictions on gatherings\n between 10-100 people", 
                        "Restrictions on gatherings\n of less than 10 people")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1", "lightblue2")
        }
        else if(input$indexInput == "Public transport closings") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C5_Close.public.transport)
            subtitle <- "Policy response: Closing of public transport"
            breaks <- c("0", "1", "2")
            labels <- c("No measures", "Recommend closing\n (or significantly reduce volume\n/route/means of transport available", "Require closing\n (or prohibit most citizens\n from using it)")
            values <- c("midnightblue", "mediumblue", "dodgerblue")
        }
        else if(input$indexInput == "Stay at home requirements") {
            color <- policy %>% 
              filter(Country == input$countryInput) %>% 
              filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
              pull(C6_Stay.at.home.requirements)
            subtitle <- "Policy response: Orders to shelter-in-place and otherwise confine to home"
            breaks <- c("0", "1", "2", "3")
            labels <- c("No measures", "Recommend not leaving house", "Require not leaving house\n with exceptions for exercise\n, grocery, and 'essential' trips",
                        "Require not leaving house\n with minimal exceptions")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1")
        }
        else if(input$indexInput == "Restrictions on internal movement") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C7_Restrictions.on.internal.movement)
            subtitle <- "Policy response: Restrictions on internal movement"
            breaks <- c("0", "1", "2")
            labels <- c("No measures", "Recommend closing", "Require closing")
            values <- c("midnightblue", "mediumblue", "dodgerblue")
        }
        else if(input$indexInput == "International travel controls") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(C8_International.travel.controls)
            subtitle <- "Policy response: Restrictions on international travel"
            breaks <- c("0", "1", "2", "3", "4")
            labels <- c("No measures", "Screening", "Quarantine on high-risk regions", "Ban on high-risk regions", "Total border closure")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1", "lightblue2")
        }
        else if(input$indexInput == "Public information campaigns") {
            color <- policy %>% 
              filter(Country == input$countryInput) %>% 
              filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
              pull(H1_Public.information.campaigns)
            subtitle <- "Policy response: Presence of public information campaigns"
            breaks <- c("0", "1", "2")
            labels <- c("No COVID-19 public information campaign", "Public officials urging caution\n about COVID-19",
                        "Coordinated public information campaign")
            values <- c("midnightblue", "mediumblue", "dodgerblue")
        }
        else if(input$indexInput == "Testing policy") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(H2_Testing.policy)
            subtitle <- "Policy response: Who can get tested?"
            breaks <- c("0", "1", "2", "3")
            labels <- c("No testing policy", 
                        "Only testing those who\n have symptoms and\n meet specific criteria", 
                        "Testing of anyone showing\n COVID-19 symptoms",
                        "Open public testing\n (e.g. drive-through)")
            values <- c("midnightblue", "mediumblue", "dodgerblue", "skyblue1")
        }
        else if(input$indexInput == "Contact tracing") {
            color <- policy %>% 
                filter(Country == input$countryInput) %>% 
                filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
                pull(H3_Contact.tracing)
            subtitle <- "Policy response: Contact tracing"
            breaks <- c("0", "1", "2")
            labels <- c("No contact tracing", "Limited contact tracing\n (not done for all cases)",
                        "Comprehensive contact tracing\n (done for all cases)")
            values <- c("midnightblue", "mediumblue", "dodgerblue")
        }
        # else {
        #     color <- policy %>% 
        #         filter(Country == input$countryInput) %>% 
        #         filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>% 
        #         pull(StringencyIndexForDisplay)
        #     subtitle <- "Stringency Index"
        # } 
        
        # Create plot for one country's response
        
        policy_plot <- policy %>%  
            filter(Country == input$countryInput) %>% 
            filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            ggplot(aes(x = new_date, color = as.factor(color))) +
            geom_line(aes(y = log_confirmed), linetype = "solid") +
            geom_line(aes(y = log_deaths), linetype = "dashed") +
            geom_line(aes(y = log_recovered), linetype = "dotted") +
            scale_color_manual(
                breaks = breaks,
                labels = labels,
                values = values
            ) +
            labs(
                title = "Spread and Stringency Response",
                subtitle = subtitle,
                x = "Time",
                y = "Count (log transformed)",
                color = "Stringency"
            ) +
            scale_x_date(
                date_breaks = "1 week", 
                date_labels = "%b %d"
            ) +
            theme_classic() 
        
        #policy_plot <- ggplotly(policy_plot, tooltip = c("case_type", "count", "factor"))
        
        policy_plot
        
    })
    
    # Create scatterplot for global response
    
    output$globalPolicy <- renderPlotly({
        
        if(input$caseInput == "Confirmed") {
            x_value <- policy %>% 
                filter(new_date == input$dateInput) %>% 
                pull(log_confirmed)
            x_axis <- "Confirmed cases (log transformed)"
            case_count <- policy %>% 
              filter(new_date == input$dateInput) %>% 
              pull(confirmed)
        }
        else if(input$caseInput == "Recovered") {
            x_value <- policy %>% 
                filter(new_date == input$dateInput) %>% 
                pull(log_recovered)
            x_axis <- "Recoveries (log transformed)"
            case_count <- policy %>% 
              filter(new_date == input$dateInput) %>% 
              pull(recovered)
        }
        else{
            x_value <- policy %>% 
                filter(new_date == input$dateInput) %>% 
                pull(log_deaths)
            x_axis <- "Deaths (log transformed)"
            case_count <- policy %>% 
              filter(new_date == input$dateInput) %>% 
              pull(deaths)
        }
        
        global_policy <- policy %>% 
            filter(new_date == input$dateInput) %>% 
            ggplot(aes(x = x_value, y = StringencyIndexForDisplay, color = sub.region, text = paste("</br> Country:", Country,
                                                                                                    "</br> Region:", sub.region,
                                                                                                    "</br> Stringency Index:", StringencyIndexForDisplay,
                                                                                                    "</br> Count:", case_count))) +
            geom_point() +
            labs(
                title = "Relationship between Number of Cases and Government Response",
                subtitle = "Overall Stringency Index",
                x = x_axis,
                y = "Stringency Index",
                color = "Region"
            ) +
            theme_classic()
        
        global_policy <- ggplotly(global_policy, tooltip = "text")
        
        global_policy
        
    })
    
    # Correlation between stringency index and number of cases, by continent
    
    output$stringencyCorrelation <- renderPlot({
      
      # Set case type based on input 
      
      # if(input$caseInput == "Confirmed") {
        title <- "Correlation between Stringency Index and Total Confirmed Cases"
        x_axis <- "Total Confirmed Cases, by Region"
        
        # Model for confirmed cases
        
        stringency_model <- policy %>%
          select(Country, new_date, confirmed, region, StringencyIndexForDisplay) %>%
          filter(!is.na(StringencyIndexForDisplay), new_date == "2020-05-01") %>%
          rep_sample_n(size = nrow(policy), replace = TRUE, reps = 500) %>%
          group_by(replicate, region) %>%
          nest() %>%
          mutate(mod = map(data, ~ lm(StringencyIndexForDisplay ~ confirmed, data = .)),
                 reg_results = map(mod, ~ tidy(., conf.int = TRUE)),
                 disp_coef = map_dbl(reg_results, ~ filter(., term == "confirmed") %>% pull(estimate)))
        
        Americas <- stringency_model %>%
          select(replicate, region, disp_coef) %>%
          filter(region == "Americas") %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        Africa <- stringency_model %>%
          select(replicate, region, disp_coef) %>%
          filter(region == "Africa") %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        Europe <- stringency_model %>%
          select(replicate, region, disp_coef) %>%
          filter(region == "Europe") %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        Asia <- stringency_model %>%
          select(replicate, region, disp_coef) %>%
          filter(region == "Asia") %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        Oceania <- stringency_model %>%
          select(replicate, region, disp_coef) %>%
          filter(region == "Oceania") %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        Other <- stringency_model %>% 
          select(replicate, region, disp_coef) %>%
          filter(is.na(region)) %>%
          pull(disp_coef) %>%
          quantile(c(0.025, 0.5, 0.975))
        
        updated_policy_tibble <- tibble(index = "Americas", conf_low = Americas[1], point_estimate = Americas[2], conf_high = Americas[3]) %>%
          add_row(index = "Africa", conf_low = Africa[1], point_estimate = Africa[2], conf_high = Africa[3]) %>%
          add_row(index = "Europe", conf_low = Europe[1], point_estimate = Europe[2], conf_high = Europe[3]) %>%
          add_row(index = "Asia", conf_low = Asia[1], point_estimate = Asia[2], conf_high = Asia[3]) %>% 
          add_row(index = "Oceania", conf_low = Oceania[1], point_estimate = Oceania[2], conf_high = Oceania[3]) %>% 
          add_row(index = "Other, including US and UK", conf_low = Other[1], point_estimate = Other[2], conf_high = Other[3])
        
        # Plot error bars
        
        ggplot(updated_policy_tibble, aes(y = point_estimate)) +
          geom_errorbar(aes(x = index, ymin = conf_low, ymax = conf_high), width = 0.1, color = "#0D47A1") +
          #ylim(-0.05, .2) %>%
          labs(
            title = title,
            subtitle = "Modeled by running a linear regression \ncomparing Stringency Index and number of cases 1000 times",
            x = x_axis,
            y = "Correlation"
          ) +
          theme_classic()
        
      
        # }
      
      # else if(input$caseInput == "Recovered") {
      #   title <- "Correlation between Stringency Index and Total Recoveries"
      #   x_axis <- "Total Recoveries, by Region"
      #   
      #   # Model for recoveries
      #   
      #   stringency_model <- policy %>%
      #     select(Country, new_date, recovered, region, StringencyIndexForDisplay) %>%
      #     filter(!is.na(StringencyIndexForDisplay), new_date == "2020-05-01") %>%
      #     rep_sample_n(size = nrow(policy), replace = TRUE, reps = 3) %>%
      #     group_by(replicate, region) %>%
      #     nest() %>%
      #     mutate(mod = map(data, ~ lm(StringencyIndexForDisplay ~ recovered, data = .)),
      #            reg_results = map(mod, ~ tidy(., conf.int = TRUE)),
      #            disp_coef = map_dbl(reg_results, ~ filter(., term == "recovered") %>% pull(estimate)))
      #   
      #  
      #   Americas <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Americas") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Africa <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Africa") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Europe <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Europe") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Asia <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Asia") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Oceania <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Oceania") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Other <- stringency_model %>% 
      #     select(replicate, region, disp_coef) %>%
      #     filter(is.na(region)) %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   updated_policy_tibble <- tibble(index = "Americas", conf_low = Americas[1], point_estimate = Americas[2], conf_high = Americas[3]) %>%
      #     add_row(index = "Africa", conf_low = Africa[1], point_estimate = Africa[2], conf_high = Africa[3]) %>%
      #     add_row(index = "Europe", conf_low = Europe[1], point_estimate = Europe[2], conf_high = Europe[3]) %>%
      #     add_row(index = "Asia", conf_low = Asia[1], point_estimate = Asia[2], conf_high = Asia[3]) %>% 
      #     add_row(index = "Oceania", conf_low = Oceania[1], point_estimate = Oceania[2], conf_high = Oceania[3]) %>% 
      #     add_row(index = "Other, including US and UK", conf_low = Other[1], point_estimate = Other[2], conf_high = Other[3])
      #   
      #   # Plot error bars
      #   
      #   ggplot(updated_policy_tibble, aes(y = point_estimate)) +
      #     geom_errorbar(aes(x = index, ymin = conf_low, ymax = conf_high), width = 0.1, color = "#0D47A1") +
      #     #ylim(-0.05, .2) %>%
      #     labs(
      #       title = title,
      #       subtitle = "Modeled by running a linear regression \ncomparing Stringency Index and number of cases 1000 times",
      #       x = x_axis,
      #       y = "Correlation"
      #     ) +
      #     theme_classic()
      #   
      # }
      # 
      # else{
      #   title <- "Correlation between Stringency Index and Total Deaths"
      #   x_axis <- "Total Deaths, by Region"
      #   
      #   # Model for deaths
      #   
      #   stringency_model <- policy %>%
      #     select(Country, new_date, deaths, region, StringencyIndexForDisplay) %>%
      #     filter(!is.na(StringencyIndexForDisplay), new_date == "2020-05-01") %>%
      #     rep_sample_n(size = nrow(policy), replace = TRUE, reps = 3) %>%
      #     group_by(replicate, region) %>%
      #     nest() %>%
      #     mutate(mod = map(data, ~ lm(StringencyIndexForDisplay ~ deaths, data = .)),
      #            reg_results = map(mod, ~ tidy(., conf.int = TRUE)),
      #            disp_coef = map_dbl(reg_results, ~ filter(., term == "deaths") %>% pull(estimate)))
      #   
      #   Americas <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Americas") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Africa <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Africa") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Europe <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Europe") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Asia <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Asia") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Oceania <- stringency_model %>%
      #     select(replicate, region, disp_coef) %>%
      #     filter(region == "Oceania") %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   Other <- stringency_model %>% 
      #     select(replicate, region, disp_coef) %>%
      #     filter(is.na(region)) %>%
      #     pull(disp_coef) %>%
      #     quantile(c(0.025, 0.5, 0.975))
      #   
      #   updated_policy_tibble <- tibble(index = "Americas", conf_low = Americas[1], point_estimate = Americas[2], conf_high = Americas[3]) %>%
      #     add_row(index = "Africa", conf_low = Africa[1], point_estimate = Africa[2], conf_high = Africa[3]) %>%
      #     add_row(index = "Europe", conf_low = Europe[1], point_estimate = Europe[2], conf_high = Europe[3]) %>%
      #     add_row(index = "Asia", conf_low = Asia[1], point_estimate = Asia[2], conf_high = Asia[3]) %>% 
      #     add_row(index = "Oceania", conf_low = Oceania[1], point_estimate = Oceania[2], conf_high = Oceania[3]) %>% 
      #     add_row(index = "Other, including US and UK", conf_low = Other[1], point_estimate = Other[2], conf_high = Other[3])
      #   
      #   # Plot error bars
      #   
      #   ggplot(updated_policy_tibble, aes(y = point_estimate)) +
      #     geom_errorbar(aes(x = index, ymin = conf_low, ymax = conf_high), width = 0.1, color = "#0D47A1") +
      #     #ylim(-0.05, .2) %>%
      #     labs(
      #       title = title,
      #       subtitle = "Modeled by running a linear regression \ncomparing Stringency Index and number of cases 1000 times",
      #       x = x_axis,
      #       y = "Correlation"
      #     ) +
      #     theme_classic()
      # }
      
     
      
    })
    
    ####### End Policy #######
    #########        #########
    
    ######## Economic ########
    #########        #########
    
    output$stock_impact <- renderPlot({
        
        if(input$countryInputs == "China") {
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #  # filter(new_date == 2020-04-01) %>%
            #   filter(stock == "SSE_China" ) %>%
            #   pull(price)
            y_axis <- "Index: SSE"
            subtitlex <- "In China"
        }
        else if(input$countryInputs == "Germany") {
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #  # filter(new_date == 2020-04-01) %>%
            #   filter(stock == "DAX" ) %>%
            #   pull(price)
            y_axis <- "Index: DAX"
            subtitlex <- "In Germany"
        }
        else if(input$countryInputs == "Italy") {
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            # #  filter(new_date == 2020-04-01) %>%
            #   filter(stock == "FTSE_Italy" ) %>%
            #   pull(price)
            y_axis <- "Index: FTSE"
            subtitlex <- "In Italy"
        }
        else if(input$countryInputs == "Korea, South") {
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #  # filter(new_date == 2020-04-01) %>%
            #   filter(stock == "KOSPI" ) %>%
            #   pull(price)
            y_axis <- "Index: KOSPI"
            subtitlex <- "In South Korea"
        }
        else if(input$countryInputs == "Spain") {
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #  # filter(new_date == 2020-04-01) %>%
            #   filter(stock == "IBEX_Spain" ) %>%
            #   pull(price)
            y_axis <- "Index: IBEX"
            subtitlex <- "In Spain"
        }
        else {
            
            # y_value <- stock_cases %>%
            #   #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #  # filter(new_date == 2020-04-01) %>%
            #   filter(stock == "NASDAQ") %>%
            #   pull(price)
            y_axis <- "Index: NASDAQ"
            subtitlex <- "In the United States"
        }
        
        # alter countryInput to fit Country variable options in economic_data
        #
        # if(input$countryInput == "South Korea") {
        #   input$countryInput <- "Korea, South"
        # }
        # if(input$countryInput == "United States") {
        #   input$countryInput <- "US"
        # }
        
        if(input$caseInputs == "Confirmed") {
            x_value <- "log_confirmed"
            # x_value <- stock_cases %>%
            #   # filter(Country == input$countryInput) %>%
            #   # #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #   # filter(new_date == 2020-04-01) %>%
            #   pull(log_confirmed)
            x_axis <- "Number of Confirmed Cases (log transformed)"
            titlex <- "Impact of Confirmed Cases on Stock Indices"
        }
        else if(input$caseInputs == "Recovered") {
            x_value <- "log_recovered"
            # x_value <- stock_cases %>%
            #   # filter(Country == input$countryInput) %>%
            #   # #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #   # filter(new_date == 2020-04-01) %>%
            #   pull(log_recovered)
            x_axis <- "Number of Recovered Cases (log transformed)"
            titlex <- "Impact of Recovered Cases on Stock Indices"
        }
        else{
            x_value <- "log_deaths"
            # x_value <- stock_cases %>%
            #   # filter(Country == input$countryInput) %>%
            #   # #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #   # filter(new_date == 2020-04-01) %>%
            #   pull(log_deaths)
            x_axis <- "Number of Deaths (log transformed)"
            titlex <- "Impact of Number of Deaths on Stock Indices"
        }
        
        # Create plot for one country's response
        
        stock_cases %>%
            #filter(y_value != "NA", log_confirmed != "0") %>%
            filter(Country == input$countryInputs, is.finite(price)) %>% 
            #filter(new_date >= input$dateRange[1], new_date <= input$dateRange[2]) %>%
            #filter(new_date == 2020-04-01) %>% 
            #filter(log_confirmed != "-Inf", price != "NA") %>% 
            ggplot(aes_string(x = x_value)) +
            geom_line(aes(y = price), linetype = "solid") +
            labs(
                title = titlex,
                subtitle = subtitlex,
                x = x_axis,
                y = y_axis
            ) +
            theme_classic()
        
    })
    
    output$gdp_cases <- renderPlotly({
        
        if(input$caseInput == "Confirmed") {
            x_value <- gdp_cases %>% 
              filter(new_date == input$dateInput) %>% 
              pull(log_confirmed)
            # x_value <- gdp_cases %>% 
            #   filter(new_date == input$dateInput) %>% 
            #   pull(log_confirmed)
            x_axis <- "Confirmed cases (log transformed)"
        }
        else if(input$caseInput == "Recovered") {
            x_value <- gdp_cases %>% 
              filter(new_date == input$dateInput) %>% 
              pull(log_recovered)
            # x_value <- gdp_cases %>% 
            #   filter(new_date == input$dateInput) %>% 
            #   pull(log_recovered)
            x_axis <- "Recoveries (log transformed)"
        }
        else{
            x_value <- gdp_cases %>% 
              filter(new_date == input$dateInput) %>% 
              pull(log_deaths)
            # x_value <- gdp_cases %>% 
            #   filter(new_date == input$dateInput) %>% 
            #   pull(log_deaths)
            x_axis <- "Deaths (log transformed)"
        }
        
        # Plot cases vs. (static) GDP per capita levels
        
        gdp_cases_plot <- gdp_cases %>%
            filter(new_date == input$dateInput) %>% 
            ggplot(aes(x = x_value, y = log(gdp_per_capita), color = sub.region, text = paste("</br> Country:", Country,
                                                                                              "</br> Region:", sub.region,
                                                                                              "</br> GDP:", gdp_2018,
                                                                                              "</br> GDP Per Capita:", gdp_per_capita,
                                                                                              "</br> Count (log transformed):", x_value))) +
            geom_point() +
            labs(
                title = "Relationship between GDP Per Capita and the Number of Cases",
                subtitle = "By Type of Case",
                x = x_axis,
                y = "GDP Per Capita (log transformed)",
                color = "Region"
            ) +
            theme_classic()
        
        gdp_cases_plot <- ggplotly(gdp_cases_plot, tooltip = "text")
        
        gdp_cases_plot
        
    })
    
}

######### End Economic #########
#########              #########


# Run the application
shinyApp(ui = ui, server = server)
