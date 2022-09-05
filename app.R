library(shiny)
library(shinydashboard)
library(dplyr)
library(readr)
library(plotly)
library(zoo)
library(usmap)
library(viridis)

# load covid vaccine dataset
covid.vax <- read_csv('https://data.cdc.gov/api/views/rh2h-3yt2/rows.csv?accessType=DOWNLOAD', col_select = 'Date')

# turn covid vaccine dataset into data.frame and 
covid.vax.df <- as.data.frame(covid.vax)
dates <- unique(as.Date(covid.vax.df$Date, format = "%m/%d/%Y"))

head(covid.vax.df)

# load maps state data
states <- us_map()
states <- states %>% filter(abbr != 'DC') %>% select(x, y, full, abbr, group)

# add centroid data to states df
centroids <- usmapdata::centroid_labels(regions = "states")
centroids <- centroids %>% filter(abbr != 'DC') %>% select(x, y, abbr) %>% rename(x_cent = x, y_cent = y)
states.centroids <- left_join(states, centroids, by = "abbr")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    dashboardPage(
        
        dashboardHeader(title="Covid Tracker"),
        dashboardSidebar(
            sidebarMenu(id = "sidebarid",
                        menuItem("Graph Data", tabName = "Graphs", icon = icon(lib = "font-awesome", name = "fa-regular fa-chart-line")),
                        menuItem("Map Data", tabName = "Map", icon = icon(lib = "font-awesome", name = "fa-regular fa-map")),
                        conditionalPanel(
                            condition = 'input.sidebarid == "Graphs"',
                            #select US or one of 50 states/DC
                            selectInput("region", "Select Region", choices = c("United States" = "US", "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR", "Arizona" = "AZ", 
                                                                               "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Washington DC" = "DC", "Delaware" = "DE",
                                                                               "Florida" = "FL", "Georgia" = "GA", "Hawaii" = "HI", "Iowa" = "IA", "Idaho" = "ID", "Illinois" = "IL",
                                                                               "Indiana" = "IN", "Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Massachusetts" = "MA",
                                                                               "Maryland" = "MD", "Maine" = "ME", "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO", "Mississippi" = "MS",
                                                                               "Montana" = "MT", "North Carolina" = "NC", "North Dakota" = "ND", "Nebraska" = "NE", "New Hampshire" = "NH",
                                                                               "New Jersey" = "NJ", "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY", "Ohio" = "OH", "Oklahoma" = "OK",
                                                                               "Oregon" = "OR", "Pennsylvania" = "PA", "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD",
                                                                               "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Virginia" = "VA", "Vermont" = "VT", "Washington" = "WA",
                                                                               "Wisconsin" = "WI", "West Virginia" = "WV", "Wyoming" = "WY"))
                            
                        ),
                        conditionalPanel(
                            condition = 'input.sidebarid == "Map"',
                            #select Month of vaccination data"
                            sliderInput(inputId = "covid.month",
                                        label = "Select Month",
                                        min = min(dates),
                                        max = max(dates),
                                        value = max(dates),
                                        ticks = FALSE
                            )
                        )
            )
            
        ),
        dashboardBody(
            tabItems(
                tabItem(tabName = "Graphs",
                        fluidRow(
                            valueBoxOutput(outputId = "mydata", width = 4),
                            valueBoxOutput(outputId = "mydata2", width = 4),
                            valueBoxOutput(outputId = "mydata3", width = 4)
                        ),
                        fluidRow(
                            box(width=4,
                                status="info",
                                title="Daily Vaccinations",
                                solidHeader = TRUE,
                                plotlyOutput("newvaccinesplot")),
                            box(width=4,
                                status="info",
                                title="Daily New Cases",
                                solidHeader = TRUE,
                                plotlyOutput("newcasesplot")),
                            box(width=4,
                                status="info",
                                title="Daily Deaths",
                                solidHeader = TRUE,
                                plotlyOutput("newdeathsplot"))
                        ),
                        fluidRow(
                            box(width=4,
                                status="info",
                                title="Total Vaccines",
                                solidHeader = TRUE,
                                plotlyOutput("totalvaxplot")),
                            box(width=4,
                                status="info",
                                title="Total Cases",
                                solidHeader = TRUE,
                                plotlyOutput("totalcasesplot")),
                            box(width=4,
                                status="info",
                                title="Total Deaths",
                                solidHeader = TRUE,
                                plotlyOutput("totaldeathplot"))
                        )        
                ),
                tabItem(tabName = "Map",
                        fluidPage(
                            box(width = 12,
                                height = 900,
                                status="info",
                                title="Percent of State Populations With Full Vaccine Protection",
                                solidHeader = TRUE,
                                plotOutput("casemap"))
                        ))
            )
            
        )
        
    )
)
# Define server logic required to draw a histogram
server <- (function(input, output, session) {
    #covid cases dataset
    dfcase <- reactiveFileReader(
        intervalMillis = 86400000,
        session = session,
        filePath = 'https://data.cdc.gov/api/views/9mfq-cb36/rows.csv?accessType=DOWNLOAD',
        readFunc = read_csv
    )
    #vaccines dataset
    dfvax <- reactiveFileReader(
        intervalMillis = 86400000,
        session = session,
        filePath = 'https://data.cdc.gov/api/views/rh2h-3yt2/rows.csv?accessType=DOWNLOAD',
        readFunc = read_csv
    )
    #show total covid cases of US or individual state depending on selected region input
    output$newvaccinesplot <- renderPlotly({
        vax.df <- dfvax()
        # convert date character to date data type
        vax.df$Date <- as.Date.character(vax.df$Date, "%m/%d/%Y")
        # filter out entries that are not one of 50 US states or DC
        vax.df <- vax.df %>% filter(!Location %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        # if input region is US, group by date and sum first, second, and booster vaccinations of every state for every date
        if (list(input$region) == 'US') {
            vax.df.US1 <- vax.df %>% filter(date_type == "Admin") %>% group_by(Date) %>% 
                summarize(First_Dose = sum(Admin_Dose_1_Daily), Second_Dose = sum(Series_Complete_Daily), Booster = sum(Booster_Daily))
            # calculate 7 day averages of first doses, second doses, and boosters
            vax.df.US2 <- vax.df.US1 %>% mutate(avg_first = rollmean(First_Dose, k = 7, fill = NA), 
                                                avg_second = rollmean(Second_Dose, k = 7, fill = NA), 
                                                avg_booster = rollmean(Booster, k = 7, fill = NA))
            # plot all data above and add axis spikes so hover texts for every line comes up at the same time
            plot_ly(data = vax.df.US2,
                    x = ~Date,
                    y = ~First_Dose,
                    name = "First Dose",
                    type = "scatter",
                    mode = "markers + lines"
            ) %>%
                config(displayModeBar = FALSE) %>%
                add_lines(y = ~avg_first, name = "7-d avg(Dose 1)", color = I("orangered")) %>%
                add_lines(y=~Second_Dose, name="Second Dose", color = I("lightsalmon")) %>%
                add_lines(y = ~avg_second, name = "7-d avg(Dose 2)", color = I("mediumpurple")) %>%
                add_lines(y=~Booster, name="Booster", color = I("lightslategray")) %>%
                add_lines(y = ~avg_booster, name = "7-d avg(Booster)", color = I("skyblue")) %>%
                layout(legend = list(x = 0.7, 
                                     y = 1, 
                                     title = list(text='<b> Dose </b>')),
                       yaxis = list(title = "Doses Delivered",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
        # if state is selected, plot only the vaccination data for that state
        else {
            vax.df.state1 <- vax.df %>% filter(date_type == "Admin", Location == input$region)
            vax.df.state2 <- vax.df.state1 %>% mutate(avg_first = rollmean(Admin_Dose_1_Daily, k = 7, fill = NA), 
                                                      avg_second = rollmean(Series_Complete_Daily, k = 7, fill = NA), 
                                                      avg_booster = rollmean(Booster_Daily, k = 7, fill = NA))
            plot_ly(data = vax.df.state2,
                    x = ~Date,
                    y = ~Admin_Dose_1_Daily,
                    name = "First Dose",
                    type = "scatter",
                    mode = "markers + lines"
            ) %>%
                config(displayModeBar = FALSE) %>%
                add_lines(y = ~avg_first, name = "7-d avg(Dose 1)", color = I("orangered")) %>%
                add_lines(y=~Series_Complete_Daily, name="Second Dose", color = I("lightsalmon")) %>%
                add_lines(y = ~avg_second, name = "7-d avg(Dose 2)", color = I("mediumpurple")) %>%
                add_lines(y=~Booster_Daily, name="Booster", color = I("lightslategray")) %>%
                add_lines(y = ~avg_booster, name = "7-d avg(Booster)", color = I("skyblue")) %>%
                layout(legend = list(x = 0.7, 
                                     y = 1, 
                                     title = list(text='<b> Dose </b>')),
                       yaxis = list(title = "Doses Delivered",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
        
    })
    #show daily covid cases of US or individual state depending on selected region input
    output$newcasesplot <- renderPlotly({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% 
            filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        if (list(input$region) == 'US') {
            covid.df.US1 <- covid.df %>% group_by(submission_date) %>% summarize(new_cases = sum(new_case))
            covid.df.US2 <- covid.df.US1 %>% mutate(avg_cases = rollmean(new_cases, k = 7, fill = NA))
            plot_ly(data = covid.df.US2,
                    x = ~submission_date,
                    y = ~new_cases,
                    type = "scatter",
                    mode = "markers + lines",
                    name = "Count"
            ) %>%
                config(displayModeBar = FALSE) %>%
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Cases </b>')),
                       yaxis = list(title = "Cases of Infection",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x') %>%
                add_lines(x = ~submission_date, y = ~avg_cases, name = "7-day avg", color = I("orangered"))
        }
        else {
            covid.df.state1 <- covid.df %>% filter(state == input$region) %>% arrange(submission_date)
            covid.df.state2 <- covid.df.state1 %>% mutate(avg_cases = rollmean(new_case, k = 7, fill = NA))
            plot_ly(data = covid.df.state2,
                    x = ~submission_date,
                    y = ~new_case,
                    type = "scatter",
                    mode = "markers + lines",
                    name = "Cases"
            ) %>%
                config(displayModeBar = FALSE) %>%
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Cases </b>')),
                       yaxis = list(title = "Cases of Infection",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x') %>%
                add_lines(x = ~submission_date, y = ~avg_cases, name = "7-day avg", color = I("orangered"))
        }
    })    
    #show daily covid deaths of US or individual state depending on selected region input
    output$newdeathsplot <- renderPlotly({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        if (list(input$region) == 'US') {
            covid.df.US1 <- covid.df %>% group_by(submission_date) %>% summarize(new_deaths = sum(new_death))
            covid.df.US2 <- covid.df.US1 %>% mutate(avg_deaths = rollmean(new_deaths, k = 7, fill = NA))
            plot_ly(data = covid.df.US2,
                    x = ~submission_date,
                    y = ~new_deaths,
                    type = "scatter",
                    mode = "lines + markers",
                    name = "Count"
            ) %>%
                config(displayModeBar = FALSE) %>%
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Deaths </b>')),
                       yaxis = list(title = "Cases of Death",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x') %>%
                add_lines(x = ~submission_date, y = ~avg_deaths, name = "7-day avg", color = I("orangered"))
        }
        else {
            covid.df.state1 <- covid.df %>% filter(state == input$region) %>% arrange(submission_date)
            covid.df.state2 <- covid.df.state1 %>% mutate(avg_deaths = rollmean(new_death, k = 7, fill = NA))
            plot_ly(data = covid.df.state2,
                    x = ~submission_date,
                    y = ~new_death,
                    type = "scatter",
                    mode = "lines + markers",
                    name = "Deaths"
            ) %>%
                config(displayModeBar = FALSE) %>%
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Deaths </b>')),
                       yaxis = list(title = "Cases of Death",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x') %>%
                add_lines(x = ~submission_date, y = ~avg_deaths, name = "7-day avg", color = I("orangered"))
        }
    })
    #show total covid vaccinations of US or individual state depending on selected region input
    output$totalvaxplot <- renderPlotly({
        vax.df <- dfvax()
        vax.df$Date <- as.Date.character(vax.df$Date, "%m/%d/%Y")
        vax.df <- vax.df %>% filter(!Location %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        if (list(input$region) == 'US') {
            vax.df %>% filter(date_type == "Admin") %>% group_by(Date) %>% summarize(Second_Dose = sum(Series_Complete_Cumulative), First_Dose = sum(Admin_Dose_1_Cumulative), Booster = sum(Booster_Cumulative))%>%
                plot_ly(
                    x = ~Date,
                    y = ~First_Dose,
                    name = "First Dose",
                    type = "scatter",
                    mode = "markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                add_lines(y=~Second_Dose, name="Second Dose", color = I("lightsalmon")) %>%
                add_lines(y=~Booster, name="Booster", color = I("lightslategray")) %>% 
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Dose </b>')),
                       yaxis = list(title = "Doses Delivered",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
        else {
            vax.df %>% filter(date_type == "Admin", Location == input$region) %>%
                plot_ly(
                    x = ~Date,
                    y = ~Admin_Dose_1_Cumulative,
                    name = "First Dose",
                    type = "scatter",
                    mode = "markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                add_lines(y=~Series_Complete_Cumulative, name="Second Dose", color = I("lightsalmon")) %>%
                add_lines(y=~Booster_Cumulative, name="Booster", color = I("lightslategray")) %>%
                layout(legend = list(x = 0.05, 
                                     y = 1, 
                                     title = list(text='<b> Dose </b>')),
                       yaxis = list(title = "Doses Delivered",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
    })
    #show total covid cases of US or individual state depending on selected region input
    output$totalcasesplot <- renderPlotly({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        if (list(input$region) == 'US') {
            covid.df %>% select(submission_date, tot_cases) %>% group_by(submission_date) %>% summarize(total_cases = sum(tot_cases))%>%
                plot_ly(
                    x = ~submission_date,
                    y = ~total_cases,
                    type="scatter",
                    mode="markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                layout(yaxis = list(title = "Cases of Infection",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
        else {
            covid.df %>% filter(state == input$region) %>% arrange(submission_date) %>% 
                plot_ly(
                    x = ~submission_date,
                    y = ~tot_cases,
                    type="scatter",
                    mode="markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                layout(yaxis = list(title = "Cases of Infection",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
    })
    #show total covid deaths of US or individual state depending on selected region input
    output$totaldeathplot <- renderPlotly({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        if (list(input$region) == 'US') {
            covid.df.US <- covid.df %>% select(submission_date, tot_death) %>% group_by(submission_date) %>% summarize(total_deaths = sum(tot_death)) %>%
                plot_ly(
                    x = ~submission_date,
                    y = ~total_deaths,
                    name = "Total Deaths",
                    type = "scatter",
                    mode = "markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                layout(yaxis = list(title = "Cases of Death",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x') 
        }
        else {
            covid.df %>% filter(state == input$region) %>% arrange(submission_date) %>% 
                plot_ly(
                    x = ~submission_date,
                    y = ~tot_death,
                    type="scatter",
                    mode="markers + lines"
                ) %>%
                config(displayModeBar = FALSE) %>%
                layout(yaxis = list(title = "Cases of Death",
                                    showgrid = F,
                                    hoverformat = ",.0f"),
                       xaxis = list(title = "Date",
                                    showgrid = F,
                                    showspikes = TRUE,
                                    spikemode = 'across',
                                    spikesnap = 'cursor',
                                    showline = TRUE,
                                    showgrid = TRUE),
                       hovermode = 'x')
        }
    })
    # Value box for latest total covid vaccinations
    output$mydata <- renderValueBox({
        vax.df <- dfvax()
        vax.df$Date <- as.Date.character(vax.df$Date, "%m/%d/%Y")
        vax.df <- vax.df %>% filter(!Location %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        vax.df.US1 <- vax.df %>% filter(date_type == "Admin") %>% group_by(Date) %>% 
            summarize(Second_Dose = sum(Series_Complete_Cumulative), First_Dose = sum(Admin_Dose_1_Cumulative), Booster = sum(Booster_Cumulative)) %>% 
            arrange(desc(Date))
        valueBox(
            value = prettyNum(vax.df.US1[1,2], big.mark = ","),
            subtitle = "Americans Fully Vaccinated as of Yesterday")
    })
    # Value box for latest total covid cases
    output$mydata2 <- renderValueBox({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        covid.df.US1 <- covid.df %>% select(submission_date, tot_cases) %>% group_by(submission_date) %>% summarize(total_cases = sum(tot_cases)) %>% arrange(desc(submission_date))
        valueBox(value = prettyNum(covid.df.US1[1,2], big.mark = ","), 
                 subtitle = "Total COVID Cases Reported in the US as of Yesterday")
    })
    # Value box for latest total covid deaths
    output$mydata3 <- renderValueBox({
        covid.df <- dfcase()
        covid.df$submission_date <- as.Date.character(covid.df$submission_date, "%m/%d/%Y")
        covid.df <- covid.df %>% filter(!state %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC"))
        covid.df.US1 <- covid.df %>% select(submission_date, tot_death) %>% group_by(submission_date) %>% summarize(total_deaths = sum(tot_death)) %>% arrange(desc(submission_date))
        valueBox(value = prettyNum(covid.df.US1[1,2], big.mark = ","),
                 subtitle = "Total COVID Deaths Reported in the US as of Yesterday")
    })
    # Map data for map tab
    output$casemap <- renderPlot({
        vax.df <- dfvax()
        vax.df$Date <- as.Date.character(vax.df$Date, "%m/%d/%Y")
        vax.df <- vax.df %>% filter(!Location %in% c("PW", "BP2", "VA2", "DD2", "FM", "MH", "VI", "IH2", "MP", "GU", "US", "PR", "RP", "LTC", "AS", "FSM", "RMI", "NYC", "DC"))
        vax.df <- vax.df %>% filter(Date == input$covid.month) %>% select(Location, Series_Complete_Pop_Pct)
        daily.doses.df <- left_join(states.centroids, vax.df, by=c("abbr"="Location"))
        ggplot(daily.doses.df, aes(x = x,
                                   y = y,
                                   group = group,
                                   fill=Series_Complete_Pop_Pct)) +
            geom_polygon(color = "grey80", size = 0.2) +
            scale_fill_continuous(type = "viridis", limits=c(0,100)) +
            labs(fill = "Percent of Residents\nFully Vaccinated") +
            theme_void() +
            theme(legend.key.size = unit(2, 'cm'),
                  legend.title = element_text(size=14),
                  legend.text = element_text(size=12)) +
            geom_text(daily.doses.df, mapping = aes(x = x_cent, y = y_cent, label = abbr), color = "grey80", size = 5)
    }, width = "auto",
    height = 800)
})

# Run the application 
shinyApp(ui = ui, server = server)