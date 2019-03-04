
#library(rsconnect)      #needed to deploy the app to the shinyserver
library(tidyverse)
library(shinydashboard)
library(shiny)
library(readxl)         # read in the data from Excel
library(DT)             # for Data tables
library(listviewer)     # for schema with plotly
library(scales)         # used to change the axis scales
library(leaflet)        # used to generate the map with pins
library(googleway)      # used to pull the google data
library(lubridate)
library(twitteR)        # used to pull twitter data


# When you are ready to deploy to shinyapps.io use the rsconnect
#rsconnect::deployApp('path_to_app')


#### Variable Inputs ------------------------------------------------------------------------------------

#twitter user name
twitterhandle = "ApartmentGuide"

#name of property
property_name = "Property Name"

#property image for landing page
property_image_landing = "shutterstock_apt.jpg"

#marketing plan PDF File name
marketingplan_PDF = "Example Marketing Plan PDF.pdf#zoom=200" #zoom makes it look better

#for the google page
property_google_searchterm = "sweetwater charleston" #random example property


####### Data Wrangling #########

# data for median rent reactive plots --------------------------------------

median.rent.wide <- read_csv("datasets/Zip_Zri_AllHomesPlusMultifamily.csv")

# wide form to long form
median.rent.long <- median.rent.wide %>%
  gather(date, median_rent, 8:98)

# rename df for ease of use
median.rent <- median.rent.long

# rename column headers
names(median.rent) <- c("RegionID", "Zip_Code", "City", "State", "Metro",
                        "County_Name", "Size_Rank", "Date", "Median_Rent")

# drop unwanted columns for tidier datatable output
drop.cols <- c('RegionID', 'Metro', 'Size_Rank')
median.rent <- median.rent %>% 
  select(-one_of(drop.cols))


# define City as character
median.rent$City <- as.character(median.rent$City)

# define Zip_Code, City, State as factors
median.rent$Zip_Code = as.factor(median.rent$Zip_Code)
median.rent$State = as.factor(median.rent$State)
median.rent$City = as.factor(median.rent$City)

median.rent$Date = as.Date(median.rent$Date)



# data for tiered affordability facet and reactive plot -------------------------

Tiered <- read_csv("datasets/TieredAffordability_Rental.csv")

Tiered$tier = as.factor(Tiered$tier)
Tiered$RegionName =  as.factor(Tiered$RegionName)
Tiered$RegionID = as.factor((Tiered$RegionID))
Tiered$date = as.Date(Tiered$date, "%y/%m/%d")


# Portfolio properties rating vs competitor table
portfolio <- read_excel("datasets/Portfolio Average Rating 5.22 to Current.xlsx", 
                        sheet = "Store_Comparison_Report-00a64b0")


# Online review emotional valence data --------------------------------------------------

MRES_Rating_Comparisons <- read_excel("datasets/MRES Rating Comparisons.xlsx", sheet = 2)

# Create df with only rows of interest for bar chart
MRES_Rating_Comparisons = MRES_Rating_Comparisons[4:6,]

# rename column names
names(MRES_Rating_Comparisons) <- c("Category", "Before_Intervention", "After_Intervention")

# Searches df$variable and replaces all occurences of "input" with "output"
MRES_Rating_Comparisons$Category %>%
  sub(" Count", "", .)


# Plots ---------------------------------------------------------------------------------

# Pie charts, review valence ggplot2---------------------

# Bar plot and base pie chart for BEFORE itervention data
bp_bef<- ggplot(MRES_Rating_Comparisons, aes(x="", y=Before_Intervention, fill=Category))+
  geom_bar(width = 1, stat = "identity")

pie_bef <- bp_bef + coord_polar("y", start=0)

# Bar plot and base pie chart for AFTER itervention data
bp_aft <- ggplot(MRES_Rating_Comparisons, aes(x="", y=After_Intervention, fill=Category))+
  geom_bar(width = 1, stat = "identity")

pie_aft <- bp_aft + coord_polar("y", start=0)

# Create Blank theme
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.title = element_text(size=14, face="bold"),
    legend.title = element_text(size=12)
  )

# Before intervention chart
MRes_Ratings_Before <- pie_bef + scale_fill_manual(values=c("#ed5d62", "#b3aead", "#62ed5d")) + blank_theme +
  ggtitle("Before Intervention") +
  labs(fill = "Emotional\nValence") +
  geom_text(aes(x = 1.15, label = percent(Before_Intervention/sum(Before_Intervention))),
            position = position_stack(vjust = 0.5), size=3.5)

# After intervention chart
MRes_Ratings_After <- pie_aft + scale_fill_manual(values=c("#ed5d62", "#b3aead", "#62ed5d")) + blank_theme +
  ggtitle("After Intervention") +
  labs(fill = "Emotional\nValence") +
  geom_text(aes(x = 1.15, label = percent(After_Intervention/sum(After_Intervention))),
            position = position_stack(vjust = 0.5), size=3.5)


# Facet Wrapped Affordability Data Graphed -----------------------------------

# Because the second graph with selection is reactive, the theme elements must
# be changed in the server function

Tiered = as.data.frame(Tiered) 
t = ggplot(Tiered, aes(date, PercentIncomeSpentOnRent)) +
  xlab("Date") +
  ylab("Percent of Income Spent on Rent") +
  labs (color = "Property Tier") +
  geom_point(aes(color = factor(tier)), size= 2 ) 

affordability_static= t + facet_wrap(.~RegionName) +
  xlab("Date") +
  ylab("Percent of Income Spent on Rent") +
  labs (color = "Property Tier") +  # this allows you to change the name of the legand title
  scale_y_continuous(labels = percent) +  #change the y-axis to percent
  theme_bw() + theme(legend.text = element_text(face = "plain", size = 12),
                     legend.title = element_text(face = "plain", size = 12),
                     legend.title.align = .5,
                     text = element_text(size = 14),
                     axis.title = element_text(face = "bold", size = 12),
                     axis.title.y = element_text(margin = margin(r =20)),
                     axis.title.x = element_text(margin = margin(t =20)))

# Data import for Branding Audit ---------------------------------------------------------------------

MFDC_branding_audit = read_excel("datasets/MFDC Branding Audit.xlsx", sheet = 1)

audit_branding_lolly =ggplot(MFDC_branding_audit, aes(x= Rating, y=factor(Metrics))) +
  geom_point(aes(color=Rating), size = 6) +
  scale_colour_gradientn(colours=c("red2", "yellow2", "green2")) +
  geom_segment(aes(x=0, xend=Rating, yend=Metrics, color=Rating), size =3) +  #the size should be outside the aes - line weight
  scale_x_continuous(expand = expand_scale(mult = c(0, .1))) +
  scale_y_discrete(expand = expand_scale(mult = c(.2, .2))) +
  geom_text(aes(label = Rating ), position = position_nudge(x=0.2, y=0)) +
  labs(caption = "Analytics Division, 2018") +
  xlab("Rating (1-5)") +
  ylab("") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 12),
        text = element_text(size = 14),
        legend.key.width = unit(4, "cm"),
        title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "plain", size = 12),
        axis.line.x = element_line(size = .8, lineend = "square"),
        axis.line.y = element_line(size = .8, lineend = "butt"),
        panel.border = element_rect(fill = NA),
        plot.caption = element_text(face = "plain", size = 9),
        legend.position = "bottom",
        legend.direction = "horizontal")


# Data import and Graphs for Marketing Audit ---------------------------------------------------------------------

MFDC_Audit_Template_Draft <- read_excel("datasets/MFDC Audit Template Draft.xlsx", sheet = "ILS, Social, Website Audit")
MFDC_Audit_Template_Draft = MFDC_Audit_Template_Draft[,1:6]
MFDC_Audit_Template_Draft$Marketing_Metric = as.numeric(MFDC_Audit_Template_Draft$Marketing_Metric) 

ILS_audit = filter(MFDC_Audit_Template_Draft, Type == "ILS Audit")
reputation_audit = filter(MFDC_Audit_Template_Draft, Type == "Reputation Audit")
social_audit = filter(MFDC_Audit_Template_Draft, Type == "Social Media/Web Audit")

#Make the links clickable hyper links that open in a new tab
ILS_audit$Link = paste0("<a href='",ILS_audit$Link,"' target='_blank'>",ILS_audit$Link,"</a>")


audit_lolly =ggplot(MFDC_Audit_Template_Draft, aes(x= Marketing_Metric, y=factor(Source))) +
  geom_point(aes(color=Marketing_Metric), size = 6) +
  scale_colour_gradientn(colours=c("red2", "yellow2", "green2")) +
  geom_segment(aes(x=0, xend=Marketing_Metric, yend=Source, color=Marketing_Metric), size =3) +  #the size should be outside the aes - line weight
  scale_x_continuous(expand = expand_scale(mult = c(0, .1))) +
  scale_y_discrete(expand = expand_scale(mult = c(.2, .2))) +
  facet_grid(Type~ ., scales = "free", space = "free", switch = "x") +
  geom_text(aes(label = Marketing_Metric ), position = position_nudge(x=0.2, y=0)) +
  labs(caption = "Analytics Division, 2018") +
  xlab("Rating (1-5)") +
  ylab("Source") +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text(size = 12),
        legend.key.width = unit(4, "cm"),
        title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "plain", size = 12),
        axis.line.x = element_line(size = .8, lineend = "square"),
        axis.line.y = element_line(size = .8, lineend = "butt"),
        panel.border = element_rect(fill = NA),
        plot.caption = element_text(face = "plain", size = 9),
        legend.position = "bottom",
        legend.direction = "horizontal",
        strip.text.y = element_text(margin = margin(l =20), size = 14))


# Google Reviews and Leaflet Map with Competitors  ------------------------------------------------------------------------------------


# create value object for API key
# fill in with you own API key
key <- 'XXXXX-XXXXX'

# first generate the data for the property of interest ###
prop = google_places(search_string = property_google_searchterm,
                     key = key)
df_prop = cbind(prop$results$name, prop$results$rating, prop$result$geometry$location$lng, prop$result$geometry$location$lat, prop$results$vicinity, prop$results$id)
df_prop= as.data.frame(df_prop)
names(df_prop) = c("Name", "Rating", "Lon","Lat", "Goodle_ID") 
df_prop$Rating = as.numeric(as.character(df_prop$Rating))
df_prop$Lon = as.numeric(as.character(df_prop$Lon))
df_prop$Lat = as.numeric(as.character(df_prop$Lat))

# then generate the data tables for the competitor props ###
res <- google_places(location = c(df_prop$Lat, df_prop$Lon),
                     keyword = "Apartment",
                     radius = 10000,
                     key = key)

df = cbind(res$results$name, res$results$rating, res$result$geometry$location$lng, res$result$geometry$location$lat, res$results$vicinity, res$results$id)
df = as.data.frame(df)
names(df) = c("Name", "Rating", "Lon","Lat", "Address", "Goodle_ID") 
df$Rating = as.numeric(as.character(df$Rating))
df$Lon = as.numeric(as.character(df$Lon))
df$Lat = as.numeric(as.character(df$Lat))


icon.fa <- makeAwesomeIcon(icon = "flag", markerColor = "red", library = "fa", iconColor = "black")
# the actual generation of the map goes in the server function

#These are the columns of interest for the data table that will be rendered as well
gmaptable = df[,c(1,2,5)]

#get the average google ratings minus those that have none
meancomp_rating_google = df %>%
  dplyr::filter(Rating != 0) %>%
  summarise(avg = mean(Rating))


# get the mean property raing
meanprop_rating_google = select(df_prop, contains("Rating"))



#####Dashboard Active Components #############################################################################################


# Simple header ---------------------------------------------------------------------------------------------------

dm <- dropdownMenu(type="messages")
mm <- dropdownMenu(type="notifications")
tm <- dropdownMenu(type="tasks")

header <- dashboardHeader(title="Dashboard", dm, mm, tm)

# Bottom Row Single Metric Data --------------------------------------------------------------------------------------

fr.welcome.page.infoboxes = fluidRow(width = 12, 
  valueBox( value = "6%",
            subtitle = "Avg. Rental Income Increase", 
            icon = icon("line-chart", lib="font-awesome"),
            color = "light-blue", 
            href = "https://github.com/DrContrary"),
  valueBox( value = "5-10%",
            subtitle = "Average Occupancy Increase",
            icon = icon("users", lib="font-awesome"), 
            color = "light-blue", 
            href = "https://github.com/DrContrary"),
  valueBox( value = "Up to 15%",
            subtitle = "Increased Conversion Ratio",
            icon = icon("building-o", lib="font-awesome"), 
            color = "light-blue", 
            href = "https://github.com/DrContrary"))


# Landing Page --------------------------------------------------------------------------------------------------------

b.data.credits = fluidRow(
  box(
  width = 12,
  status = "primary",
  "Interactive Dashboard Designed and Delivered by Analytics Division"
  ))

welcome.page = fluidRow(
  column(width = 4,
         box(
           title = "Created by", 
           width = NULL,
           collapsible = FALSE,
           solidHeader = TRUE,
           status = "primary",
           a(img(src = "Color Logo.png", height = "100%", width = "100%"),href = "https://github.com/DrContrary/", target="_blank")
           ),
         box(
           h2("Partner with the Best"),
           width = NULL,
           collapsible = FALSE,
           solidHeader = TRUE,
           status = "primary",
           h4("With a combined 30 years experience in the multifamily industry, we deliver focused  individualized marketing plans to fit the needs of our clients. We have hands on experience as Property Managers, Regional Property Managers, National Marketing Directors, and Direct Asset Managers.  When you work with us, you get a marketing plan from individuals who know the industry from the top down. Our proven strategic approach will raise your property's brand visibility and provide the highest return on investment.")
         )
  ),
  column(width = 8,
         box(
           title = "Your Dashboard", width = NULL, solidHeader = TRUE, status = "primary",
           h3("Make Data Driven Decisions to Increase Your Return on Investment"),
           "Better understand your market to make your investments count"
         ),
         box(
           title = property_name, width = NULL, solidHeader = TRUE, status = "primary",
           tags$img(src = property_image_landing , height = "100%", width = "100%")
         )
  )
)

# Credits page -----------------------------------------------------------------------

credits_page = 
  fluidPage(br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
    fluidRow(
      column(width = 5),
      column(width = 4,
             actionButton("credits_dialog", "Roll Credits")),
  column(width = 3)))

# Body Boxes with Graphs Tables and PDFs ---------------------------------------------------------------------------------------------------------------------------

tb.design.branding = tabBox(
  title = "", # not necessary here. titlePanel() in UI looks better.
  width = 12,
    tabPanel("Audit Summary",
             br(), DT::dataTableOutput("table.branding.audit")
             )
  )

tb.strategic.plan = tabBox(
  title = "", # not necessary here. titlePanel() in UI looks better.
  width = 12,
  tabPanel("Plan Detail PDF",
           tags$iframe(style = "width: 100%; height: 500px; scrolling = yes",
                       src =  marketingplan_PDF)
  )
)    

facet_affordability = box(
  title = "Tiered Affordability",
  width = 12,
  status = "primary", 
  solidHeader = TRUE, 
  collapsible = TRUE,
  collapsed = FALSE,
  column(width = 12, 
         plotOutput(outputId = "facet_afford_output")))

affordability_selected = box(title = "Tiered Affordability Detail",
                             width = 12,
                             status = "primary", 
                             solidHeader = TRUE, 
                             collapsible = TRUE,
                             collapsed = TRUE,
                             column(width = 12, 
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "Region", "Select Region:",
                                                    choices = sort(unique(Tiered$RegionName)),
                                                    selected = "New York, NY"),
                                        helpText("Select a Region of Interest")),
                                      mainPanel(
                                        plotOutput(outputId = "afford_selected")))))

median_rent_reactive = fluidPage(
  sidebarLayout(
    sidebarPanel ("Choose a State and City below to view rent data for your area.", width = 12, 
                  fluidRow(
                    column(width = 4, #helpText("Choose a State and City\n\to view rent data for your area"), 
                        selectInput("State", label = "State:", choices = sort(unique(median.rent$State)), selected = '')),
                    column(width = 4, 
                        selectInput("City", label = "City", choices = NULL, selected = NULL)),
                    column(width = 3, offset = 1, br(), actionButton("goButton", "Get Data"))
                    )
                  
    ),
    mainPanel(width = 12,
      tabBox(width = 12, height = "3000px",
             tabPanel("Stacked", br(), fluidRow(column(width = 12, height = "100%", plotOutput(outputId = "medianrentplot")))),
             tabPanel("Multi-panel", br(), plotOutput(outputId = "medianrentfacet")),
             tabPanel("Data Table", br(), DT::dataTableOutput("table"))
      )
    )
  )
)

ILS = tabBox(
  title = "ILS Reporting",
  width = 12,
  # The id lets us use input$tabset1 on the server to find the current tab
  id = "tabset1", height = "700px",
  tabPanel("Summary", "Summary of Findings"),
  tabPanel("Data", "Data from ILS Report", 
           column(width = 12, DT::dataTableOutput("ilstable"),
                  downloadButton("downloadData", "Download")),
           helpText("Click Here to Download")))



# Google Reviews and Maps ---------------------------------------------------------

### Get the revew information for the property using google place details function
dfprop_details <- google_place_details(place_id = prop$results[1, "place_id"],  #the place ID is generated from the property place search
                                       key = key)
df_reviews <- dfprop_details$result$reviews
Greview_1 = dQuote(df_reviews[1,7])
Greview_2 = dQuote(df_reviews[2,7])
Greview_3 = dQuote(df_reviews[3,7])
Greview_4 = dQuote(df_reviews[4,7])
Greview_5 = dQuote(df_reviews[5,7])

Grating_1 = paste0("Rating:", " ", (df_reviews[1,5]), "/", "5")
Grating_2 = paste0("Rating:", " ", (df_reviews[2,5]), "/", "5")
Grating_3 = paste0("Rating:", " ", (df_reviews[3,5]), "/", "5")
Grating_4 = paste0("Rating:", " ", (df_reviews[4,5]), "/", "5")
Grating_5 = paste0("Rating:", " ", (df_reviews[5,5]), "/", "5")

Gtime_1 = paste0("Review Posted:", " ",(df_reviews[1,6])) 
Gtime_2 = paste0("Review Posted:", " ",(df_reviews[2,6])) 
Gtime_3 = paste0("Review Posted:", " ",(df_reviews[3,6])) 
Gtime_4 = paste0("Review Posted:", " ",(df_reviews[4,6])) 
Gtime_5 = paste0("Review Posted:", " ",(df_reviews[5,6])) 

#to get the value box of the other properties ratings in the area and round to 1 decimal places
fr.c.avg.google.rating = 
  fluidRow(
    infoBox(value= as.character(round(meancomp_rating_google,1)), 
          "Mean Comparable \n Property Rating", icon = icon("building-o", lib="font-awesome"), color = "light-blue", width = 6),
    infoBox(value= as.character(round(meanprop_rating_google,1)), 
          "Mean Property Rating", icon = icon("building-o", lib="font-awesome"), color = "light-blue", width = 6)
)

# Title boxes ----------------------------------------------------------------------

meanrent.title.box = box(
  title = "Vizualize Trends in Regional Rent Prices to Implement Targeted Pricing Strategies",
  h5("Data:  Median Rent by Zip Code, 2011-2018"),
  status = "primary",
  solidHeader = FALSE,
  width = 12,
  height = "85px",
  collapsible = FALSE
)

afforadability.title.box = box(
  title = "Analyze Rent Affordability Data to Develop More Effective Leasing Goals",
  h5("Data:  Percent of income spent on Rent based on Region and Apartment Price tier, 2011-2016"),
  status = "primary",
  solidHeader = FALSE,
  width = 12,
  height = "85px",
  collapsible = FALSE
)

rentgrowth.title.box = box(
  title = "Evaluate Projected Rent Growth Data to Better Estimate Income Potential",
  h5("Data:  Regional Rent Growth Year over Year, Current and Forecasted"),
  status = "success",
  solidHeader = FALSE,
  width = 12,
  height = "85px",
  collapsible = FALSE
  )

greport.title.box = box(
  title = "Average Google Rating of Local Competitors",
  h5("Interactive map and search table display all competitors within set radius of subject poperty."),
  h6(property_name),
  status = "warning",
  solidHeader = FALSE,
  width = 12,
  height = "110px",
  collapsible = FALSE
)

greport.review.title.box = box(
  title = "What people are saying about your property.",
  h5("Recent Google Reviews on the subject property."),
  h6("Generated live using Google APIs"),
  status = "warning",
  solidHeader = FALSE,
  width = 12,
  height = "110px",
  collapsible = FALSE
)

review.valence.title.box = box(
  title = "Overall Emotional Valence of Online Customer Reviews",
  h6("The proportion of Positve and Negative reviews before and after intervention "),
  width = 12,
  status = "warning",
  height = "110px",
  solidHeader = FALSE,
  collapsible = FALSE
)

ROIcalculator.title.box = box(
  title = "Marketing ROI Calculator",
  h5("Choose values below to simulate your potential Return on Marketing Investment"),
  status = "primary",
  solidHeader = FALSE,
  width = 12,
  height = "85px",
  collapsible = FALSE
)

# box elements for UI to have pie plots rendered within ---------------------

review.valence.before = box(
  collapsible = FALSE
)

review.valence.after = box(
  collapsible = FALSE
)

reviews.before.after = fluidRow(
  width = 12
)

# Google report UI module --------------------------------------------------

greport = 
  fluidPage(
  greport.title.box,
           fluidRow(width = 12, 
                    box(leafletOutput("propertymap", width = "100%", height = 680)),
                    box(DT::dataTableOutput("gtable"), width = 6, height = 700
                        )
                    ),
  fr.c.avg.google.rating,
  hr(), br(),
  greport.review.title.box,
  fluidRow( 
    box(p(em(Greview_1)), br(), hr(), p(strong(Grating_1)), Gtime_1, width = 3, solidHeader = F),
    box(p(em(Greview_2)), br(), hr(), p(strong(Grating_2)), Gtime_2, width = 3, solidHeader = F),
    box(p(em(Greview_3)), br(), hr(), p(strong(Grating_3)), Gtime_3, width = 3, solidHeader = F),
    box(p(em(Greview_4)), br(), hr(), p(strong(Grating_4)), Gtime_4, width = 3, solidHeader = F)
    ),
  hr(), br(),
  review.valence.title.box,
  fluidRow(box(plotOutput("pie_before")), box(plotOutput("pie_after"))
  )
)

# Marketing ROI Calculator -----------------------------------------------------

ROIcalculator_page = fluidPage(
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    sidebarPanel(
      helpText("Change values below to calculate potential Return On Investment (ROI) for a given marketing strategy."),
      hr(), #horizontal rule?
      sliderInput("cost_monthly", "Monthly Marketing Budget:",
                  min = 0, max = 10000,
                  value = 1000, step = 100,
                  pre = "$", sep = ",",
                  animate = FALSE),
      
      hr(),
      
      sliderInput("prospective", "Number of Prospective Leads per Month:",
                  min = 0, max = 500,
                  value = 200, step = 10,
                  animate = FALSE),
      
      hr(),
      
      sliderInput("conversions", "Conversion Rate (Lead-to-Lease):",
                  min = 0, max = 100,
                  value = 10, step = 1,
                  post = "%", sep = ",",
                  animate = FALSE),
      
      hr(),
      
      sliderInput("monthly_rent", "Average Monthly Rent:", # couldn't figure out why my code was broken. mispelled 'montly' here...
                  min = 500, max = 5000,
                  value = 1000, step = 50,
                  pre = "$", sep = ",",
                  animate = FALSE),
      width = 6
      ),
    
    
    # Main panel for displaying outputs 
    mainPanel(width = 6,
              box(title = "Monthly Revenue from New Lease Conversions", status = "success", solidHeader = FALSE,
                  # hr(), 
                  tableOutput("return_month_rev"), 
                  width = "100%")
              ,
              box(title = "Annual Revenue per Lease Conversion", status = "success", solidHeader = FALSE,
              #    hr(),
                  tableOutput("return_per_con"), 
                  width = "100%")
    )
  )
)

#### Twitter Page ---------------------------------------------------------


#you will need make your own app with Twitte Developer to get these keys

api_key <- "XXXXX" #in the quotes, put your API key 
api_secret <- "XXXXXX" #in the quotes, put your API secret token 
token <- "XXXXX-XXXXX" #in the quotes, put your token 
token_secret <- "XXXXXXX" #in the quotes, put your token secret
setup_twitter_oauth(api_key, api_secret, token, token_secret)

tweets= userTimeline(twitterhandle, n = 500) #use this command to get all from the user
tweets.df <-twListToDF(tweets)

#find the most succesful tweet
tweets.df = dplyr::mutate(tweets.df, suminteraction = retweetCount + favoriteCount)
maxtweet = tweets.df %>%
  slice(which.max(suminteraction))
maxtweettime = as.Date(maxtweet$created)
maxtweettime =format(maxtweettime,
                     "%A %B %d")
tweetfavoritecount = maxtweet$favoriteCount
tweetretweetcount = maxtweet$retweetCount


#get the tweets with more than 0 interaction
interactions = tweets.df
interactions = tweets.df %>%
  #dplyr::filter(suminteraction > 0) %>%
  dplyr::mutate(yr = strftime(interactions$created, "%Y")) %>%
  dplyr::mutate(mo = strftime(interactions$created, "%m")) %>%
  dplyr::mutate(day = weekdays(interactions$created))
interactions$created = as.Date(interactions$created)
interactions$day = as.factor(interactions$day)


interactionsbyday = interactions %>%
  dplyr::group_by(day) %>%
  dplyr::summarise(sum = sum(suminteraction))
interactionsbyday$day = ordered(interactionsbyday$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 
interactionsbyday = as.data.frame(interactionsbyday)

plot_tweetsbyday =ggplot(interactionsbyday, aes(x = day, y = sum, fill = day)) +
  geom_bar(stat = "identity") +
  labs(caption = "Analytics Division, 2018") +
  xlab("Day of the Week") +
  ylab("Total Number of Interactions") +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.text = element_text(size = 12),
        title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "plain", size = 12),
        axis.line.x = element_line(size = .8, lineend = "square"),
        axis.line.y = element_line(size = .8, lineend = "butt"),
        plot.caption = element_text(face = "plain", size = 9))


twitterstats= fluidRow(
  infoBox("Twitter User Name:", twitterhandle, icon = icon("twitter", lib="font-awesome"), color = "light-blue", width = 6)
)
succesfultweets = fluidRow(
  box(h3("Most Interacted with Tweet"), hr(), 
      p(em(dQuote(maxtweet$text))), br(), 
      p(paste0("Posted:", "", maxtweettime )), br(),
      p(paste0("Number of Favorites: ", tweetfavoritecount)),
      p(paste0("Number of Retweets: ", tweetretweetcount))))

row_plot_tweetsbyday = fluidRow(column( width = 12, plotOutput(outputId = "tweetsbydayplot"))) 

#get all the data together
numtweetsbyday = tweets.df %>%
  mutate(yr = strftime(tweets.df$created, "%Y")) %>%
  mutate(mo = strftime(tweets.df$created, "%m")) %>%
  mutate(day = weekdays(tweets.df$created))

numtweetsbyday$created = as.Date(numtweetsbyday$created)
numtweetsbyday$day = as.factor(numtweetsbyday$day)
tweet_day = numtweetsbyday %>%
  dplyr::group_by(day) %>%
  dplyr::count(day)
tweet_day = as.data.frame(tweet_day)
tweet_day$day = ordered(tweet_day$day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) 

plot_tweetssentbyday =ggplot(tweet_day, aes(x = day, y = n, fill = day)) +
  geom_bar(stat = "identity") +
  labs(caption = "Analytics Division, 2018") +
  xlab("Day of the Week") +
  ylab("Total Number of Tweets") +
  theme_minimal() +
  theme(legend.position="none",
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20)),
        axis.text = element_text(size = 12),
        title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(face = "plain", size = 12),
        axis.line.x = element_line(size = .8, lineend = "square"),
        axis.line.y = element_line(size = .8, lineend = "butt"),
        plot.caption = element_text(face = "plain", size = 9))

row_plot_tweetssentbyday = fluidRow(column( width = 12, plotOutput(outputId = "tweetsbydayplot2"))) 
           



