library(readr)
library(ggplot2)


Tiered <- read_csv("datasets/TieredAffordability_Rental.csv")
Tiered$tier = as.factor(Tiered$tier)
Tiered$RegionName =  as.factor(Tiered$RegionName)
Tiered$RegionID = as.factor((Tiered$RegionID))
Tiered$date = as.Date(Tiered$date, "%y/%m/%d")


Tiered = as.data.frame(Tiered) 
affordability_static= ggplot(Tiered, aes(date, PercentIncomeSpentOnRent)) +
  geom_point(aes(color = factor(tier)), size= 2) + 
  facet_wrap(.~RegionName) +
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

#########################
#     Define Elements   #
#########################

### Title Box ###
afforadability.title.box = box(
  title = "Analyze Rent Affordability Data to Develop More Effective Leasing Goals",
  h4("Percent of income spent on Rent based on Region and Apartment Price tier, 2011-2016"),
  h5("Data: Courtesy of Zillow"),
  status = "primary",
  solidHeader = FALSE,
  width = 12,
  collapsible = FALSE
)

### Non-Reactive Element Box ###
facet_affordability = box(
  title = "Tiered Affordability",
  width = 12,
  status = "primary", 
  solidHeader = TRUE, 
  collapsible = TRUE,
  collapsed = FALSE,
  column(width = 12, 
         plotOutput(outputId = "facet_afford_output")))

### NReactive Element Box ###
affordability_selected = box(title = "Tiered Affordability Detail",
                             width = 12,
                             status = "primary", 
                             solidHeader = TRUE, 
                             collapsible = TRUE,
                             collapsed = FALSE,
                             column(width = 12, 
                                    sidebarLayout(
                                      sidebarPanel(
                                        selectInput(inputId = "Region", "Select Region:",
                                                    choices = sort(unique(Tiered$RegionName)),
                                                    selected = "New York, NY"),
                                        helpText("Select a Region of Interest")),
                                      mainPanel(
                                        plotOutput(outputId = "afford_selected")))))


