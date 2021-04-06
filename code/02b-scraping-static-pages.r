### -----------------------------
## simon munzert
## web scraping
### -----------------------------


## load packages -----------------

library(rvest)
library(stringr)


## inspect the source code in your browser ---------------

browseURL("https://www.nytimes.com/")

# Chrome:
  # 1. right click on page
  # 2. select "view source"

# Firefox:
  # 1. right click on page
  # 2. select "view source"

# Microsoft Edge:
# 1. right click on page
# 2. select "view source"

# Safari
  # 1. click on "Safari"
  # 2. select "Preferences"
  # 3. go to "Advanced"
  # 4. check "Show Develop menu in menu bar"
  # 5. click on "Develop"
  # 6. select "show page source"
  # 7. alternatively to 5./6., right click on page and select "view source"



## basic workflow of scraping with rvest  ----------

# see also: https://github.com/hadley/rvest
# convenient package to scrape information from web pages
# builds on other packages, such as xml2 and httr
# provides very intuitive functions to import and process webpages

# 1. specify URL
url <- "https://www.hotnigerianjobs.com/"

# 2. download static HTML behind the URL and parse it
url_parsed <- read_html(url)


# 3. extract specific nodes with XPath


header3 <- lapply(paste0('https://www.jobberman.com/jobs?sort_by=featured&page=', 1:100),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes("h3") %>% 
                      html_text()
                  }) %>% unlist() %>% as.data.frame()


role3 <- lapply(paste0('https://www.jobberman.com/jobs?sort_by=featured&page=', 1:113),
                  function(url){
                    url %>% read_html() %>% 
                      html_nodes(".padding-lr-10.gutter-flush-under-lg") %>% 
                      html_text()
                  }) %>% unlist() %>% as.data.frame()




role4<- trimws(role3$.) %>% as.data.frame #remove all white spaces

table<-table(role4$.) %>% as.data.frame()


table1<-table %>% 
  group_by(Var1) %>% 
  mutate(Percentage = 100*(round(Freq/ sum(table$Freq), 3))) %>% rename( Job_Function = Var1) 



table1$per_z <- round((table1$Percentage - mean(table1$Percentage))/sd(table1$Percentage), 1)  # compute normalized mpg

table1$per_type <- ifelse(table1$per_z < 0, "below", "above")  # above / below avg flag

table1$per_type2 <- ifelse(table1$Percentage < 50, "below", "above")  # above / below avg flag

table1 <- table1[order(table1$per_z ), ]  # sort
table1$Job_Function <- factor(table1$Job_Function , levels = table1$Job_Function )  # convert to factor to retain sorted order in plot.


library(ggplot2)
theme_set(theme_bw())

# Plot
myplot<-ggplot(table1, aes(x=Job_Function, y=Percentage, label=Percentage)) + 
  geom_point(stat='identity', fill="black", size=8)  +
  geom_segment(aes(y = 5, 
                   x = Job_Function, 
                   yend = Percentage, 
                   xend = Job_Function), 
               color = "black") +
  #scale_color_manual(name="Job_Type", 
                     #labels = c("Above Average", "Below Average"), 
                    # values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Vacancies Posted on Jobberman by Job Functions", 
       subtitle="Percentages from 2,247 jobs posted in the past 3 months",
       caption = "©Abiola Oyebanjo, March 31, 2021" )+ 
  ylim(0.3, 14) +
  coord_flip()

myplot



# Plot
myplot2<-ggplot(table1, aes(x=Job_Function, y=Percentage, label=Percentage)) + 
  geom_point(stat='identity',aes(col=per_type), size=8)  +
    scale_color_manual(name="Job_Type", 
  labels = c("Above Average", "Below Average"), 
  values = c("above"="#00ba38", "below"="#f8766d")) + 
  geom_text(color="white", size=2) +
  labs(title="Vacancies Posted on Jobberman by Job Functions", 
       subtitle="Percentages from 2,247 jobs posted in the past 3 months",
       caption = "©Abiola Oyebanjo, March 31, 2021" )+ 
  ylim(0.3, 14) +
  coord_flip()

myplot2






# 2. Save the plot to a pdf
ggsave("myplot2.png", scale = 2)

?ggsave


?labs


  
 

# 5. tidy headlines
headings <- str_replace_all(header2, "\\n|\\t|\\r", "") %>% str_trim()
headings<-headings %>% as.data.frame()
head(headings)
length(headings)

head(headings)
length(headings)




## extract data from tables --------------

## HTML tables 
  # ... are a special case for scraping because they are already very close to the data structure you want to build up in R
  # ... come with standard tags and are usually easily identifiable

## scraping HTML tables with rvest

url_p <- read_html("https://en.wikipedia.org/wiki/List_of_potentially_habitable_exoplanets")
tables <- html_table(url_p, header = TRUE, fill = TRUE)
exoplanets <- tables[[2]]
exoplanets

## note: HTML tables can get quite complex. there are more flexible solutions than html_table() on the market (e.g., package "htmltab") 



### working with SelectorGadget ----------

# to learn about it, visit
vignette("selectorgadget")

# to install it, visit
browseURL("http://selectorgadget.com/")
# and follow the advice below: "drag this link to your bookmark bar: >>SelectorGadget>> (updated August 7, 2013)"

## SelectorGadget is magic. Proof:
browseURL("https://eu.usatoday.com/")

url <- "https://eu.usatoday.com/"
xpath <-  '//*[contains(concat( " ", @class, " " ), concat( " ", "headline", " " ))]' # important, use single quotation marks to embrace the entire expression to avoid conflict with SelectorGadget's double quotation marks in generated expressions)
url_parsed <- read_html(url)
html_nodes(url_parsed, xpath = xpath) %>% html_text()





