suppressPackageStartupMessages(library(tidyverse)) 
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales))
vc_fund<- read.csv("investments_VC.csv")
vc_Fund <- na.omit(vc_fund)
attach(vc_Fund)
#Top 10 States visualization
vc_Fund %>% filter(state_code!="") %>% count(state_code)%>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(state_code, n), n)) + geom_col(aes(fill=state_code)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Top 10 States in USA") + xlab("State") + ylab("Count")
#Top 10 countries visualization
vc_Fund %>% filter(country_code!="") %>% count(country_code)%>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(country_code, n), n)) + geom_col(aes(fill=country_code)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Country of Origin") + xlab("County") + ylab("Count")
#Top 10 market leaders visualization
vc_Fund %>% filter(market!="") %>% count(market)%>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(market, n), n)) + geom_col(aes(fill=market)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Top 10 Market Leaders") + xlab("Market") + ylab("Count")
#Founded years (>=2000) visualization
vc_Fund %>% filter(founded_year>1999) %>% count(founded_year)%>% arrange(-n) %>%
  ggplot(aes(founded_year, n), n) + geom_col(aes(fill=founded_year)) +
  theme(legend.position="none") +
  ggtitle("Year Founded") + xlab("Year") + ylab("Count") +
  theme(axis.text.x=element_text(angle=40))
#Top 10 region visualization
vc_Fund %>% filter(region!="") %>% count(region)%>% arrange(-n) %>% head(10) %>%
  ggplot(aes(reorder(region, n), n)) + geom_col(aes(fill=region)) + coord_flip() +
  theme(legend.position="none") +
  ggtitle("Top 10 Regions with Most Startups") + xlab("Region") + ylab("Count")
#Pie chart visualization of companies' status
slices <- c(sum(status=="operating"), sum(status=="acquired"),
            sum(status=="closed"))
lbls <- c("operating", "acquired", "closed")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls,"%",sep="")
colors = c("green", "yellow", "grey")
pie(slices, labels = lbls, main="Status of Startup", col=colors)
# v_and_s <- venture+seed
# Regression venture+seed vs. # funding rounds
# reg_vs_fr <- lm(funding_rounds~log(v_and_s))
# summary(reg_vs_fr)
# plot(reg_vs_fr)
# chisq.test(market)
# Count companies based on market sectors

# #Chisq-test to figure out the proportion 
# #H0: p(bio)=p(cur)=p
# chisq.test(head(market_table, n=5))
detach()
sample_top5_funding_total<- subset(vc_Fund, market ==" Biotechnology " | market ==" Software " |
                            market ==" Mobile " 
                          | market==" E-Commerce " | market==" Curated Web ", 
                          select=c(market, funding_total_usd))
sample_top5_funding_total2<-subset(sample_top5_funding_total, funding_total_usd!=" NA   ")
sample_top5_funding_total2$funding_total_usd<-as.numeric(gsub(",", "", sample_top5_funding_total2$funding_total_usd))
options(scipen=999)
hist(sample_top5_funding_total2$funding_total_usd)
boxplot(funding_total_usd~market, data=sample_top5_funding_total2)
write.csv(sample_top5_funding_total2, "sample_top5_funding_total2.csv")
#ANOVA
#h0: mean(soft) = mean(boit) = meam(mobile) = ....
#alternative: at least one of the means is different
total_lm <- lm(funding_total_usd~market, data=sample_top5_funding_total2)
anova(total_lm)
#Fail to reject h0 at 0.05 significant level
#It's different than which one is different? Use pairwise t-test to figure out
attach(sample_top5_funding_total2)
pairwise.t.test(funding_total_usd, market, "bonferroni")
#there's a significant difference between mobile and software 
#there's a difference between curate web and mobile
#Find top 5 proportions
market_table = sort(table(market), decreasing = TRUE)
head(market_table, n=5)
proportion <- function (x){
  x/9370
}
top_5_count <- c(3327,2373,1402,1175,1093)
sample_top_5_proportion <- proportion(top_5_count)
#Chisq-test Goodnes-of-fit Test
chisq.test(top_5_count, correct = FALSE)
#rh0 ; p-value too small 
#There is strong evidence that the proportion of each market are different among the top 5 market leaders
