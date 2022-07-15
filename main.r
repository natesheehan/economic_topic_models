install.packages("stm")
library(stm)
library(stringr)
library(quanteda)
library(ggplot2)
library(readxl)
library(tidyverse)
# SET UP ------------------------------------------------------------------

# specifying the path for file
path <- "../nathanael/Downloads/wos_eco/"


a = read_xls("savedrecs.xls")
b = read_xls("savedrecs(1).xls")
c = read_xls("savedrecs(2).xls")
d = read_xls("savedrecs(3).xls")
e = read_xls("savedrecs(4).xls")
f = read_xls("savedrecs(5).xls")
g = read_xls("savedrecs(6).xls")
h = read_xls("savedrecs(7).xls")
i = read_xls("savedrecs(8).xls")
j = read_xls("savedrecs(9).xls")


main_df = rbind(a,b,c,d,e,f,g,h,i,j)
rm(a,b,c,d,e,f,g,h,i,j)


#tokenization & removing punctuation/numbers/URLs etc.
tokens <- main_df$`Article Title` %>%
  str_remove_all("comment") %>%
  str_remove_all("COMMENT") %>%
  str_remove_all("us") %>%
  str_remove_all("US") %>%
  str_remove_all("new") %>%
  tokens(what = "word",
         remove_punct = TRUE,
         remove_numbers = TRUE,
         remove_url = TRUE) %>%
  tokens_tolower() %>%
  tokens_remove(stopwords("english"))

#applying relative pruning
dfm <- dfm_trim(dfm(tokens),
                docfreq_type = "prop", verbose = TRUE)

# convert to stm format
dfm_stm <- convert(dfm, to = "stm")



# Statistically find the best number of k ---------------------------------
K <- c(2,4,6,8,10,12,14,18,20)
fit <- searchK(dfm_stm$documents, dfm_stm$vocab, K = K, verbose = TRUE)
#plot
ggplot(plot, aes(K, value, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~variable,scales = "free_y") +
  labs(x = "Number of topics K",
       title = "Statistical fit of models with different K")




# Plot optimal number -----------------------------------------------------
main_df$year <- as.numeric(main_df$`Publication Year`)

model <- stm(documents = dfm_stm$documents,
             vocab = dfm_stm$vocab, 
             K = 15,
             prevalence = ~year,
             data = main_df,
             verbose = TRUE)

plot(model)
labelTopics(model,topics = c(1:10), n=5)

effect <- estimateEffect(formula=~year, stmobj=model, metadata=main_df)
plot(effect, "year", method = "continuous", topics = c(1:15), model = model)





