### librarys

library(readxl)
library(tidyverse)
library(ggpubr)
library(xlsx)

### open file
data_path <-  "D:/Art_Exp_Data/BEH_AN/practice/New_Data/6_1_familiarity.csv"
df <- read.csv(data_path,  header = TRUE, sep = "", quote = "\"", dec = ".", row.names=NULL)
  

rt.mean <- mean(df$RT) ### compute mean reaction time
st.sd <- sd(df$RT) ### compute sd reaction time
upper_lim <- rt.mean + (2*st.sd) ###define limits

if ((rt.mean - (2*st.sd)) < 0) {
  lower_lim <- 0
} else {
    lower_lim <- rt.mean - (2*st.sd)
}


art <- c("10","11","12","13","14","15","16","17") # 1 unart 2 art
animals <- c("10","11","14","15","40","41","44","45") # 1 animal 2 tool
implicit <- c("10","12","14","16", "40", "42", "44","46") #1 implicit 2 explicit
real <- c("10","11","12","13","40","41","42","43") #1 real 2 unreal


############################################################################################################################
##############################_________________FUNCTIONS____________________################################################


mean.by.bool <- function(datafr, by, var) {
  
  X <- datafr %>%
    group_by({{ by }},name) %>%
    summarise(avg = mean({{ var }})) %>%
    as.data.frame() 
  
  kal <- names(X)
  print(kal)
  kkal <- kal[1]
  print(kkal)
  kkall <- wilcox.test(avg ~ eval(as.symbol(kkal)), data = X, paired = FALSE)
  
  print(kkall)
  column <- ensym(by)
  plot <- ggplot(X, 
                 aes(x = !!column, 
                     y = avg, 
                     fill  = !!column, 
                     group = !!column)) +
    geom_col()+
    ggtitle(kkall$p.value)
  print(plot)
}

mean.by.rt <- function(datafr, by, var) {
  
  X <- datafr %>%
    group_by({{ by }},name) %>%
    summarise(avg = mean({{ var }})) %>%
    as.data.frame()
  
  kal <- names(X)
  kkal <- kal[1]
  
  kkall <- wilcox.test(avg ~ eval(as.symbol(kkal)), data = X, paired = FALSE)
  
  
  ggboxplot(X, x = kal[1], y = "avg", 
            color = kal[1], palette = c("#00AFBB", "#E7B800"),
            ylab = "RT", xlab = "Groups", title =  kkall$p.value)
  
}

create.types <- function (df_temp) {
  df_t <- df_temp %>%
    mutate(art = ifelse(trigger %in% art, "art",ifelse(trigger == "0", 0, "unart"))) %>% 
    mutate( animals = ifelse(trigger %in% animals, "animal", ifelse(trigger == "0", 0, "tool")))%>%
    mutate( implicit = ifelse(trigger %in% implicit, "implicit", ifelse(trigger == "0", 0, "explicit")))%>%
    mutate( real = ifelse(trigger %in% real, "real", ifelse(trigger == "0", 0, "unreal")))%>%
    data.frame()
  
  assign(deparse(substitute(df_temp)), df_t, envir=.GlobalEnv) #Create new var 
}

chi.square.bool <-function (datafr) {
  X <- datafr
  freq_data<-table (X$animals, X$bool)
  result <- chisq.test(freq_data)
  ggplot(X, aes(bool) ) +
    geom_bar() + facet_grid(animals ~ .) + ggtitle(result$p.value)
}




###############################################################################################################################
#####BOOL_ANALYSIS####



#PREPARING DATASET

df_bool_base <- df %>% 
  filter(between(RT, lower_lim, upper_lim))%>% 
  filter(trigger != "0") %>%
  filter(trigger != "20")
  
    
df_bool_wilc <- df_bool_base %>% 
  mutate( bool = ifelse(bool == "True",1,0))%>%
  group_by(name, trigger)%>%
  summarise(avg = mean(bool))


create.types(df_bool_wilc)
create.types(df_bool_base)




######################################################
####BOOL ANALYSIS #################################
######################################################


mean.by.bool(df_bool_wilc,impl,avg)
chi.square.bool(df_bool_base)


#SPLITTING REAL

df_bool_wilc_new <- df_bool_wilc %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (real == "unreal") 


mean.by.bool(df_bool_wilc_new,implicit,avg)
 
#SPLITTING_NEW_EXPL_IMPL
df_bool_implicit <- df_bool_wilc_new %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (implicit == "implicit") 

mean.by.bool(df_bool_implicit,animals,avg)


df_bool_explicit <- df_bool_wilc_new %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (implicit == "explicit") 

mean.by.bool(df_bool_explicit,animals,avg)

###############################################################################################################################
#################################_____________________RT_ANALYSIS____________##################################################
###############################################################################################################################


#PREPARING 

df <- df %>% 
  filter(between(RT, lower_lim, upper_lim)) %>% 
  filter (bool == "True") 



df <- group_by(df, name, trigger)
df <- summarise(df, avg = mean(RT))


df <- df %>% mutate(art = ifelse(trigger %in% art, 1,ifelse(trigger == "0", 0, 2))) %>% 
  mutate( animals = ifelse(trigger %in% animals, 1, ifelse(trigger == "0", 0, 2)))%>%
  mutate( implicit = ifelse(trigger %in% implicit, 1, ifelse(trigger == "0", 0, 2)))%>%
  mutate( real = ifelse(trigger %in% real, 1, ifelse(trigger == "0", 0, 2)))

df <- df %>% 
  filter(trigger != "0") %>%
  filter(trigger != "20")


#ANALYSIS 

mean.by.rt(df,real,avg)


#splitting_NEW
df_cond_1 <- df %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (real == "1") 

df_cond_2 <- df %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (real == "2") 


mean.by.rt(df_cond_2,implicit,avg)


#SPLOTTING NEW EXPL IMPL

df_impl <- df_cond_2 %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (implicit == "1") 

mean.by.rt(df_impl,art,avg)
  
  

df_expl <- df_cond_2 %>% 
  select(name, trigger, avg, art, animals, implicit, real) %>% 
  filter (implicit == "2") 

mean.by.rt(df_expl,animals,avg)


