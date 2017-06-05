
#load libraries
library(pacman)
.libPaths("/Users/scworlan/Rpackages") 
p_load(dplyr,ggplot2,coefplot,rstan,arm,ggmcmc,coda,rethinking,ggrepel,scales,gridExtra)

select <- dplyr::select
rename <- dplyr::rename
summarize <- dplyr::summarize
mutate <- dplyr::mutate

# set wd
setwd("~/Documents/Water Conservation/WRR_paper/analysis")

# read in data
city_data <- read.csv("data/city/awwa_vwci_data_subset.csv",stringsAsFactors = F)

climate_link <- city_data %>%
  mutate(climate_index = coerce_index(climate_region)) %>%
  distinct(climate_index,climate_region)

# load data
m_fixed <- readRDS("scripts/city/models/mg_fixed_city.rds")
m_vary <- readRDS("scripts/city/models/mg_vary_city.rds")
m_avary <- readRDS("scripts/city/models/mg_a_vary_city.rds")

# post_b <- data.frame(extract.samples(m_vary,n=1000)) %>%
#   select(starts_with("b_"),starts_with("a_")) 

# fixed parameters
param_fixed <- data.frame(extract.samples(m_fixed,n=1000)) %>%
  #select(starts_with("b_"),starts_with("a_")) %>%
  select(starts_with("b_")) %>%
  gather() %>%
  rename(parameter=key) %>%
  mutate(parameter = substring(parameter, 3)) %>%
  group_by(parameter) %>%
  summarize(mu=mean(value),
            low=PI(value, prob=0.5)[[1]],
            high=PI(value, prob=0.5)[[2]],
            lower=PI(value, prob=0.8)[[1]],
            higher=PI(value, prob=0.8)[[2]]) %>%
  ungroup() %>%
  arrange(parameter) %>%
  #mutate(parameter=c("aridity","bill_type",
                      #climate_link$climate_region,
                      #"rpp","vwci","water_bill")) %>%
  arrange(mu) %>%
  mutate(parameter=factor(parameter,parameter),
         model="fully pooled") 
  #mutate(type=c(rep("beta",5),rep("alpha",9)))

ggplot(post_b) +
  geom_linerange(aes(x=parameter,ymin=low,ymax=high),size=1,alpha=0.9) +
  geom_linerange(aes(x=parameter,ymin=lower,ymax=higher),size=0.5,alpha=0.7) +
  geom_point(aes(x=parameter,y=mu),color="black", shape=23, size = 1) +
  geom_point(aes(x=parameter,y=mu),color="black", fill="white", shape=23, size = 1) +
  coord_flip() + theme_bw() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(y="parameter estimates",x=expression(beta)) 
  #facet_wrap(~type,ncol=2,scales="free")

# arrange plots
#grid.arrange(p1,p2,ncol=2)

# varying parameters
param_vary <- data.frame(extract.samples(m_vary,n=1000)) %>%
  select(starts_with("b_")) %>%
  gather() %>%
  rename(parameter=key) %>%
  mutate(climate_index=as.numeric(sub(".*\\.", "",parameter)),
         parameter = substring(parameter, 3) %>% 
           substring(.,1,nchar(.)-2)) %>%
  group_by(parameter,climate_index) %>%
  summarize(mu=mean(value),
            low=PI(value, prob=0.5)[[1]],
            high=PI(value, prob=0.5)[[2]],
            lower=PI(value, prob=0.8)[[1]],
            higher=PI(value, prob=0.8)[[2]]) %>%
  ungroup() %>%
  left_join(climate_link,by = "climate_index") %>%
  arrange(mu,climate_region) %>%
  mutate(climate_region = ifelse(climate_region=="East North Central",
                                 "E.N. Central",climate_region),
         climate_region = ifelse(climate_region=="West North Central",
                                 "W.N. Central",climate_region),
         model="partially pooled") 

ggplot(post_b) +
  geom_linerange(aes(x=parameter,ymin=low,ymax=high),size=1,alpha=0.9) +
  geom_linerange(aes(x=parameter,ymin=lower,ymax=higher),size=0.5,alpha=0.7) +
  geom_point(aes(x=parameter,y=mu),color="black", shape=23, size = 1) +
  geom_point(aes(x=parameter,y=mu),color="black", fill="white", shape=23, size = 1) +
  coord_flip() + theme_bw() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(y="parameter estimates",x=expression(beta)) +
  facet_grid(climate_region~.)

ggplot(post_b) +
  geom_linerange(aes(x=climate_region,ymin=low,ymax=high),size=1,alpha=0.9) +
  geom_linerange(aes(x=climate_region,ymin=lower,ymax=higher),size=0.5,alpha=0.7) +
  geom_point(aes(x=climate_region,y=mu),shape=23, size = 1) +
  geom_point(aes(x=climate_region,y=mu), fill="white", shape=23, size = 1) +
  coord_flip() + theme_bw() +
  geom_hline(yintercept=0, linetype="dashed") +
  labs(y="parameter estimates",x=expression(beta)) +
  facet_wrap(~parameter,nrow=1,scales="free_x")
