# Binomial GLMMs

```{r}
# d <- readr::read_csv("~/Dropbox/Mesocosm 2012/Meta emerg.csv")

library(tidyverse)
d <- read_csv("data/raw/oregan-etal-2013/meso.csv")
d$date <- lubridate::mdy(d$date)
d <- filter(d, !tank %in% c(23, 30, 35)) # simplify

max_time <- max(d$time)
dat <- list()
for (i in seq_len(nrow(d))) {
  ti <- d[i,]$time
  dat[[i]] <- data.frame(day = seq(1, max_time), 
    meta = c(rep(0, ti-1), rep(1, max_time - ti + 1)),
    frog = i, tank = d[i,"tank"], warming = d[i,"climate"], 
    drying = d[i,"drying"], species = d[i,"species"])
}
dat <- bind_rows(dat)
d <- filter(dat, day %in% seq(0, max_time, 4)) # downsample 
species_letters <- tibble(species = c("spadefoot", "treefrog", "redlegged"),
  sp = c("b", "c", "a"))
d <- inner_join(d, species_letters) %>%
  rename(species_true = species, species = sp) %>%
  mutate(treatment = paste(climate, drying, sep = "+")) %>% 
  mutate(warming = ifelse(climate == "warming", 1, 0),
    drying = ifelse(drying == "temp", 1, 0))  %>% 
    select(-climate)
d <- mutate(d, day_centered = day - mean(day), week_centered = day_centered/7)

library(lme4)

m_global <- glm(meta ~ week_centered + species + warming * drying, 
  family = binomial(link = "logit"), data = d)
summary(m_global)

m_list <- lmList(meta ~ week_centered | tank, data = d, family = binomial(link = "logit"))

intercepts <- map_dbl(m_list, function(x) coef(x)[[1]])
slopes <- map_dbl(m_list, function(x) coef(x)[[2]])
hist(intercepts)
hist(slopes)

m <- glmer(meta ~ week_centered + species + warming * drying + (1 | tank), 
  family = binomial(link = "logit"), data = d)

# m2 <- glmer(meta ~ week_centered + species + warming * drying + (1 + week_centered | tank), 
  # family = binomial(link = "logit"), data = d)

summary(m, correlation = FALSE)

VarCorr(m)

plot(m, type = c("p", "smooth"))
plot(m, type = c("p", "smooth"), ylim = c(-2, 2))
plot(m, species~resid(.,type = "pearson"), xlim = c(-5, 5))
plot(m, paste(warming, drying)~resid(.,type = "pearson"), xlim = c(-5, 5))

lattice::dotplot(ranef(m, condVar = TRUE))

confint(m, method = "Wald")

m1 <- update(m,.~.-species)
anova(m, m1)

d$prediction <- predict(m, type = "response")

ggplot(d, aes(day, prediction, group = frog, color = species_true)) +
  geom_line(alpha = 0.3) +
  facet_wrap(warming~drying) +
  geom_point(aes(y = meta), alpha = 0.1, position = position_jitter(height = 0.1))

library("glmmTMB")

m2 <- glmmTMB(meta ~ week_centered + species + warming * drying + 
  (1 + week_centered | tank), 
  family = binomial(link = "logit"), data = d)
summary(m2)
d$prediction2 <- predict(m2, type = "response")

ggplot(d, aes(day, prediction2, group = frog, color = species_true)) +
  geom_line(alpha = 0.3) +
  facet_wrap(~treatment) +
  geom_point(aes(y = meta), alpha = 0.1, position = position_jitter(height = 0.1))
```
