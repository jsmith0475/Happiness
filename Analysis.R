# World Happiness Report
# https://www.kaggle.com/unsdsn/world-happiness


library(ggplot2)
library(tidyverse)
library(tidymodels)
library(archivist)
library(stringr)

library(DALEX)
library(modelStudio)

library(ranger)    # Random Forest
library(e1071)       # SVM

# Import Training Data (2015, 2016, 2017, 2018)
happiness_tr <- happiness_train
happiness_tr <-add_rownames(happiness_tr, var = "rowname")
head(happiness_tr)
happiness_tr %>% filter(str_detect(rowname, 'United States'))
happiness_tr %>% filter(str_detect(rowname, 'Mexico'))

happieness_tr_data <- happiness_tr[,!names(happiness_tr) %in% "rowname"]


# Import Testing Data (2019)
happiness_te <- happiness_test
happiness_te <- add_rownames(happiness_te, var = "rowname")
happiness_te_usa <- happiness_te %>% filter(str_detect(rowname, "United States"))
happiness_te %>% top_n(5, score)
happiness_te %>% top_n(-5, score)
happieness_te_data <- happiness_te[,!names(happiness_te) %in% "rowname"]

# Plot Score vs GDP
ggplot(happieness_tr_data, aes(y=score, x=gdp_per_capita)) + 
  geom_point() +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")


# Fit a Random Forest and Generalized Linear Model
model_ranger <- ranger(score ~., data = happieness_tr_data)
model_glm <- glm(score ~ ., data = happieness_tr_data)
model_svm <- svm(score ~ ., data = happieness_tr_data)

# Create an explainer for the models    
explainer_ranger <- DALEX::explain(model_ranger,
                     data = happieness_te_data,
                     y = happieness_te_data$score,
                     label = "Random Forest")
explainer_glm <- DALEX::explain(model_glm,
                                   data = happieness_te_data,
                                   y = happieness_te_data$score,
                                   label = "General Linear Model")
explainer_svm <- DALEX::explain(model_svm,
                                data = happieness_te_data,
                                y = happieness_te_data$score,
                                label = "Support Vector Machine")

vip_glm <- variable_importance(explainer = explainer_glm,
                               loss_function = loss_root_mean_square) 
vip_ranger <- variable_importance(explainer = explainer_ranger,
                                  loss_function = loss_root_mean_square) 
vip_svm <- variable_importance(explainer = explainer_svm,
                                  loss_function = loss_root_mean_square)
plot(vip_glm, vip_ranger, vip_svm, max_vars = 10)


# Make a studio for the model
modelStudio(explainer_svm, N=500, B=15)


# Show results for USA
bd_usa <- predict_parts(explainer = explainer_svm,
                       new_observation = happiness_te_usa,
                       type = "break_down_interactions")
bd_usa
plot(bd_usa,
     title = "USA SVM Break Down")


