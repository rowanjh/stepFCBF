load_all()

## Test a tidymodels workflow
library(tidymodels)

# Create test dataset
numeric_feats <-
    as.data.frame(matrix(rnorm(1000), nrow = 100, ncol = 5)) %>%
    set_names(c(paste0("feat_num_", 1:5)))
categ_feats <-
    as.data.frame(matrix(sample(c('A','B','C','D'), replace = TRUE, size = 1000),
                         nrow = 100, ncol = 5)) %>%
    set_names(c(paste0("feat_categ_",1:5)))

dat_dummy <-
    data.frame(out = sample(c("A","B","C","D"), replace = TRUE, size = 100),
               id = paste0("id_",1:100)) %>%
    bind_cols(numeric_feats, categ_feats)

# Put into a recipe
recipe_test <- recipe(out ~ ., data = dat_dummy) %>%
    update_role(id, new_role = "ID") %>%
    step_normalize(all_numeric_predictors()) %>%
    step_FCBF(all_nominal_predictors(), min_su = 0.001)

recipe_prepped <- prep(recipe_test, training = dat_dummy)
dat_baked <- bake(recipe_prepped, dat_dummy)

# model_test <- logistic_reg()
#
# wf_test <- workflow() %>% add_recipe(recipe_test) %>% add_model(model_test)
#
# wf_test %>% fit(dat_dummy)


## TESTS:
# only one outcome
