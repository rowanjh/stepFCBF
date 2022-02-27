library(recipes)
test_that("basic usage: expected columns retrieved", {
    data(iris)
    my_iris <- iris
    my_iris[['lglfeat']] <- c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)
    my_iris[['partial_NAfeat']] <- c(2,3,6,4,3,NA)
    rec <- recipe(Species ~ ., data = my_iris) %>%
        step_fcbf(all_predictors(), min_su = 0.001)
    rec_p <- prep(rec, training = my_iris)
    iris_bake <- bake(rec_p, new_data = my_iris)

    expect_equal(names(iris_bake), c("Sepal.Width", "Petal.Width", "Species"))
})

test_that("warns/breaks if not enough predictors are provided", {
    # Warn if one usable predictor is provided to fcbf
    rec1 <- recipe(Species ~ Sepal.Length, iris) %>%
        step_fcbf(all_predictors())
    expect_warning(prep(rec1, training = iris), "Only one usable")

    # Stop if no usable predictors are provided to fcbf
    rec2 <- recipe(Species ~ ., iris[,'Species', drop = FALSE]) %>%
        step_fcbf(all_predictors())

    expect_error(prep(rec2, training = iris), "No usable predictors")
})

test_that("step_fcbf rejects bad min_su or cutpoint argument input", {
    rec <- recipes::recipe(Species ~ ., data = iris)
    error_cut <- "cutpoint must be a number"
    error_su <- "min_su must be a number"

    expect_error(rec %>% stepFCBF::step_fcbf(min_su = 1.5), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(min_su = NA), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(min_su = 0), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(min_su = "median"), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(min_su = TRUE), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(min_su = -0.01), error_su)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = 1.5), error_cut)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = NA), error_cut)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = 0), error_cut)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = "median"), error_cut)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = TRUE), error_cut)
    expect_error(rec %>% stepFCBF::step_fcbf(cutpoint = -0.01), error_cut)
})

# Return warning if NA columns are provided
test_that("NA columns get removed with warning", {
    na_vec <- rep(NA, 10)
    na_dat <- tibble(out = rep(c("A","B"), 5),
                     f1 = as.character(na_vec),
                     f2 = as.numeric(na_vec),
                     f3 = na_vec,
                     f4 = c(1,4,32,6,4,23,44,54,23,6),
                     f5 = c(1:10))

    inpt_cols <- c('f1','f2','f3','f4','f5')

    rec <- recipes::recipe(out ~ f1+f2+f3+f4+f5, data = na_dat) %>%
        stepFCBF::step_fcbf(recipes::all_predictors())

    expect_warning(remove_NA_cols(inpt_cols, na_dat), "3 features were full")
    expect_warning(recipes::prep(rec, na_dat), "3 features were full")
})


# Return error if outcome is not provided, or not in expected format
test_that("bad outcome variables handled correctly", {
    # No outcome variable specified in recipe
    rec <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors())
    expect_error(recipes::prep(rec, iris), "outcome variable was not found")

    # Code works if outcome = argument supplied, despite no outcome in the recipe
    rec2 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = "Species")
    expect_equal(recipes::prep(rec2, iris) %>% recipes::bake(iris) %>% names,
                 c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

    # Outcome supplied in unexpected format.
    rec3 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = 5)
    expect_error(recipes::prep(rec3, iris), "supplied as a character")

    rec4 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = c("Species", "Petal.Length"))
    expect_error(recipes::prep(rec4, iris), "single outcome variable can be")

    rec5 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = TRUE)
    expect_error(recipes::prep(rec5, iris), "supplied as a character")

    rec6 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = 'doesnt_exist')
    expect_error(recipes::prep(rec6, iris), "not found")

    rec7 <- recipes::recipe(iris) %>%
        recipes::update_role(Sepal.Length, Sepal.Width, Petal.Length, new_role =  'predictor') %>%
        stepFCBF::step_fcbf(recipes::all_predictors(), outcome = NA)
    expect_error(recipes::prep(rec7, iris), "outcome variable was not found")
})

# Test if user provides columns by name rather than using tidyselect helpers
test_that("function works if user provides columns by name", {
    rec <- recipes::recipe(Species ~ . , iris) %>%
        stepFCBF::step_fcbf(c("Petal.Length", "Sepal.Length"))
    expect_equal(recipes::prep(rec, iris) %>% bake(iris) %>% names,
                 c("Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

})
