library(recipes)
library(rsample)
library(stringr)

num <- iris %>%
  select(-Species) %>%
  tibble::rownames_to_column(var = 'id') %>%
  sample_n(size = 10000, replace = TRUE)

rec <- recipe(~ ., data = num)

steps <- rec %>%
  update_role(id, new_role = 'id variable', old_role = 'predictor') %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_pca(all_predictors())

train_steps <- prep(steps, training = num)

full_pca <- juice(train_steps)


# Maybe comparing median PC1 or absolute difference from Apparent median PC#
num %>%
  bootstraps(times = 10, apparent = TRUE) %>%
  mutate(data = purrr::map(splits, analysis),
         pcs = purrr::map(data, ~bake(train_steps, new_data = .))) %>%
  unnest(pcs) %>%
  group_by(id) %>%
  summarise(median(PC1),
            median(PC2),
            median(PC3)) %>%
  ggplot(aes(id, `median(PC1)`)) +
  geom_point() +
  coord_flip()
