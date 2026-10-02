source(here("scripts","r","03_gams.R"))
# f1 model coefs

s_f1 <- summary(gam_f1)

param_f1 <- as.data.frame(s_f1$p.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    Estimate = round(Estimate, 2),
    SE = round(`Std. Error`, 2),
    t = round(`t value`, 2),
    p = format.pval(`Pr(>|t|)`, digits = 3, eps = .001)
  )

smooth_f1 <- as.data.frame(s_f1$s.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    EDF = round(edf, 2),
    `Ref. df` = round(`Ref.df`, 2),
    F = round(F, 2),
    p = format.pval(`p-value`, digits = 3, eps = .001)
  )

# f2 model coefs

s_f2 <- summary(gam_f2)

param_f2 <- as.data.frame(s_f2$p.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    Estimate = round(Estimate, 2),
    SE = round(`Std. Error`, 2),
    t = round(`t value`, 2),
    p = format.pval(`Pr(>|t|)`, digits = 3, eps = .001)
  )

smooth_f2 <- as.data.frame(s_f2$s.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    EDF = round(edf, 2),
    `Ref. df` = round(`Ref.df`, 2),
    F = round(F, 2),
    p = format.pval(`p-value`, digits = 3, eps = .001)
  )

# tl model coefs

s_tl <- summary(gam_tl)

param_tl <- as.data.frame(s_tl$p.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    Estimate = round(Estimate, 2),
    SE = round(`Std. Error`, 2),
    t = round(`t value`, 2),
    p = format.pval(`Pr(>|t|)`, digits = 3, eps = .001)
  )

smooth_tl <- as.data.frame(s_tl$s.table) %>%
  tibble::rownames_to_column("Term") %>%
  transmute(
    Term,
    EDF = round(edf, 2),
    `Ref. df` = round(`Ref.df`, 2),
    F = round(F, 2),
    p = format.pval(`p-value`, digits = 3, eps = .001)
  )

# combine all

# Add model + section labels
all_param <- bind_rows(
  param_f1 %>% mutate(Model = "F1", Type = "Parametric"),
  param_f2 %>% mutate(Model = "F2", Type = "Parametric"),
  param_tl %>% mutate(Model = "Trajectory length", Type = "Parametric")
)

all_smooth <- bind_rows(
  smooth_f1 %>% mutate(Model = "F1", Type = "Smooth"),
  smooth_f2 %>% mutate(Model = "F2", Type = "Smooth"),
  smooth_tl %>% mutate(Model = "Trajectory length", Type = "Smooth")
)

# Put everything into the same columns
all_tab <- bind_rows(
  all_param %>%
    transmute(
      Model, Type, Term,
      Estimate, SE, t,
      EDF = NA_real_,
      `Ref. df` = NA_real_,
      F = NA_real_,
      p
    ),
  all_smooth %>%
    transmute(
      Model, Type, Term,
      Estimate = NA_real_,
      SE = NA_real_,
      t = NA_real_,
      EDF, `Ref. df`, F, p
    )
) %>%
  arrange(
    factor(Model, levels = c("F1", "F2", "Trajectory length")),
    factor(Type, levels = c("Parametric", "Smooth"))
  )