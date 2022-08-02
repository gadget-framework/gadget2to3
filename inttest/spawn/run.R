library(gadget3)
library(gadget2to3)
library(magrittr)
library(unittest)

year_range <- 1982:1990

lingimm <- g3_stock('lingimm', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c("1" = 1)) %>%
    g3s_age(3, 10)

lingmat <- g3_stock('lingmat', seq(20, 156, 4)) %>% 
    g3s_livesonareas(c("1" = 1)) %>%
    g3s_age(5, 15)

igfs <- g3_fleet('igfs') %>% g3s_livesonareas(c("1" = 1))

lingimm_actions <- list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~(age * g3_param("lingimm.init")) * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_age(lingimm),
    list())

lingmat_actions <- list(
    g3a_initialconditions_normalparam(lingmat,
        factor_f = ~(age * g3_param("lingmat.init")) * g3_param("lingmat.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingmat.walpha"),
        beta_f = ~g3_param("lingmat.wbeta")),
    g3a_naturalmortality(lingmat, g3a_naturalmortality_exp(~g3_param("lingmat.M"))),
    g3a_age(lingmat),
    g3a_spawn(
        lingmat,
        recruitment_f = g3a_spawn_recruitment_ricker(
            ~g3_param("ricker.mu"),
            ~g3_param("ricker.lambda")),
        # NB: exponentiall50 needs to negate alpha to match gadget2
        proportion_f = g3_suitability_exponentiall50(alpha = ~-g3_param("spawn.prop.alpha"), l50 =  ~g3_param("spawn.prop.l50")),
        mortality_f = g3_suitability_straightline(alpha = ~g3_param("spawn.mort.alpha"), beta =  ~g3_param("spawn.mort.beta")),
        weightloss_f = ~g3_param("spawn.weightloss"),
        output_stocks = list(lingimm),
        mean_f = 50,
        stddev_f = 0.9,
        alpha_f = 1,
        beta_f = 1,
        run_f = ~cur_step==1L && area == area_1),
    list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
  lingmat_actions,
  time_actions)
actions <- c(actions, list(g3a_report_history(actions)))

model_fn <- g3_to_r(actions, strict = TRUE, trace = FALSE)

actions <- local({
    eval(g2to3_mainfile('inttest/spawn'))
    c(actions, list(g3a_report_history(actions)))
})
model_fn_auto <- g3_to_r(actions, strict = TRUE, trace = FALSE)
ok(ut_cmp_identical(deparse(body(model_fn_auto), width.cutoff = 500L), deparse(body(model_fn), width.cutoff = 500L)), "g2to3 model matched hand-crafted model")
