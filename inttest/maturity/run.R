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

tv_annualgrowth <- g3_timevariable('annualgrowth', list(
    "init" = quote( 0.001 * g3_param("ling.k1") ),
    "1983-01" = quote( 0.001 * g3_param("ling.k2") ),
    "1984-01" = quote( 0.001 * g3_param("ling.k1") ),
    "1985-01" = quote( 0.001 * g3_param("ling.k2") ),
    "1986-01" = quote( 0.001 * g3_param("ling.k1") ),
    "1987-01" = quote( 0.001 * g3_param("ling.k2") ),
    "1988-01" = quote( 0.001 * g3_param("ling.k1") ),
    "1989-01" = quote( 0.001 * g3_param("ling.k2") ),
    "1990-01" = quote( 0.001 * g3_param("ling.k1") ) ))

lingimm_actions <- list(
    g3a_initialconditions_normalparam(lingimm,
        factor_f = ~(age * g3_param("lingimm.init")) * g3_param("lingimm.init.scalar"),
        mean_f = ~g3_param("ling.Linf"),
        stddev_f = ~10, 
        alpha_f = ~g3_param("lingimm.walpha"),
        beta_f = ~g3_param("lingimm.wbeta")),
    g3a_naturalmortality(lingimm, g3a_naturalmortality_exp(~g3_param("lingimm.M"))),
    g3a_growmature(lingimm,
        impl_f = g3a_grow_impl_bbinom(
            g3a_grow_lengthvbsimple(~g3_param("ling.Linf"), tv_annualgrowth),
            g3a_grow_weightsimple(~g3_param("lingimm.walpha"), ~g3_param("lingimm.wbeta")),
            beta_f = ~10 * g3_param("ling.bbin"),
            maxlengthgroupgrowth = 15),
        maturity_f = g3a_mature_constant(
            alpha = ~g3_param("ling.mat.alpha"),
            l50 = ~g3_param("ling.mat.l50")),
        transition_f = quote( cur_step == 4L ),
        output_stocks = list(lingmat)),
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
    list())

time_actions <- list(
    g3a_time(min(year_range), max(year_range), c(3,3,3,3)),
    list())

actions <- c(
  lingimm_actions,
  lingmat_actions,
  time_actions)
actions <- c(actions, list(g3a_report_history(actions)))

# NB: with strict we'll report prevtotal, which isn't comparable with TMB
model_fn <- g3_to_r(actions, strict = FALSE, trace = FALSE)

actions <- local({
    eval(g2to3_mainfile('inttest/maturity'))
    c(actions, list(g3a_report_history(actions)))
})
model_fn_auto <- g3_to_r(actions, strict = FALSE, trace = FALSE)
ok(ut_cmp_identical(deparse(body(model_fn_auto), width.cutoff = 500L), deparse(body(model_fn), width.cutoff = 500L)), "g2to3 model matched hand-crafted model")
