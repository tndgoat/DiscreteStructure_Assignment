new_cases = x4_Vietnam$new_cases
new_deaths = x4_Vietnam$new_deaths
ggscatter(x4_Vietnam, x="new_cases", y="new_deaths", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson")
cor.test(new_cases, new_deaths)
