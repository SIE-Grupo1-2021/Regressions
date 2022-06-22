cd "C:\Users\jigon\OneDrive\Documentos\Economía\MunicipalitiesPoliticalBudgetCycles\Regressions"

log using "output.smcl", replace

use base_regresiones, clear

encode Gender, gen(sex)
encode type, gen(party_type)
egen mun_id = group(municipality)

global expenditures "remu serv d_goods total_expenses remu_bas remu_ev rentals serv_cf cap_prot maintenance cap_mef cap_cai salaries ext_time sub_all rent_mef publicity activities main_bcl cap_roads"

global controles_exo "Age i.sex k_12centers gdp interest_rate debt deficit"
global controles_pre "i.party_type win_margin abstentionism pop_share014 pop_share65plus"

foreach var of varlist $expenditures{
	gen r_`var' = 100 * `var' / price_index
	gen l_rpc_`var' = log(r_`var' / pop)
}

xtset mun_id year

gen elec_t = (year == 2010 | year == 2015 | year == 2019) 

ds l_rpc_*
foreach v in `r(varlist)'{
	reghdfe `v' L(0/2).elec_t ${controles_exo} ${controles_pre}, absorb(mun_id) cluster(mun_id year)///
}

ds l_rpc_*
foreach v in `r(varlist)'{
	forvalues lagelec = 0/1{
		forvalues z = 1/2{
			forvalues lag = 1/4{
				xtdpdgmm L(0/`z').`v' L(0/`lagelec').elec_t ${controles_exo} ${controles_pre}, ///
				gmm(L(`z').`v' ${controles_exo} ${controles_pre}, l(1 `lag') c m(d)) /// 
				iv(L(1/`z').`v' L(0/`lagelec').elec_t ${controles_exo}, d) two vce(r) overid collapse
				estat overid // sargan test
				estat serial, ar(1/2) // for serial correlation
			}
		}
	}
}

* POR SI DA TIEMPO DE REVISAR
ds l_rpc_*
foreach v in `r(varlist)'{
	forvalues z = 1/2{
		forvalues lag = 1/4{
			xtdpdgmm L(0/`z').`v' elec_t l_rpc_total_expenses ${controles_exo} ${controles_pre}, ///
			gmm(L(`z').`v' ${controles_exo} ${controles_pre} l_rpc_total_expenses, l(1 `lag') c m(d)) /// 
			iv(L(1/`z').`v' elec_t ${controles_exo}, d) two vce(r) overid collapse
			estat overid // sargan test
			estat serial, ar(1/2) // for serial correlation
		}
	}
}

log close
translate output.smcl output.pdf