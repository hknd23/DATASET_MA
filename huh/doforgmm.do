clear
set mem 200m
use devdem2019.dta
xtset ccode year
gen FDI_in_persr=FDI_inflows*jw_persr
xtabond FDI_outflows FDI_inflows ln_GDP_PerCapita h_polcon5 jw_persr PX_REX_REER wdi_oilrent FDI_in_persr, lags(2) twostep vce(robust) artests(2)
