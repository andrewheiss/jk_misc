* --------------------
* Clean cables_panel
* --------------------
use "data/cables_panel.dta"

label data ""

ds, not(type string)
local not_strings `r(varlist)'

foreach x of varlist `not_strings' {
    replace `x' = . if `x' == .z
}

ds, has(type string)
local string_vars `r(varlist)'

foreach x of varlist `string_vars' {
    replace `x' = "" if `x' == "NA"
}

save "data/cables_panel.dta", replace

