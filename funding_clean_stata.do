use "data/funding_clean.dta"

label data ""

ds, has(type string)
local string_vars `r(varlist)'

foreach x of varlist `string_vars' {
    replace `x' = "" if `x' == "NA"
}

saveold "data/funding_clean.dta", replace
