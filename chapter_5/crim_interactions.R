source("clean_data.R")

# crim <- crim.raw %>%
#   filter(year > 1999) %>%
#   mutate(econasstP = econasst * 1000000,
#          econasstP = ifelse(is.na(econasstP) | econasstP == 0, 1, econasstP),
#          logeconasstP = log(econasstP)) %>%
#   group_by(cowcode) %>%
#   mutate(L.adjbicrimlevel = lag(adjbicrimlevel),
#          L.tier = lag(tier),
#          L.fh_cl = lag(fh_cl),
#          logeconasstP_1 = lag(logeconasstP),
#          meanlogeconasstP_1 = mean(logeconasstP_1, na.rm=TRUE)) %>%
#   ungroup()
# summary(crim$logeconasstP_1)

prob.crim.democracy <- df.complete.with.lags.correct %>%
  filter(crim1_minus1 == 0, year > 2000) %>%
  mutate(in.report = ifelse(tier1 == 0, "Not in report", "In report")) %>%
  group_by(in.report, fh_cl1) %>%
  summarize(total = n(),
            crim.no = sum(crim1 == 0),
            crim.yes = sum(crim1 == 1),
            prob.yes = crim.yes / total)

# # Lines
# ggplot(prob.crim.democracy, aes(x=fh_cl1, y=prob.yes, colour=in.report)) + 
#   geom_line()

# summary(df.complete.with.lags.correct$logeconasstP_1)
# 
# crim1 <- df.complete.with.lags.correct %>%
#   group_by(cowcode) %>%
#   mutate(L.adjbicrimlevel = lag(adjbicrimlevel),
#          L.tier = lag(tier),
#          L.fh_cl = lag(fh_cl)) %>%
#   ungroup()
# 
# 
# prob.crim.aid <- crim %>%
#   select(cowcode, year, adjbicrimlevel, L.adjbicrimlevel, meanlogeconasstP_1, L.tier) %>%
#   filter(L.adjbicrimlevel == 0, year > 2000) %>%
#   mutate(in.report = ifelse(L.tier == 555, "Not in report", "In report"),
#          aid = cut(meanlogeconasstP_1, c(0, 10.14, 16.25, 17.93, Inf)),
#          aid1 = cut(meanlogeconasstP_1, seq(0, 22, 1))) %>%
#   filter(!is.na(in.report), !is.na(aid)) %>%
#   group_by(in.report, aid) %>%
#   summarize(total = n(),
#             crim.no = sum(adjbicrimlevel == 0),
#             crim.yes = sum(adjbicrimlevel == 2),
#             prob.yes = crim.yes / total)
# 
# # Original chart
# ggplot(prob.crim.aid, aes(x=aid, y=prob.yes, fill=in.report)) + 
#   geom_bar(stat="identity", position="dodge")
# 
# ggplot(prob.crim.aid, aes(x=aid1, y=prob.yes, fill=in.report)) + 
#   geom_bar(stat="identity", position="dodge")
# 
