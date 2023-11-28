#' Title
#'
#' @param drug
#' @param age
#' @param gender
#' @param weight
#' @param agent
#' @param country
#' @param drugEffect
#' @param year
#' @param illnesses
#' @param ptname
#' @param path
#'
#' @return
#' @export
#'
#' @examples
CFun2 = function (drug, age, gender, weight, agent, country, drugEffect,
                   year, illnesses, ptname, path)
{
  load(paste(getwd(), "DEMO.RData", sep = "/"))
  load(paste(getwd(), "REAC.RData", sep = "/"))
  load(paste(getwd(), "INDI.RData", sep = "/"))
  load(paste(getwd(), "DRUG.RData", sep = "/"))
  load(paste(getwd(), "THER.RData", sep = "/"))
  load(paste(getwd(), "RPSR.RData", sep = "/"))
  load(paste(getwd(), "OUTC.RData", sep = "/"))
  load(paste(getwd(), "DIC.RData", sep = "/"))
  inter.demo <- copy(DEMO)
  REAC <- REAC[, `:=`(PT, fcase(PT %in% ptname, "target_PT",
                                !PT %in% ptname, "NO_target_PT"))]
  data.table::setkeyv(REAC, "primaryid")
  if (!missing(year)) {
    inter.demo <- inter.demo[GetDataYear %between% year]
  }
  inter.demo <- inter.demo
  if (!missing(age)) {
    inter.demo <- inter.demo[AGE %between% age]
  }
  inter.demo <- inter.demo
  if (!missing(gender)) {
    inter.demo <- inter.demo[SEX %in% gender]
  }
  inter.demo <- inter.demo
  if (!missing(weight)) {
    inter.demo <- inter.demo[WT %between% weight]
  }
  inter.demo <- inter.demo
  if (!missing(country)) {
    inter.demo <- inter.demo[REPORTER_COUNTRY %in% country]
  }
  inter.demo <- inter.demo
  if (!missing(agent)) {
    inter.demo <- inter.demo[OCCP_COD %in% agent]
  }
  inter.demo <- inter.demo
  if (!missing(illnesses)) {
    inter.demo <- INDI[unique(inter.demo$primaryid)]
    inter.demo <- inter.demo[INDI_PT %in% illnesses]
  }
  inter.demo <- inter.demo
  data.table::setkeyv(inter.demo, "primaryid")
  inter.drug <- DRUG[unique(inter.demo$primaryid)]
  inter.outc <- OUTC[unique(inter.demo$primaryid)]
  inter.indi <- INDI[unique(inter.demo$primaryid)]
  inter.reac <- REAC[unique(inter.demo$primaryid)]
  inter.rpsr <- RPSR[unique(inter.demo$primaryid)]
  inter.ther <- THER[unique(inter.demo$primaryid)]
  inter.demo <- DEMO[unique(inter.demo$primaryid)]
  rm(DEMO)
  rm(DRUG)
  rm(OUTC)
  rm(INDI)
  rm(REAC)
  rm(RPSR)
  rm(THER)
  data.table::setkeyv(inter.demo, "primaryid")
  data.table::setkeyv(inter.drug, "primaryid")
  data.table::setkeyv(inter.outc, "primaryid")
  data.table::setkeyv(inter.indi, "primaryid")
  data.table::setkeyv(inter.reac, "primaryid")
  data.table::setkeyv(inter.rpsr, "primaryid")
  data.table::setkeyv(inter.ther, "primaryid")
  target.drug <- inter.drug[(DRUGNAME %in% drug | prod_ai %in%
                               drug) & (ROLE_COD %in% drugEffect)]
  datalib1.demo <- inter.demo[unique(target.drug$primaryid)]
  datalib2.drug <- inter.drug[unique(datalib1.demo$primaryid)]
  datalib2.outc <- inter.outc[unique(datalib1.demo$primaryid)]
  datalib2.indi <- inter.indi[unique(datalib1.demo$primaryid)]
  datalib2.reac <- inter.reac[unique(datalib1.demo$primaryid)]
  datalib2.rpsr <- inter.rpsr[unique(datalib1.demo$primaryid)]
  datalib2.ther <- inter.ther[unique(datalib1.demo$primaryid)]
  if (!file.exists(path))
    dir.create(path, recur = T)
  data.table::fwrite(datalib1.demo, file = file.path(path,
                                                     paste0("demo.csv")))
  data.table::fwrite(datalib2.drug, file = file.path(path,
                                                     paste0("drug.csv")))
  data.table::fwrite(datalib2.outc, file = file.path(path,
                                                     paste0("outc.csv")))
  data.table::fwrite(datalib2.indi, file = file.path(path,
                                                     paste0("indi.csv")))
  data.table::fwrite(datalib2.reac, file = file.path(path,
                                                     paste0("reac.csv")))
  data.table::fwrite(datalib2.rpsr, file = file.path(path,
                                                     paste0("rpsr.csv")))
  data.table::fwrite(datalib2.ther, file = file.path(path,
                                                     paste0("ther.csv")))
  PPPP1 <- data.frame(datalib2.outc)
  PPPP1 <- PPPP1 %>% dplyr::mutate(OUTC_COD = dplyr::case_when(OUTC_COD ==
                                                                 "LT" | OUTC_COD == "DE" ~ "DE or LT", TRUE ~ "OT")) %>%
    dplyr::mutate(OUTC_COD = dplyr::case_when(OUTC_COD ==
                                                "DE or LT" ~ 1, OUTC_COD == "OT" ~ 2))
  PPPP1 <- na.omit(PPPP1)
  PPPP1 <- dplyr::arrange(PPPP1, primaryid, OUTC_COD)
  PPPP1 <- dplyr::distinct(PPPP1, primaryid, .keep_all = T)
  GGGG <- data.frame(datalib2.reac)
  PPPP1GGGG <- dplyr::left_join(GGGG, PPPP1, by = "primaryid")
  PPPP1GGGG <- na.omit(PPPP1GGGG)
  PPPP1GGGG$OUTC_COD <- as.factor(PPPP1GGGG$OUTC_COD)
  PPPP1GGGG <- PPPP1GGGG %>% dplyr::filter(PT == "target_PT")
  NNNNNN = NROW(PPPP1GGGG)
  PPPP1GGGG <- PPPP1GGGG %>% dplyr::group_by(OUTC_COD) %>%
    dplyr::summarise(frequency = n()) %>% dplyr::mutate(OUTC_COD = dplyr::case_when(OUTC_COD ==
                                                                                      2 ~ "其它结局指标", OUTC_COD == 1 ~ "死亡或威胁生命")) %>%
    dplyr::mutate(N = NNNNNN)
  PPPP1GGGG$N <- as.numeric(PPPP1GGGG$N)
  PPPP1GGGG$frequency <- as.numeric(PPPP1GGGG$frequency)
  PPPP1GGGG <- PPPP1GGGG %>% dplyr::mutate(`各结局指标占比百分比(%)` = percent(N/frequency))
  PPPP1GGGG <- PPPP1GGGG %>% dplyr::rename(目标不良反应结局指标总数 = N,
                                           频数 = frequency)
  write.xlsx(PPPP1GGGG, file = file.path(path, "目标AE的死亡或威胁生命结局指标比例.xlsx"))
  T1 <- as.data.frame(datalib1.demo)
  T2 <- as.data.frame(datalib2.drug)
  T2 <- T2 %>% dplyr::filter(ROLE_COD == "PS")
  T3 <- as.data.frame(datalib2.ther)
  T4 <- as.data.frame(datalib2.reac)
  T1 <- T1 %>% dplyr::select(primaryid, EVENT_DT)
  T2 <- T2 %>% dplyr::select(primaryid, DRUG_SEQ, DRUGNAME,
                             ROLE_COD)
  T1 <- dplyr::left_join(T1, T2, by = "primaryid")
  T1 <- dplyr::left_join(T1, T3, by = c("primaryid", "DRUG_SEQ"))
  T1 <- T1 %>% dplyr::select(primaryid, START_DT, EVENT_DT,
                             DRUGNAME)
  T1 <- na.omit(T1)
  T1 <- T1 %>% dplyr::filter(nchar(T1$START_DT) == 8)
  T1 <- T1 %>% dplyr::filter(nchar(T1$EVENT_DT) == 8)
  T1$START_DT <- as.Date(as.character(T1$START_DT), format = "%Y%m%d")
  T1$EVENT_DT <- as.Date(as.character(T1$EVENT_DT), format = "%Y%m%d")
  T1 <- T1 %>% dplyr::filter(T1$START_DT < T1$EVENT_DT)
  T1 <- T1 %>% dplyr::mutate(TIME = difftime(T1$EVENT_DT,
                                             T1$START_DT, units = "days"))
  T2 <- merge(T4, T1, by = "primaryid", all.x = T)
  T2 <- na.omit(T2)
  write.xlsx(T1, file = file.path(path, "不良反应诱发时间.xlsx"))
  write.xlsx(T2, file = file.path(path, "不良反应诱发时间(每个PT的不良反应诱发时间).xlsx"))
  ok <- datalib2.reac[, .(a = as.numeric(.N)), PT]
  ok[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  ok1 <- inter.reac[, .(ac = as.numeric(.N)), PT]
  ok1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  ok2 <- ok1[ok, on = .(PT)]
  ok2[, `:=`(c, ac - a)]
  ok2[, `:=`(d, n - a - b - c)]
  if (NROW(ok2)) {
    ok2[, `:=`(ROR, ((a * d)/(b * c)))]
    ok2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a + 1/b +
                                                  1/c + 1/d)))]
    ok2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a + 1/b +
                                                  1/c + 1/d)))]
    ok2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    ok2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                      (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                          (b + d)))]
    ok2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) * (a +
                                                         c)))]
    ok2[, `:=`(EBGM05, exp(log(EBGM) - 1.64 * (sqrt(1/a +
                                                      1/b + 1/c + 1/d))))]
    ok2[, `:=`(IC2, log2((a * (a + b + c + d))/((a + c) *
                                                  (a + b))))]
    ok2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b + c +
                                               d + 2))/((a + b + 1) * (a + c + 1)))]
    ok2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d + 2) *
                            (a + b + c + d + 2))/((a + b + c + d + GMAE) * (a +
                                                                              b + 1) * (a + c + 1))))]
    ok2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                      b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                    GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                            (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                          c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    ok2[, `:=`(SD, sqrt(VIC))]
    ok2[, `:=`(BCPNN250, EIC - 2 * SD)]
    ok2[, `:=`(C025, IC2 - 2 * SD)]
  }
  ok2 <- data.frame(ok2)
  ok2 <- ok2 %>% dplyr::select(PT, a, b, c, d, ROR, RORL,
                               RORU, PRR, XX, EBGM, EBGM05, IC2, C025) %>% dplyr::arrange(-a)
  ok2$"ROR(95%Cl)" <- paste(ok2$ROR, "(", ok2$RORL, "-", ok2$RORU,
                            ")")
  ok2$"PRR(卡方值)" <- paste(ok2$PRR, "(", ok2$XX, ")")
  ok2$"EBGM(95%下限)" <- paste(ok2$EBGM, "(", ok2$EBGM05,
                             ")")
  ok2$"IC(95%下限)" <- paste(ok2$IC2, "(", ok2$C025, ")")
  write.xlsx(ok2, file = file.path(path, "不良反应信号(PT).xlsx"))
  df1 <- datalib1.demo[, `:=`(GetDataYear, case_when(41 <=
                                                       GetDataYear & GetDataYear <= 44 ~ "2004年", 51 <= GetDataYear &
                                                       GetDataYear <= 54 ~ "2005年", 61 <= GetDataYear & GetDataYear <=
                                                       64 ~ "2006年", 71 <= GetDataYear & GetDataYear <= 74 ~
                                                       "2007年", 81 <= GetDataYear & GetDataYear <= 84 ~ "2008年",
                                                     91 <= GetDataYear & GetDataYear <= 94 ~ "2009年", 101 <=
                                                       GetDataYear & GetDataYear <= 104 ~ "2010年", 111 <=
                                                       GetDataYear & GetDataYear <= 114 ~ "2011年", 121 <=
                                                       GetDataYear & GetDataYear <= 124 ~ "2012年", 131 <=
                                                       GetDataYear & GetDataYear <= 134 ~ "2013年", 141 <=
                                                       GetDataYear & GetDataYear <= 144 ~ "2014年", 151 <=
                                                       GetDataYear & GetDataYear <= 154 ~ "2015年", 161 <=
                                                       GetDataYear & GetDataYear <= 164 ~ "2016年", 171 <=
                                                       GetDataYear & GetDataYear <= 174 ~ "2017年", 181 <=
                                                       GetDataYear & GetDataYear <= 184 ~ "2018年", 191 <=
                                                       GetDataYear & GetDataYear <= 194 ~ "2019年", 201 <=
                                                       GetDataYear & GetDataYear <= 204 ~ "2020年", 211 <=
                                                       GetDataYear & GetDataYear <= 214 ~ "2021年", 221 <=
                                                       GetDataYear & GetDataYear <= 224 ~ "2022年", 231 <=
                                                       GetDataYear & GetDataYear <= 234 ~ "2023年"))]
  df1 <- data.frame(df1[, .(Number = as.numeric(.N)), GetDataYear])
  p1 <- ggplot(df1, aes(x = GetDataYear, y = Number)) + geom_bar(stat = "identity",
                                                                 width = 0.4) + xlab("") + theme_bw()
  ggsave("path/上报年份图.png", p1, width = 20, height = 8.27,
         dpi = 600)
  write.xlsx(df1, file = file.path(path, "季度上报频数统计.xlsx"))
  df2 <- data.frame(datalib2.outc)
  df2 <- apply(df2, 2, function(x) ifelse(x == "", NA, x))
  df2 <- data.frame(df2)
  df2 <- df2 %>% dplyr::mutate(OUTC_COD = as.factor(OUTC_COD))
  df2 <- table1(~OUTC_COD, data = df2)
  df2 <- data.frame(df2)
  write.xlsx(df2, file = file.path(path, "结局指标频数统计.xlsx"))
  datalib1.demo <- data.frame(datalib1.demo)
  datalib1.demo <- datalib1.demo %>% dplyr::select(AGE, WT,
                                                   SEX, OCCP_COD, REPORTER_COUNTRY, GetDataYear) %>% dplyr::mutate(GetDataYear = dplyr::case_when(41 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 44 ~ "2004年", 51 <= GetDataYear &
                                                                                                                                                    GetDataYear <= 54 ~ "2005年", 61 <= GetDataYear & GetDataYear <=
                                                                                                                                                    64 ~ "2006年", 71 <= GetDataYear & GetDataYear <= 74 ~
                                                                                                                                                    "2007年", 81 <= GetDataYear & GetDataYear <= 84 ~ "2008年",
                                                                                                                                                  91 <= GetDataYear & GetDataYear <= 94 ~ "2009年", 101 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 104 ~ "2010年", 111 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 114 ~ "2011年", 121 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 124 ~ "2012年", 131 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 134 ~ "2013年", 141 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 144 ~ "2014年", 151 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 154 ~ "2015年", 161 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 164 ~ "2016年", 171 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 174 ~ "2017年", 181 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 184 ~ "2018年", 191 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 194 ~ "2019年", 201 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 204 ~ "2020年", 211 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 214 ~ "2021年", 221 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 224 ~ "2022年", 231 <=
                                                                                                                                                    GetDataYear & GetDataYear <= 234 ~ "2023年", is.na(GetDataYear) ~
                                                                                                                                                    "missing")) %>% dplyr::mutate(AGE = case_when(AGE <=
                                                                                                                                                                                                    17 ~ "≤17岁", 17 < AGE & AGE <= 64 ~ "18～64岁",
                                                                                                                                                                                                  65 <= AGE & AGE <= 85 ~ "65～85岁", 85 < AGE ~ "≥86岁")) %>%
    dplyr::mutate(WT = case_when(WT <= 49 ~ "＜50 kg",
                                 50 < WT & WT <= 100 ~ "50～100 kg", 100 < WT ~
                                   "＞100 kg")) %>% dplyr::mutate(SEX = case_when(SEX ==
                                                                                   "F" ~ "F", SEX == "M" ~ "M")) %>% dplyr::mutate(SEX = as.factor(SEX),
                                                                                                                                   AGE = as.factor(AGE), GetDataYear = as.factor(GetDataYear),
                                                                                                                                   WT = as.factor(WT), OCCP_COD = as.factor(OCCP_COD),
                                                                                                                                   REPORTER_COUNTRY = as.factor(REPORTER_COUNTRY))
  datalib1.demo <- data.frame(datalib1.demo)
  datalib1.demo <- apply(datalib1.demo, 2, function(x) ifelse(x ==
                                                                "", NA, x))
  datalib1.demo <- data.frame(datalib1.demo)
  datalib1.demo <- table1::table1(~SEX + WT + AGE + OCCP_COD +
                                    REPORTER_COUNTRY, data = datalib1.demo)
  datalib1.demo <- data.frame(datalib1.demo)
  write.xlsx(datalib1.demo, file = file.path(path, "基线信息整理汇总.xlsx"))
  return(list(datalib2.reac = datalib2.reac, inter.reac = inter.reac,
              ok2 = ok2))
}
