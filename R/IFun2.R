#' Title
#'
#' @param drug_A
#' @param drug_B
#' @param age
#' @param gender
#' @param weight
#' @param agent
#' @param country
#' @param year
#' @param illnesses
#' @param path
#'
#' @return
#' @export
#'
#' @examples
IFun2 = function (drug_A, drug_B, age, gender, weight, agent, country,
          year, illnesses, path)
{
  library(dplyr)
  load(paste(getwd(), "DEMO.RData", sep = "/"))
  load(paste(getwd(), "REAC.RData", sep = "/"))
  load(paste(getwd(), "INDI.RData", sep = "/"))
  load(paste(getwd(), "DRUG.RData", sep = "/"))
  load(paste(getwd(), "THER.RData", sep = "/"))
  load(paste(getwd(), "RPSR.RData", sep = "/"))
  load(paste(getwd(), "OUTC.RData", sep = "/"))
  load(paste(getwd(), "DIC.RData", sep = "/"))
  inter.demo <- copy(DEMO)
  REAC <- DIC[REAC, on = .(PT)]
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
  inter.drug0 <- DRUG[unique(inter.demo$primaryid)]
  rm(DRUG)
  inter.outc0 <- OUTC[unique(inter.demo$primaryid)]
  rm(OUTC)
  inter.indi0 <- INDI[unique(inter.demo$primaryid)]
  rm(INDI)
  inter.reac0 <- REAC[unique(inter.demo$primaryid)]
  rm(REAC)
  inter.rpsr0 <- RPSR[unique(inter.demo$primaryid)]
  rm(RPSR)
  inter.ther0 <- THER[unique(inter.demo$primaryid)]
  rm(THER)
  inter.demo0 <- DEMO[unique(inter.demo$primaryid)]
  rm(DEMO)
  data.table::setkeyv(inter.demo0, "primaryid")
  data.table::setkeyv(inter.drug0, "primaryid")
  data.table::setkeyv(inter.outc0, "primaryid")
  data.table::setkeyv(inter.indi0, "primaryid")
  data.table::setkeyv(inter.reac0, "primaryid")
  data.table::setkeyv(inter.rpsr0, "primaryid")
  data.table::setkeyv(inter.ther0, "primaryid")
  target.drugA <- inter.drug0[(DRUGNAME %in% drug_A | prod_ai %in%
                                 drug_A) & ROLE_COD %in% c("PS")]
  target.drugA <- inter.drug0[unique(target.drugA$primaryid)]
  target.drugA <- target.drugA[!unique(target.drugA[DRUGNAME %in%
                                                      drug_B | prod_ai %in% drug_B]$primaryid)]
  target.drugB <- inter.drug0[(DRUGNAME %in% drug_B | prod_ai %in%
                                 drug_B) & ROLE_COD %in% c("PS")]
  target.drugB <- inter.drug0[unique(target.drugB$primaryid)]
  target.drugB <- target.drugB[!unique(target.drugB[DRUGNAME %in%
                                                      drug_A | prod_ai %in% drug_A]$primaryid)]
  target.drugA_B <- inter.drug0[(DRUGNAME %in% drug_A | prod_ai %in%
                                   drug_A) & ROLE_COD %in% c("PS")]
  target.drugA_B <- inter.drug0[unique(target.drugA_B$primaryid)]
  target.drugA_B <- target.drugA_B[unique(target.drugA_B[DRUGNAME %in%
                                                           drug_B | prod_ai %in% drug_B]$primaryid)]
  target.drugB_A <- inter.drug0[(DRUGNAME %in% drug_B | prod_ai %in%
                                   drug_B) & ROLE_COD %in% c("PS")]
  target.drugB_A <- inter.drug0[unique(target.drugB_A$primaryid)]
  target.drugB_A <- target.drugB_A[unique(target.drugB_A[DRUGNAME %in%
                                                           drug_A | prod_ai %in% drug_A]$primaryid)]
  target.drugAandB <- rbind(target.drugB_A, target.drugA_B)
  target.drug <- rbind(target.drugA, target.drugAandB)
  datalib1.demo <- inter.demo0[unique(target.drugAandB$primaryid)]
  data.table::setkey(target.drug, "primaryid")
  inter.drug <- inter.drug0[unique(target.drug$primaryid)]
  inter.outc <- inter.outc0[unique(target.drug$primaryid)]
  inter.indi <- inter.indi0[unique(target.drug$primaryid)]
  inter.reac <- inter.reac0[unique(target.drug$primaryid)]
  inter.rpsr <- inter.rpsr0[unique(target.drug$primaryid)]
  inter.ther <- inter.ther0[unique(target.drug$primaryid)]
  inter.demo <- inter.demo0[unique(target.drug$primaryid)]
  datalib2.drug <- inter.drug0[unique(datalib1.demo$primaryid)]
  datalib2.outc <- inter.outc0[unique(datalib1.demo$primaryid)]
  datalib2.indi <- inter.indi0[unique(datalib1.demo$primaryid)]
  datalib2.reac <- inter.reac0[unique(datalib1.demo$primaryid)]
  datalib2.rpsr <- inter.rpsr0[unique(datalib1.demo$primaryid)]
  datalib2.ther <- inter.ther0[unique(datalib1.demo$primaryid)]
  if (!file.exists(path))
    dir.create(path, recur = T)
  data.table::fwrite(datalib1.demo, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比demo.csv")))
  data.table::fwrite(datalib2.drug, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比drug.csv")))
  data.table::fwrite(datalib2.outc, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比outc.csv")))
  data.table::fwrite(datalib2.indi, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比indi.csv")))
  data.table::fwrite(datalib2.reac, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比reac.csv")))
  data.table::fwrite(datalib2.rpsr, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比rpsr.csv")))
  data.table::fwrite(datalib2.ther, file = file.path(path,
                                                     paste0("两药联用与药物A单用相比ther.csv")))
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
  write.xlsx(T1, file = file.path(path, "两药联用与药物A单用相比.xlsx"))
  write.xlsx(T2, file = file.path(path, "两药联用与药物A单用相比每个PT的不良反应诱发时间).xlsx"))
  ok <- datalib2.reac[, .(a = as.numeric(.N)), PT]
  ok[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  ok1 <- inter.reac[, .(ac = as.numeric(.N)), PT]
  ok1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  ok2 <- ok1[ok, on = .(PT)]
  ok2[, `:=`(c, ac - a)]
  ok2[, `:=`(d, n - a - b - c)]
  ok2 <- DIC[ok2, on = .(PT)]
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
                               RORU, PRR, XX, EBGM, EBGM05, IC2, C025, pt_name_cn,
                               soc_name_en, soc_name_cn) %>% arrange(-a)
  ok2$"ROR(95%Cl)" <- paste(ok2$ROR, "(", ok2$RORL, "-", ok2$RORU,
                            ")")
  ok2$"PRR(卡方值)" <- paste(ok2$PRR, "(", ok2$XX, ")")
  ok2$"EBGM(95%下限)" <- paste(ok2$EBGM, "(", ok2$EBGM05,
                             ")")
  ok2$"IC(95%下限)" <- paste(ok2$IC2, "(", ok2$C025, ")")
  write.xlsx(ok2, file = file.path(path, "两药联用与药物A单用相比不良反应信号(PT).xlsx"))
  okk <- datalib2.reac[, .(a = as.numeric(.N)), soc_name_cn]
  okk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okk1 <- inter.reac[, .(ac = as.numeric(.N)), soc_name_cn]
  okk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okk2 <- okk1[okk, on = .(soc_name_cn)]
  okk2[, `:=`(c, ac - a)]
  okk2[, `:=`(d, n - a - b - c)]
  if (NROW(okk2)) {
    okk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a + 1/b +
                                                   1/c + 1/d)))]
    okk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a + 1/b +
                                                   1/c + 1/d)))]
    okk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                       (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                           (b + d)))]
    okk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) * (a +
                                                          c)))]
    okk2[, `:=`(EBGM05, exp(log(EBGM) - 1.96 * (sqrt(1/a +
                                                       1/b + 1/c + 1/d))))]
    okk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a + c) *
                                                   (a + b))))]
    okk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b + c +
                                                d + 2))/((a + b + 1) * (a + c + 1)))]
    okk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d + 2) *
                             (a + b + c + d + 2))/((a + b + c + d + GMAE) * (a +
                                                                               b + 1) * (a + c + 1))))]
    okk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                       b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                     GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                             (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                           c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okk2[, `:=`(SD, sqrt(VIC))]
    okk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okk2 <- data.frame(okk2)
  okk2$"ROR(95%Cl)" <- paste(okk2$ROR, "(", okk2$RORL, "-",
                             okk2$RORU, ")")
  okk2$"PRR(卡方值)" <- paste(okk2$PRR, "(", okk2$XX, ")")
  okk2$"EBGM(95%下限)" <- paste(okk2$EBGM, "(", okk2$EBGM05,
                              ")")
  okk2$"IC(95%下限)" <- paste(okk2$IC2, "(", okk2$C025,
                            ")")
  write.xlsx(okk2, file = file.path(path, "两药联用与药物A单用相比不良反应信号(soc).xlsx"))
  okkk <- datalib2.reac[, .(a = as.numeric(.N)), hlt_name_cn]
  okkk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okkk1 <- inter.reac[, .(ac = as.numeric(.N)), hlt_name_cn]
  okkk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okkk2 <- okkk1[okkk, on = .(hlt_name_cn)]
  okkk2[, `:=`(c, ac - a)]
  okkk2[, `:=`(d, n - a - b - c)]
  if (NROW(okkk2)) {
    okkk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okkk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a +
                                                    1/b + 1/c + 1/d)))]
    okkk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a +
                                                    1/b + 1/c + 1/d)))]
    okkk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okkk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                        (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                            (b + d)))]
    okkk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) *
                                                (a + c)))]
    okkk2[, `:=`(EBGM05, exp(log(EBGM) - 1.64 * (sqrt(1/a +
                                                        1/b + 1/c + 1/d))))]
    okkk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a + c) *
                                                    (a + b))))]
    okkk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b + c +
                                                 d + 2))/((a + b + 1) * (a + c + 1)))]
    okkk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d + 2) *
                              (a + b + c + d + 2))/((a + b + c + d + GMAE) * (a +
                                                                                b + 1) * (a + c + 1))))]
    okkk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                        b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                      GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                              (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                            c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okkk2[, `:=`(SD, sqrt(VIC))]
    okkk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okkk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okkk2 <- data.frame(okkk2)
  okkk2 <- okkk2 %>% dplyr::mutate_if(is.numeric, round, digits = 2)
  okkk2$"ROR(95%Cl)" <- paste(okkk2$ROR, "(", okkk2$RORL,
                              "-", okkk2$RORU, ")")
  okkk2$"PRR(卡方值)" <- paste(okkk2$PRR, "(", okkk2$XX,
                            ")")
  okkk2$"EBGM(95%下限)" <- paste(okkk2$EBGM, "(", okkk2$EBGM05,
                               ")")
  okkk2$"IC(95%下限)" <- paste(okkk2$IC2, "(", okkk2$C025,
                             ")")
  write.xlsx(okkk2, file = file.path(path, "两药联用与药物A单用相比不良反应信号(hlt).xlsx"))
  okkkk <- datalib2.reac[, .(a = as.numeric(.N)), hlgt_name_cn]
  okkkk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okkkk1 <- inter.reac[, .(ac = as.numeric(.N)), hlgt_name_cn]
  okkkk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okkkk2 <- okkkk1[okkkk, on = .(hlgt_name_cn)]
  okkkk2[, `:=`(c, ac - a)]
  okkkk2[, `:=`(d, n - a - b - c)]
  if (NROW(okkkk2)) {
    okkkk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okkkk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a +
                                                     1/b + 1/c + 1/d)))]
    okkkk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a +
                                                     1/b + 1/c + 1/d)))]
    okkkk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okkkk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                         (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                             (b + d)))]
    okkkk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) *
                                                 (a + c)))]
    okkkk2[, `:=`(EBGM05, exp(log(EBGM) - 1.64 * (sqrt(1/a +
                                                         1/b + 1/c + 1/d))))]
    okkkk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a +
                                                      c) * (a + b))))]
    okkkk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b +
                                                  c + d + 2))/((a + b + 1) * (a + c + 1)))]
    okkkk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d +
                                          2) * (a + b + c + d + 2))/((a + b + c + d + GMAE) *
                                                                       (a + b + 1) * (a + c + 1))))]
    okkkk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                         b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                       GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                               (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                             c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okkkk2[, `:=`(SD, sqrt(VIC))]
    okkkk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okkkk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okkkk2 <- data.frame(okkkk2)
  okkkk2$"ROR(95%Cl)" <- paste(okkkk2$ROR, "(", okkkk2$RORL,
                               "-", okkkk2$RORU, ")")
  okkkk2$"PRR(卡方值)" <- paste(okkkk2$PRR, "(", okkkk2$XX,
                             ")")
  okkkk2$"EBGM(95%下限)" <- paste(okkkk2$EBGM, "(", okkkk2$EBGM05,
                                ")")
  okkkk2$"IC(95%下限)" <- paste(okkkk2$IC2, "(", okkkk2$C025,
                              ")")
  write.xlsx(okkkk2, file = file.path(path, "两药联用与药物A单用相比不良反应信号(hlgt).xlsx"))
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
  ggsave(file.path(path, "两药联用与药物A单用相比上报年份图.png"),
         p1, width = 20, height = 8.27, dpi = 600)
  write.xlsx(df1, file = file.path(path, "两药联用与药物A单用相比季度上报频数统计.xlsx"))
  df2 <- data.frame(datalib2.outc)
  df2 <- apply(df2, 2, function(x) ifelse(x == "", NA, x))
  df2 <- data.frame(df2)
  df2 <- df2 %>% dplyr::mutate(OUTC_COD = as.factor(OUTC_COD))
  df2 <- table1(~OUTC_COD, data = df2)
  df2 <- data.frame(df2)
  write.xlsx(df2, file = file.path(path, "两药联用与药物A单用相比结局指标频数统计.xlsx"))
  datalib1.demo <- data.frame(datalib1.demo)
  datalib1.demo <- datalib1.demo %>% dplyr::select(AGE, WT,
                                                   SEX, OCCP_COD, REPORTER_COUNTRY, GetDataYear) %>% dplyr::mutate(GetDataYear = case_when(41 <=
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
  datalib1.demo <- table1(~SEX + WT + AGE + OCCP_COD + REPORTER_COUNTRY,
                          data = datalib1.demo)
  datalib1.demo <- data.frame(datalib1.demo)
  write.xlsx(datalib1.demo, file = file.path(path, "两药联用与药物A单用相比基线信息整理汇总.xlsx"))
  target.drug <- rbind(target.drugB, target.drugAandB)
  datalib1.demo <- inter.demo0[unique(target.drugAandB$primaryid)]
  inter.drug <- inter.drug0[unique(target.drug$primaryid)]
  inter.outc <- inter.outc0[unique(target.drug$primaryid)]
  inter.indi <- inter.indi0[unique(target.drug$primaryid)]
  inter.reac <- inter.reac0[unique(target.drug$primaryid)]
  inter.rpsr <- inter.rpsr0[unique(target.drug$primaryid)]
  inter.ther <- inter.ther0[unique(target.drug$primaryid)]
  inter.demo <- inter.demo0[unique(target.drug$primaryid)]
  datalib2.drug <- inter.drug0[unique(datalib1.demo$primaryid)]
  datalib2.outc <- inter.outc0[unique(datalib1.demo$primaryid)]
  datalib2.indi <- inter.indi0[unique(datalib1.demo$primaryid)]
  datalib2.reac <- inter.reac0[unique(datalib1.demo$primaryid)]
  datalib2.rpsr <- inter.rpsr0[unique(datalib1.demo$primaryid)]
  datalib2.ther <- inter.ther0[unique(datalib1.demo$primaryid)]
  if (!file.exists(path))
    dir.create(path, recur = T)
  data.table::fwrite(datalib1.demo, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比demo.csv")))
  data.table::fwrite(datalib2.drug, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比drug.csv")))
  data.table::fwrite(datalib2.outc, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比outc.csv")))
  data.table::fwrite(datalib2.indi, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比indi.csv")))
  data.table::fwrite(datalib2.reac, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比reac.csv")))
  data.table::fwrite(datalib2.rpsr, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比rpsr.csv")))
  data.table::fwrite(datalib2.ther, file = file.path(path,
                                                     paste0("两药联用与药物B单用相比ther.csv")))
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
  write.xlsx(T1, file = file.path(path, "两药联用与药物B单用相比不良反应诱发时间.xlsx"))
  write.xlsx(T2, file = file.path(path, "两药联用与药物B单用相比不良反应诱发时间(每个PT的不良反应诱发时间).xlsx"))
  ok <- datalib2.reac[, .(a = as.numeric(.N)), PT]
  ok[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  ok1 <- inter.reac[, .(ac = as.numeric(.N)), PT]
  ok1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  ok2 <- ok1[ok, on = .(PT)]
  ok2[, `:=`(c, ac - a)]
  ok2[, `:=`(d, n - a - b - c)]
  ok2 <- DIC[ok2, on = .(PT)]
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
                               RORU, PRR, XX, EBGM, EBGM05, IC2, C025, pt_name_cn,
                               soc_name_en, soc_name_cn) %>% arrange(-a)
  ok2$"ROR(95%Cl)" <- paste(ok2$ROR, "(", ok2$RORL, "-", ok2$RORU,
                            ")")
  ok2$"PRR(卡方值)" <- paste(ok2$PRR, "(", ok2$XX, ")")
  ok2$"EBGM(95%下限)" <- paste(ok2$EBGM, "(", ok2$EBGM05,
                             ")")
  ok2$"IC(95%下限)" <- paste(ok2$IC2, "(", ok2$C025, ")")
  write.xlsx(ok2, file = file.path(path, "两药联用与药物B单用相比不良反应信号(PT).xlsx"))
  okk <- datalib2.reac[, .(a = as.numeric(.N)), soc_name_cn]
  okk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okk1 <- inter.reac[, .(ac = as.numeric(.N)), soc_name_cn]
  okk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okk2 <- okk1[okk, on = .(soc_name_cn)]
  okk2[, `:=`(c, ac - a)]
  okk2[, `:=`(d, n - a - b - c)]
  if (NROW(okk2)) {
    okk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a + 1/b +
                                                   1/c + 1/d)))]
    okk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a + 1/b +
                                                   1/c + 1/d)))]
    okk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                       (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                           (b + d)))]
    okk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) * (a +
                                                          c)))]
    okk2[, `:=`(EBGM05, exp(log(EBGM) - 1.96 * (sqrt(1/a +
                                                       1/b + 1/c + 1/d))))]
    okk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a + c) *
                                                   (a + b))))]
    okk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b + c +
                                                d + 2))/((a + b + 1) * (a + c + 1)))]
    okk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d + 2) *
                             (a + b + c + d + 2))/((a + b + c + d + GMAE) * (a +
                                                                               b + 1) * (a + c + 1))))]
    okk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                       b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                     GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                             (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                           c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okk2[, `:=`(SD, sqrt(VIC))]
    okk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okk2 <- data.frame(okk2)
  okk2$"ROR(95%Cl)" <- paste(okk2$ROR, "(", okk2$RORL, "-",
                             okk2$RORU, ")")
  okk2$"PRR(卡方值)" <- paste(okk2$PRR, "(", okk2$XX, ")")
  okk2$"EBGM(95%下限)" <- paste(okk2$EBGM, "(", okk2$EBGM05,
                              ")")
  okk2$"IC(95%下限)" <- paste(okk2$IC2, "(", okk2$C025,
                            ")")
  write.xlsx(okk2, file = file.path(path, "两药联用与药物B单用相比不良反应信号(soc).xlsx"))
  okkk <- datalib2.reac[, .(a = as.numeric(.N)), hlt_name_cn]
  okkk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okkk1 <- inter.reac[, .(ac = as.numeric(.N)), hlt_name_cn]
  okkk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okkk2 <- okkk1[okkk, on = .(hlt_name_cn)]
  okkk2[, `:=`(c, ac - a)]
  okkk2[, `:=`(d, n - a - b - c)]
  if (NROW(okkk2)) {
    okkk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okkk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a +
                                                    1/b + 1/c + 1/d)))]
    okkk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a +
                                                    1/b + 1/c + 1/d)))]
    okkk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okkk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                        (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                            (b + d)))]
    okkk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) *
                                                (a + c)))]
    okkk2[, `:=`(EBGM05, exp(log(EBGM) - 1.64 * (sqrt(1/a +
                                                        1/b + 1/c + 1/d))))]
    okkk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a + c) *
                                                    (a + b))))]
    okkk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b + c +
                                                 d + 2))/((a + b + 1) * (a + c + 1)))]
    okkk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d + 2) *
                              (a + b + c + d + 2))/((a + b + c + d + GMAE) * (a +
                                                                                b + 1) * (a + c + 1))))]
    okkk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                        b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                      GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                              (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                            c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okkk2[, `:=`(SD, sqrt(VIC))]
    okkk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okkk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okkk2 <- data.frame(okkk2)
  okkk2 <- okkk2 %>% dplyr::mutate_if(is.numeric, round, digits = 2)
  okkk2$"ROR(95%Cl)" <- paste(okkk2$ROR, "(", okkk2$RORL,
                              "-", okkk2$RORU, ")")
  okkk2$"PRR(卡方值)" <- paste(okkk2$PRR, "(", okkk2$XX,
                            ")")
  okkk2$"EBGM(95%下限)" <- paste(okkk2$EBGM, "(", okkk2$EBGM05,
                               ")")
  okkk2$"IC(95%下限)" <- paste(okkk2$IC2, "(", okkk2$C025,
                             ")")
  write.xlsx(okkk2, file = file.path(path, "两药联用与药物B单用相比不良反应信号(hlt).xlsx"))
  okkkk <- datalib2.reac[, .(a = as.numeric(.N)), hlgt_name_cn]
  okkkk[, `:=`(b, as.numeric(NROW(datalib2.reac)) - a)]
  okkkk1 <- inter.reac[, .(ac = as.numeric(.N)), hlgt_name_cn]
  okkkk1[, `:=`(n, as.numeric(NROW(inter.reac)))]
  okkkk2 <- okkkk1[okkkk, on = .(hlgt_name_cn)]
  okkkk2[, `:=`(c, ac - a)]
  okkkk2[, `:=`(d, n - a - b - c)]
  if (NROW(okkkk2)) {
    okkkk2[, `:=`(ROR, ((a * d)/(b * c)))]
    okkkk2[, `:=`(RORL, exp(log(ROR) - 1.96 * sqrt(1/a +
                                                     1/b + 1/c + 1/d)))]
    okkkk2[, `:=`(RORU, exp(log(ROR) + 1.96 * sqrt(1/a +
                                                     1/b + 1/c + 1/d)))]
    okkkk2[, `:=`(PRR, (a/(a + b))/(c/(c + d)))]
    okkkk2[, `:=`(XX, ((a * d - b * c) * (a * d - b * c) *
                         (a + b + c + d))/((a + b) * (c + d) * (a + c) *
                                             (b + d)))]
    okkkk2[, `:=`(EBGM, (a * (a + b + c + d))/((a + b) *
                                                 (a + c)))]
    okkkk2[, `:=`(EBGM05, exp(log(EBGM) - 1.64 * (sqrt(1/a +
                                                         1/b + 1/c + 1/d))))]
    okkkk2[, `:=`(IC2, log2((a * (a + b + c + d))/((a +
                                                      c) * (a + b))))]
    okkkk2[, `:=`(GMAE, ((a + b + c + d + 2) * (a + b +
                                                  c + d + 2))/((a + b + 1) * (a + c + 1)))]
    okkkk2[, `:=`(EIC, log2(((a + 1) * (a + b + c + d +
                                          2) * (a + b + c + d + 2))/((a + b + c + d + GMAE) *
                                                                       (a + b + 1) * (a + c + 1))))]
    okkkk2[, `:=`(VIC, (1/(log(2))) * (1/(log(2))) * ((a +
                                                         b + c + d - 3 + GMAE)/(3 * (1 + a + b + c + d +
                                                                                       GMAE)) + (a + b + c + d - a - b + 1)/((a + b + 1) *
                                                                                                                               (1 + a + b + c + d + 2)) + (a + b + c + d - a -
                                                                                                                                                             c + 1)/((a + c + 1) * (a + b + c + d + 3))))]
    okkkk2[, `:=`(SD, sqrt(VIC))]
    okkkk2[, `:=`(BCPNN250, EIC - 2 * SD)]
    okkkk2[, `:=`(C025, IC2 - 2 * SD)]
  }
  okkkk2 <- data.frame(okkkk2)
  okkkk2$"ROR(95%Cl)" <- paste(okkkk2$ROR, "(", okkkk2$RORL,
                               "-", okkkk2$RORU, ")")
  okkkk2$"PRR(卡方值)" <- paste(okkkk2$PRR, "(", okkkk2$XX,
                             ")")
  okkkk2$"EBGM(95%下限)" <- paste(okkkk2$EBGM, "(", okkkk2$EBGM05,
                                ")")
  okkkk2$"IC(95%下限)" <- paste(okkkk2$IC2, "(", okkkk2$C025,
                              ")")
  write.xlsx(okkkk2, file = file.path(path, "两药联用与药物B单用相比不良反应信号(hlgt).xlsx"))
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
  ggsave(file.path(path, "两药联用与药物B单用相比上报年份图.png"),
         p1, width = 20, height = 8.27, dpi = 600)
  write.xlsx(df1, file = file.path(path, "两药联用与药物B单用相比季度上报频数统计.xlsx"))
  df2 <- data.frame(datalib2.outc)
  df2 <- apply(df2, 2, function(x) ifelse(x == "", NA, x))
  df2 <- data.frame(df2)
  df2 <- df2 %>% dplyr::mutate(OUTC_COD = as.factor(OUTC_COD))
  df2 <- table1(~OUTC_COD, data = df2)
  df2 <- data.frame(df2)
  write.xlsx(df2, file = file.path(path, "两药联用与药物B单用相比结局指标频数统计.xlsx"))
  datalib1.demo <- data.frame(datalib1.demo)
  datalib1.demo <- datalib1.demo %>% dplyr::select(AGE, WT,
                                                   SEX, OCCP_COD, REPORTER_COUNTRY, GetDataYear) %>% dplyr::mutate(GetDataYear = case_when(41 <=
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
  datalib1.demo <- table1(~SEX + WT + AGE + OCCP_COD + REPORTER_COUNTRY,
                          data = datalib1.demo)
  datalib1.demo <- data.frame(datalib1.demo)
  write.xlsx(datalib1.demo, file = file.path(path, "两药联用与药物B单用相比基线信息整理汇总.xlsx"))
  return(list(datalib2.reac = datalib2.reac, inter.reac = inter.reac,
              ok2 = ok2))
}
