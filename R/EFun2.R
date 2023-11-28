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
#' @param path
#'
#' @return
#' @export
#'
#' @examples
EFun2 =
  function (drug, age, gender, weight, agent, country, drugEffect,
            year, illnesses, path)
  {
    load(paste(geted(), "rawDEMO.Rdata", sep = "/"))
    if (!missing(year)) {
      DEMOQQQ <- DEMO[GetDataYear %between% year]
    }
    rawDEMO <- as.numeric(NROW(DEMOQQQ))
    rm(DEMOQQQ)
    rm(DEMO)
    load(paste(geted(), "DEMO.RData", sep = "/"))
    load(paste(geted(), "REAC.RData", sep = "/"))
    load(paste(geted(), "INDI.RData", sep = "/"))
    load(paste(geted(), "DRUG.RData", sep = "/"))
    load(paste(geted(), "THER.RData", sep = "/"))
    load(paste(geted(), "RPSR.RData", sep = "/"))
    load(paste(geted(), "OUTC.RData", sep = "/"))
    load(paste(geted(), "DIC.RData", sep = "/"))
    inter.demo <- copy(DEMO)
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
    if (!missing(drug)) {
      target.drug <- inter.drug[(DRUGNAME %in% drug | prod_ai %in%
                                   drug) & (ROLE_COD %in% drugEffect)]
      target.drug <- as.numeric(NROW(target.drug))
      df1 <- data.frame(Name = c("目标药物数据量"),
                        number = c(target.drug))
    }
    inter.drug <- as.numeric(NROW(inter.drug))
    rm(DRUG)
    inter.outc <- OUTC[unique(inter.demo$primaryid)]
    inter.outc <- as.numeric(NROW(inter.outc))
    rm(OUTC)
    inter.indi <- INDI[unique(inter.demo$primaryid)]
    inter.indi <- as.numeric(NROW(inter.indi))
    rm(INDI)
    inter.reac <- REAC[unique(inter.demo$primaryid)]
    inter.reac <- as.numeric(NROW(inter.reac))
    rm(REAC)
    inter.rpsr <- RPSR[unique(inter.demo$primaryid)]
    inter.rpsr <- as.numeric(NROW(inter.rpsr))
    rm(RPSR)
    inter.ther <- THER[unique(inter.demo$primaryid)]
    inter.ther <- as.numeric(NROW(inter.ther))
    rm(THER)
    inter.demo <- DEMO[unique(inter.demo$primaryid)]
    inter.demo <- as.numeric(NROW(inter.demo))
    rm(DEMO)
    df2 <- data.frame(Name = c("去重前DEMO数据量", "去重后DEMO数据量",
                               "去重后DRUG数据量", "去重后reac数据量",
                               "去重后THER数据量", "去重后RPSR数据量",
                               "去重后THER数据量", "去重后OUTC数据量"),
                      number = c(rawDEMO, inter.demo, inter.drug, inter.reac,
                                 inter.ther, inter.rpsr, inter.ther, inter.outc))
    df2 <- rbind(df2, df1)
    write.xlsx(df2, file = file.path(path, "流程图数据.xlsx"))
    return(list(df2 = df2))
  }
