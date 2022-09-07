## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ---- echo = FALSE, results='hide', message=FALSE-----------------------------
library(tidyverse)

## ---- echo = FALSE------------------------------------------------------------
mydat <- tribble(
~缩写, ~全称, ~中文, 
"sl"          , "Subject Level"                       , "受试者层面", 
"ae"          , "Adverse Event"                       , "不良事件", 
"dm"          , "Demographics"                        , "人口学", 
"eg"          , "Electrocardiogram"                   , "心电图", 
"lb"          , "Laboratory findings/ test results"   , "实验室检测结果", 
"mh"          , "Medical History"                     , "既往病史", 
"pe"          , "Physical examination"                , "体格检查", 
"vs"          , "Vital Signs"                         , "生命体征", 
"cm"          , "Concomitant Medications"             , "合并用药/伴随用药", 
"su"          , "Substance Use"                       , "物质使用", 
"ie"          , "Inclusion/Exclusion Criteria Not Met", "纳入/排除标准不符合", 
"ex"          , "Exposure"                            , "暴露", 
"ds(EOS/EOT)" , "Disposition (End of Study/Treat)"    , "病例去向（终止实验/治疗)", 
"dv"          , "Protocol Deviations"                 , "方案偏离", 
"co"          , "Comments"                            , "", 
"pr"          , "Procedures"                          , "非药物治疗", 
"dd"          , "Death Details"                       , "死亡信息", 
"qs"          , "Questionnaire/Patient Report Outcome", "问卷/患者报告结局", 
"pc"          , "Pharmacokinetics Concentration"      , "药代浓度", 
"pp"          , "Pharmacokinetics Parameters"         , "药代参数", 
"da"          , "Drug Accountability"                 , "药物依从性"
)


## ---- echo = FALSE------------------------------------------------------------
knitr::kable(
    mydat[order(mydat %>% pull("缩写")), ]
)

