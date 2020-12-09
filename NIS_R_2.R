library(readr)
library(rsample)       
library(gbm)          
library(xgboost)      
library(caret)        
library(h2o)          
library(pdp)          
library(ggplot2)      
library(lime)
library(tidyverse)
library(dplyr)
full2016core <- read_fwf("NIS_2016_Core.ASC", fwf_positions(c(1,4,6,8,10,12,23,25,27,30,32,35,37,39,41,44,46,51,58,65,72,79,86,93,100,107,114,121,128,135,142,149,156,163,170,177,184,191,198,205,212,219,226,233,240,247,254,261,268,275,282,289,291,294,296,303,310,317,324,331,338,345,352,359,366,373,380,387,394,401,411,416,418,420,424,426,429,432,435,438,441,444,447,450,453,456,459,462,465,468,471,474,476,478,488,490,492,496), 
                                                            c(3,5,7,9,11,22,24,26,29,31,34,36,38,40,43,45,50,57,64,71,78,85,92,99,106,113,120,127,134,141,148,155,162,169,176,183,190,197,204,211,218,225,232,239,246,253,260,267,274,281,288,290,293,295,302,309,316,323,330,337,344,351,358,365,372,379,386,393,400,410,415,417,419,423,425,428,431,434,437,440,443,446,449,452,455,458,461,464,467,470,473,475,477,487,489,491,495,497),
                                                            ))  
colnames(full2016core) <-  c("AGE",	"AGE_NEONATE",	"AMONTH",	"AWEEKEND",	"DIED",	"DISCWT",	"DISPUNIFORM",	"DQTR",	"DRG",	"DRGVER",	"DRG_NoPOA",	"DXVER",	"ELECTIVE",	"FEMALE",	"HCUP_ED",	"HOSP_DIVISION",	"HOSP_NIS",	"I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_ECAUSE1",	"I10_ECAUSE2",	"I10_ECAUSE3",	"I10_ECAUSE4",	"I10_NDX",	"I10_NECAUSE",	"I10_NPR",	"I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"KEY_NIS",	"LOS",	"MDC",	"MDC_NoPOA",	"NIS_STRATUM",	"PAY1",	"PL_NCHS",	"PRDAY1",	"PRDAY2",	"PRDAY3",	"PRDAY4",	"PRDAY5",	"PRDAY6",	"PRDAY7",	"PRDAY8",	"PRDAY9",	"PRDAY10",	"PRDAY11",	"PRDAY12",	"PRDAY13",	"PRDAY14",	"PRDAY15",	"PRVER",	"RACE",	"TOTCHG",	"TRAN_IN",	"TRAN_OUT",	"YEAR",	"ZIPINC_QRTL")
?read_fwf()
core2016 <- as.data.frame(full2016core)

core2016[, c("I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39",	"I10_DX40", "I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25", "PRDAY16",	"PRDAY17",	"PRDAY18",	"PRDAY19",	"PRDAY20",	"PRDAY21",	"PRDAY22",	"PRDAY23",	"PRDAY24",	"PRDAY25")] <- NA
gc()
remove(full2016core)

full2017core <- read_fwf("NIS_2017_Core.ASC", fwf_positions(c(1,	4,	6,	8,	10,	12,	23,	25,	27,	30,	32,	35,	37,	39,	41,	44,	46,	51,	58,	65,	72,	79,	86,	93,	100,	107,	114,	121,	128,	135,	142,	149,	156,	163,	170,	177,	184,	191,	198,	205,	212,	219,	226,	233,	240,	247,	254,	261,	268,	275,	282,	289,	296,	303,	310,	317,	324,	331,	333,	335,	342,	349,	356,	363,	370,	377,	384,	391,	398,	405,	412,	419,	426,	433,	440,	447,	454,	461,	468,	475,	482,	489,	496,	503,	510,	520,	525,	527,	529,	533,	535,	538,	541,	544,	547,	550,	553,	556,	559,	562,	565,	568,	571,	574,	577,	580,	583,	586,	589,	592,	595,	598,	601,	604,	607,	610,	613,	615,	617,	627,	629,	631,	635)	,
                                                            c(3,	5,	7,	9,	11,	22,	24,	26,	29,	31,	34,	36,	38,	40,	43,	45,	50,	57,	64,	71,	78,	85,	92,	99,	106,	113,	120,	127,	134,	141,	148,	155,	162,	169,	176,	183,	190,	197,	204,	211,	218,	225,	232,	239,	246,	253,	260,	267,	274,	281,	288,	295,	302,	309,	316,	323,	330,	332,	334,	341,	348,	355,	362,	369,	376,	383,	390,	397,	404,	411,	418,	425,	432,	439,	446,	453,	460,	467,	474,	481,	488,	495,	502,	509,	519,	524,	526,	528,	532,	534,	537,	540,	543,	546,	549,	552,	555,	558,	561,	564,	567,	570,	573,	576,	579,	582,	585,	588,	591,	594,	597,	600,	603,	606,	609,	612,	614,	616,	626,	628,	630,	634,	636)
                                                            ))  

colnames(full2017core) <-  c("AGE",	"AGE_NEONATE",	"AMONTH",	"AWEEKEND",	"DIED",	"DISCWT",	"DISPUNIFORM",	"DQTR",	"DRG",	"DRGVER",	"DRG_NoPOA",	"DXVER",	"ELECTIVE",	"FEMALE",	"HCUP_ED",	"HOSP_DIVISION",	"HOSP_NIS",	"I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_ECAUSE1",	"I10_ECAUSE2",	"I10_ECAUSE3",	"I10_ECAUSE4",	"I10_NDX",	"I10_NECAUSE",	"I10_NPR",	"I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"KEY_NIS",	"LOS",	"MDC",	"MDC_NoPOA",	"NIS_STRATUM",	"PAY1",	"PL_NCHS",	"PRDAY1",	"PRDAY2",	"PRDAY3",	"PRDAY4",	"PRDAY5",	"PRDAY6",	"PRDAY7",	"PRDAY8",	"PRDAY9",	"PRDAY10",	"PRDAY11",	"PRDAY12",	"PRDAY13",	"PRDAY14",	"PRDAY15",	"PRVER",	"RACE",	"TOTCHG",	"TRAN_IN",	"TRAN_OUT",	"YEAR",	"ZIPINC_QRTL")
core2017 <- as.data.frame(full2017core)
colnames(core2017) <- c("AGE",	"AGE_NEONATE",	"AMONTH",	"AWEEKEND",	"DIED",	"DISCWT",	"DISPUNIFORM",	"DQTR",	"DRG",	"DRGVER",	"DRG_NoPOA",	"DXVER",	"ELECTIVE",	"FEMALE",	"HCUP_ED",	"HOSP_DIVISION",	"HOSP_NIS",	"I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39",	"I10_DX40",	"I10_NDX",	"I10_NPR",	"I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25",	"KEY_NIS",	"LOS",	"MDC",	"MDC_NoPOA",	"NIS_STRATUM",	"PAY1",	"PL_NCHS",	"PRDAY1",	"PRDAY2",	"PRDAY3",	"PRDAY4",	"PRDAY5",	"PRDAY6",	"PRDAY7",	"PRDAY8",	"PRDAY9",	"PRDAY10",	"PRDAY11",	"PRDAY12",	"PRDAY13",	"PRDAY14",	"PRDAY15",	"PRDAY16",	"PRDAY17",	"PRDAY18",	"PRDAY19",	"PRDAY20",	"PRDAY21",	"PRDAY22",	"PRDAY23",	"PRDAY24",	"PRDAY25",	"PRVER",	"RACE",	"TOTCHG",	"TRAN_IN",	"TRAN_OUT",	"YEAR",	"ZIPINC_QRTL")

# NIS 2017 Severty Data Read-in
SevNIS2017 <- read_fwf("NIS_2017_Severity.asc", fwf_positions(c(1,	6,	16,	20,	22),
                                                              c(5,	15,	19,	21,	23)))

colnames(SevNIS2017) <- c("HOSP_NIS",	"KEY_NIS",	"APRDRG",	"APRDRG_Risk_Mortality",	"APRDRG_Severity")
SevNIS2017 <- as.data.frame(SevNIS2017)

# NIS 2016 Severity Data Read-in
SevNIS2016 <- read_fwf("NIS_2016_Severity.asc", fwf_positions(c(1,	6,	16,	20,	22),
                                                              c(5,	15,	19,	21,	23)))

colnames(SevNIS2016) <- c("HOSP_NIS",	"KEY_NIS",	"APRDRG",	"APRDRG_Risk_Mortality",	"APRDRG_Severity")
SevNIS2016 <- as.data.frame(SevNIS2016)

#NIS Hospital 2016 Data Read-in

NISH16 <- read_fwf("NIS_2016_Hospital.asc", fwf_positions(c(1,	12,	14,	16,	18,	23,	25,	27,	31,	39,	43,	51,	55,	61),
                                                c(11,	13,	15,	17,	22,	24,	26,	30,	38,	42,	50,	54,	60,	64)))

colnames(NISH16) <-c("DISCWT",	"HOSP_BEDSIZE",	"HOSP_DIVISION",	"HOSP_LOCTEACH",	"HOSP_NIS",	"HOSP_REGION",	"H_CONTRL",	"NIS_STRATUM",	"N_DISC_U",	"N_HOSP_U",	"S_DISC_U",	"S_HOSP_U",	"TOTAL_DISC",	"YEAR")
NISH16 <- as.data.frame(NISH16)

#NIS Hospital 2017 Data Read-in

NISH17 <- read_fwf("NIS_2017_Hospital.asc", fwf_positions(c(1,	12,	14,	16,	18,	23,	25,	27,	31,	39,	43,	51,	55,	61),
                                                          c(11,	13,	15,	17,	22,	24,	26,	30,	38,	42,	50,	54,	60,	64)))

colnames(NISH17) <-c("DISCWT",	"HOSP_BEDSIZE",	"HOSP_DIVISION",	"HOSP_LOCTEACH",	"HOSP_NIS",	"HOSP_REGION",	"H_CONTRL",	"NIS_STRATUM",	"N_DISC_U",	"N_HOSP_U",	"S_DISC_U",	"S_HOSP_U",	"TOTAL_DISC",	"YEAR")
NISH17 <- as.data.frame(NISH17)


# NIS 2017 Severty Data Read-in
SevNIS2017 <- read_fwf("NIS_2017_Severity.asc", fwf_positions(c(1,	6,	16,	20,	22),
                                                              c(5,	15,	19,	21,	23)))

colnames(SevNIS2017) <- c("HOSP_NIS",	"KEY_NIS",	"APRDRG",	"APRDRG_Risk_Mortality",	"APRDRG_Severity")

SevNIS2017 <- as.data.frame(SevNIS2017)
# NIS 2016 Severity Data Read-in
SevNIS2016 <- read_fwf("NIS_2016_Severity.asc", fwf_positions(c(1,	6,	16,	20,	22),
                                                              c(5,	15,	19,	21,	23)))

colnames(SevNIS2016) <- c("HOSP_NIS",	"KEY_NIS",	"APRDRG",	"APRDRG_Risk_Mortality",	"APRDRG_Severity")
SevNIS2016 <- as.data.frame(SevNIS2016)

# NIS 2016 Fiter
library(sqldf)


core2016_2 <- sqldf(" select * from core2016 where
    '0BTC0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7, I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTC4ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTD0ZZ' in (   	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTD4ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTF0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTF4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTG0ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTG4ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTH0ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTH4ZZ' in (    	I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTJ0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)
or '0BTJ4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15)")

core2016_2[, c("I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39",	"I10_DX40", "I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25", "PRDAY16",	"PRDAY17",	"PRDAY18",	"PRDAY19",	"PRDAY20",	"PRDAY21",	"PRDAY22",	"PRDAY23",	"PRDAY24",	"PRDAY25")] <- NA


core2016_2<-core2016_2[c("AGE",	"AGE_NEONATE",	"AMONTH",	"AWEEKEND",	"DIED",	"DISCWT",	"DISPUNIFORM",	"DQTR",	"DRG",	"DRGVER",	"DRG_NoPOA",	"DXVER",	"ELECTIVE",	"FEMALE",	"HCUP_ED",	"HOSP_DIVISION",	"HOSP_NIS",	"I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39",	"I10_DX40",	"I10_NDX",	"I10_NPR",	"I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25",	"KEY_NIS",	"LOS",	"MDC",	"MDC_NoPOA",	"NIS_STRATUM",	"PAY1",	"PL_NCHS",	"PRDAY1",	"PRDAY2",	"PRDAY3",	"PRDAY4",	"PRDAY5",	"PRDAY6",	"PRDAY7",	"PRDAY8",	"PRDAY9",	"PRDAY10",	"PRDAY11",	"PRDAY12",	"PRDAY13",	"PRDAY14",	"PRDAY15",	"PRDAY16",	"PRDAY17",	"PRDAY18",	"PRDAY19",	"PRDAY20",	"PRDAY21",	"PRDAY22",	"PRDAY23",	"PRDAY24",	"PRDAY25",	"PRVER",	"RACE",	"TOTCHG",	"TRAN_IN",	"TRAN_OUT",	"YEAR",	"ZIPINC_QRTL","I10_ECAUSE1",	"I10_ECAUSE2",	"I10_ECAUSE3",	"I10_ECAUSE4",	"I10_NECAUSE")]

core2016_2 <- sqldf("Select core2016_2.*,")
remove(full2016core)

remove(core2016)

remove(core2016_2)
memory.limit()
memory.limit(56000)
core2017_2 <- sqldf(" select * from core2017 where
   '0BTC0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTC4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTD0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTD4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTF0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTF4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTG0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTG4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTH0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTH4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTJ0ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)
or '0BTJ4ZZ' in (     I10_NPR,  I10_PR1,  I10_PR2,  I10_PR3,  I10_PR4,  I10_PR5,  I10_PR6,  I10_PR7,  I10_PR8,  I10_PR9,  I10_PR10, I10_PR11, I10_PR12, I10_PR13, I10_PR14, I10_PR15,	I10_PR16,	I10_PR17,	I10_PR18,	I10_PR19,	I10_PR20,	I10_PR21,	I10_PR22,	I10_PR23,	I10_PR24,	I10_PR25)")
remove(core2017)
remove(full2017core)

core2017_2 [,c( "I10_ECAUSE1",	"I10_ECAUSE2",	"I10_ECAUSE3",	"I10_ECAUSE4",	"I10_NECAUSE")] <- NA
ccore2017_2<-core2017_2[c("AGE",	"AGE_NEONATE",	"AMONTH",	"AWEEKEND",	"DIED",	"DISCWT",	"DISPUNIFORM",	"DQTR",	"DRG",	"DRGVER",	"DRG_NoPOA",	"DXVER",	"ELECTIVE",	"FEMALE",	"HCUP_ED",	"HOSP_DIVISION",	"HOSP_NIS",	"I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39",	"I10_DX40",	"I10_NDX",	"I10_NPR",	"I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25",	"KEY_NIS",	"LOS",	"MDC",	"MDC_NoPOA",	"NIS_STRATUM",	"PAY1",	"PL_NCHS",	"PRDAY1",	"PRDAY2",	"PRDAY3",	"PRDAY4",	"PRDAY5",	"PRDAY6",	"PRDAY7",	"PRDAY8",	"PRDAY9",	"PRDAY10",	"PRDAY11",	"PRDAY12",	"PRDAY13",	"PRDAY14",	"PRDAY15",	"PRDAY16",	"PRDAY17",	"PRDAY18",	"PRDAY19",	"PRDAY20",	"PRDAY21",	"PRDAY22",	"PRDAY23",	"PRDAY24",	"PRDAY25",	"PRVER",	"RACE",	"TOTCHG",	"TRAN_IN",	"TRAN_OUT",	"YEAR",	"ZIPINC_QRTL","I10_ECAUSE1",	"I10_ECAUSE2",	"I10_ECAUSE3",	"I10_ECAUSE4",	"I10_NECAUSE")]

core2017_2 <- sqldf("Select core2017_2*, 	SevNIS2017.APRDRG,	SevNIS2017.APRDRG_Risk_Mortality,	SevNIS2017.APRDRG_Severity
                    From core2017_2
                    Left Join SevNIS2017
                    On core2017_2.KEY_NIS = SevNIS2017.KEY_NIS")
core2017_2 <- sqldf("Select core2017_2.*, NISH17.DISCWT,	NISH17.HOSP_BEDSIZE,NISH17.HOSP_LOCTEACH,NISH17.HOSP_REGION,	NISH17.H_CONTRL,	NISH17.NIS_STRATUM,	NISH17.N_DISC_U,	NISH17.N_HOSP_U,	NISH17.S_DISC_U,	NISH17.S_HOSP_U,	NISH17.TOTAL_DISC,	NISH17.YEAR)
                    From core2017_2
                    Left Join NISH17
                    On core2017_2.HOSP_NIS = NISH17.HOSP_NIS")

Core <- sqldf(" Select * from ccore2016_2
               Union All
               Select * from ccore2017_2
               Order by KEY_NIS")
core <- as.data.frame(core)
write.csv(core, "CoreNIS.csv")

Core <- read_csv("Core_revised1.csv")



Core = mutate(Core, Payer_0 = case_when(
    PAY1 < 0 ~ 1, PAY1 >= 0 ~ 0
))
Core = mutate(Core, Payer_1 = case_when(
    PAY1 == 1 ~ 1, PAY1 != 1 ~ 0
))
Core = mutate(Core, Payer_2 = case_when(
    PAY1 == 2 ~ 1, PAY1 != 2 ~ 0
))
Core = mutate(Core, Payer_3 = case_when(
    PAY1 == 3 ~ 1, PAY1 != 3 ~ 0
))
Core = mutate(Core, Payer_4 = case_when(
    PAY1 == 4 ~ 1, PAY1 != 4 ~ 0
))
Core = mutate(Core, Payer_5 = case_when(
    PAY1 == 5 ~ 1, PAY1 != 5 ~ 0
))
Core = mutate(Core, Payer_6 = case_when(
    PAY1 == 6 ~ 1, PAY1 != 6 ~ 0
))



library(readr)
Core <- read_csv("Core_revised1.csv")




library(caret)
set.seed(123)
train <- createDataPartition(Core$KEY_NIS, p = .7, 
                                  list = FALSE, 
                                  times = 1)
CoreTrain <- Core[ train,]
CoreTest  <- Core[-train,]


allrows <- 1:nrow(Core)

set.seed(123)
trainrows <- sample(allrows, replace = F, size = 0.7*length(allrows))
test_cvrows <- allrows[-trainrows]
testrows <- sample(test_cvrows, replace=F, size = 0.5*length(test_cvrows))
cvrows <- test_cvrows[-which(test_cvrows %in% testrows)]

train <- Core[trainrows,]
test <- Core[testrows,]
cvr <- Core[cvrows,]

core_d <- Core[,c("KEY_NIS","I10_DX1",	"I10_DX2",	"I10_DX3",	"I10_DX4",	"I10_DX5",	"I10_DX6",	"I10_DX7",	"I10_DX8",	"I10_DX9",	"I10_DX10",	"I10_DX11",	"I10_DX12",	"I10_DX13",	"I10_DX14",	"I10_DX15",	"I10_DX16",	"I10_DX17",	"I10_DX18",	"I10_DX19",	"I10_DX20",	"I10_DX21",	"I10_DX22",	"I10_DX23",	"I10_DX24",	"I10_DX25",	"I10_DX26",	"I10_DX27",	"I10_DX28",	"I10_DX29",	"I10_DX30",	"I10_DX31",	"I10_DX32",	"I10_DX33",	"I10_DX34",	"I10_DX35",	"I10_DX36",	"I10_DX37",	"I10_DX38",	"I10_DX39")]
core_d <- as.data.frame(core_d)
library(sqldf)

core_p <- Core[,c("KEY_NIS", "I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25")] 
core_p <- as.data.frame(core_p)
library(reshape)
diagnosis <- melt(data=core_d, id.vars=c("KEY_NIS"), measure = c("I10_DX1","I10_DX2","I10_DX3","I10_DX4","I10_DX5","I10_DX6","I10_DX7","I10_DX8","I10_DX9","I10_DX10","I10_DX11","I10_DX12","I10_DX13","I10_DX14","I10_DX15","I10_DX16","I10_DX17","I10_DX18","I10_DX19","I10_DX20","I10_DX21","I10_DX22","I10_DX23","I10_DX24","I10_DX25","I10_DX26","I10_DX27","I10_DX28","I10_DX29","I10_DX30","I10_DX31","I10_DX32","I10_DX33","I10_DX34","I10_DX35","I10_DX36","I10_DX37","I10_DX38","I10_DX39"))
diagnosis <- sqldf("Select diagnosis.*, leftstr(value,1) as 'F1', substr(value,2,2) as 'value1' from diagnosis")
diagnosis$value1 <- as.numeric(as.character(diagnosis$value1))
diagnosis1 <- sqldf("Select diagnosis.*, 
case when F1 = 'F' and value1 >0 and value1 < 10 then 1 else 0 end as 'F01-F09', 
case when F1 = 'F' and value1 >9 and value1 < 20 then 1 else 0 end as 'F10-F19',
case when F1 = 'F' and value1 >19 and value1 < 30 then 1 else 0 end as 'F20-F29',
case when F1 = 'F' and value1 >29 and value1 < 40 then 1 else 0 end as 'F30-F39',
case when F1 = 'F' and value1 >39 and value1 < 50 then 1 else 0 end as 'F40-F49',
case when F1 = 'F' and value1 >49 and value1 < 60 then 1 else 0 end as 'F50-F59',
case when F1 = 'F' and value1 >59 and value1 < 70 then 1 else 0 end as 'F60-F69',
case when F1 = 'F' and value1 >69 and value1 < 80 then 1 else 0 end as 'F70-F79',
case when F1 = 'F' and value1 >79 and value1 < 90 then 1 else 0 end as 'F80-F89',
case when F1 = 'F' and value1 >89 and value1 < 99 then 1 else 0 end as 'F90-F98',
case when F1 = 'F' and value1 >=99 then 1 else 0 end as 'F99'  
From diagnosis")
write.csv(diagnosis1,"diagnosis1.csv")
diagnosis$value1 <- as.numeric(as.character(diagnosis$value1))
sapply(diagnosis1,class)
cols.num<-c("F01-F09","F10-F19","F20-F29","F30-F39","F40-F49","F50-F59","F60-F69","F70-F79","F80-F89","F90-F98","F99")
diagnosis1[cols.num] <- sapply(diagnosis1[cols.num], as.numeric)

diagnosis1 <- as.data.frame(diagnosis1)

write.csv(diagnosisf, "diagnosisf.csv")
summary(diagnosisff)

library(reshape2)
diagnosisff <- sqldf("Select KEY_NIS, sum('F01-F09') as 'F01-F09' from diagnosis1 group by KEY_NIS")


diagnosisf <- sqldf("Select KEY_NIS, sum(F01-F09) as 'F01-F09',sum(F10-F19) as 'F10-F19', sum('F20-F29') as 'F20-F29' ,sum('F30-F39')as 'F30-F39'
                    , sum('F40-F49')as 'F40-F49', sum('F50-F59') as 'F50-F59',sum('F60-F69') as 'F60-F69',sum('F70-F79') as 'F70-F79', sum('F80-F89') as 'F80-F89',sum('F90-F98') as 'F90-F98', sum('F99') as 'F99'
                    From diagnosis1
                    Group by KEY_NIS")
diagnosisf <- sqldf("Select KEY_NIS, sum(F01-F09) as 'F01-F09',sum(F10-F19) as 'F10-F19', sum(F20-F29) as 'F20-F29' ,sum(F30-F39)as 'F30-F39'
                    , sum(F40-F49)as 'F40-F49', sum(F50-F59) as 'F50-F59',sum(F60-F69) as 'F60-F69',sum(F70-F79) as 'F70-F79', sum(F80-F89) as 'F80-F89',sum(F90-F98) as 'F90-F98', sum(F99) as 'F99'
                    From diagnosis1
                    Group by KEY_NIS")

summary(diagnosisf)

diagnosisf <- sqldf( "Select diagnosisf.*, 
                     case when 'F01-F09' >0 then 1 else 0 end as 'F01-F10c',
                     case when 'F10-F19' >0 then 1 else 0 end as 'F10-F19c',
                     case when 'F20-F29' >0 then 1 else 0 end as 'F20-F29c',
                     case when 'F30-F39' >0 then 1 else 0 end as 'F30-F39c',
                     case when 'F40-F49' >0 then 1 else 0 end as 'F40-F49c',
                     case when 'F50-F59' >0 then 1 else 0 end as 'F50-F59c',
                     case when 'F60-F69' >0 then 1 else 0 end as 'F60-F69c',
                     case when 'F70-F79' >0 then 1 else 0 end as 'F70-F79c',
                     case when 'F80-F89' >0 then 1 else 0 end as 'F80-F89c',
                     case when 'F90-F98' >0 then 1 else 0 end as 'F90-F98c',
                     case when 'F99' >0 then 1 else 0 end as 'F99c'
                     From diagnosisf")

procedure <- melt(data=core_p, id.vars=c("KEY_NIS"), measure = c("I10_PR1",	"I10_PR2",	"I10_PR3",	"I10_PR4",	"I10_PR5",	"I10_PR6",	"I10_PR7",	"I10_PR8",	"I10_PR9",	"I10_PR10",	"I10_PR11",	"I10_PR12",	"I10_PR13",	"I10_PR14",	"I10_PR15",	"I10_PR16",	"I10_PR17",	"I10_PR18",	"I10_PR19",	"I10_PR20",	"I10_PR21",	"I10_PR22",	"I10_PR23",	"I10_PR24",	"I10_PR25"))
procedure <- sqldf("Select diagnosis.*, leftstr(value,1) as 'F1', substr(value,2,2) as 'value1' from diagnosis")
diagnosis$value1 <- as.numeric(as.character(diagnosis$value))




procedure2 <- sqldf("Select procedure.*,
                    case when value = '0BTC0ZZ' then 1 else 0 end as 'L0BTC0ZZ',
                    case when value = '0BTC4ZZ' then 1 else 0 end as 'L0BTC4ZZ',
                    case when value = '0BTD0ZZ' then 1 else 0 end as 'L0BTD0ZZ',
                    case when value = '0BTD4ZZ' then 1 else 0 end as 'L0BTD4ZZ',
                    case when value = '0BTF0ZZ' then 1 else 0 end as 'L0BTF0ZZ',
                    case when value = '0BTF4ZZ' then 1 else 0 end as 'L0BTF4ZZ',
                    case when value = '0BTH0ZZ' then 1 else 0 end as 'L0BTH0ZZ',
                    case when value = '0BTH4ZZ' then 1 else 0 end as 'L0BTH4ZZ',
                    case when value = '0BTJ0ZZ' then 1 else 0 end as 'L0BTJ0ZZ',
                    case when value = '0BTJ4ZZ' then 1 else 0 end as 'L0BTJ4ZZ'
                    From procedure")
write.csv(procedure2, "procedure2.csv")

summary(procedure2)
proc <-dcast(data = procedure2,formula = KEY_NIS~L0BTC0ZZ,value.var = "freqvalue")


Core <- subset(Core, DIED>=0)
Core <- subset(Core, FEMALE >=0)
Core <- subset(Core, RACE >=0)
Core <- subset(Core, ZIPINC_QRTL >=0)
Core <- subset(Core, PAY1 >=0)
Core <- subset(Core, HOSP_LOCTEACH >=0)

Core <- read_csv("Core_revised1.csv")
library(caret)
set.seed(123)

smp_size <- floor(0.7 * nrow(Core))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Core)), size = smp_size)

CoreTrain <- Core[train_ind, ]
CoreTest <- Core[-train_ind, ]


train <- createDataPartition(Core$KEY_NIS, p = .7, 
                             list = FALSE, 
                             times = 1)
CoreTrain <- Core[ train,]
CoreTest  <- Core[-train,]

CoreTraincos <- subset(CoreTrain, TOTCHG >=0)
CoreTescost <- subset(CoreTest, TOTCHG >=0)

CoreTrain_d <- CoreTrain %>% filter(DIED >=0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)
CoreTest_d <- CoreTest %>% filter(DIED >=0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)
summary(CoreTrain_d$DIED)
summary(CoreTest_d$DIED)

summary(diagnosisL)

#Death Prediction


#Logistic Regression

model <- glm(DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
             + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c ,
             data = CoreTrain_d, family = binomial)

summary(model)

probabilities <- model %>% predict(CoreTest_d, type = "response")

summary(probabilities)

diedlog1 <- CoreTest_d %>% select(DIED)

diedlog1$predicted.classes35 <- as.numeric(ifelse(probabilities > 0.035, "1", "0"))
diedlog1$predicted.classes4 <- as.numeric(ifelse(probabilities > 0.04, "1", "0"))
diedlog1$predicted.classes5 <- as.numeric(ifelse(probabilities > 0.05, "1", "0"))

table(pred=diedlog1$predicted.classes35,true=diedlog1$DIED)
table(pred=diedlog1$predicted.classes4,true=diedlog1$DIED)
table(pred=diedlog1$predicted.classes5,true=diedlog1$DIED)

model <- glm(DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
             + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL +SMI_Total ,
             data = CoreTrain_d, family = binomial)

summary(model)

probabilities <- model %>% predict(CoreTest_d, type = "response")

summary(probabilities)

diedlog2 <- CoreTest_d %>% select(DIED)

diedlog2$predicted.classes35 <- as.numeric(ifelse(probabilities > 0.035, "1", "0"))
diedlog2$predicted.classes4 <- as.numeric(ifelse(probabilities > 0.04, "1", "0"))
diedlog2$predicted.classes5 <- as.numeric(ifelse(probabilities > 0.05, "1", "0"))

table(pred=diedlog2$predicted.classes35,true=diedlog2$DIED)
table(pred=diedlog2$predicted.classes4,true=diedlog2$DIED)
table(pred=diedlog2$predicted.classes5,true=diedlog2$DIED)

model <- glm(DIED ~ F01F09c +  F70F79c  + AGE +  FEMALE +  SMI_Total ,
             data = CoreTrain_d, family = binomial)

summary(model)

probabilities <- model %>% predict(CoreTest_d, type = "response")

summary(probabilities)

diedlog3 <- CoreTest_d %>% select(DIED)

diedlog3$predicted.classes35 <- as.numeric(ifelse(probabilities > 0.025, "1", "0"))
diedlog3$predicted.classes4 <- as.numeric(ifelse(probabilities > 0.04, "1", "0"))
diedlog3$predicted.classes5 <- as.numeric(ifelse(probabilities > 0.05, "1", "0"))

table(pred=diedlog3$predicted.classes35,true=diedlog2$DIED)
table(pred=diedlog3$predicted.classes4,true=diedlog2$DIED)
table(pred=diedlog3$predicted.classes5,true=diedlog2$DIED)


#Random Forest

fit <- randomForest(DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c 
                    + F70F79c + F80F89c + F90F98c + F99c , data=CoreTrain_d, ntree = 200)

print(fit)

pred <- predict(fit, CoreTest_d)
summary(pred)

diedrf1 <- CoreTest_d %>% select(DIED)

diedrf1$predicted.classes35 <- as.numeric(ifelse(pred > 0.035, "1", "0"))
diedrf1$predicted.classes4 <- as.numeric(ifelse(pred > 0.04, "1", "0"))
diedrf1$predicted.classes5 <- as.numeric(ifelse(pred > 0.05, "1", "0"))

table(pred=diedrf1$predicted.classes35,true=diedrf1$DIED)
table(pred=diedrf1$predicted.classes4,true=diedrf1$DIED)
table(pred=diedrf1$predicted.classes5,true=diedrf1$DIED)



fit <- randomForest(DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + 
F60F69c + F70F79c + F80F89c + F90F98c + F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL, data=CoreTrain_d, ntree = 200)

print(fit)

pred <- predict(fit, CoreTest_d)
summary(pred)

diedrf2 <- CoreTest_d %>% select(DIED)

diedrf2$predicted.classes35 <- as.numeric(ifelse(pred > 0.035, "1", "0"))
diedrf2$predicted.classes4 <- as.numeric(ifelse(pred > 0.04, "1", "0"))
diedrf2$predicted.classes5 <- as.numeric(ifelse(pred > 0.05, "1", "0"))

table(pred=diedrf2$predicted.classes35,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes4,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes5,true=diedrf2$DIED)

fit <- randomForest(DIED ~ F01F09c +  F70F79c  + AGE +  FEMALE +  SMI_Total , data=CoreTrain_d, ntree = 200)

print(fit)

pred <- predict(fit, CoreTest_d)
summary(pred)

diedrf2 <- CoreTest_d %>% select(DIED)

diedrf2$predicted.classes35 <- as.numeric(ifelse(pred > 0.035, "1", "0"))
diedrf2$predicted.classes4 <- as.numeric(ifelse(pred > 0.04, "1", "0"))
diedrf2$predicted.classes5 <- as.numeric(ifelse(pred > 0.05, "1", "0"))

table(pred=diedrf2$predicted.classes35,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes4,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes5,true=diedrf2$DIED)


#Random Forest Stratified
train_live <- select(filter(CoreTrain_d, CoreTrain_d$DIED == 0), everything())
train_died <- select(filter(CoreTrain_d, CoreTrain_d$DIED == 1), everything())
set.seed(123)
train_rows <- 1:nrow(train_live)
live_subset_indexes <- sample(train_rows, replace = F, size = nrow(train_died))
live_subset <- train_live[live_subset_indexes,]
stratified <- rbind(live_subset, train_died)
stratified_fit <- randomForest(factor(DIED) ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + 
                                   F70F79c + F80F89c + F90F98c + F99c, data=stratified, type = "classification", ntree = 200)

print(stratified_fit)

diedrfs1 <- CoreTest_d %>% select(DIED)
diedrfs1$predstrat <- predict(stratified_fit, CoreTest_d)
table(pred=diedrfs1$predstrat,true=diedrfs1$DIED)


stratified_fit <- randomForest(factor(DIED) ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c +
        F70F79c + F80F89c + F90F98c + F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL , data=stratified, type = "classification", ntree = 200)

print(stratified_fit)

diedrfs2 <- CoreTest_d %>% select(DIED)
diedrfs2$predstrat <- predict(stratified_fit, CoreTest_d)
table(pred=diedrfs2$predstrat,true=diedrfs2$DIED)


#Logistic Regression Stratified

strat_reg <- glm(formula = DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c +
                     F70F79c + F80F89c + F90F98c + F99c, family = binomial, data = stratified)
summary(strat_reg)

diedlogs1 <- CoreTest_d %>% select(DIED)

diedlogs1$strat_reg_fit = predict(strat_reg, CoreTest_d)
diedrf2$predicted.classes35 <- as.numeric(ifelse(pred > 0.035, "1", "0"))
diedrf2$predicted.classes4 <- as.numeric(ifelse(pred > 0.04, "1", "0"))
diedrf2$predicted.classes5 <- as.numeric(ifelse(pred > 0.05, "1", "0"))

table(pred=diedrf2$predicted.classes35,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes4,true=diedrf2$DIED)
table(pred=diedrf2$predicted.classes5,true=diedrf2$DIED)

summary(diedlogs1$strat_reg_fit)

table(pred=diedlogs1$strat_reg_fit,true=diedlogs1$DIED)

strat_reg <- glm(formula = DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + 
    F70F79c + F80F89c + F90F98c + F99c +  AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL, family = binomial, data = stratified)
summary(strat_reg)

diedlogs2 <- CoreTest_d %>% select(DIED)

diedlogs2$strat_reg_fit = predict(strat_reg, CoreTest_d)
table(pred=diedlogs1$strat_reg_fit,true=diedlogs1$DIED)


strat_reg <- glm(formula = DIED ~ AGE + FEMALE + F70F79 + Open + PE + Both, family = binomial, data = stratified)

write.csv(CoreTrain_d , "CoreTrain_d.csv")
write.csv(CoreTest_d , "CoreTest_d.csv")

#Total Cost of Stay

#Random Forest Quantiles

CoreTraincos <- CoreTrain %>% filter(TOTCHG>=0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)
CoreTestcos <- CoreTest %>% filter(TOTCHG>=0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)



CoreTraincos = mutate(CoreTraincos, Payer_0 = case_when(
    PAY1 < 0 ~ 1, PAY1 >= 0 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_1 = case_when(
    PAY1 == 1 ~ 1, PAY1 != 1 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_2 = case_when(
    PAY1 == 2 ~ 1, PAY1 != 2 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_3 = case_when(
    PAY1 == 3 ~ 1, PAY1 != 3 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_4 = case_when(
    PAY1 == 4 ~ 1, PAY1 != 4 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_5 = case_when(
    PAY1 == 5 ~ 1, PAY1 != 5 ~ 0
))
CoreTraincos = mutate(CoreTraincos, Payer_6 = case_when(
    PAY1 == 6 ~ 1, PAY1 != 6 ~ 0
))

CoreTestcos = mutate(CoreTestcos, Payer_0 = case_when(
    PAY1 < 0 ~ 1, PAY1 >= 0 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_1 = case_when(
    PAY1 == 1 ~ 1, PAY1 != 1 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_2 = case_when(
    PAY1 == 2 ~ 1, PAY1 != 2 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_3 = case_when(
    PAY1 == 3 ~ 1, PAY1 != 3 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_4 = case_when(
    PAY1 == 4 ~ 1, PAY1 != 4 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_5 = case_when(
    PAY1 == 5 ~ 1, PAY1 != 5 ~ 0
))
CoreTestcos = mutate(CoreTestcos, Payer_6 = case_when(
    PAY1 == 6 ~ 1, PAY1 != 6 ~ 0
))


summary(CoreTestcos$TOTCHG)

CoreTraincos$TOTAL_CHARGE <- ifelse(CoreTraincos$TOTCHG < 60000, 'Group 1', ifelse(CoreTraincos$TOTCHG < 87000, 'Group 2', ifelse(CoreTraincos$TOTCHG < 134000, 'Group 3', 'Group 4')))
CoreTraincos$TOTAL_CHARGE = as.factor(CoreTraincos$TOTAL_CHARGE)
CoreTestcos$TOTAL_CHARGE <- ifelse(CoreTestcos$TOTCHG < 60000, 'Group 1', ifelse(CoreTestcos$TOTCHG < 87000, 'Group 2', ifelse(CoreTestcos$TOTCHG < 134000, 'Group 3', 'Group 4')))

print(head(CoreTrain$TOTAL_CHARGE))

total_charge <- randomForest(TOTAL_CHARGE ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c , data=CoreTraincos, ntree = 100)
print(total_charge)

totchg1 <- CoreTestcos %>% select(TOTAL_CHARGE)
totchg1$predstrat <- predict(total_charge, CoreTestcos)
table(pred=totchg1$predstrat,true=totchg1$TOTAL_CHARGE)

total_charge <- randomForest(TOTAL_CHARGE ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c +  AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL, data=CoreTraincos, ntree = 100)
print(total_charge)

totchg2 <- CoreTestcos %>% select(TOTAL_CHARGE)
totchg2$predstrat <- predict(total_charge, CoreTestcos)
table(pred=totchg2$predstrat,true=totchg2$TOTAL_CHARGE)

total_charge <- randomForest(TOTAL_CHARGE ~ AGE+ RACE + FEMALE +  PAY1 + F01F09c + F10F19c +F20F29c+  +F40F49c 
                             +I10_NDX +SMI_Total, data=CoreTraincos, ntree = 100)
print(total_charge)

totchg3 <- CoreTestcos %>% select(TOTAL_CHARGE)
totchg3$predstrat <- predict(total_charge, CoreTestcos)
table(pred=totchg3$predstrat,true=totchg3$TOTAL_CHARGE)


cos_reg <- lm(TOTCHG ~ AGE + FEMALE + RACE + F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60.F69c + F70.F79c + F80.F89c + F90.F98c + F99c + SMI_Total + Open + PE + Both + ZIPINC_QRTL + Payer_0 + Payer_1 + Payer_2 + Payer_3 + Payer_4 + Payer_5 + Payer_6 + HOSP_LOCTEACH + HOSP_BEDSIZE + HOSP_DIVISION + HOSP_REGION + H_CONTRL + Has_SMI, data = train)
summary(cos_reg)

# Linear Regression 

totchglr1 <- lm(TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c, data = CoreTraincos)
summary(totchglr1)


tochglinreg1 <- CoreTestcos %>% select(TOTCHG)
tochglinreg1$pred <- predict(totchglr1, CoreTestcos)

RSS <- c(crossprod(totchglr1$residuals))
MSE <- RSS / length(totchglr1$residuals)
RMSE <- sqrt(MSE)
RMSE
write.csv(tochglinreg1 , "actualpredlr1.csv")

tochglinreg1$Bet10 <- ifelse(tochglinreg1$pred >= (tochglinreg1$TOTCHG*0.9) & tochglinreg1$pred <= (tochglinreg1$TOTCHG*1.1) , 1,0)
tochglinreg1$Bet20 <- ifelse(tochglinreg1$pred >= (tochglinreg1$TOTCHG*0.8) & tochglinreg1$pred <= (tochglinreg1$TOTCHG*1.2) , 1,0)
tochglinreg1$Bet30 <- ifelse(tochglinreg1$pred >= (tochglinreg1$TOTCHG*0.7) & tochglinreg1$pred <= (tochglinreg1$TOTCHG*1.3) , 1,0)

PM10lr_1 = sum(tochglinreg1$Bet10)/nrow(tochglinreg1)
PM20lr_1 = sum(tochglinreg1$Bet20)/nrow(tochglinreg1)
PM30lr_1 = sum(tochglinreg1$Bet30)/nrow(tochglinreg1)
PM10lr_1
PM20lr_1
PM30lr_1

RMSElr_1 <- caret::RMSE(tochglinreg1$pred, tochglinreg1$TOTCHG)
RMSElr_1

totchglr2 <- lm(TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL +  Payer_1 + Payer_2 + Payer_3 + Payer_4 + Payer_5 + Payer_6  + HOSP_BEDSIZE + HOSP_DIVISION + HOSP_REGION + H_CONTRL, data = CoreTraincos)
summary(totchglr2)


tochglinreg2 <- CoreTestcos %>% select(TOTCHG)
tochglinreg2$pred <- predict(totchglr2, CoreTestcos)
write.csv(tochglinreg2 , "actualpredlr2.csv")

tochglinreg2$Bet10 <- ifelse(tochglinreg2$pred >= (tochglinreg2$TOTCHG*0.9) & tochglinreg2$pred <= (tochglinreg2$TOTCHG*1.1) , 1,0)
tochglinreg2$Bet20 <- ifelse(tochglinreg2$pred >= (tochglinreg2$TOTCHG*0.8) & tochglinreg2$pred <= (tochglinreg2$TOTCHG*1.2) , 1,0)
tochglinreg2$Bet30 <- ifelse(tochglinreg2$pred >= (tochglinreg2$TOTCHG*0.7) & tochglinreg2$pred <= (tochglinreg2$TOTCHG*1.3) , 1,0)

PM10lr_2 = sum(tochglinreg2$Bet10)/nrow(tochglinreg2)
PM20lr_2 = sum(tochglinreg2$Bet20)/nrow(tochglinreg2)
PM30lr_2 = sum(tochglinreg2$Bet30)/nrow(tochglinreg2)
PM10lr_2
PM20lr_2
PM30lr_2

RMSElr_2 <- caret::RMSE(tochglinreg2$pred, tochglinreg2$TOTCHG)
RMSElr_2

totchglr3 <- lm(TOTCHG ~ F01F09c + F10F19c + F20F29c + F40F49c + AGE + RACE + FEMALE + SMI_Total, data = CoreTraincos)
summary(totchglr3)


tochglinreg3 <- CoreTestcos %>% select(TOTCHG)
tochglinreg3$pred <- predict(totchglr3, CoreTestcos)

tochglinreg3$Bet10 <- ifelse(tochglinreg3$pred >= (tochglinreg3$TOTCHG*0.9) & tochglinreg3$pred <= (tochglinreg3$TOTCHG*1.1) , 1,0)
tochglinreg3$Bet20 <- ifelse(tochglinreg3$pred >= (tochglinreg3$TOTCHG*0.8) & tochglinreg3$pred <= (tochglinreg3$TOTCHG*1.2) , 1,0)
tochglinreg3$Bet30 <- ifelse(tochglinreg3$pred >= (tochglinreg3$TOTCHG*0.7) & tochglinreg3$pred <= (tochglinreg3$TOTCHG*1.3) , 1,0)

write.csv(tochglinreg3 , "actualpredlr3.csv")


PM10lr_3 = sum(tochglinreg3$Bet10)/nrow(tochglinreg3)
PM20lr_3 = sum(tochglinreg3$Bet20)/nrow(tochglinreg3)
PM30lr_3 = sum(tochglinreg3$Bet30)/nrow(tochglinreg3)
PM10lr_3
PM20lr_3
PM30lr_3

RMSElr_3 <- caret::RMSE(tochglinreg3$pred, tochglinreg3$TOTCHG)
RMSElr_3

cos_reg <- lm(TOTCHG ~ AGE + FEMALE + RACE + F01.F09c + F10.F19c + F20.F29c + F30.F39c + F40.F49c + F50.F59c + F60.F69c + F70.F79c + F80.F89c + F90.F98c + F99c + SMI_Total + Open + PE + Both + ZIPINC_QRTL + Payer_0 + Payer_1 + Payer_2 + Payer_3 + Payer_4 + Payer_5 + Payer_6 + HOSP_LOCTEACH + HOSP_BEDSIZE + HOSP_DIVISION + HOSP_REGION + H_CONTRL + Has_SMI, data = train)
summary(cos_reg)



#GBM



TOTCHGGBMtrain <- CoreTrain %>% filter(TOTCHG >0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)
TOTCHGGBMtest <-  CoreTest %>% filter(TOTCHG >0, FEMALE >=0, RACE >=0, ZIPINC_QRTL >=0, PAY1 >=0,HOSP_LOCTEACH >=0)

set.seed(123)

# train GBM model
gbm.fit <- gbm(
    formula = TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c,
    distribution = "gaussian",
    data = TOTCHGGBMtrain,
    n.trees = 10000,
    interaction.depth = 1,
    shrinkage = 0.001,
    cv.folds = 5,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
)  

print(gbm.fit)
sqrt(min(gbm.fit$cv.error))
gbm.perf(gbm.fit, method = "cv")

set.seed(123)

gbm.fit2 <- gbm(
    formula = TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c,
    distribution = "gaussian",
    data = TOTCHGGBMtrain,
    n.trees = 10000,
    interaction.depth = 3,
    shrinkage = 0.1,
    cv.folds = 5,
    n.cores = NULL, 
    verbose = FALSE
)  


min_MSE <- which.min(gbm.fit2$cv.error)

sqrt(gbm.fit2$cv.error[min_MSE])

gbm.perf(gbm.fit2, method = "cv")

hyper_grid <- expand.grid(
    shrinkage = c( .01, .1, .3),
    interaction.depth = c( 3, 4, 5),
    n.minobsinnode = c(5, 10, 15),
    bag.fraction = c(.65, .8, 1), 
    optimal_trees = 0,               
    min_RMSE = 0                     
)


nrow(hyper_grid)

random_index <- sample(1:nrow(TOTCHGGBMtrain), nrow(TOTCHGGBMtrain))
random_Core_train <- TOTCHGGBMtrain[random_index, ]


for(i in 1:nrow(hyper_grid)) {
    
    # reproducibility
    set.seed(123)
    
    # train model
    gbm.tune <- gbm(
        formula = TOTCHG ~  F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c,
        distribution = "gaussian",
        data = random_Core_train,
        n.trees = 2500,
        interaction.depth = hyper_grid$interaction.depth[i],
        shrinkage = hyper_grid$shrinkage[i],
        n.minobsinnode = hyper_grid$n.minobsinnode[i],
        bag.fraction = hyper_grid$bag.fraction[i],
        train.fraction = .75,
        n.cores = NULL, 
        verbose = FALSE
    )
    
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
    dplyr::arrange(min_RMSE) %>%
    head(10)

gbm.fit.final <- gbm(
    formula = TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c,
    distribution = "gaussian",
    data = TOTCHGGBMtrain,
    n.trees = 6,
    interaction.depth = 3,
    shrinkage = 0.1,
    n.minobsinnode = 3,
    bag.fraction = 0.65, 
    train.fraction = 1,
    n.cores = NULL, 
    verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary(
    gbm.fit.final, 
    cBars = 10,
    method = relative.influence, 
    las = 2
)


pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, TOTCHGGBMtest)


actualtotchg <- TOTCHGGBMtest %>% select(TOTCHG)
predgbmtg <- as.data.frame(pred)

actualpredtc <- cbind(actualtotchg,predgbmtg)
actualpredtc$Bet10 <- ifelse(actualpredtc$pred >= (actualpredtc$TOTCHG*0.9) & actualpredtc$pred <= (actualpredtc$TOTCHG*1.1) , 1,0)
actualpredtc$Bet20 <- ifelse(actualpredtc$pred >= (actualpredtc$TOTCHG*0.8) & actualpredtc$pred <= (actualpredtc$TOTCHG*1.2) , 1,0)
actualpredtc$Bet30 <- ifelse(actualpredtc$pred >= (actualpredtc$TOTCHG*0.7) & actualpredtc$pred <= (actualpredtc$TOTCHG*1.3) , 1,0)

PM10 = sum(actualpredtc$Bet10)/nrow(actualpredtc)
PM20 = sum(actualpredtc$Bet20)/nrow(actualpredtc)
PM30 = sum(actualpredtc$Bet30)/nrow(actualpredtc)
PM10
PM20
PM30
write.csv(actualpredtc, "actualpredtc.csv")


caret::RMSE(pred, TOTCHGGBMtest$TOTCHG)

for(i in 1:nrow(hyper_grid)) {
    
    # reproducibility
    set.seed(123)
    
    # train model
    gbm.tune <- gbm(
        formula = TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + 
            F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL,
        distribution = "gaussian",
        data = random_Core_train,
        n.trees = 5000,
        interaction.depth = hyper_grid$interaction.depth[i],
        shrinkage = hyper_grid$shrinkage[i],
        n.minobsinnode = hyper_grid$n.minobsinnode[i],
        bag.fraction = hyper_grid$bag.fraction[i],
        train.fraction = .75,
        n.cores = NULL, # will use all cores by default
        verbose = FALSE
    )
    
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
    hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
    dplyr::arrange(min_RMSE) %>%
    head(10)

gbm.fit.final <- gbm(
    formula = TOTCHG ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + 
        F99c + AGE + RACE + FEMALE + HOSP_LOCTEACH + PAY1 + ZIPINC_QRTL,
    distribution = "gaussian",
    data = TOTCHGGBMtrain,
    n.trees = 18,
    interaction.depth = 3,
    shrinkage = 0.1,
    n.minobsinnode = 10,
    bag.fraction = 0.65, 
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary(
    gbm.fit.final, 
    cBars = 10,
    method = relative.influence, # also can use permutation.test.gbm
    las = 2
)


pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, TOTCHGGBMtest)


actualtotchg1 <- TOTCHGGBMtest %>% select(TOTCHG)
predgbmtg1 <- as.data.frame(pred)

actualpredtc1 <- cbind(actualtotchg1,predgbmtg1)
actualpredtc1$Bet10 <- ifelse(actualpredtc1$pred >= (actualpredtc1$TOTCHG*0.9) & actualpredtc1$pred <= (actualpredtc1$TOTCHG*1.1) , 1,0)
actualpredtc1$Bet20 <- ifelse(actualpredtc1$pred >= (actualpredtc1$TOTCHG*0.8) & actualpredtc1$pred <= (actualpredtc1$TOTCHG*1.2) , 1,0)
actualpredtc1$Bet30 <- ifelse(actualpredtc1$pred >= (actualpredtc1$TOTCHG*0.7) & actualpredtc1$pred <= (actualpredtc1$TOTCHG*1.3) , 1,0)

write.csv(actualpredtc1, "actualpredtc2.csv")


PM10_12 = sum(actualpredtc1$Bet10)/nrow(actualpredtc1)
PM20_22 = sum(actualpredtc1$Bet20)/nrow(actualpredtc1)
PM30_32 = sum(actualpredtc1$Bet30)/nrow(actualpredtc1)
PM10_12
PM20_22
PM30_32


RMSE_2 <- caret::RMSE(pred, TOTCHGGBMtest$TOTCHG)
RMSE_2


#XGBoost

TOTCHGXGBtrain_1 <- TOTCHGGBMtrain[c( "TOTCHG","AGE" , "RACE" , "FEMALE" , "HOSP_LOCTEACH", "PAY1" , "ZIPINC_QRTL","F01F09c","F10F19c" , "F20F29c" , "F30F39c" , "F40F49c" 
                                      , "F50F59c" , "F60F69c" , "F70F79c" , "F80F89c" , "F90F98c" , "F99c" )]
summary(TOTCHGXGBtrain_1$TOTCHG)
TOTCHGXGBtest_1 <- TOTCHGGBMtest[c( "TOTCHG","AGE" , "RACE" , "FEMALE" , "HOSP_LOCTEACH", "PAY1" , "ZIPINC_QRTL","F01F09c","F10F19c" , "F20F29c" , "F30F39c" , "F40F49c" 
                                    , "F50F59c" , "F60F69c" , "F70F79c" , "F80F89c" , "F90F98c" , "F99c" )]


features <- setdiff(names(TOTCHGXGBtrain_1), "TOTCHG")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(TOTCHGXGBtrain_1, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%        
    dplyr::filter(code %in% c("clean", "lev")) %>% 
    magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, TOTCHGXGBtrain_1, varRestriction = new_vars) %>% as.matrix()
response_train <- TOTCHGXGBtrain_1$TOTCHG

# Prepare the test data
features_test <- vtreat::prepare(treatplan, TOTCHGXGBtest_1, varRestriction = new_vars) %>% as.matrix()
response_test <- TOTCHGXGBtest_1$TOTCHG

# dimensions of one-hot encoded data
dim(features_train)
## [1] 2051  208
dim(features_test)
## [1] 879 208

set.seed(123)

xgb.fit1 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0               # silent,
)

xgb.fit1$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )


# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

set.seed(123)

xgb.fit2 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 250,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

params <- list(
    eta = .1,
    max_depth = 5,
    min_child_weight = 2,
    subsample = .8,
    colsample_bytree = .9
)

# reproducibility
set.seed(123)

# train model
xgb.fit3 <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

xgb.fit3$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )

hyper_grid <- expand.grid(
    eta = c(.01, .05, .1, .3),
    max_depth = c(1, 3, 5, 7, 9),
    min_child_weight = c(1, 3, 5),
    subsample = c(.65, .8, 1), 
    colsample_bytree = c(.8, .9, 1),
    optimal_trees = 0,               # a place to dump results
    min_RMSE = 0                     # a place to dump results
)


nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
    
    # create parameter list
    params <- list(
        eta = hyper_grid$eta[i],
        max_depth = hyper_grid$max_depth[i],
        min_child_weight = hyper_grid$min_child_weight[i],
        subsample = hyper_grid$subsample[i],
        colsample_bytree = hyper_grid$colsample_bytree[i]
    )
    
    # reproducibility
    set.seed(123)
    
    # train model
    xgb.tune <- xgb.cv(
        params = params,
        data = features_train,
        label = response_train,
        nrounds = 5000,
        nfold = 5,
        objective = "reg:linear",  # for regression models
        verbose = 0,               # silent,
        early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
    )
    
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
    dplyr::arrange(min_RMSE) %>%
    head(10)

# parameter list
params <- list(
    eta = 0.3,
    max_depth = 1,
    min_child_weight = 5,
    subsample = 1.00,
    colsample_bytree = 1
)

# train final model
xgb.fit.final <- xgboost(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 35,
    objective = "reg:linear",
    verbose = 0
)

# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

par(mar = c(5, 8, 1, 1))
summary(
    importance_matrix, 
    cBars = 10,
    method = relative.influence, # also can use permutation.test.gbm
    las = 2
)

predxgb_1 <- predict(xgb.fit.final, features_test)

# results
rmsexgb_1 <- caret::RMSE(predxgb_1, response_test)
rmsexgb_1








actualtotchgxg_1 <- TOTCHGXGBtest_1  %>% select(TOTCHG)
predgbmtgxg_1 <- as.data.frame(predxgb_1)

actualpredtcxg_1 <- cbind(actualtotchgxg_1,predxgb_1)

write.csv(actualpredtcxg_1, "actualpredtcxg_1.csv")

actualpredtcxg_1$Bet10 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$LOS-0.5) & actualpredtcxg_1$pred <= (actualpredtcxg_1$LOS+0.5) , 1,0)
actualpredtcxg_1$Bet20 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$LOS-1) & actualpredtcxg_1$pred <= (actualpredtcxg_1$LOS+1) , 1,0)
actualpredtcxg_1$Bet30 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$LOS-1.5) & actualpredtcxg_1$pred <= (actualpredtcxg_1$LOS+1.5) , 1,0)

actualpredtcxg_1$Bet10 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.9) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.1) , 1,0)
actualpredtcxg_1$Bet20 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.8) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.2) , 1,0)
actualpredtcxg_1$Bet30 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.7) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.3) , 1,0)

PM10xg_1 = sum(actualpredtcxg_1$Bet10)/nrow(actualpredtcxg_1)
PM20xg_1 = sum(actualpredtcxg_1$Bet20)/nrow(actualpredtcxg_1)
PM30xg_1 = sum(actualpredtcxg_1$Bet30)/nrow(actualpredtcxg_1)
PM10xg_1
PM20xg_1
PM30xg_1


RMSExg_1 <- caret::RMSE(predxgb_1, TOTCHGXGBtest_1$TOTCHG)
RMSExg_1




TOTCHGXGBtrain_1 <- TOTCHGGBMtrain[c( "TOTCHG","F01F09c","F10F19c" , "F20F29c" , "F30F39c" , "F40F49c" 
                                      , "F50F59c" , "F60F69c" , "F70F79c" , "F80F89c" , "F90F98c" , "F99c" )]
TOTCHGXGBtest_1 <- subset(TOTCHGXGBtest_1, LOS>=0)
summary(TOTCHGXGBtrain_1$TOTCHG)
TOTCHGXGBtest_1 <- TOTCHGGBMtest[c( "TOTCHG","F01F09c","F10F19c" , "F20F29c" , "F30F39c" , "F40F49c" 
                                    , "F50F59c" , "F60F69c" , "F70F79c" , "F80F89c" , "F90F98c" , "F99c" )]
TOTCHGXGBtrain_1 <- subset(TOTCHGXGBtrain_1, LOS>=0)


features <- setdiff(names(TOTCHGXGBtrain_1), "TOTCHG")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(TOTCHGXGBtrain_1, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%        
    dplyr::filter(code %in% c("clean", "lev")) %>% 
    magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, TOTCHGXGBtrain_1, varRestriction = new_vars) %>% as.matrix()
response_train <- TOTCHGXGBtrain_1$TOTCHG

# Prepare the test data
features_test <- vtreat::prepare(treatplan, TOTCHGXGBtest_1, varRestriction = new_vars) %>% as.matrix()
response_test <- TOTCHGXGBtest_1$TOTCHG

# dimensions of one-hot encoded data
dim(features_train)
## [1] 2051  208
dim(features_test)
## [1] 879 208

set.seed(123)

xgb.fit1 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0               # silent,
)

xgb.fit1$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )


# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

set.seed(123)

xgb.fit2 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 500,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

params <- list(
    eta = .1,
    max_depth = 5,
    min_child_weight = 2,
    subsample = .8,
    colsample_bytree = .9
)

# reproducibility
set.seed(123)

# train model
xgb.fit3 <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

xgb.fit3$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )

hyper_grid <- expand.grid(
    eta = c(.01, .1, .3),
    max_depth = c(1, 3, 5),
    min_child_weight = c( 3, 5),
    subsample = c(.65, .8, 1), 
    colsample_bytree = c(.8, .9, 1),
    optimal_trees = 0,               # a place to dump results
    min_RMSE = 0                     # a place to dump results
)


nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
    
    # create parameter list
    params <- list(
        eta = hyper_grid$eta[i],
        max_depth = hyper_grid$max_depth[i],
        min_child_weight = hyper_grid$min_child_weight[i],
        subsample = hyper_grid$subsample[i],
        colsample_bytree = hyper_grid$colsample_bytree[i]
    )
    
    # reproducibility
    set.seed(123)
    
    # train model
    xgb.tune <- xgb.cv(
        params = params,
        data = features_train,
        label = response_train,
        nrounds = 5000,
        nfold = 5,
        objective = "reg:linear",  # for regression models
        verbose = 0,               # silent,
        early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
    )
    
    # add min training error and trees to grid
    hyper_grid$optimal_trees[i] <- which.min(xgb.tune$evaluation_log$test_rmse_mean)
    hyper_grid$min_RMSE[i] <- min(xgb.tune$evaluation_log$test_rmse_mean)
}

hyper_grid %>%
    dplyr::arrange(min_RMSE) %>%
    head(10)

# parameter list
params <- list(
    eta = 0.3,
    max_depth = 1,
    min_child_weight = 5,
    subsample = 0.8,
    colsample_bytree = 0.9
)

# train final model
xgb.fit.final <- xgboost(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 19,
    objective = "reg:linear",
    verbose = 0
)

# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

par(mar = c(5, 8, 1, 1))
summary(
    importance_matrix, 
    cBars = 10,
    method = relative.influence, # also can use permutation.test.gbm
    las = 2
)

predxgb_1 <- predict(xgb.fit.final, features_test)

# results
rmsexgb_1 <- caret::RMSE(predxgb_1, response_test)
rmsexgb_1

actualtotchgxg_1 <- TOTCHGXGBtest_1  %>% select(TOTCHG)
predgbmtgxg_1 <- as.data.frame(predxgb_1)

actualpredtcxg_1 <- cbind(actualtotchgxg_1,predxgb_1)

write.csv(actualpredtcxg_1, "actualpredtcxg_2.csv")


actualpredtcxg_1$Bet10 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.9) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.1) , 1,0)
actualpredtcxg_1$Bet20 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.8) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.2) , 1,0)
actualpredtcxg_1$Bet30 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.7) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.3) , 1,0)

PM10xg_1 = sum(actualpredtcxg_1$Bet10)/nrow(actualpredtcxg_1)
PM20xg_1 = sum(actualpredtcxg_1$Bet20)/nrow(actualpredtcxg_1)
PM30xg_1 = sum(actualpredtcxg_1$Bet30)/nrow(actualpredtcxg_1)
PM10xg_1
PM20xg_1
PM30xg_1

TOTCHGXGBtrain_1 <- TOTCHGGBMtrain[c( "TOTCHG","AGE" , "RACE" , "FEMALE" ,  "PAY1" , "F01F09c","F10F19c" , "F20F29c" ,  "F40F49c" 
                                      , "I10_NDX" , "SMI_Total"  )]
TOTCHGXGBtest_1 <- subset(TOTCHGXGBtest_1, LOS>=0)
summary(TOTCHGXGBtrain_1$TOTCHG)
TOTCHGXGBtest_1 <- TOTCHGGBMtest[c( "TOTCHG","AGE" , "RACE" , "FEMALE" ,  "PAY1" , "F01F09c","F10F19c" , "F20F29c" ,  "F40F49c" 
                                    , "I10_NDX" , "SMI_Total"  )]
FEMALE +  PAY1 + F01F09c + F10F19c 
+     + F20F29c + F40F49c + I10_NDX + SMI_Total


features <- setdiff(names(TOTCHGXGBtrain_1), "TOTCHG")

# Create the treatment plan from the training data
treatplan <- vtreat::designTreatmentsZ(TOTCHGXGBtrain_1, features, verbose = FALSE)

# Get the "clean" variable names from the scoreFrame
new_vars <- treatplan %>%
    magrittr::use_series(scoreFrame) %>%        
    dplyr::filter(code %in% c("clean", "lev")) %>% 
    magrittr::use_series(varName)     

# Prepare the training data
features_train <- vtreat::prepare(treatplan, TOTCHGXGBtrain_1, varRestriction = new_vars) %>% as.matrix()
response_train <- TOTCHGXGBtrain_1$TOTCHG

# Prepare the test data
features_test <- vtreat::prepare(treatplan, TOTCHGXGBtest_1, varRestriction = new_vars) %>% as.matrix()
response_test <- TOTCHGXGBtest_1$TOTCHG

# dimensions of one-hot encoded data
dim(features_train)
## [1] 2051  208
dim(features_test)
## [1] 879 208

set.seed(123)

xgb.fit1 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0               # silent,
)

xgb.fit1$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )


# plot error vs number trees
ggplot(xgb.fit1$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

set.seed(123)

xgb.fit2 <- xgb.cv(
    data = features_train,
    label = response_train,
    nrounds = 500,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

# plot error vs number trees
ggplot(xgb.fit2$evaluation_log) +
    geom_line(aes(iter, train_rmse_mean), color = "red") +
    geom_line(aes(iter, test_rmse_mean), color = "blue")

params <- list(
    eta = .1,
    max_depth = 5,
    min_child_weight = 2,
    subsample = .8,
    colsample_bytree = .9
)

# reproducibility
set.seed(123)

# train model
xgb.fit3 <- xgb.cv(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 1000,
    nfold = 5,
    objective = "reg:linear",  # for regression models
    verbose = 0,               # silent,
    early_stopping_rounds = 10 # stop if no improvement for 10 consecutive trees
)

xgb.fit3$evaluation_log %>%
    dplyr::summarise(
        ntrees.train = which(train_rmse_mean == min(train_rmse_mean))[1],
        rmse.train   = min(train_rmse_mean),
        ntrees.test  = which(test_rmse_mean == min(test_rmse_mean))[1],
        rmse.test   = min(test_rmse_mean),
    )

hyper_grid <- expand.grid(
    eta = c(.01, .1, .3),
    max_depth = c(1, 3, 5),
    min_child_weight = c( 3, 5),
    subsample = c(.65, .8, 1), 
    colsample_bytree = c(.8, .9, 1),
    optimal_trees = 0,               # a place to dump results
    min_RMSE = 0                     # a place to dump results
)


nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) 

hyper_grid %>%
    dplyr::arrange(min_RMSE) %>%
    head(10)

# parameter list
params <- list(
    eta = 0.3,
    max_depth = 5,
    min_child_weight = 3,
    subsample = 0.65,
    colsample_bytree = 0.8
)

# train final model
xgb.fit.final <- xgboost(
    params = params,
    data = features_train,
    label = response_train,
    nrounds = 10,
    objective = "reg:linear",
    verbose = 0
)

# create importance matrix
importance_matrix <- xgb.importance(model = xgb.fit.final)

# variable importance plot
xgb.plot.importance(importance_matrix, top_n = 10, measure = "Gain")

par(mar = c(5, 8, 1, 1))
summary(
    importance_matrix, 
    cBars = 10,
    method = relative.influence, # also can use permutation.test.gbm
    las = 2
)

predxgb_1 <- predict(xgb.fit.final, features_test)

# results
rmsexgb_1 <- caret::RMSE(predxgb_1, response_test)
rmsexgb_1

actualtotchgxg_1 <- TOTCHGXGBtest_1  %>% select(TOTCHG)
predgbmtgxg_1 <- as.data.frame(predxgb_1)

actualpredtcxg_1 <- cbind(actualtotchgxg_1,predxgb_1)

write.csv(actualpredtcxg_1, "actualpredtcxg_3.csv")


actualpredtcxg_1$Bet10 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.9) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.1) , 1,0)
actualpredtcxg_1$Bet20 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.8) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.2) , 1,0)
actualpredtcxg_1$Bet30 <- ifelse(actualpredtcxg_1$pred >= (actualpredtcxg_1$TOTCHG*0.7) & actualpredtcxg_1$pred <= (actualpredtcxg_1$TOTCHG*1.3) , 1,0)

PM10xg_1 = sum(actualpredtcxg_1$Bet10)/nrow(actualpredtcxg_1)
PM20xg_1 = sum(actualpredtcxg_1$Bet20)/nrow(actualpredtcxg_1)
PM30xg_1 = sum(actualpredtcxg_1$Bet30)/nrow(actualpredtcxg_1)
PM10xg_1
PM20xg_1
PM30xg_1


vres <- data.frame (CoreTest$LOS, LOSpred, residuals = CoreTest$LOS - LOSpred)
summary(vres)




mean(predicted.classes == CoreTest$DIED)

CoreTest$Probability <- predicted.classes
table(pred=CoreTest$Probability,true=CoreTest$DIED)


model <- glm(DIED ~ AGE + FEMALE + F70F79c +  Open + EP + I10_NDX + I10_NPR ,
             data = CoreTrain, family = binomial)


model <- glm(DIED ~ AGE + FEMALE + F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
             + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c + SMI_Total + Open + EP + Both,
             data = stratified_train, family = binomial)

model <- glm(DIED ~ F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
             + F50F59c + F60F69c + F70F79c + F80F89c + F90F98c + F99c ,
             data = stratified_train, family = binomial)

model <- glm(DIED ~ AGE + FEMALE + F70F79c +  Open + EP + I10_NDX + I10_NPR ,
             data = stratified_train, family = binomial)





summary(Core$LOS)

# Classification Tree with rpart
library(rpart)
RSS <- c(crossprod(fit$residuals))

transform(Core, DIED = as.factor(DIED))

# grow tree
fit <- rpart(LOS ~ AGE + FEMALE + F01F09c  + F40F49c 
              + F90F98c + F99c + SMI_Total + Open + EP ,
             method="anova", data=CoreTrain,control =rpart.control(minsplit =10,minbucket=4,maxdepth = 20, cp=0))

fit <- rpart(DIED ~ AGE + FEMALE + I10_NDX + RACE + I10_NPR,
             method="anova", data=train,control =rpart.control(minsplit =4,minbucket=4, cp=0))

printcp(fit) # display the results
plotcp(fit) # visualize cross-validation results
summary(fit) # detailed summary of splits

table(pred=rpart_predict,true=test$Class)

rpart_predict <- predict(fit,CoreTest,type="anova")
rpart_predict <- predict(fit,CoreTest,type="prob")
mean(rpart_predict==test$LOS)

pred <- predict(fit, newdata = CoreTest)
RMSE(pred = pred, obs = CoreTest$LOS)

bestcp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
tree.pruned <- prune(fit, cp = bestcp)



#confusion matrix
table(pred=rpart_predict,true=CoreTest$LOS)
# plot tree
plot(fit, uniform=TRUE,
     main="Classification Tree for Death of Patients Undergoing Lobectomys")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree
plot(pfit, uniform=TRUE,
     main="Pruned Classification Tree for Kyphosis")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
post(pfit, file = "c:/ptree.ps",
     title = "Pruned Classification Tree for Kyphosis")


# create attractive postscript plot of tree
post(fit, file = "c:/Documents",
     title = "Classification Tree for Death of Patients Undergoing Lobectomys")

model <- glm(DIED ~  F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
             + F50F59c + F60F69c + F70F79c + F80F89c + + F90F98c + F99c,
             data = CoreTrain, family = binomial)
summary(model)

model <- glm(DIED ~ AGE + FEMALE + F70F79c +  Open + EP + I10_NDX + I10_NPR,
             data = CoreTrain, family = binomial)


model <- glm(DIED ~ .,
             data = CoreTrain, family = binomial)

probabilities <- model %>% predict(CoreTest, type = "response")

summary(probabilities)

predicted.classes35 <- ifelse(probabilities > 0.035, "1", "0")

p35 = sum(predicted.classes35)/nrow(predicted.classes35)
p35

mean(predicted.classes == CoreTest$DIED)

CoreTest$Probability <- predicted.classes
table(pred=CoreTest$Probability,true=CoreTest$DIED)

write.csv(CoreTest, "CoreTest.csv")

CoreTest1 = as.data.frame(CoreTest)

library(readr)
Core <- read_csv("Core_revised1.csv")

library(caret)
set.seed(123)
train <- createDataPartition(Core$KEY_NIS, p = .7, 
                             list = FALSE, 
                             times = 1)
CoreTrain <- Core[ train,]
CoreTest  <- Core[-train,]

Core <- subset(Core, DIED>=0)
Core <- subset(Core, FEMALE >=0)
Core <- subset(Core, RACE >=0)
Core <- subset(Core, ZIPINC_QRTL >=0)
Core <- subset(Core, PAY1 >=0)
Core <- subset(Core, HOSP_LOCTEACH >=0)

Core <- subset(Core, LOS<=50)
summary(Core$HOSP_LOCTEACH)

linearMod <- lm(LOS ~  AGE + FEMALE + F01F09c + F40F49c  + SMI_Total + I10_NDX + I10_NPR, data=CoreTest)

linearMod <- lm(LOS ~  AGE + FEMALE + F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
                + F50F59c + F60F69c + F70F79c + F80F89c + + F90F98c + SMI_Total + Open + EP , data=CoreTest)  
summary(linearMod)
LOSpred <- predict(linearMod, CoreTest)
vres <- data.frame (CoreTest$LOS, LOSpred, residuals = CoreTest$LOS - LOSpred)
summary(vres)
mean()



plot(vres$CoreTest.LOS, vres$LOSpred, xlab = "Predictor variable", ylab = "Predicted variable")


actuals_preds <- data.frame(cbind(actuals=CoreTest$LOS, predicteds=LOSpred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape



linearMod <- lm(LOS ~ AGE  + I10_NDX + I10_NPR  , data=CoreTest)  
summary(linearMod)
LOSpred <- predict.lm(linearMod, CoreTest)

actuals_preds <- data.frame(cbind(actuals=CoreTest$TOTCHG, predicteds=LOSpred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds) 
correlation_accuracy

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)

set.seed(123)
fit <- bagging(LOS ~ AGE + FEMALE + F01F09c  + F40F49c 
             + F90F98c + F99c + SMI_Total + Open + EP ,
             method="anova", data=CoreTrain,coob = TRUE)

fit

ntree <- 10:50

# create empty vector to store OOB RMSE values
rmse <- vector(mode = "numeric", length = length(ntree))

for (i in seq_along(ntree)) {
    # reproducibility
    set.seed(123)
    
    # perform bagged model
    model <- bagging(
        formula = LOS ~ AGE + FEMALE + F01F09c + F10F19c + F20F29c + F30F39c + F40F49c 
        + F50F59c + F60F69c + F70F79c + F80F89c + + F90F98c + F99c + SMI_Total + Open + EP + Both,
        data    = CoreTrain,
        coob    = TRUE,
        nbagg   = ntree[i]
    )
    # get OOB error
    rmse[i] <- model$err
}

plot(ntree, rmse, type = 'l', lwd = 2)
abline(v = 25, col = "red", lty = "dashed")

library(readr)
library(randomForest)
library(dplyr)
library(tree)
library(rpart)

allrows <- 1:nrow(Core)
set.seed(123)
trainrows <- sample(allrows, replace = F, size = 0.7*length(allrows))
test_cvrows <- allrows[-trainrows]
testrows <- sample(test_cvrows, replace=F, size = 0.5*length(test_cvrows))
cvrows <- test_cvrows[-which(test_cvrows %in% testrows)]

train <- Core[trainrows,]
test <- Core[testrows,]
cvr <- Core[cvrows,]

Core$DIED <- as.character(Core$DIED)
Core$DIED <- as.factor(Core$DIED)
train <- Core[trainrows,]
test <- Core[testrows,]
cvr <- Core[cvrows,]

CoreTrain$DIED <- as.numeric(CoreTrain$DIED)
CoreTest$DIED <- as.numeric(CoreTest$DIED)

train_live <- select(filter(train, train$DIED == 0), everything())
train_died <- select(filter(train, train$DIED == 1), everything())
set.seed(123)
train_rows <- 1:nrow(train_live)
live_subset_indexes <- sample(train_rows, replace = F, size = nrow(train_died))
live_subset <- train_live[live_subset_indexes,]
stratified <- rbind(live_subset, train_died)



stratified_fit <- randomForest(DIED ~ AGE + FEMALE + I10_NDX + RACE + I10_NPR, data=stratified, ntree = 1)
print(stratified_fit)
```
