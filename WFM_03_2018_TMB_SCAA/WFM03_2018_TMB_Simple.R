# Lake Michigan 03 Stock Assessment in TMB
# Emily Morgan Liljestrand
# Created: 4/17/19
# Updated: 11/1/22

# Remove Old Objects and set working directory to file location:
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############################################################################
# Load Necessary Libraries
library(TMB)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)
library(boot)
library(colorspace)
library(PerformanceAnalytics)

# Compile cpp file, which makes a "dynamic load library" (.dll) file
compile("WFM03_2018_TMB_MN.cpp")
# Load the .dll file. The unload function is provided but commented out
dyn.load("WFM03_2018_TMB_MN")
# dyn.unload("WFM03_2018_TMB_MN")

############################################################################
# DATA SECTION
# Specify the data and make a list that'll be read in by the cpp file
############################################################################

fyear <- 1986
ldyear <- 2017
lyear <- 2017
fage <- 4
lage <- 15
targ_age <- 4
totlmort <- 3
fishmort <- 2
# Slightly different from the ADMB version, but useful for indexing things
years <- c(fyear:ldyear)
ryears <- c(fyear:lyear)
ages <- c(fage:lage)

rhoSR <- 15.0
rhoCG <- 1.5
rhoCT <- 1.5
rhoEG <- 4.0
rhoET <- 4.0
rhosel <- 1.0
sp_time <- 0.838
harv_time <-0.5
in_watage <-matrix(data=c(0.594,0.829,1.049,1.173,1.235,1.271,1.292,1.3,1.307,1.307,1.307,1.307,1.307,
                          0.443,0.834,1.082,1.22,1.289,1.33,1.354,1.363,1.363,1.37,1.37,1.37,1.37,
                          0.56,0.742,0.965,1.19,1.261,1.297,1.312,1.327,1.327,1.335,1.335,1.335,1.335,
                          0.713,0.846,0.983,1.091,1.276,1.317,1.332,1.346,1.354,1.354,1.354,1.354,1.354,
                          0.75,0.978,1.202,1.37,1.363,1.691,1.719,1.738,1.747,1.757,1.757,1.757,1.757,
                          0.588,0.942,1.111,1.221,1.308,1.323,1.423,1.438,1.446,1.446,1.446,1.454,1.454,
                          0.65,0.869,1.14,1.424,1.63,1.746,1.63,1.9,1.921,1.921,1.921,1.932,1.932,
                          0.665,0.827,0.969,1.103,1.27,1.359,1.409,1.373,1.46,1.46,1.467,1.467,1.467,
                          0.717,0.819,0.997,1.1,1.196,1.319,1.372,1.403,1.396,1.42,1.42,1.42,1.42,
                          0.641,0.835,0.966,1.149,1.225,1.275,1.379,1.418,1.434,1.425,1.45,1.45,1.45,
                          0.658,0.791,0.951,1.102,1.302,1.365,1.358,1.474,1.519,1.534,1.496,1.557,1.557,
                          0.523,0.74,0.862,0.999,1.13,1.258,1.303,1.303,1.349,1.365,1.381,1.389,1.372,
                          0.373,0.684,0.885,1.009,1.112,1.27,1.451,1.488,1.427,1.535,1.598,1.624,1.551,
                          0.248,0.501,0.818,1.033,1.181,1.273,1.506,1.787,1.848,1.689,1.889,2.027,2.082,
                          0.249,0.399,0.633,0.891,1.057,1.163,1.222,1.354,1.446,1.488,1.42,1.446,1.506,
                          0.257,0.406,0.536,0.708,0.884,0.997,1.058,1.089,1.14,1.154,1.18,1.147,1.135,
                          0.312,0.448,0.658,0.817,0.983,1.15,1.272,1.353,1.362,1.5,1.638,1.696,1.509,
                          0.232,0.443,0.616,0.819,0.949,1.054,1.161,1.254,1.336,1.32,1.479,1.65,1.703,
                          0.235,0.348,0.562,0.724,0.891,0.988,1.071,1.153,1.21,1.269,1.254,1.338,1.377,
                          0.155,0.359,0.487,0.704,0.874,1.022,1.099,1.146,1.201,1.252,1.303,1.288,1.371,
                          0.17,0.276,0.496,0.636,0.848,1.027,1.155,1.212,1.249,1.286,1.333,1.38,1.364,
                          0.223,0.303,0.407,0.598,0.708,0.864,0.984,1.063,1.102,1.128,1.147,1.161,1.174,
                          0.265,0.384,0.46,0.559,0.717,0.823,0.955,1.078,1.14,1.165,1.178,1.178,1.184,
                          0.34,0.458,0.594,0.666,0.765,0.893,1.001,1.125,1.272,1.34,1.354,1.327,1.299,
                          0.354,0.513,0.648,0.786,0.856,0.95,1.058,1.181,1.314,1.501,1.569,1.579,1.527,
                          0.434,0.548,0.663,0.751,0.843,0.883,0.952,1.02,1.102,1.182,1.281,1.314,1.328,
                          0.327,0.658,0.793,0.891,0.976,1.05,1.079,1.133,1.182,1.252,1.325,1.436,1.465,
                          0.427,0.535,0.81,0.929,1.005,1.067,1.131,1.15,1.191,1.231,1.295,1.361,1.445,
                          0.357,0.588,0.673,0.845,0.921,0.972,1.015,1.057,1.076,1.108,1.14,1.18,1.222,
                          0.4,0.626,0.812,0.926,1.052,1.137,1.174,1.212,1.243,1.251,1.282,1.299,1.348,
                          0.557,0.777,0.859,0.959,1.048,1.117,1.176,1.19,1.216,1.237,1.237,1.257,1.264,
                          0.535,0.73,0.884,0.908,0.962,1.024,1.069,1.11,1.123,1.143,1.164,1.171,1.185),
                   nrow=(lyear-fyear+1),ncol=(lage-fage+2),byrow=T)
in_latage <-matrix(c(449,487,506,515,520,523,524,525,525,525,525,525,
                     445,484,503,512,517,520,521,521,522,522,522,522,
                     438,479,514,524,529,531,533,533,534,534,534,534,
                     459,484,502,530,536,538,540,541,541,541,541,541,
                     484,517,539,538,576,579,581,582,583,583,583,583,
                     467,494,510,522,524,537,539,540,540,540,541,541,
                     457,498,534,557,569,557,584,586,586,586,587,587,
                     459,486,509,535,548,555,550,562,562,563,563,563,
                     453,484,500,514,531,538,542,541,544,544,544,544,
                     454,477,506,517,524,538,543,545,544,547,547,547,
                     442,473,499,530,539,538,554,560,562,557,565,565,
                     436,459,482,502,520,526,526,532,534,536,537,535,
                     420,461,483,500,524,549,554,546,560,568,571,562,
                     382,449,484,505,517,545,575,581,565,585,598,603,
                     352,412,461,487,502,510,527,538,543,535,538,545,
                     353,391,432,467,487,497,502,510,512,516,511,509,
                     362,412,442,469,493,509,519,520,536,551,557,537,
                     370,415,457,480,497,513,526,537,535,555,575,581,
                     340,402,438,469,485,498,510,518,526,524,535,540,
                     344,384,436,469,494,506,513,521,528,535,533,544,
                     311,385,419,461,491,510,518,523,528,534,540,538,
                     317,355,408,433,464,485,498,504,508,511,513,515,
                     348,373,401,439,461,486,507,517,521,523,523,524,
                     371,409,427,449,475,495,516,539,549,551,547,543,
                     394,426,454,467,483,500,518,536,559,567,568,562,
                     398,427,447,466,474,487,499,513,526,541,546,548,
                     419,449,469,485,498,503,512,520,531,542,558,562,
                     390,452,474,487,497,507,510,516,522,531,540,551,
                     407,427,462,476,485,492,499,502,507,512,518,524,
                     414,450,469,488,500,505,510,514,515,519,521,527,
                     444,460,478,493,504,513,515,519,522,522,525,526,
                     438,467,471,480,490,497,503,505,508,511,512,514),
                   nrow=(lyear-fyear+1),ncol=(lage-fage+1),byrow=T)
in_mat <- matrix(c(0.792,0.920,0.943,0.962,0.993,0.993,1.000,1.000,1,1,1,1,
                   0.792,0.920,0.943,0.962,0.993,0.993,1.000,1.000,1,1,1,1,
                   0.792,0.920,0.943,0.962,0.993,0.993,1.000,1.000,1,1,1,1,
                   0.792,0.920,0.943,0.962,0.993,0.993,1.000,1.000,1,1,1,1,
                   0.792,0.920,0.943,0.962,0.993,0.993,1.000,1.000,1,1,1,1,
                   0.807,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.807,1.000,1.000,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.786,0.993,1.000,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.787,0.976,0.990,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.782,0.971,0.992,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.808,0.959,0.992,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.808,0.959,0.992,1.000,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.692,0.937,0.990,0.992,1.000,1.000,1.000,1.000,1,1,1,1,
                   0.635,0.921,0.991,0.990,0.992,1.000,1.000,1.000,1,1,1,1,
                   0.512,0.822,0.949,0.991,0.990,0.992,1.000,1.000,1,1,1,1,
                   0.423,0.731,0.886,0.949,0.991,0.990,1.000,1.000,1,1,1,1,
                   0.269,0.626,0.848,0.920,0.958,0.991,1.000,1.000,1,1,1,1,
                   0.325,0.574,0.803,0.912,0.991,0.958,0.991,1.000,1,1,1,1,
                   0.239,0.489,0.776,0.911,0.990,0.991,0.988,1.000,1,1,1,1,
                   0.262,0.502,0.762,0.924,0.988,0.985,0.991,1.000,1,1,1,1,
                   0.247,0.523,0.756,0.936,0.981,0.981,0.991,0.995,1,1,1,1,
                   0.207,0.469,0.682,0.885,0.947,0.981,0.986,0.995,1,1,1,1,
                   0.125,0.409,0.649,0.867,0.930,0.969,0.995,0.995,1,1,1,1,
                   0.149,0.430,0.644,0.852,0.927,0.972,0.995,0.995,1,1,1,1,
                   0.159,0.416,0.644,0.826,0.912,0.972,0.995,0.995,1,1,1,1,
                   0.196,0.384,0.640,0.810,0.900,0.958,0.972,0.995,1,1,1,1,
                   0.214,0.418,0.688,0.830,0.924,0.961,0.958,0.978,1,1,1,1,
                   0.240,0.482,0.713,0.843,0.936,0.971,0.961,0.978,1,1,1,1,
                   0.251,0.467,0.670,0.825,0.931,0.955,0.971,0.978,1,1,1,1,
                   0.254,0.457,0.665,0.812,0.937,0.946,0.955,0.978,1,1,1,1,
                   0.213,0.462,0.673,0.797,0.944,0.956,1.000,0.993,1,1,1,1,
                   0.350,0.496,0.681,0.807,0.951,0.955,0.984,1.000,1,1,1,1),
                 nrow=(lyear-fyear+1),ncol=(lage-fage+1),byrow=T)
H2O_T <- 6.0
Linf <- 51.9
vb_K <- 0.44
lnmedM <- -1.60944
sdM <- 0.1
surv_num <- 0
in_obs_PAT <- matrix(c(0.2762,0.5058,0.1831,0.0233,0.0058,0.0058,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0289,0.2851,0.3306,0.2479,0.0785,0.0207,0.0083,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0148,0.2414,0.4631,0.2069,0.0493,0.0148,0.0049,0.0049,0.0000,0.0000,0.0000,0.0000,
                       0.2021,0.2583,0.3250,0.1500,0.0417,0.0146,0.0063,0.0021,0.0000,0.0000,0.0000,0.0000,
                       0.7180,0.2035,0.0349,0.0174,0.0174,0.0087,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.4355,0.3618,0.1106,0.0507,0.0184,0.0161,0.0023,0.0023,0.0023,0.0000,0.0000,0.0000,
                       0.4271,0.3555,0.1611,0.0358,0.0128,0.0026,0.0000,0.0026,0.0026,0.0000,0.0000,0.0000,
                       0.2670,0.5624,0.1247,0.0328,0.0022,0.0066,0.0000,0.0044,0.0000,0.0000,0.0000,0.0000,
                       0.2516,0.4239,0.2252,0.0745,0.0202,0.0031,0.0000,0.0000,0.0016,0.0000,0.0000,0.0000,
                       0.3689,0.2882,0.2046,0.1066,0.0231,0.0058,0.0029,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.1403,0.5156,0.2326,0.0803,0.0264,0.0024,0.0012,0.0012,0.0000,0.0000,0.0000,0.0000,
                       0.0787,0.3370,0.4557,0.0887,0.0310,0.0067,0.0011,0.0000,0.0000,0.0011,0.0000,0.0000,
                       0.0142,0.2251,0.3858,0.3191,0.0426,0.0098,0.0033,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0097,0.1667,0.3722,0.3269,0.1052,0.0178,0.0016,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0019,0.0328,0.2254,0.3699,0.2697,0.0771,0.0193,0.0039,0.0000,0.0000,0.0000,0.0000,
                       0.0011,0.0435,0.2329,0.3439,0.2612,0.0968,0.0131,0.0076,0.0000,0.0000,0.0000,0.0000,
                       0.0016,0.0262,0.1491,0.3307,0.2538,0.1570,0.0658,0.0111,0.0032,0.0016,0.0000,0.0000,
                       0.0000,0.0423,0.1493,0.3465,0.3099,0.1155,0.0282,0.0085,0.0000,0.0000,0.0000,0.0000,
                       0.0015,0.0176,0.0809,0.2206,0.2588,0.2221,0.1338,0.0441,0.0176,0.0029,0.0000,0.0000,
                       0.0000,0.0216,0.0747,0.1827,0.2279,0.2338,0.1650,0.0747,0.0118,0.0059,0.0020,0.0000,
                       0.0060,0.1057,0.1086,0.1131,0.2113,0.1964,0.1399,0.0595,0.0372,0.0104,0.0089,0.0030,
                       0.0000,0.0000,0.0930,0.0698,0.2442,0.2791,0.1628,0.0698,0.0349,0.0116,0.0233,0.0116,
                       0.0000,0.0216,0.1169,0.3420,0.2684,0.1385,0.0779,0.0303,0.0043,0.0000,0.0000,0.0000,
                       0.0030,0.0122,0.1067,0.2683,0.2896,0.1829,0.0640,0.0457,0.0183,0.0000,0.0061,0.0030,
                       0.0083,0.0248,0.1488,0.2397,0.3223,0.1818,0.0579,0.0165,0.0000,0.0000,0.0000,0.0000,
                       0.0000,0.0218,0.0568,0.1485,0.2445,0.2052,0.1354,0.1048,0.0524,0.0218,0.0087,0.0000,
                       0.0000,0.0285,0.1026,0.1709,0.2450,0.2251,0.1282,0.0655,0.0313,0.0028,0.0000,0.0000,
                       0.0019,0.0490,0.1601,0.1808,0.2222,0.1299,0.0885,0.0734,0.0471,0.0056,0.0207,0.0207,
                       0.0556,0.1595,0.2401,0.2330,0.1272,0.0968,0.0251,0.0125,0.0125,0.0108,0.0072,0.0197,
                       0.0244,0.1067,0.1778,0.2044,0.1600,0.1067,0.0711,0.0556,0.0289,0.0089,0.0200,0.0356,
                       0.1567,0.2074,0.0922,0.1152,0.0691,0.0599,0.0783,0.0737,0.0645,0.0323,0.0138,0.0369,
                       0.0114,0.0857,0.1971,0.1800,0.1857,0.1571,0.0771,0.0486,0.0143,0.0171,0.0086,0.0171),
                     nrow=(lyear-fyear+1),ncol=(lage-fage+1),byrow=T)
in_NtildeT <- c(344,242,203,480,344,434,391,457,644,347,834,902,915,618,519,919,
                1261,355,680,509,672,86,231,328,121,229,351,527,558,450,217,350)
in_N_SampT <- c(40,28,24,56,40,51,46,53,75,40,97,105,107,72,60,107,147,
                41,79,59,78,10,27,38,14,27,41,61,65,52,25,41)
in_harv_wgtT <- c(118065,174693,201989,189454,213174,206636,274034,294428,
                  210446,207172,277517,312879,316565,166873,279848,307084,
                  307347,271060,228608,233297,235096,188957,227311,427777,
                  241548,287277,229570,158778,124146,69404,71789,49150)
in_mnwgtT <- c(1.002,1.129,1.206,1.100,1.078,1.100,0.965,0.965,
               0.992,1.007,0.946,0.947,0.953,0.999,0.978,0.898,
               1.117,1.087,0.942,1.006,1.037,1.032,0.812,0.916,
               0.819,0.834,0.830,0.885,0.899,0.938,0.991,0.965)
in_effortT <- c(10.57,10.90,11.75,19.37,17.03,8.17,12.44,14.24,
                9.84,10.03,13.73,16.82,21.39,5.65,15.51,18.99,
                24.47,14.12,9.08,10.40,8.83,8.02,13.10,24.06,
                20.90,27.00,22.42,22.52,18.64,13.66,17.55,13.10)
maxNT <- 50
W_ageT <- c(0.783,0.826,0.852,0.892,0.924,0.967,1.000,1.046,1.103,1.120,1.194,1.278)
in_obs_PAG <- matrix(c(0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0380,0.3671,0.5190,0.0759,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0580,0.3333,0.4928,0.1159,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.2807,0.4678,0.1637,0.0468,0.0117,0.0175,0.0058,0.0058,0.0000,0.0000,0.0000,0.0000,
                       0.1256,0.4623,0.2814,0.0603,0.0201,0.0352,0.0050,0.0101,0.0000,0.0000,0.0000,0.0000,
                       0.1992,0.5089,0.1716,0.0769,0.0256,0.0079,0.0020,0.0059,0.0000,0.0020,0.0000,0.0000,
                       0.0962,0.4017,0.3473,0.1339,0.0126,0.0000,0.0042,0.0042,0.0000,0.0000,0.0000,0.0000,
                       0.0867,0.2449,0.3724,0.2500,0.0357,0.0102,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0962,0.5357,0.2115,0.1181,0.0357,0.0027,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,
                       0.0860,0.2022,0.5621,0.1051,0.0334,0.0064,0.0016,0.0016,0.0000,0.0016,0.0000,0.0000,
                       0.0129,0.1404,0.2923,0.4083,0.0974,0.0372,0.0072,0.0014,0.0014,0.0014,0.0000,0.0000,
                       0.0038,0.1189,0.3717,0.3170,0.1264,0.0453,0.0132,0.0019,0.0000,0.0000,0.0000,0.0019,
                       0.0000,0.0065,0.1042,0.4007,0.3550,0.0977,0.0293,0.0065,0.0000,0.0000,0.0000,0.0000,
                       0.0040,0.0361,0.1606,0.3133,0.3092,0.1285,0.0321,0.0080,0.0080,0.0000,0.0000,0.0000,
                       0.0000,0.0078,0.0959,0.2254,0.3342,0.1813,0.0959,0.0415,0.0104,0.0052,0.0026,0.0000,
                       0.0000,0.0118,0.0433,0.1890,0.2323,0.2795,0.1614,0.0669,0.0118,0.0039,0.0000,0.0000,
                       0.0000,0.0119,0.0893,0.1488,0.2857,0.2202,0.1369,0.0774,0.0238,0.0060,0.0000,0.0000,
                       0.0000,0.0053,0.0246,0.1336,0.2091,0.2566,0.2004,0.1002,0.0457,0.0141,0.0088,0.0000,
                       0.0000,0.0000,0.0436,0.0981,0.2044,0.2289,0.1989,0.1499,0.0490,0.0054,0.0191,0.0027,
                       0.0000,0.0065,0.0163,0.0784,0.1275,0.2614,0.2190,0.1601,0.0850,0.0229,0.0033,0.0098,
                       0.0000,0.0000,0.0110,0.0934,0.2198,0.2912,0.2088,0.1264,0.0110,0.0220,0.0165,0.0000,
                       0.0000,0.0126,0.0628,0.1841,0.2259,0.2971,0.1255,0.0628,0.0251,0.0000,0.0000,0.0000,
                       0.0000,0.0118,0.0533,0.1006,0.1006,0.2604,0.2604,0.1006,0.0592,0.0237,0.0059,0.0178,
                       0.0033,0.0198,0.0627,0.1221,0.1419,0.2343,0.1980,0.0924,0.0759,0.0297,0.0132,0.0066,
                       0.0000,0.0545,0.1550,0.1816,0.1913,0.1662,0.1159,0.0726,0.0377,0.0154,0.0042,0.0042,
                       0.0000,0.0263,0.0931,0.1842,0.2267,0.1862,0.1134,0.0789,0.0385,0.0162,0.0121,0.0000,
                       0.0000,0.0199,0.0697,0.0896,0.2090,0.2289,0.1343,0.1095,0.0647,0.0348,0.0199,0.0100,
                       0.0240,0.0743,0.1247,0.1607,0.1463,0.1511,0.0959,0.0791,0.0600,0.0336,0.0216,0.0096,
                       0.0044,0.0619,0.1681,0.1416,0.1106,0.1195,0.0885,0.0973,0.0708,0.0841,0.0310,0.0088,
                       0.0000,0.0270,0.1149,0.1824,0.1351,0.1453,0.1182,0.0878,0.0946,0.0203,0.0236,0.0203),
                     nrow=(lyear-fyear+1),ncol=(lage-fage+1),byrow=T)
in_NtildeG <- c(0,79,69,0,0,171,199,507,239,196,364,628,
                698,530,307,249,386,254,168,569,367,306,
                182,239,169,303,716,494,201,417,226,296)
in_N_SampG <- c(0,5,4,0,0,10,12,30,14,11,21,37,41,31,18,
                15,23,15,10,33,21,18,11,14,10,18,42,29,12,24,13,17)
in_harv_wgtG <- c(132548,212341,284612,289297,218073,318960,
                  583241,501073,279138,309276,232433,185046,
                  130869,138446,94713,74821,38729,27520,
                  23864,12816,27494,10222,42345,58598,
                  132603,131388,207987,101251,128992,
                  42136,70174,48190)
in_mnwgtG <- c(1.329,1.329,1.616,1.374,1.374,1.132,
               1.303,1.113,1.160,1.114,1.099,1.117,
               1.117,1.183,1.158,0.892,1.172,1.083,
               1.107,1.021,1.163,1.109,1.120,0.987,
               1.164,0.973,1.068,1.107,1.062,1.108,
               1.108,1.128)
in_effortG <- c(2.348,4.048,5.144,8.188,7.286,7.841,
                13.467,12.943,9.718,7.580,7.651,5.741,
                3.748,4.016,3.270,1.877,0.870,0.894,
                0.575,0.311,0.510,0.366,1.090,1.851,
                3.375,4.000,5.977,4.926,5.388,3.429,
                3.197,4.576)
maxNG <- 50
W_ageG <- c(0.838,0.902,0.994,1.053,1.098,1.119,1.119,1.220,1.182,1.266,1.299,1.188)
in_effort_adjust <- c(1.008,1.008,1.008,1.000,1.000,1.000,
                      1.000,1.000,1.000,1.024,1.049,1.049,
                      1.081,1.057,1.057,1.122,1.130,1.122,
                      1.106,1.057,1.073,1.073,1.073,1.073,
                      1.073,1.073,1.073,1.073,1.073,1.073,
                      1.073,1.073)
in_Tharv_adjust <- c(0.90,0.90,0.90,0.90,0.90,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94)
in_Gharv_adjust <- c(0.90,0.90,0.90,0.90,0.90,0.90,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94,0.94,0.94,0.94,0.94,
                     0.94,0.94)
percent_female <- 0.4845
eggs_per_kg <- 19937
reflenG <- 505
reflenT <- 532
targ_ptn <- 0.630
targ_A <- 0.65
targ_SPR <- 0.2
data<-list(fyear=fyear,ldyear=ldyear,lyear=lyear,
           fage=fage,lage=lage,targ_age=targ_age,
           totlmort=totlmort,fishmort=fishmort,
           years=years,ryears=ryears,ages=ages,
           rhoSR=rhoSR,rhoCG=rhoCG,rhoCT=rhoCT,rhoEG=rhoEG,rhoET=rhoET,rhosel=rhosel,
           sp_time=sp_time,harv_time=harv_time,
           in_watage=as.matrix(in_watage),
           in_latage=as.matrix(in_latage),
           in_mat=as.matrix(in_mat),
           H2O_T=H2O_T,Linf=Linf,vb_K=vb_K,
           lnmedM=lnmedM,sdM=sdM,surv_num=surv_num,
           in_obs_PAT=as.matrix(in_obs_PAT),in_NtildeT=in_NtildeT,in_N_SampT=in_N_SampT,
           in_harv_wgtT=in_harv_wgtT,in_mnwgtT=in_mnwgtT,in_effortT=in_effortT,
           maxNT=maxNT,W_ageT=W_ageT,
           in_obs_PAG=as.matrix(in_obs_PAG),in_NtildeG=in_NtildeG,in_N_SampG=in_N_SampG,
           in_harv_wgtG=in_harv_wgtG,in_mnwgtG=in_mnwgtG,in_effortG=in_effortG,
           maxNG=maxNG,W_ageG=W_ageG,
           in_effort_adjust=in_effort_adjust,
           in_Tharv_adjust=in_Tharv_adjust,in_Gharv_adjust=in_Gharv_adjust,
           percent_female=percent_female,eggs_per_kg=eggs_per_kg,
           reflenG=reflenG,reflenT=reflenT,
           targ_ptn=targ_ptn,targ_A=targ_A,targ_SPR=targ_SPR)

############################################################################
# PARAMETER SECTION
# Specify the parameters and make a list that'll be read in by the cpp file
############################################################################

# log_sig <- -3.11209870007
# lnM <- -1.693847
# log_qT <- -3.81198895562
# log_qG <- -2.28728683332
# logselG_p1 <- -2.48479641276
# logselG_p2 <- 1.83895901637
# logselT_p1 <- 6.16411908029
# logselT_p2 <- -2.81346704020
# logdevT_p1 <-  c(0.0159009802701,-0.0149602501516,-0.00878552840958,0.0154885420221,-0.0322714349097,
#                  0.00681632762740,0.00738500924209,-0.00668506486120,0.0249410090274,0.00338573081808,
#                  -0.00385407242652,0.0253999864813,-0.0422559638563,0.00610259925609,-0.0564475914384,
#                  0.0550671892507,-0.00111897895265,-0.0304223271797,0.00559305194301,-0.0528546975672,
#                  -0.00519739225798,0.000136078703803,0.0353068446421,0.0466160254852,-0.00541393833336,
#                  0.0184999204937,-0.00188823873992,-0.0827789231715,0.0525095590165,0.0284334691286,
#                  0.00332581048330)
# # The parameters for recruitment are also slightly different because you cannot constrain a vector to
# # add up to 0 in TMB like you can in ADMB
# log_R0 <- 13.5108817
# log_recdev <- c(-0.48740367,-1.07270825,-0.84608929,0.10464284,0.33547214,
#                  0.48857833,0.13406690,-0.05518238,0.73974689,0.56444995,
#                  0.52432938,0.44845101,0.45766934,0.35070274,0.12958624,
#                  0.05436630,-0.12803229,0.39475109,0.69776256,0.52331311,
#                  0.31805699,0.12476211,-0.01866468,-0.27474613,-0.45306412,
#                  -0.57677033,-0.59414899,-0.66013421,-0.40804346,-0.85852214,
#                  -0.79989065,-0.37432545,-1.29364694,-2.76303567,-4.10179583,
#                  -4.69455386)
# lnalpha <- -8.17085012085
# lnbeta <- -22.8729436736
# effort_devsT <-  c(0.216923714229,0.0445830509089,-0.174312009244,-0.147120206174,0.261118980349,
#                    0.0282324673642,0.0821334645900,0.102478857553,-0.0203905484756,-0.0337718874173,
#                    -0.0458843682151,-0.0737326172145,0.0167260569355,-0.116790645281,-0.142952007237,
#                    -0.157075661671,0.0871917294481,0.310675321801,-0.0819263643693,-0.0461054282701,
#                    -0.0474850405616,-0.219308718093,-0.140076968377,-0.142796177187,0.0828692905367,
#                    -0.0480488247183,-0.238910792275,-0.218579948892,-0.160135285478,-0.220610735894,
#                    0.0525444126708)
# effort_devsG <-  c(-0.167941243914,-0.0592295532093,0.00228882337898,-0.511246202710,0.345488090944,
#                    -0.122622890528,0.115679603474,-0.105582833837,0.0702911200045,-0.307611373426,
#                    0.0803945374864,-0.143806318040,-0.155773938788,0.223600673989,0.779844634509,
#                    -0.368595701144,-0.524028406212,0.373876113991,0.0386813567895,0.138072693075,
#                    -0.0728018414212,0.0581557748107,-0.333393407363,-0.171039551050,0.212344552329,
#                    -0.243144691061,-0.278970442754,0.435754387105,-0.681226687895,0.162119283084,
#                    -0.424039882192)

#Starting parameters are identical to where ADMB started
log_sig <- -3
lnM <- -1.6094
log_qT <- -3.5
log_qG <- -2

logselG_p1 <- -2.3
logselG_p2 <- 1.8
logselT_p1 <- 6
logselT_p2 <- -2.5
logdevT_p1 <- rep(0,31)

log_R0 <- 12
log_recdev <- rep(0,36)
lnalpha <- -9
lnbeta <- -22
effort_devsT <-  rep(0,31)
effort_devsG <-  rep(0,31)
#------------------------------------------------------------

parameters <- list(log_sig=log_sig,lnM=lnM,
                   log_qT=log_qT,log_qG=log_qG,
                   logselG_p1=logselG_p1,logselG_p2=logselG_p2,
                   logselT_p1=logselT_p1,logselT_p2=logselT_p2,
                   logdevT_p1=logdevT_p1,
                   log_R0=log_R0,
                   log_recdev=log_recdev,
                   lnalpha=lnalpha,lnbeta=lnbeta,
                   effort_devsT=effort_devsT,effort_devsG=effort_devsG)

############################################################################
# BOUNDS SECTION
# Set bounds that'll be read into the model
############################################################################

#Upper Bounds
log_sig_upper <- 0
lnM_upper <- 5
log_qT_upper <- 1
log_qG_upper <- 1
logselG_p1_upper <- 2
logselG_p2_upper <- 2
logselT_p1_upper <- 7
logselT_p2_upper <- -1
logdevT_p1_upper <- rep(1,(length(data$years)-1))
log_R0_upper <- 20
log_recdev_upper <- rep(15,((data$lyear-data$fyear+data$lage-data$fage)-6))
lnalpha_upper <- -1
lnbeta_upper <- -10
effort_devsT_upper <- rep(2,(length(data$years)-1))
effort_devsG_upper <- rep(2,(length(data$years)-1))
upper_bounds <- c(log_sig_upper=log_sig_upper,lnM_upper=lnM_upper,
                  log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
                  logselG_p1_upper=logselG_p1_upper,logselG_p2_upper=logselG_p2_upper,
                  logselT_p1_upper=logselT_p1_upper,logselT_p2_upper=logselT_p2_upper,
                  logdevT_p1_upper=logdevT_p1_upper,log_R0_upper=log_R0_upper,
                  log_recdev_upper=log_recdev_upper,
                  lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper,
                  effort_devsT_upper=effort_devsT_upper,effort_devsG_upper=effort_devsG_upper)


#Lower Bounds
log_sig_lower <- -5 #Equivalent of variance = 0.07 for obs error
lnM_lower <- -5
log_qT_lower <- -20
log_qG_lower <- -20
logselG_p1_lower <- -4
logselG_p2_lower <- 1
logselT_p1_lower <- 5
logselT_p2_lower <- -5.5
logdevT_p1_lower <- rep(-1,(length(data$years)-1))
log_R0_lower <- -15
log_recdev_lower <- rep(-15,((data$lyear-data$fyear+data$lage-data$fage)-6))
lnalpha_lower <- -15
lnbeta_lower <- -42
effort_devsT_lower <- rep(-2,(length(data$years)-1))
effort_devsG_lower <- rep(-2,(length(data$years)-1))
lower_bounds <- c(log_sig_lower=log_sig_lower,lnM_lower=lnM_lower,
                  log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
                  logselG_p1_lower=logselG_p1_lower,logselG_p2_lower=logselG_p2_lower,
                  logselT_p1_lower=logselT_p1_lower,logselT_p2_lower=logselT_p2_lower,
                  logdevT_p1_lower=logdevT_p1_lower,log_R0_lower=log_R0_lower,
                  log_recdev_lower=log_recdev_lower,
                  lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower,
                  effort_devsT_lower=effort_devsT_lower,effort_devsG_lower=effort_devsG_lower)

############################################################################
# RANDOM EFFECTS SECTION
# Optional area to specify random effects, those parameters assumed to be
# derived from the same distribution
############################################################################

# reffects <- c("log_recdev","logdevT_p1","effort_devsT","effort_devsG")
# reffects <- c("effort_devsT","effort_devsG")
# reffects <- c("log_recdev","logdevT_p1")
# reffects <- c("log_recdev")
# reffects <- c("logdevT_p1")
# reffects <- c()

############################################################################
# OBJ Section
# Several options for specifying the model object obj
############################################################################

# obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB_MN")
obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB_MN",random=reffects)
# obj$report()$lnmedM

############################################################################
# FIT Section
# Several options for fitting the model (with or without bounds)
############################################################################

#Make the model fit using the specified model object and bounds or no bounds
fit <- nlminb(obj$par, obj$fn, obj$gr)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=lower_bounds,upper=upper_bounds)

############################################################################
# OBJ and FIT Section version 2
# The same as previous two sections, but with phases
############################################################################

# Phase 1
mymap <- list(log_sig=as.factor(NA),
              logselG_p1=as.factor(NA),
              logselG_p2=as.factor(NA),
              logselT_p1=as.factor(NA),
              logselT_p2=as.factor(NA),
              logdevT_p1=rep(as.factor(NA),length(logdevT_p1)),
              log_recdev=rep(as.factor(NA),length(log_recdev)),
              effort_devsT=rep(as.factor(NA),length(effort_devsT)),
              effort_devsG=rep(as.factor(NA),length(effort_devsG)))
obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB_MN", map=mymap)
# upper_bounds <- c(lnM_upper=lnM_upper,
#                   log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
#                   log_R0_upper=log_R0_upper,
#                   lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper)
# lower_bounds <- c(lnM_lower=lnM_lower,
#                   log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
#                   log_R0_lower=log_R0_lower,
#                   lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=lower_bounds,upper=upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)

# Phase 2
parameters <- list(log_sig=log_sig,lnM=obj$report()$lnM,
                   log_qT=obj$report()$log_qT,log_qG=obj$report()$log_qG,
                   logselG_p1=logselG_p1,logselG_p2=logselG_p2,
                   logselT_p1=logselT_p1,logselT_p2=logselT_p2,
                   logdevT_p1=logdevT_p1,
                   log_R0=obj$report()$log_R0,
                   log_recdev=log_recdev,
                   lnalpha=obj$report()$lnalpha,lnbeta=obj$report()$lnbeta,
                   effort_devsT=effort_devsT,effort_devsG=effort_devsG)
mymap <- list(log_sig=as.factor(NA),
              effort_devsT=rep(as.factor(NA),length(effort_devsT)),
              effort_devsG=rep(as.factor(NA),length(effort_devsG)))
obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB_MN", map=mymap)
# upper_bounds <- c(lnM_upper=lnM_upper,
#                   log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
#                   logselG_p1_upper=logselG_p1_upper,logselG_p2_upper=logselG_p2_upper,
#                   logselT_p1_upper=logselT_p1_upper,logselT_p2_upper=logselT_p2_upper,
#                   logdevT_p1_upper=logdevT_p1_upper,log_R0_upper=log_R0_upper,
#                   log_recdev_upper=log_recdev_upper,
#                   lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper)
# lower_bounds <- c(lnM_lower=lnM_lower,
#                   log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
#                   logselG_p1_lower=logselG_p1_lower,logselG_p2_lower=logselG_p2_lower,
#                   logselT_p1_lower=logselT_p1_lower,logselT_p2_lower=logselT_p2_lower,
#                   logdevT_p1_lower=logdevT_p1_lower,log_R0_lower=log_R0_lower,
#                   log_recdev_lower=log_recdev_lower,
#                   lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=lower_bounds,upper=upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)

# Phase 3
parameters <- list(log_sig=log_sig,lnM=obj$report()$lnM,
                   log_qT=obj$report()$log_qT,log_qG=obj$report()$log_qG,
                   logselG_p1=obj$report()$logselG_p1,logselG_p2=obj$report()$logselG_p2,
                   logselT_p1=obj$report()$logselT_p1,logselT_p2=obj$report()$logselT_p2,
                   logdevT_p1=obj$report()$logdevT_p1,
                   log_R0=obj$report()$log_R0,
                   log_recdev=obj$report()$log_recdev,
                   lnalpha=obj$report()$lnalpha,lnbeta=obj$report()$lnbeta,
                   effort_devsT=effort_devsT,effort_devsG=effort_devsG)
obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB_MN")
# upper_bounds <- c(log_sig_upper=log_sig_upper,lnM_upper=lnM_upper,
#                   log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
#                   logselG_p1_upper=logselG_p1_upper,logselG_p2_upper=logselG_p2_upper,
#                   logselT_p1_upper=logselT_p1_upper,logselT_p2_upper=logselT_p2_upper,
#                   logdevT_p1_upper=logdevT_p1_upper,log_R0_upper=log_R0_upper,
#                   log_recdev_upper=log_recdev_upper,
#                   lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper,
#                   effort_devsT_upper=effort_devsT_upper,effort_devsG_upper=effort_devsG_upper)
# lower_bounds <- c(log_sig_lower=log_sig_lower,lnM_lower=lnM_lower,
#                   log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
#                   logselG_p1_lower=logselG_p1_lower,logselG_p2_lower=logselG_p2_lower,
#                   logselT_p1_lower=logselT_p1_lower,logselT_p2_lower=logselT_p2_lower,
#                   logdevT_p1_lower=logdevT_p1_lower,log_R0_lower=log_R0_lower,
#                   log_recdev_lower=log_recdev_lower,
#                   lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower,
#                   effort_devsT_lower=effort_devsT_lower,effort_devsG_lower=effort_devsG_lower)
# fit <- nlminb(obj$par, obj$fn, obj$gr,lower=lower_bounds,upper=upper_bounds)
fit <- nlminb(obj$par, obj$fn, obj$gr)

############################################################################
# REPORT Section
############################################################################

#Report the model into a sdreport file using the model object
sdr <- sdreport(obj)

(summary <- summary(sdr))
#Did the model converge:
sdr$pdHess

############################################################################
# GRAPHS Section
############################################################################
ADMBdat.table<-read.table("ADMBResults.csv",sep=",",head=F)

pubtheme <- theme_classic()+theme(axis.title=element_text(size=48),axis.text=element_text(size=48),
                                  text = element_text(family="serif"),
                                  legend.text=element_text(size=36),
                                  legend.title=element_text(size=36),
                                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                  panel.background = element_blank(), axis.line = element_line(colour = "black"),
                                  axis.text.x = element_text(color="black",size=36),
                                  axis.text.y = element_text(color="black",size=36,angle=90,hjust=0.5))

#What color to make the SSM plot for abundance, recruitment, and SSBiomass
SSMcolor<-'blue'

#POPULATION SIZE AND RECRUITMENT
#############################################################################
Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
Abundance<- apply(Nmatrix,1,sum)/1000
Abundance.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Popsize"),2]/1000
Abundance.df <- data.frame(Year=rep(ryears,2),Model=rep(c("SCA","SSM"),each=length(ryears)),Abundance=c(Abundance.admb,Abundance))
Recruitment <- Nmatrix[,1]/1000
Recruitment.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Recruitment"),2]/1000
Recruitment.df <- data.frame(Year=rep(ryears,2),Model=rep(c("SCA","SSM"),each=length(ryears)),Recruitment=c(Recruitment.admb,Recruitment))

Abundance<-ggplot(data=Abundance.df,aes(x=Year,y=Abundance,color=as.factor(Model)))+
  geom_line(aes(linetype=Model, color=Model),size=3)+
  geom_point(aes(color=Model))+
  xlab("")+
  ylab("Number of fish (x1000)")+
  # ylim(0,5000)+
  labs(color="Model")+
  annotate(geom="text",x=ryears[1], y=5000, label="A",size=24,fontface="bold")+
  pubtheme+scale_color_manual(values=c('black',SSMcolor))+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text( size = 48),
        legend.title=element_text(size=48))

Recruitment<-ggplot(data=Recruitment.df,aes(x=Year,y=Recruitment,color=as.factor(Model)))+
  geom_line(aes(linetype=Model, color=Model),size=3)+
  geom_point(aes(color=Model))+
  xlab("Years")+
  ylab("Number of fish (x1000)")+
  # ylim(0,1800)+
  theme(legend.text = element_text( size = 48))+
  scale_x_continuous(n.breaks = 6)+
  labs(color="Model")+
  annotate(geom="text",x=ryears[1], y=1800, label="B",size=24,fontface="bold")+
  pubtheme+scale_color_manual(values=c('black',SSMcolor))

jpeg('AbundanceRecruitment.jpg',width=1200,height=1800)
grid.arrange(Abundance,Recruitment,ncol=1)
dev.off() 
#############################################################################

#SPAWNING STOCK BIOMASS
#############################################################################
SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
SP_BIO.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SPBiomass"),2]/1000
Biomass.df <- data.frame(Year=rep(ryears,2),Model=rep(c("SCA","SSM"),each=length(ryears)),Biomass=c(SP_BIO.admb,SP_BIO))

jpeg('SSBiomass.jpg',width=1200,height=800)
plot(ggplot(data=Biomass.df,aes(x=Year,y=Biomass,color=as.factor(Model)))+
       geom_line(aes(linetype=Model, color=Model),size=3)+
       geom_point(aes(color=Model))+
       xlab("Years")+
       ylim(1500,7000)+
       ylab("Spawning Stock Biomass (x1000) lbs")+
       scale_x_continuous(n.breaks = 7)+
       # ylim(0,7000)+
       # theme(legend.text = element_text( size = 24))+
       labs(color="Model")+
       pubtheme+scale_color_manual(values=c('black',SSMcolor))+
       theme(axis.title.y = element_text(hjust=5)))
dev.off()
#############################################################################

#SELECTIVITY CURVES
#############################################################################
#SSM

Blank <- ggplot()+annotate(geom="text",x=10, y=10, label="",size=24,fontface="bold")+theme_void()

Traplabel <- ggplot()+annotate(geom="text",x=10, y=10, label="Trap Net Fishery",size=24,fontface="bold",angle=90)+theme_void()
Gilllabel <- ggplot()+annotate(geom="text",x=10, y=10, label="Gill Net Fishery",size=24,fontface="bold",angle=90)+theme_void()
SSMlabel <- ggplot()+annotate(geom="text",x=10, y=10, label="SSM",size=24,fontface="bold")+theme_void()
SCAlabel <- ggplot()+annotate(geom="text",x=10, y=10, label="SCA",size=24,fontface="bold")+theme_void()

FTmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
FTmatrix.max <- apply(FTmatrix,1,max)
AdjustedqTmatrix <- FTmatrix/FTmatrix.max
AdjustedqT.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqTmatrix))

coul<-brewer.pal(11,"RdBu")

SelectivityTrapNetSSM<-ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
  geom_line(size=1.25)+
  xlab("Ages")+
  ylab("Normalized Catchability (100s of lifts * year)")+
  scale_color_manual(values=colorRampPalette(coul)(length(years)))+
  labs(color="Year")+
  annotate(geom="text",x=ages[1], y=1, label="D",size=24,fontface="bold")+
  pubtheme + theme(axis.text.x = element_text(color="black",size=36),
                   axis.title.y = element_text(size = 38))

# SelectivityTrapNetSSM_AFS<-ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
#                                   geom_line(size=1.25)+
#                                   xlab("Ages")+
#                                   ylab("Normalized Catchability (100's of lifts * year)")+
#                                   scale_color_manual(values=colorRampPalette(coul)(length(years)))+
#                                   pubtheme+theme(legend.position = "none")

FGmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
FGmatrix.max <- apply(FGmatrix,1,max)
AdjustedqGmatrix <- FGmatrix/FGmatrix.max
AdjustedqG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqGmatrix))

SelectivityGillNetSSM<-ggplot(data=AdjustedqG.df,aes(x=age,y=catchability,color=as.factor(year)))+
  geom_line(size=1.25)+
  xlab("")+
  ylab("Normalized Catchability (millions of feet * year)")+
  scale_color_manual(values=colorRampPalette(coul)(length(years)))+
  labs(color="Year")+
  annotate(geom="text",x=ages[1], y=1, label="B",size=24,fontface="bold")+
  pubtheme+ theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y = element_text(size = 38))

# SelectivityGillNetSSM_AFS<-ggplot(data=AdjustedqG.df,aes(x=age,y=catchability,color=as.factor(year)))+
#                                   geom_line(size=1.25)+
#                                   xlab("Ages")+
#                                   ylab("Normalized Catchability (millions of feet * year)")+
#                                   scale_color_manual(values=colorRampPalette(coul)(length(years)))+
#                                   pubtheme+theme(legend.position = "none")

#SCA
SelT.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelT"),2]
SelTmatrix.admb<-matrix(SelT.admb,nrow=length(ryears),ncol=length(ages))
SelTmatrix.matrix.max <- apply(SelTmatrix.admb,1,max)
AdjustedSelTmatrix.admb <- SelTmatrix.admb/SelTmatrix.matrix.max
AdjustedSelT.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(AdjustedSelTmatrix.admb))

SelectivityTrapNetSCAAdjusted<-ggplot(data=AdjustedSelT.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
  geom_line(size=1.25)+
  xlab("Ages")+
  ylab("Selectivity")+
  scale_color_manual(values=colorRampPalette(coul)(length(years)))+
  labs(color="Year")+
  annotate(geom="text",x=ages[1], y=1, label="C",size=24,fontface="bold")+
  pubtheme 

# SelectivityTrapNetSCAAdjusted_AFS<-ggplot(data=AdjustedSelT.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
#                                           geom_line(size=1.25)+
#                                           xlab("Ages")+
#                                           ylab("Selectivity")+
#                                           scale_color_manual(values=colorRampPalette(coul)(length(years)))+
#                                           pubtheme+theme(legend.position = "none")


SelG.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelG"),2]
SelGmatrix.admb<-matrix(SelG.admb,nrow=length(ryears),ncol=length(ages))
SelGmatrix.matrix.max <- apply(SelGmatrix.admb,1,max)
AdjustedSelGmatrix.admb <- SelGmatrix.admb/SelGmatrix.matrix.max
AdjustedSelG.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(AdjustedSelGmatrix.admb))

SelectivityGillNetSCAAdjusted<-ggplot(data=AdjustedSelG.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
  geom_line(size=1.25)+
  xlab("")+
  ylab("Selectivity")+
  scale_color_manual(values=colorRampPalette(coul)(length(years)))+
  labs(color="Year")+
  annotate(geom="text",x=ages[1], y=1, label="A",size=24,fontface="bold")+
  pubtheme+
  theme(legend.text = element_text(size = 36),
        legend.title = element_text(size = 36),
        axis.text.x =element_blank(),
        axis.ticks.x=element_blank())
# legend.key.size= unit(2,'cm'),
# legend.key.height=unit(2,'cm'),
# legend.key.width=unit(2,'cm'))

Selectivitylegend <- cowplot::get_legend(SelectivityGillNetSCAAdjusted)

SelectivityGillNetSCAAdjusted <- SelectivityGillNetSCAAdjusted + theme(legend.position="none")
SelectivityTrapNetSCAAdjusted <- SelectivityTrapNetSCAAdjusted + theme(legend.position="none")
SelectivityGillNetSSM <- SelectivityGillNetSSM + theme(legend.position="none")
SelectivityTrapNetSSM <- SelectivityTrapNetSSM + theme(legend.position="none")

jpeg('Selectivity.jpg',width=2400,height=1800)
grid.arrange(Blank,
             SCAlabel,
             SSMlabel,
             Selectivitylegend,
             Gilllabel,
             SelectivityGillNetSCAAdjusted,
             SelectivityGillNetSSM,
             Traplabel,
             SelectivityTrapNetSCAAdjusted,
             SelectivityTrapNetSSM,
             ncol=4,
             layout_matrix = rbind(c(1,2,3,4), c(5,6,7,4),c(8,9,10,4)),
             widths = c(0.5, 4, 4, 1), heights = c(0.5,3,3))
dev.off() 


#############################################################################

#MORTALITY
#############################################################################
FG<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
FG_Avg<-apply(FG,1,mean)
FT<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
FT_Avg<-apply(FT,1,mean)
M <-summary(sdr)[which(row.names(summary(sdr))=="M"),1]
M <- rep(M,length(ryears))
Z.df <- data.frame(ryears,morts=c(FG_Avg,FT_Avg,M))
Z.type <- c(rep("FG",length(ryears)),rep("FT",length(ryears)),rep("M",length(ryears)))

FG_Avg.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="FGill"),2]
FT_Avg.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Ftrap"),2]
M.admb <-  ADMBdat.table[which(ADMBdat.table[,1]=="M"),2]
M.admb <- rep(M.admb,length(years))
Z.df.admb <- data.frame(years,morts=c(FG_Avg.admb,FT_Avg.admb,M.admb))
Z.type.admb <- c(rep("FG",length(years)),rep("FT",length(years)),rep("M",length(years)))


Mortality<-ggplot(Z.df,aes(ryears,morts))+
  geom_bar(stat="identity",aes(fill=Z.type))+
  ylab("")+
  # ylim(0,1.5)+
  xlab("Years")+
  scale_fill_manual(values=c("deepskyblue","deepskyblue4","darkgrey"))+
  scale_x_continuous(n.breaks = 8)+
  annotate(geom="text",x=ryears[1], y=1.5, label="B",size=24,fontface="bold")+
  labs(fill="Mortality Type")+
  pubtheme+theme(legend.position = c(0.85,0.6),
                 axis.text.x = element_text(color="black",size=48),
                 # axis.text.y = element_text(color="black",size=48),
                 axis.text.y=element_blank(),
                 axis.ticks.y=element_blank())  #remove y axis ticks

MortalityADMB<-ggplot(Z.df.admb,aes(years,morts))+
  geom_bar(stat="identity",aes(fill=Z.type.admb))+
  ylab("Mortality Rate (Year^-1)")+
  # ylim(0,1.5)+
  xlab("Years")+
  scale_fill_grey()+
  scale_x_continuous(n.breaks = 8)+
  annotate(geom="text",x=ryears[1], y=1.5, label="A",size=24,fontface="bold")+
  labs(fill="Mortality Type")+
  pubtheme+theme(legend.position = c(0.85,0.6),
                 axis.text.x = element_text(color="black",size=48),
                 axis.text.y = element_text(color="black",size=48))

jpeg('Mortality.jpg',width=2400,height=900)
grid.arrange(MortalityADMB,Mortality,ncol=2)
dev.off() 



#############################################################################

#OBSERVED VERSUS PREDICTED HARVEST
#############################################################################
ObsT<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsT"),2]/1000
ObsG<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsG"),2]/1000

PredT.admb<-ADMBdat.table[which(ADMBdat.table[,1]=="PredT"),2]/1000
PredG.admb<-ADMBdat.table[which(ADMBdat.table[,1]=="PredG"),2]/1000

PredT <- summary(sdr)[which(row.names(summary(sdr))=="CT"),1]/1000
PredG <- summary(sdr)[which(row.names(summary(sdr))=="CG"),1]/1000

PredT.df<- data.frame(Year=rep(ryears),Model=rep(c("SCA","SSM"),each=length(ryears)),Prediction=c(PredT.admb,PredT))
PredG.df<- data.frame(Year=rep(ryears),Model=rep(c("SCA","SSM"),each=length(ryears)),Prediction=c(PredG.admb,PredG))

PredictedTrapHarvest<-ggplot(data=PredT.df,aes(x=Year,y=Prediction,color=as.factor(Model)))+
  geom_line(data=data.frame(years,ObsT),aes(x=years,y=ObsT),color="black",size=2)+
  geom_line(aes(color=Model),size=2,linetype="dotted")+
  scale_color_manual(values=c('red','blue'))+
  xlab("Years")+
  ylab("Harvest (x1,000)")+ylim(0,600)+
  theme(legend.text = element_text( size = 24))+
  annotate(geom="text",x=ryears[1], y=600, label="B",size=24,fontface="bold")+
  labs(color="Model")+
  pubtheme

PredictedGillHarvest<-ggplot(data=PredG.df,aes(x=Year,y=Prediction,color=as.factor(Model)))+
  geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
  geom_line(aes(color=Model),size=2,linetype="dotted")+
  scale_color_manual(values=c('red','blue'))+
  xlab("Years")+
  ylab("Harvest (x1,000)")+ylim(0,600)+
  theme(legend.text = element_text( size = 24))+
  annotate(geom="text",x=ryears[1], y=600, label="A",size=24,fontface="bold")+
  labs(color="Model")+
  pubtheme

Predictedlegend <- cowplot::get_legend(PredictedGillHarvest)

PredictedGillHarvest <- PredictedGillHarvest + theme(legend.position="none")
PredictedTrapHarvest <- PredictedTrapHarvest + theme(legend.position="none")

jpeg('Predicted.jpg',width=2400,height=900)
grid.arrange(PredictedGillHarvest,PredictedTrapHarvest,Predictedlegend,ncol=3,
             widths = c(4, 4, 0.75))
dev.off() 
#############################################################################