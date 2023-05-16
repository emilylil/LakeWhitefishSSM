# WFM03_2018_TMB_Functions.R

#############################################################################
# FUNCTIONS SECTION:
#############################################################################

# DATA SECTION
############################################################################
setdata <- function(retroyear=2017)
{
  fyear <- 1986
  ldyear <- 2017
  lyear <- retroyear
  
  fage <- 4
  lage <- 15
  
  targ_age <- 4
  totlmort <- 3
  fishmort <- 2
  
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
  
  return(data<-list(fyear=fyear,ldyear=ldyear,lyear=lyear,
                    fage=fage,lage=lage,targ_age=targ_age,
                    totlmort=totlmort,fishmort=fishmort,
                    years=years,ryears=ryears,ages=ages,
                    rhoSR=rhoSR,rhoCG=rhoCG,rhoCT=rhoCT,rhoEG=rhoEG,rhoET=rhoET,rhosel=rhosel,
                    sp_time=sp_time,harv_time=harv_time,
                    in_watage=as.matrix(in_watage),
                    in_latage=as.matrix(in_latage),
                    in_mat=as.matrix(in_mat),
                    H2O_T=H2O_T,Linf=Linf,vb_K=vb_K,
                    sdM=sdM,surv_num=surv_num,
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
                    targ_ptn=targ_ptn,targ_A=targ_A,targ_SPR=targ_SPR))
}
#############################################################################

#PARAMETER SECTION Recruitment (N1,j and Ni,1), and catchability (qi,j) are RE
#############################################################################
setparam_at_est <- function(data,log_sig = -3.11209870007)
{
  # As much as possible (where it matches with the ADMB file) the starting values
  # match the estimated values in the ADMB version from PAR file
  log_sig <- log_sig #Equivalent of variance = 0.07 for obs error
  lnM <- -1.69384485333
  log_qT <- -3.81198895562
  log_qG <- -2.28728683332
  logselG_p1 <- -2.48479641276
  logselG_p2 <- 1.83895901637
  logselT_p1 <- 6.16411908029
  logselT_p2 <- -2.81346704020
  
  logdevT_p1 <-  c(0.0159009802701,-0.0149602501516,-0.00878552840958,0.0154885420221,-0.0322714349097,
                   0.00681632762740,0.00738500924209,-0.00668506486120,0.0249410090274,0.00338573081808,
                   -0.00385407242652,0.0253999864813,-0.0422559638563,0.00610259925609,-0.0564475914384,
                   0.0550671892507,-0.00111897895265,-0.0304223271797,0.00559305194301,-0.0528546975672,
                   -0.00519739225798,0.000136078703803,0.0353068446421,0.0466160254852,-0.00541393833336,
                   0.0184999204937,-0.00188823873992,-0.0827789231715,0.0525095590165,0.0284334691286,
                   0.00332581048330)
  log_R0 <- 13.51088172
  log_recdev <- c(-0.487403693,-0.58530459,0.226618973,0.950732126,0.230829297,0.153106186,-0.354511417,
                  -0.18924929,0.794929272,-0.175296942,-0.04012056,-0.075878369,0.009218329,-0.106966599,
                  -0.221116513,-0.075219933,-0.182398598,0.522783392,0.303011458,-0.174449447,-0.20525612,
                  -0.193294876,-0.143426792,-0.256081446,-0.17831799,-0.123706207,-0.017378673,-0.065985205,
                  0.25209074,-0.45047868,0.05863151,0.425565194,-0.919321148,-1.469389015,-1.33876143,-0.592756242)
  lnalpha <- -8.17085012085
  lnbeta <- -22.8729436736
  effort_devsT <-  c(0.216923714229,0.0445830509089,-0.174312009244,-0.147120206174,0.261118980349,
                     0.0282324673642,0.0821334645900,0.102478857553,-0.0203905484756,-0.0337718874173,
                     -0.0458843682151,-0.0737326172145,0.0167260569355,-0.116790645281,-0.142952007237,
                     -0.157075661671,0.0871917294481,0.310675321801,-0.0819263643693,-0.0461054282701,
                     -0.0474850405616,-0.219308718093,-0.140076968377,-0.142796177187,0.0828692905367,
                     -0.0480488247183,-0.238910792275,-0.218579948892,-0.160135285478,-0.220610735894,
                     0.0525444126708)
  
  effort_devsG <-  c(-0.167941243914,-0.0592295532093,0.00228882337898,-0.511246202710,0.345488090944,
                     -0.122622890528,0.115679603474,-0.105582833837,0.0702911200045,-0.307611373426,
                     0.0803945374864,-0.143806318040,-0.155773938788,0.223600673989,0.779844634509,
                     -0.368595701144,-0.524028406212,0.373876113991,0.0386813567895,0.138072693075,
                     -0.0728018414212,0.0581557748107,-0.333393407363,-0.171039551050,0.212344552329,
                     -0.243144691061,-0.278970442754,0.435754387105,-0.681226687895,0.162119283084,
                     -0.424039882192)
  
  #Return a list of parameters
  return(parameters <- list(log_sig=log_sig,lnM=lnM,
                            log_qT=log_qT,log_qG=log_qG,
                            logselG_p1=logselG_p1,logselG_p2=logselG_p2,
                            logselT_p1=logselT_p1,logselT_p2=logselT_p2,
                            logdevT_p1=logdevT_p1,
                            log_R0=log_R0,
                            log_recdev=log_recdev,
                            lnalpha=lnalpha,lnbeta=lnbeta,
                            effort_devsT=effort_devsT,effort_devsG=effort_devsG))
}
#############################################################################

#############################################################################
setparam_at_init <- function(data,log_sig = -3)
{
  #Starting parameters are identical to where ADMB started
  log_sig <- -3
  lnM <- -1.6094
  # lnM <- -1.69384485333
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
  
  return(parameters <- list(log_sig=log_sig,lnM=lnM,
                            log_qT=log_qT,log_qG=log_qG,
                            logselG_p1=logselG_p1,logselG_p2=logselG_p2,
                            logselT_p1=logselT_p1,logselT_p2=logselT_p2,
                            logdevT_p1=logdevT_p1,
                            log_R0=log_R0,
                            log_recdev=log_recdev,
                            lnalpha=lnalpha,lnbeta=lnbeta,
                            effort_devsT=effort_devsT,effort_devsG=effort_devsG))
}
#############################################################################

#BOUNDS SECTION 
#############################################################################
setbounds <- function(data)
{
  #Upper Bounds
  # log_sig_upper <- 5 #Equivalent of variance = 0.07 for obs error
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
  
  upper_bounds <- c(log_sig_upper=log_sig_upper,
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
  
  lower_bounds <- c(log_sig_lower=log_sig_lower,
                    log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
                    logselG_p1_lower=logselG_p1_lower,logselG_p2_lower=logselG_p2_lower,
                    logselT_p1_lower=logselT_p1_lower,logselT_p2_lower=logselT_p2_lower,
                    logdevT_p1_lower=logdevT_p1_lower,log_R0_lower=log_R0_lower,
                    log_recdev_lower=log_recdev_lower,
                    lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower,
                    effort_devsT_lower=effort_devsT_lower,effort_devsG_lower=effort_devsG_lower)
  
  #Return list of upper and lower bounds
  return(list(upper_bounds = upper_bounds,
              lower_bounds = lower_bounds))
}
#############################################################################

#BOUNDS ON FIRST ITERATION OF FITTING
#############################################################################
setbounds1 <- function(data)
{
  #Upper Bounds
  # log_sig_upper <- 5 #Equivalent of variance = 0.07 for obs error
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
  
  upper_bounds <- c(log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
                    log_R0_upper=log_R0_upper,
                    lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper)
  
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
  
  lower_bounds <- c(log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
                    log_R0_lower=log_R0_lower,
                    lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower)
  
  #Return list of upper and lower bounds
  return(list(upper_bounds = upper_bounds,
              lower_bounds = lower_bounds))
}
#############################################################################

#BOUNDS SECTION 
#############################################################################
setbounds2 <- function(data)
{
  #Upper Bounds
  # log_sig_upper <- 5 #Equivalent of variance = 0.07 for obs error
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
  
  upper_bounds <- c(log_qT_upper=log_qT_upper,log_qG_upper=log_qG_upper,
                    logselG_p1_upper=logselG_p1_upper,logselG_p2_upper=logselG_p2_upper,
                    logselT_p1_upper=logselT_p1_upper,logselT_p2_upper=logselT_p2_upper,
                    logdevT_p1_upper=logdevT_p1_upper,log_R0_upper=log_R0_upper,
                    log_recdev_upper=log_recdev_upper,
                    lnalpha_upper=lnalpha_upper,lnbeta_upper=lnbeta_upper)
  
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
  
  lower_bounds <- c(log_sig_lower=log_sig_lower,
                    log_qT_lower=log_qT_lower,log_qG_lower=log_qG_lower,
                    logselG_p1_lower=logselG_p1_lower,logselG_p2_lower=logselG_p2_lower,
                    logselT_p1_lower=logselT_p1_lower,logselT_p2_lower=logselT_p2_lower,
                    logdevT_p1_lower=logdevT_p1_lower,log_R0_lower=log_R0_lower,
                    log_recdev_lower=log_recdev_lower,
                    lnalpha_lower=lnalpha_lower,lnbeta_lower=lnbeta_lower,
                    effort_devsT_lower=effort_devsT_lower,effort_devsG_lower=effort_devsG_lower)
  
  #Return list of upper and lower bounds
  return(list(upper_bounds = upper_bounds,
              lower_bounds = lower_bounds))
}
#############################################################################

#MODEL SET UP AND CREATING TMB OBJECT:
#############################################################################
setmodel <- function(data,parameters,agecompfit,modelname,mymapinput = list(log_sig=as.factor(NA)))
{
  #Specify random effects and mapping (turn off estimation)
  # RANDOM EFFECTS:
  
  reffects <- c()
  # if(agecompfit==1){reffects <- c("log_rec","log_qG","log_qT")}
  # if(agecompfit==2){reffects <- c("log_rec","log_qG","log_qT","log_gamT","log_gamG")}
  
  # MAPPING:
  mymap <- mymapinput
  # mymap = list(log_sig=as.factor(NA))
  # mymap <- c()
  # mymap=list(log_rec=rep(as.factor(NA),length(parameters$log_rec)))
  # mymap <- list(log_sig=as.factor(3),lnM=as.factor(1),log_qT=as.factor(1),log_qG=as.factor(1),logselG_p1=as.factor(2),logselG_p2=as.factor(2),
  #               logselT_p1=as.factor(2),logselT_p2=as.factor(2),log_rec=as.factor(1),
  #               lnalpha=as.factor(1),lnbeta=as.factor(1))
  # Add mapping such that log_std_log_gamT and G are not age-varying
  # Could add even more nuance if you think there's groupings for certain ages (1-3, 4-6, etc.)
  # mymap <- list(log_sig=as.factor(NA),log_std_log_gamT=factor(rep("all"),length(data$ages)),log_std_log_gamG=factor(rep("all"),length(data$ages)))
  
  #Make the TMB object (Builds the bridge between DLL and R functions and objects)
  # No random effects, no map:
  # obj <- MakeADFun(data,parameters,DLL=modelname)
  # Yes random effects, no map:
  # obj <- MakeADFun(data,parameters,DLL=modelname,random=reffects)
  # No random effects, yes map:
  obj <- MakeADFun(data,parameters,DLL=modelname, map=mymap)
  # Yes random effects, yes map:
  # obj <- MakeADFun(data,parameters,DLL=modelname,random=reffects, map=mymap)
  
  # For diagnostics, can turn on some stuff to help track estimation:
  # Trace the value as the model fits:
  # config(optimize.instantly=0, DLL="WFM03_2018_TMB")
  
  # Trace values for the parameter output:
  # obj$env$tracepar <- FALSE
  # obj$env$tracepar <- TRUE
  
  # All the report variables as a list
  # objreportlist <- obj$report()
  
  return(obj)
}
#############################################################################

#FIT THE TMB MODEL AND TEST CONVERGENCE
#############################################################################
fitmodel <- function(obj,bounds)
{
  # Fit the model with defaults:
  fit <- nlminb(obj$par, obj$fn, obj$gr)
  # fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
  # Fit the model with increases in the caps on iterations and convergence criteria:
  # fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds,
  #               control=list(eval.max=20000000,iter.max=10000000))
  # Test for convergence and estimated parameter values
  # Convergence message:
  # fit$message
  # Convergence message number (0 means success):
  # fit$convergence
  # Final parameter values:
  # fit$par
  # Number of iterations performed
  # fit$iterations
  # Number of objective function and gradient function evaluations
  # fit$evaluations
  return(fit)
}
#############################################################################

#MAKE THE SD REPORT OBJECT AND TEST CONVERGENCE
#############################################################################
reportmodel <- function(obj)
{
  # Make the SD Report object and examine it
  sdr <- sdreport(obj)
  # # Look at sdr:
  # summary(sdr)
  # head(summary(sdr))
  print("")
  print("Summary: ")
  print(summary(sdr))
  # Secondary convergence message:
  print("Did the model converge: ")
  print(sdr$pdHess)
  
  #obj$report()
  return(sdr)
}
#############################################################################

#Calculate the variance of the age compositions. Should equal 1
#############################################################################
calcvar <- function(sdr,data)
{
  PAT <- matrix(summary(sdr)[which(row.names(summary(sdr))=="PAT"),1],nrow=length(data$ryears),ncol=length(data$ages))
  PAG <- matrix(summary(sdr)[which(row.names(summary(sdr))=="PAG"),1],nrow=length(data$ryears),ncol=length(data$ages))
  
  obs_PAT <- matrix(summary(sdr)[which(row.names(summary(sdr))=="obs_PAT"),1],nrow=length(data$ryears),ncol=length(data$ages))
  obs_PAG <- matrix(summary(sdr)[which(row.names(summary(sdr))=="obs_PAG"),1],nrow=length(data$ryears),ncol=length(data$ages))
  
  N_SampT <- summary(sdr)[which(row.names(summary(sdr))=="N_SampT"),1]
  N_SampG <- summary(sdr)[which(row.names(summary(sdr))=="N_SampG"),1]
  
  PAT_bar <- apply(PAT, 1, function(x) sum(data$ages*x))
  PAG_bar <- apply(PAG, 1, function(x) sum(data$ages*x))
  obs_PAT_bar <- apply(obs_PAT, 1, function(x) sum(data$ages*x))
  obs_PAG_bar <- apply(obs_PAG, 1, function(x) sum(data$ages*x))
  
  PAT_xbeb <- apply(PAT,1,function(x) sum(data$ages^2*x))
  PAG_xbeb <- apply(PAG,1,function(x) sum(data$ages^2*x))
  
  v_PAT=PAT_xbeb-PAT_bar^2
  v_PAG=PAG_xbeb-PAG_bar^2
  
  PAT_var <- var((obs_PAT_bar-PAT_bar)/sqrt(v_PAT/N_SampT))
  PAG_var <- var((obs_PAT_bar-PAT_bar)/sqrt(v_PAG/N_SampT))
  
  var_vec <- c(PAT_var,PAG_var)
  
  return(var_vec)
}
#############################################################################

#Calculate the weights, w, for how to adjust sample size according to Francis method
#############################################################################
calcw <- function(sdr,data)
{
  #Pull out relevant values from the sdr object:
  PAT <- matrix(summary(sdr)[which(row.names(summary(sdr))=="PAT"),1],nrow=length(data$ryears),ncol=length(data$ages))
  PAG <- matrix(summary(sdr)[which(row.names(summary(sdr))=="PAG"),1],nrow=length(data$ryears),ncol=length(data$ages))
  obs_PAT <- matrix(summary(sdr)[which(row.names(summary(sdr))=="obs_PAT"),1],nrow=length(data$ryears),ncol=length(data$ages))
  obs_PAG <- matrix(summary(sdr)[which(row.names(summary(sdr))=="obs_PAG"),1],nrow=length(data$ryears),ncol=length(data$ages))
  N_sampT <- summary(sdr)[which(row.names(summary(sdr))=="N_sampT"),1]
  N_sampG <- summary(sdr)[which(row.names(summary(sdr))=="N_sampG"),1]
  NtildeT <- summary(sdr)[which(row.names(summary(sdr))=="NtildeT"),1]
  NtildeG <- summary(sdr)[which(row.names(summary(sdr))=="NtildeG"),1]
  
  #Calculate the expected age from the observed and predicted age compostions
  PAT_bar <- apply(PAT, 1, function(x) sum(data$ages*x))
  PAG_bar <- apply(PAG, 1, function(x) sum(data$ages*x))
  obs_PAT_bar <- apply(obs_PAT, 1, function(x) sum(data$ages*x))
  obs_PAG_bar <- apply(obs_PAG, 1, function(x) sum(data$ages*x))
  
  PAT_xbeb <- apply(PAT,1,function(x) sum(data$ages^2*x))
  PAG_xbeb <- apply(PAG,1,function(x) sum(data$ages^2*x))
  
  v_PAT=PAT_xbeb-PAT_bar^2
  v_PAG=PAG_xbeb-PAG_bar^2
  
  w_PAT=1/(var((obs_PAT_bar-PAT_bar)/sqrt(v_PAT/NtildeT)))
  w_PAG=1/(var((obs_PAG_bar-PAG_bar)/sqrt(v_PAG/NtildeG)))
  
  w_vec=c(w_PAT,w_PAG)
  
  return(w_vec)
}
#############################################################################

#ESTABLISH THE WORKING DIRECTORY FOR RETROSPECTIVE PLOTS
#############################################################################
setretrowd<-function(originalwd)
{
  retrowd<-paste(originalwd,"/",Sys.Date(),"_Retro",sep='')
  if(!dir.exists(retrowd)){dir.create(retrowd)}
  else
  {
    c=0
    while(dir.exists(retrowd)&&c<100)
    {
      c=c+1
      retrowd<-paste(originalwd,"/",Sys.Date(),"_Retro","(",c,")",sep='')
    }
    dir.create(retrowd)
  }
  return(retrowd)
}
#############################################################################

#ESTABLISH THE WORKING DIRECTORY FOR REPORT AND IMAGES
#############################################################################
setnewwd <- function(originalwd,isretro=F,retroend=2007,retrostart=2017,retroyr=2017,data,...)
{
  originalwd=originalwd
  lryear=data$lryear
  # Set the working directory to separate folder to store all the output visuals/diagnostics
  if(isretro==T){
    newwd<-paste(retrowd,"/",lryear,sep='')
    dir.create(newwd)
  }
  else{
    newwd<-paste(getwd(),"/",Sys.Date(),sep='')
    if(!dir.exists(newwd)){dir.create(newwd)}
    else
    {
      c=0
      while(dir.exists(newwd)&&c<100)
      {
        c=c+1
        newwd<-paste(getwd(),"/",Sys.Date(),"(",c,")",sep='')
      }
      dir.create(newwd)
    }
  }
  return(newwd)
}
#############################################################################

#ESTABLISH NEW WD AND SAVE SD REPORT OBJECT THERE
#############################################################################
saveresults <- function(originalwd,newwd,sdr)
{
  setwd(newwd)
  # Write out sdr object in excel for convenient viewing
  write.csv(summary(sdr),file="SDReport.csv")
  
  sink("Summarized_Output.txt")
  cat("Did the model converge?: ")
  cat("\n")
  cat(sdr$pdHess)
  cat("\n")
  cat("\n")
  cat("Marginal Likelihood (fit$objective): ")
  cat("\n")
  cat(fit$objective)
  cat("\n")
  cat("\n")
  cat("Management Metrics: ")
  cat("\n")
  cat("SSBR: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="SSBR"),1])
  cat("\n")
  cat("SPR: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="SPR"),1])
  cat("\n")
  cat("YPR: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="YPR"),1])
  cat("\n")
  cat("Average F Gill Net: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="AvgF_gill"),1])
  cat("\n")
  cat("Average F Trap Net: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="AvgF_trap"),1])
  cat("\n")
  cat("Average Recruitment: ")
  cat("\n")
  cat(summary(sdr)[which(row.names(summary(sdr))=="Rec_avg"),1])
  cat("\n")
  cat("Maximum Total Mortality: ")
  cat("\n")
  cat(max(summary(sdr)[which(row.names(summary(sdr))=="F"),1])+summary(sdr)[which(row.names(summary(sdr))=="M"),1])
  cat("\n")
  cat("\n")
  cat("Parameter output: ")
  cat("\n")
  for(i in 1:length(sdr$par.fixed))
  {
    cat(names(sdr$par.fixed)[i])
    cat(":   ")
    cat(sdr$par.fixed[i])
    cat("\n")
  }
  cat("\n")
  cat("Random Effects Output: ")
  cat("\n")
  for(i in 1:length(sdr$par.random))
  {
    cat(names(sdr$par.random)[i])
    cat(":   ")
    cat(sdr$par.random[i])
    cat("\n")
  }
  sink()
  
  setwd(originalwd)
}
#############################################################################

# MAKING ALL THE GRAPHS FOR AN INDIVIDUAL RUN
#############################################################################
savegraphs<-function(checkagainstADMB=F,oridinalwd,newwd,sdr,data)
{
  #Set some important values that determine dynamics of the plots
  years<-data$years
  ryears<-data$ryears
  lyears<-data$lyears
  asymptoteage<-data$asymptoteage
  asymptoteages<-data$asymptoteages
  ages<-data$ages
  
  #Read in the ADMB estimates, in case we want to compare against previous
  ADMBdat.table<-read.table("ADMBResults.csv",sep=",",head=F)
  setwd(newwd)
  
  if(checkagainstADMB==T)
  {
    #POPULATION SIZE
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Abundance<- apply(Nmatrix,1,sum)/1000
    Abundance.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Popsize"),2]/1000
    
    jpeg('Abundance.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,Abundance.admb),aes(x=years,y=Abundance.admb),color="black",size=2)+
           geom_line(data=data.frame(ryears,Abundance),aes(x=ryears,y=Abundance),color="blue",linetype = "dashed",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,5000)+
           # ggtitle("Estimated Lake Whitefish Abundance in WFM03, Black-ADMB, Blue- TMB")+ #For myself
           ggtitle("")+ #For publications and presentation
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publication
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
    dev.off()
    #############################################################################
    
    #BIOMASS
    #############################################################################
    BIOMASS <- summary(sdr)[which(row.names(summary(sdr))=="BIOMASS"),1]/1000*2.20462
    SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
    BIOMASS.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Biomass"),2]/1000
    SP_BIO.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SPBiomass"),2]/1000
    
    jpeg('Biomass.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,BIOMASS.admb),aes(x=years,y=BIOMASS.admb),color="black",size=2)+
           geom_line(data=data.frame(years,SP_BIO.admb),aes(x=years,y=SP_BIO.admb),color="black",linetype=2,size=2)+
           geom_line(data=data.frame(ryears,BIOMASS),aes(x=ryears,y=BIOMASS),color="blue",size=2)+
           geom_line(data=data.frame(ryears,SP_BIO),aes(x=ryears,y=SP_BIO),color="blue",linetype=2,size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Biomass (x1000) lbs")+ylim(0,7000)+
           # ggtitle("Estimated Lake Whitefish Biomass in WFM03, Black-ADMB, Blue- TMB")+ #For self
           ggtitle("")+ #For publications and presentations
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
    dev.off()
    #############################################################################
    
    #SPAWNING STOCK BIOMASS
    #############################################################################
    SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
    SP_BIO.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SPBiomass"),2]/1000
    
    jpeg('SSBiomass.jpg',width=1200,height=600)
    plot(ggplot()+
           geom_line(data=data.frame(years,SP_BIO.admb),aes(x=years,y=SP_BIO.admb),color="black",size=2)+
           geom_line(data=data.frame(ryears,SP_BIO),aes(x=ryears,y=SP_BIO),color="blue",linetype = "dashed",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Spawning Stock Biomass (x1000) lbs")+ylim(0,7000)+
           # ggtitle("Estimated Lake Whitefish Spawning Stock Biomass in WFM03, Black-ADMB, Blue- TMB")+ #For self
           ggtitle("")+ #For presentations and publications
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
    dev.off()
    #############################################################################
    
    #RECRUITMENT
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Recruitment <- Nmatrix[,1]/1000
    Recruitment.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Recruitment"),2]/1000
    
    jpeg('Recruitment.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,Recruitment.admb),aes(x=years,y=Recruitment.admb),color="black",size=2)+
           geom_line(data=data.frame(ryears,Recruitment),aes(x=ryears,y=Recruitment),color="blue",linetype = "dashed",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,1800)+
           # ggtitle("Estimated Lake Whitefish Age-4 Recruits in WFM03, Black-ADMB, Blue- TMB")+ #For self
           ggtitle("")+
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
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
    
    jpeg('Mortality.jpg',width=1200,height=900)
    plot(ggplot(Z.df,aes(ryears,morts))+
           theme_bw()+
           geom_bar(stat="identity",aes(fill=Z.type))+
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           # ggtitle("Estimated Mortality (Natural, Trap, Gill) by year in WFM03")+ #For self
           ggtitle("")+ #For presentations and publications
           scale_fill_manual(values=c("blueviolet","blue","lightblue"))+
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15),legend.text=element_text(size=15))) #For Publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30),legend.text=element_text(size=30))) #For Presentations
    dev.off()                  
    
    FG_Avg.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="FGill"),2]
    FT_Avg.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="Ftrap"),2]
    M.admb <-  ADMBdat.table[which(ADMBdat.table[,1]=="M"),2]
    M.admb <- rep(M.admb,length(years))
    Z.df.admb <- data.frame(years,morts=c(FG_Avg.admb,FT_Avg.admb,M.admb))
    Z.type.admb <- c(rep("FG",length(years)),rep("FT",length(years)),rep("M",length(years)))
    
    jpeg('MortalityADMB.jpg',width=1200,height=900)
    plot(ggplot(Z.df.admb,aes(years,morts))+
           theme_bw()+
           geom_bar(stat="identity",aes(fill=Z.type.admb))+
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           # ggtitle("Estimated Mortality (Natural, Trap, Gill) by year in WFM03")+ #For self
           ggtitle("")+ #For presentations and publications
           scale_fill_grey()+
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15),legend.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30),legend.text=element_text(size=30))) #For presentations
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
    
    jpeg('PredictedTrapHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsT),aes(x=years,y=ObsT),color="black",size=2)+
           geom_line(data=data.frame(years,PredT.admb),aes(x=years,y=PredT.admb),color="darkgoldenrod1",linetype=2,size=2)+
           geom_line(data=data.frame(ryears,PredT),aes(x=ryears,y=PredT),color="red",linetype=2,size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           # ggtitle("Observed and Predicted Lake Whitefish Trap Net Harvest in WFM03, Yellow-Obs, Blue-SSM, Black-ADMB")+ #For self
           ggtitle("")+ #For publications and presentations
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(data=data.frame(years,PredG.admb),aes(x=years,y=PredG.admb),color="darkgoldenrod1",linetype=2,size=2)+
           geom_line(data=data.frame(ryears,PredG),aes(x=ryears,y=PredG),color="red",linetype=2,size=2)+
           theme_bw()+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           # ggtitle("Observed and Predicted Lake Whitefish Gill Net Harvest in WFM03, Yellow-Obs, Blue-SSM, Black-ADMB")+ #For self
           ggtitle("")+ #For publications and presentations
           # theme(axis.title=element_text(size=15),axis.text=element_text(size=15))) #For publications
           theme(axis.title=element_text(size=30),axis.text=element_text(size=30))) #For presentations
    dev.off()
    #############################################################################
    
  }
  
  else
  {
    #POPULATION SIZE
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Abundance<- apply(Nmatrix,1,sum)/1000
    
    jpeg('Abundance.jpg',width=1200,height=600)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,Abundance),aes(x=ryears,y=Abundance),color="black",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,5000)+
           ggtitle("Estimated lake whitefish abundance in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15)))
    dev.off()
    #############################################################################
    
    #BIOMASS
    #############################################################################
    BIOMASS <- summary(sdr)[which(row.names(summary(sdr))=="BIOMASS"),1]/1000*2.20462
    SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
    
    jpeg('Biomass.jpg',width=1200,height=600)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,BIOMASS),aes(x=ryears,y=BIOMASS),color="black",size=2)+
           geom_line(data=data.frame(ryears,SP_BIO),aes(x=ryears,y=SP_BIO),color="black",linetype=2,size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Biomass (x1000) lbs")+ylim(0,7000)+
           ggtitle("Estimated Lake Whitefish Biomass in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15),legend.position="right"))
    dev.off()
    #############################################################################
    
    #SPAWNING STOCK BIOMASS
    #############################################################################
    SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
    
    jpeg('SSBiomass.jpg',width=1200,height=600)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,SP_BIO),aes(x=ryears,y=SP_BIO),color="black",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Spawning Stock Biomass (x1000) lbs")+ylim(0,7000)+
           ggtitle("Estimated Lake Whitefish Spawning Stock Biomass in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15)))
    dev.off()
    #############################################################################
    
    #RECRUITMENT
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Recruitment <- Nmatrix[,1]/1000
    
    jpeg('Recruitment.jpg',width=1200,height=600)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,Recruitment),aes(x=ryears,y=Recruitment),color="black",size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,1800)+
           ggtitle("Estimated Lake Whitefish Age-4 Recruits in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15)))
    dev.off()
    #############################################################################
    
    #SELECTIVITY CURVES
    #############################################################################
    FTmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
    FTmatrix.max <- apply(FTmatrix,1,max)
    AdjustedqTmatrix <- FTmatrix/FTmatrix.max
    AdjustedqT.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqTmatrix))
    
    jpeg('SelectivityTrapNet.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Adjusted q")+
           #scale_colour_grey()+
           ggtitle("Selectivity Patterns by Year and Age of Lake Whitefish in WFM03")+
           theme(axis.title=element_text(size=20),axis.text=element_text(size=20)))
    dev.off()
    
    FGmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
    FGmatrix.max <- apply(FGmatrix,1,max)
    AdjustedqGmatrix <- FGmatrix/FGmatrix.max
    AdjustedqG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqGmatrix))
    
    jpeg('SelectivityGillNet.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedqG.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Adjusted q")+
           #scale_colour_grey()+
           ggtitle("Selectivity Patterns by Year and Age of Lake Whitefish in WFM03")+
           theme(axis.title=element_text(size=20),axis.text=element_text(size=20)))
    dev.off()
    #############################################################################
    
    #CATCHABILITY
    #############################################################################
    #Function from colorspace I needed to specify color scale:
    pal<-function (n, h = c(300, 123), c = 74, l = c(33, 85), power = 0.955555555555556, 
                   fixup = TRUE, gamma = NULL, alpha = 1, ...) 
    {
      if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
      if (n < 1L) 
        return(character(0L))
      h <- rep(h, length.out = 2L)
      c <- c[1L]
      l <- rep(l, length.out = 2L)
      power <- rep(power, length.out = 2L)
      rval <- seq(1, -1, length = n)
      rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                           C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                                   h[2L])), fixup = fixup, ...)
      if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                        width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
      }
      return(rval)
    }
    
    qG <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qG"),1])
    qGmatrix<-matrix(qG,nrow=length(ryears),ncol=length(ages))
    qG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix))
    jpeg('CatchabilityGillNet.jpg',width=1200,height=600)
    plot(ggplot(data=qG.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=pal(length(ages)))+
           xlab("Years")+
           ylab("Catchability")+
           ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           theme(axis.title=element_text(size=20),axis.text=element_text(size=20)))
    dev.off()
    
    qT <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qT"),1])
    qTmatrix<-matrix(qT,nrow=length(ryears),ncol=length(asymptoteages))
    qT.df <- data.frame(year=rep(ryears,length(asymptoteages)),age=rep(asymptoteages,each=length(ryears)),catchability=c(qTmatrix))
    jpeg('CatchabilityTrapNet.jpg',width=1200,height=600)
    plot(ggplot(data=qT.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=pal(length(asymptoteages)))+
           xlab("Years")+
           ylab("Catchability")+
           ggtitle("Catchability of Trap Net by Age and Year of Lake Whitefish in WFM03")+
           theme(axis.title=element_text(size=20),axis.text=element_text(size=20)))
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
    
    jpeg('Mortality.jpg',width=1200,height=900)
    plot(ggplot(Z.df,aes(ryears,morts))+
           theme_bw()+
           geom_bar(stat="identity",aes(fill=Z.type))+
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           ggtitle("Estimated Mortality (Natural, Trap, Gill) by year in WFM03")+
           scale_fill_grey()+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15),legend.text=element_text(size=15)))
    dev.off()                  
    #############################################################################
    
    #OBSERVED VERSUS PREDICTED HARVEST
    #############################################################################
    ObsT<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsT"),2]/1000
    ObsG<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsG"),2]/1000
    
    PredT <- summary(sdr)[which(row.names(summary(sdr))=="CT"),1]/1000
    PredG <- summary(sdr)[which(row.names(summary(sdr))=="CG"),1]/1000
    
    jpeg('PredictedTrapHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsT),aes(x=years,y=ObsT),color="black",size=2)+
           geom_line(data=data.frame(ryears,PredT),aes(x=ryears,y=PredT),color="black",linetype=2,size=2)+
           theme_bw()+
           xlab("Years")+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           ggtitle("Observed (solid) and Estimated (dashed) Lake Whitefish Trap Net Harvest in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15)))
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(data=data.frame(ryears,PredG),aes(x=ryears,y=PredG),color="black",linetype=2,size=2)+
           theme_bw()+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           ggtitle("Observed (solid) and Estimated (dashed) Lake Whitefish Gill Net Harvest in WFM03")+
           theme(axis.title=element_text(size=15),axis.text=element_text(size=15)))
    dev.off()
    #############################################################################
    
  }
  setwd(originalwd)
}
#############################################################################

# MAKING ALL THE GRAPHS AND PLOTS FOR RESIDUAL EXAMINATION
#############################################################################
saveresiduals <- function(originalwd,newwd,sdr,data)
{
  setwd(newwd)
  years=data$years
  ryears=data$ryears
  lyears=data$lyears
  asymptoteage=data$asymptoteage
  asymptoteages=data$asymptoteages
  ages=data$ages
  
  # Extract all the important residual information from the output and organize accordingly
  residCT <- summary(sdr)[which(row.names(summary(sdr))=="residCT"),1]
  residCG <- summary(sdr)[which(row.names(summary(sdr))=="residCG"),1]
  residPAT <- summary(sdr)[which(row.names(summary(sdr))=="residPAT"),1]
  residPAT.mat <-matrix(residPAT,nrow=length(ryears),ncol=length(ages))
  colnames(residPAT.mat) <- c(ages)
  rownames(residPAT.mat) <- c(ryears)
  residPAG <- summary(sdr)[which(row.names(summary(sdr))=="residPAG"),1]
  residPAG.mat <- matrix(residPAG,nrow=length(ryears),ncol=length(ages))
  colnames(residPAG.mat) <- c(ages)
  rownames(residPAG.mat) <- c(ryears)
  resid_effT <- summary(sdr)[which(row.names(summary(sdr))=="resid_effT"),1]
  resid_effT.mat <- matrix(resid_effT,nrow=(length(ryears)-1),ncol=length(asymptoteages))
  colnames(resid_effT.mat) <- c(asymptoteages)
  rownames(resid_effT.mat) <- ryears[1:length(ryears)-1]
  resid_effG <- summary(sdr)[which(row.names(summary(sdr))=="resid_effG"),1]
  resid_effG.mat <- matrix(resid_effG,nrow=(length(ryears)-1),ncol=length(ages))
  colnames(resid_effG.mat) <- c(ages)
  rownames(resid_effG.mat) <- ryears[1:length(ryears)-1]
  
  zeroline <- rep(0,length(ryears))
  
  jpeg("Gill Net Catch Residual.jpg",width=600,height=480)
  plot(ggplot()+geom_point(data=data.frame(residCG,ryears),aes(x=ryears,y=residCG),size=3,color="red")+
         geom_line(data=data.frame(ryears,zeroline),aes(x=ryears,y=zeroline),size=1)+
         theme_bw()+
         ggtitle("Gill Net Catch Residuals")+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  jpeg("Trap Net Catch Residual.jpg",width=600,height=480)
  plot(ggplot()+geom_point(data=data.frame(residCT,ryears),aes(x=ryears,y=residCT),size=3,color="red")+
         geom_line(data=data.frame(ryears,zeroline),aes(x=ryears,y=zeroline),size=1)+
         theme_bw()+
         ggtitle("Trap Net Catch Residuals")+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  #Function from colorspace I needed to specify color scale:
  pal<-function (n, h = c(300, 123), c = 74, l = c(33, 85), power = 0.955555555555556, 
                 fixup = TRUE, gamma = NULL, alpha = 1, ...) 
  {
    if (!is.null(gamma)) 
      warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
      return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- c[1L]
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
                         C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
                                                                 h[2L])), fixup = fixup, ...)
    if (!missing(alpha)) {
      alpha <- pmax(pmin(alpha, 1), 0)
      alpha <- format(as.hexmode(round(alpha * 255 + 0.0001)), 
                      width = 2L, upper.case = TRUE)
      rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
  }
  
  residPAT.df <- data.frame(residPAT,year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)))
  jpeg("Trap Net Proportion at Age Residual.jpg",width=1200,height=600)
  plot(ggplot(data=residPAT.df,aes(x=year,y=residPAT,color=as.factor(age)))+
         geom_point(size=2)+
         geom_line()+
         theme_bw()+
         scale_color_manual(values=pal(length(ages)))+
         ggtitle("Trap Net Proportion at Age Residuals")+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  residPAG.df <- data.frame(residPAG,year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)))
  jpeg("Gill Net Proportion at Age Residual.jpg",width=1200,height=600)
  plot(ggplot(data=residPAG.df,aes(x=year,y=residPAG,color=as.factor(age)))+
         geom_point(size=2)+
         geom_line()+
         theme_bw()+
         scale_color_manual(values=pal(length(ages)))+
         ggtitle("Gill Net Proportion at Age Residuals")+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  # Pull out the correlation values between years of catchability process error
  resid_effT.cor.logit <- summary(sdr)[which(row.names(summary(sdr))=="logit_rhoalphaT"),1]
  resid_effT.cor <- inv.logit(resid_effT.cor.logit)
  resid_effG.cor.logit <- summary(sdr)[which(row.names(summary(sdr))=="logit_rhoalphaG"),1]
  resid_effG.cor <- inv.logit(resid_effG.cor.logit)
  
  resid_effT.df <- data.frame(resid_effT,year=rep(ryears[1:length(ryears)-1],length(asymptoteages)),age=rep(asymptoteages,each=(length(ryears)-1)))
  jpeg("Trap Net Catchability Process Error Residuals.jpg",width=1200,height=600)
  plot(ggplot(data=resid_effT.df,aes(x=year,y=resid_effT,color=as.factor(age)))+
         geom_point(size=2)+
         geom_line()+
         theme_bw()+
         scale_color_manual(values=pal(length(asymptoteages)))+
         ggtitle(paste("Trap Net Catchability Process Error Residuals, correlation: ",round(resid_effT.cor,4),sep=""))+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  resid_effG.df <- data.frame(resid_effG,year=rep(ryears[1:length(ryears)-1],length(ages)),age=rep(ages,each=(length(ryears)-1)))
  jpeg("Gill Net Catchability Process Error Residuals.jpg",width=1200,height=600)
  plot(ggplot(data=resid_effG.df,aes(x=year,y=resid_effG,color=as.factor(age)))+
         geom_point(size=2)+
         geom_line()+
         theme_bw()+
         scale_color_manual(values=pal(length(ages)))+
         ggtitle(paste("Gill Net Catchability Process Error Residuals, correlation: ",round(resid_effG.cor,4),sep=""))+
         xlab("Years")+
         ylab("Residual"))
  dev.off()
  
  jpeg("Trap Net Catchability Residuals Correlation.jpg",width=1200,height=1200)
  par(mar=c(5.1,4.1,1.1,2.1))
  chart.Correlation(resid_effT.mat, histogram=F)
  mtext(paste("Expected Correlation: ",round(resid_effT.cor,4),sep=""),side=3)
  dev.off()
  
  jpeg("Gill Net Catchability Residuals Correlation.jpg",width=1200,height=1200)
  par(mar=c(5.1,4.1,1.1,2.1))
  chart.Correlation(resid_effG.mat, histogram=F)
  mtext(paste("Expected Correlation: ",round(resid_effG.cor,4),sep=""),side=3)
  dev.off()
  
  jpeg("Residuals Histograms.jpg",width=1200,height=1440)
  par(mfrow=c(3,2))
  hist(residCT,main="Trap Catch Residuals")
  hist(residCG,main="Gill Catch Residuals")
  hist(residPAT,main="Proportion at age Trap Residuals")
  hist(residPAG,main="Proportion at age Gill Residuals")
  hist(resid_effT,main="Trap Catchability Process Error Residuals")
  hist(resid_effG,main="Gill Catchability Process Error Residuals")
  dev.off()
  
  sink("Residuals_Shapiro_Test_Output.txt")
  cat("Trap Catch (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(residCT)))
  cat("\n")
  cat("Gill Catch (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(residCG)))
  cat("\n")
  cat("Trap Propotion at Age (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(residPAT)))
  cat("\n")
  cat("Gill Propotion at Age (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(residPAG)))
  cat("\n")
  cat("Trap Catchability Process Error (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(resid_effT)))
  cat("\n")
  cat("Gill Catchability Process Error (W then p-value): ")
  cat("\n")
  cat(toString(shapiro.test(resid_effG)))
  cat("\n")
  sink()
  
  
}
#############################################################################

# MAKING THE RETROSPECTIVE PLOTS THAT COMPARE GRAPHS ACROSS RETROS
#############################################################################
saveretroplots <- function(originalwd,retrowd,retrostart,retroend,data)
{
  retrowd=retrowd
  data=data
  retrostart=retrostart
  retroend=retroend
  
  setwd(retrowd)
  colorlength <- retrostart-retroend+1
  retroresults <- list()
  Retroabundance.plot <- ggplot()
  Retrorecruitment.plot <- ggplot()
  RetroSSB.plot <- ggplot()
  count <- 1
  for(i in retrostart:retroend)
  {
    yearlength <- i-data$fyear+1
    yearlengthvec <- 1:yearlength
    retroresults[[count]]= data.frame(read.csv(paste(retrowd,"/",i,"/","SDReport.csv",sep='')))
    
    RetroNmatrix<-matrix(retroresults[[count]][which(retroresults[[count]][,1]=="N"),2],nrow=yearlength,ncol=length(data$ages))
    RetroAbundance<- apply(RetroNmatrix,1,sum)/1000
    RetroRecruitment<-RetroNmatrix[,1]
    RetroSSB <-retroresults[[count]][which(retroresults[[count]][,1]=="SP_BIO"),2]*2.20462
    
    Retroabundance.plot <- Retroabundance.plot+
      geom_line(data=data.frame(yearlengthvec,RetroAbundance),aes(x=yearlengthvec,y=RetroAbundance),size=1,col=heat.colors(colorlength)[count])
    
    Retrorecruitment.plot <- Retrorecruitment.plot+
      geom_line(data=data.frame(yearlengthvec,RetroRecruitment),aes(x=yearlengthvec,y=RetroRecruitment),size=1,col=heat.colors(colorlength)[count])
    
    RetroSSB.plot <- RetroSSB.plot+
      geom_line(data=data.frame(yearlengthvec,RetroSSB),aes(x=yearlengthvec,y=RetroSSB),size=1,col=heat.colors(colorlength)[count])
    
    count=count+1
  }
  
  Retroabundance.plot<-Retroabundance.plot+xlab("Years")+ylab("Abundance")+theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                                                                                                 size = 2, linetype = "solid"))
  jpeg("AbundanceRetroplot.jpg",width=600,height=480)
  plot(Retroabundance.plot)
  dev.off()
  
  Retrorecruitment.plot<-Retrorecruitment.plot+xlab("Years")+ylab("Recruitment")+theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                                                                                                       size = 2, linetype = "solid"))
  jpeg("RecruitmentRetroplot.jpg",width=600,height=480)
  plot(Retrorecruitment.plot)
  dev.off()
  
  RetroSSB.plot<-RetroSSB.plot+xlab("Years")+ylab("Spawning Stock Biomass")+theme(panel.background = element_rect(fill = "#BFD5E3", colour = "#6D9EC1",
                                                                                                                  size = 2, linetype = "solid"))
  jpeg("SSBRetroplot.jpg",width=600,height=480)
  plot(RetroSSB.plot)
  dev.off()
  
  setwd(originalwd)
}
#############################################################################

# RUN SIMULATIONS, GENERATING DATA FROM MODEL PARAMETER OUTPUT
#############################################################################
simmodel <- function(data,obj,parameters,agecompfit,modelname,originalwd,newwd,estimateESS,maxreps,numsims=1,ESS_W_T_SIM=c(0.11,0.057))
{
  # Create a new folder and optional working directory to store just the results of the simulation
  simresultswd<-paste(newwd,"/simresults",sep='')
  if(!dir.exists(simresultswd)){dir.create(simresultswd)}
  
  # Set back to the original working directory where the important files are
  setwd(originalwd)
  
  # Pull out important factors from the data file like the number of years and ages
  ryears=data$ryears
  ages=data$ages
  mymap <- list(log_sig=as.factor(NA))
  
  # Make empty vectors to hold report objects and relative errors (between real values and those estimated from sim data)
  mysimstudy <- vector(mode="list",length=numsims)
  Mestimates <- c(NA,length=numsims)
  Abundancerelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  Recruitmentrelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  FGrelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  FTrelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  Biomassrelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  SPBiomassrelativeerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  
  setwd(simresultswd)
  sink("SimResults.txt",append=T)
  cat("Start of a new set of simulation results: ")
  cat("\n")
  sink()
  
  if(estimateESS==T)
  {
    sink("Weights.txt",append=T)
    sink()
  }
  setwd(originalwd)
  
  # Run through all the simulations:
  for(i in 1:numsims){
    
    #Simulate composition and yield data from the simulate function and fitted model object:
    simres <- obj$simulate()
    
    #Specify random effects, depending on age comp likelihood
    if(agecompfit==1){reffects <- c("log_rec","log_qG","log_qT")}
    if(agecompfit==2){reffects <- c("log_rec","log_qG","log_qT","log_gamT","log_gamG")}
    
    #Set most of the data the same (start by setting simdata to data), but replace the different composition and yield:
    #If you are planning to estimate the effective sample sizes, the intital run has to start with ESS=SS (same as real sample size)
    if(estimateESS==F)
    {
      simdata <- setdata(ESS_W_T=ESS_W_T_SIM[1],ESS_W_G=ESS_W_T_SIM[2])
    }
    else
    {
      simdata <- setdata(ESS_W_T=1,ESS_W_G=1)
    }
    # simdata <- setdata(ESS_W_T=2,ESS_W_G=2) # Option where ESS is twice the actual number of samples
    # simdata <- setdata(ESS_W_T=0.5,ESS_W_G=0.5) # Option where ESS is half the actual number of samples
    
    simdata$in_obs_PAT=as.matrix(simres$obs_PAT_sim)
    simdata$in_harv_wgtT=simres$harv_wgtT_sim
    simdata$in_obs_PAG=as.matrix(simres$obs_PAG_sim)
    simdata$in_harv_wgtG=simres$harv_wgtG_sim
    
    #Create model object from simulated data and same parameters
    simobj <- setmodel(simdata,parameters,agecompfit,modelname,mymapinput = mymap)
    
    #Fit and report the model
    # simfit <- nlminb(simobj$par, simobj$fn, simobj$gr,lower=lower_bounds,upper=upper_bounds)
    simfit <- nlminb(simobj$par, simobj$fn, simobj$gr)
    simsdr <- sdreport(simobj)
    
    # IF you are also calculating weights for composition data ESS, follow this track, else the weights stay as 1
    if(estimateESS==T){
      
      #Calculate the initial observed variance and ergo, the weights
      model_var<-calcvar(simsdr,simdata)
      ESS_w<-calcw(simsdr,simdata)
      if(ESS_w[1]>1){
        ESS_w[1]<-0.05
      }
      if(ESS_w[2]>1){
        ESS_w[2]<-0.05
      }
      
      #If the initial variance of the age composition (model_var) is not 1, proceed with the iterative reweighting procedure:
      if(model_var>1.05||model_var<0.95)
      {
        #To run the remainder of the ESS calclations, we have to fix the SD values at their initial estimated numbers
        log_sig_fix <- parameters$log_sig
        log_sdSR_fix <- summary(simsdr)[which(row.names(summary(simsdr))=="log_sdSR"),1]
        log_sd_logeffortT_fix <- summary(simsdr)[which(row.names(summary(simsdr))=="log_sd_logeffortT"),1]
        log_sd_logeffortG_fix <- summary(simsdr)[which(row.names(summary(simsdr))=="log_sd_logeffortG"),1]
        logit_rhoalphaT_fix <- summary(simsdr)[which(row.names(summary(simsdr))=="logit_rhoalphaT"),1]
        logit_rhoalphaG_fix <- summary(simsdr)[which(row.names(summary(simsdr))=="logit_rhoalphaG"),1]
        
        #While the variance is still not 1, recalculate the expected proportions at age and find results
        nreps=0
        while(model_var>1.05||model_var<0.95)
        {
          if(nreps>maxreps){break}
          simdata.ess<-setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2])
          
          simdata.ess$in_obs_PAT=as.matrix(simres$obs_PAT_sim)
          simdata.ess$in_harv_wgtT=simres$harv_wgtT_sim
          simdata.ess$in_obs_PAG=as.matrix(simres$obs_PAG_sim)
          simdata.ess$in_harv_wgtG=simres$harv_wgtG_sim
          
          simparameters.ess<-setparam(simdata.ess,agecompfit,log_sig = log_sig_fix,log_sdSR = log_sdSR_fix,log_sd_logeffortT = log_sd_logeffortT_fix,log_sd_logeffortG = log_sd_logeffortG_fix)
          simbounds.ess<-setbounds()
          
          simobj.ess<-setmodel(simdata.ess,simparameters.ess,agecompfit,modelname,mymapinput = list(log_sig=as.factor(NA),log_sdSR=as.factor(NA),log_sd_logeffortT=as.factor(NA),log_sd_logeffortG=as.factor(NA)))
          # obj.ess<-setmodel(data=data.ess,parameters=parameters.ess)
          simfit.ess<-fitmodel(obj=simobj.ess,bounds=simbounds.ess)
          simsdr.ess <- reportmodel(simobj.ess)
          
          model_var<-calcvar(sdr=simsdr.ess,data=simdata.ess) #Calculate the variance in age composition
          ESS_w<-calcw(sdr=simsdr.ess,data=simdata.ess)
          
          nreps=nreps+1
          if(ESS_w[1]>1){
            ESS_w[1]<-(1/nreps)
          }
          if(ESS_w[2]>1){
            ESS_w[2]<-(1/nreps)
          }
        }
        #At this point, the model either found a set of weights for which the estimated variance is close enough to 1, or fell out after 5 iterations
      }
      
      #Collect all the estimated ESS weights from the simulated data:
      setwd(simresultswd)
      sink("Weights.txt",append=T)
      cat("Weight for ESS of Trap Net of simulation number ")
      cat(i)
      cat("\n")
      cat(ESS_w[1])
      cat("\n")
      cat("Weight for ESS of Gill Net of simulation number ")
      cat(i)
      cat("\n")
      cat(ESS_w[2])
      cat("\n")
      cat("Model Variance of Trap Net of simulation number ")
      cat(i)
      cat("\n")
      cat(model_var[1])
      cat("\n")
      cat("Model Variance of Gill Net of simulation number ")
      cat(i)
      cat("\n")
      cat(model_var[2])
      cat("\n")
      cat("\n")
      sink()
      setwd(originalwd)
      
      if(model_var<1.05&&model_var>0.95)
      {
        #Repeat the whole modelling process with weights estimated from iterative reweighting:
        simdata <- setdata(ESS_W_T=ESS_w[1],ESS_W_G=ESS_w[2])
        simdata$in_obs_PAT=as.matrix(simres$obs_PAT_sim)
        simdata$in_harv_wgtT=simres$harv_wgtT_sim
        simdata$in_obs_PAG=as.matrix(simres$obs_PAG_sim)
        simdata$in_harv_wgtG=simres$harv_wgtG_sim
        
        #Create model object from simulated data and same parameters
        simobj <- setmodel(simdata,parameters,agecompfit,modelname,mymapinput = mymap)
        
        #Fit and report the model
        # simfit <- nlminb(simobj$par, simobj$fn, simobj$gr,lower=lower_bounds,upper=upper_bounds)
        simfit <- nlminb(simobj$par, simobj$fn, simobj$gr)
        simsdr <- sdreport(simobj)
      }
      else
      {
        print("Iterative reweighting failed to find weights where observed and expected variance match after 50 iterations")
      }
      
    }
    
    if(estimateESS==F||(estimateESS==T&&nreps<maxreps))
    {
      # Save the sdr object from fit of simulated data
      # mysimstudy[[i]] <- simsdr
      
      # Collect the absolute error of values of interest (M, N, Recruitment, F, Biomass)
      #M
      Mestimates[i] <- summary(simsdr)[which(row.names(summary(simsdr))=="M"),1]
      
      #N
      Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
      Abundance<- apply(Nmatrix,1,sum)/1000
      simNmatrix<-matrix(summary(simsdr)[which(row.names(summary(simsdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
      simAbundance<- apply(simNmatrix,1,sum)/1000
      Abundancerelativeerror[,i]<-(simAbundance-Abundance)/Abundance
      
      #Recruitment
      Recruitment <- Nmatrix[,1]/1000
      simRecruitment <- simNmatrix[,1]/1000
      Recruitmentrelativeerror[,i]<-(simRecruitment-Recruitment)/Recruitment
      
      #F
      FG<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
      FG_Avg<-apply(FG,1,mean)
      FT<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
      FT_Avg<-apply(FT,1,mean)
      simFG<-matrix(summary(simsdr)[which(row.names(summary(simsdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
      simFG_Avg<-apply(simFG,1,mean)
      simFT<-matrix(summary(simsdr)[which(row.names(summary(simsdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
      simFT_Avg<-apply(simFT,1,mean)
      FGrelativeerror[,i]<-(simFG_Avg-FG_Avg)/FG_Avg
      FTrelativeerror[,i]<-(simFT_Avg-FT_Avg)/FT_Avg
      
      #Biomass
      BIOMASS <- summary(sdr)[which(row.names(summary(sdr))=="BIOMASS"),1]/1000*2.20462
      SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
      simBIOMASS <- summary(simsdr)[which(row.names(summary(simsdr))=="BIOMASS"),1]/1000*2.20462
      simSP_BIO <-summary(simsdr)[which(row.names(summary(simsdr))=="SP_BIO"),1]/1000*2.20462
      Biomassrelativeerror[,i]<-(simBIOMASS-BIOMASS)/BIOMASS
      SPBiomassrelativeerror[,i]<-(simSP_BIO-SP_BIO)/SP_BIO
      
      setwd(simresultswd)
      sink("SimResults.txt",append=T)
      cat("Simulation: ")
      cat(i)
      cat(" ")
      cat(Abundancerelativeerror[,i])
      cat(" ")
      cat(Biomassrelativeerror[,i])
      cat(" ")
      cat(FGrelativeerror[,i])
      cat(" ")
      cat(FTrelativeerror[,i])
      cat(" ")
      cat(Mestimates[i])
      cat(" ")
      cat(Recruitmentrelativeerror[,i])
      cat(" ")
      cat(SPBiomassrelativeerror[,i])
      cat("\n")
      sink()
      setwd(originalwd)
    }
    
    #Just to be sure the simulation process begins afresh, remove all previous objects
    rm("simres","simdata","simobj","simfit","simsdr")
  }
  
  #At this point, all the simulations have been iterated through and errors calculated.
  #What follows are the boxplots of the errors:
  
  setwd(simresultswd)
  # Plot boxplot of M Error
  jpeg("MEstimatesBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=data.frame(Mestimates),aes(Mestimates))+
         geom_boxplot()+
         ylab("Estimated M")+
         xlab(" ")+
         scale_fill_grey())
  dev.off()
  
  # Calculate and Plot boxplot of N Error
  Abundancerelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    Abundancerelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Abundancerelativeerror[i,]
    Abundancerelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Abundancerelativeerror.df)=c("Difference","Years")
  jpeg("AbundanceRelativeErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Abundancerelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated N"))
  dev.off()
  
  # Calculate and Plot boxplot of Recruitment Error
  Recruitmentrelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    Recruitmentrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Recruitmentrelativeerror[i,]
    Recruitmentrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Recruitmentrelativeerror.df)=c("Difference","Years")
  jpeg("RecruitmentErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Recruitmentrelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated Recruitment"))
  dev.off()
  
  # Calculate and Plot boxplot of Gill Net Mortality Error
  FGrelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    FGrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- FGrelativeerror[i,]
    FGrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(FGrelativeerror.df)=c("Difference","Years")
  jpeg("FGRelativeErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=FGrelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated Gill Net Mortality"))
  dev.off()
  
  # Calculate and Plot boxplot of Trap Net Mortality Error
  FTrelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    FTrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- FTrelativeerror[i,]
    FTrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(FTrelativeerror.df)=c("Difference","Years")
  jpeg("FTRelativeErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=FTrelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated Trap Net Mortality"))
  dev.off()
  
  # Calculate and Plot boxplot of Biomass Error
  Biomassrelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    Biomassrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Biomassrelativeerror[i,]
    Biomassrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Biomassrelativeerror.df)=c("Difference","Years")
  jpeg("BiomassRelativeErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Biomassrelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated Biomass"))
  dev.off()
  
  # Calculate and Plot boxplot of Biomass Error
  SPBiomassrelativeerror.df=data.frame()
  for(i in 1:length(ryears)){
    SPBiomassrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- SPBiomassrelativeerror[i,]
    SPBiomassrelativeerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(SPBiomassrelativeerror.df)=c("Difference","Years")
  jpeg("SPBiomassRelativeErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=SPBiomassrelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Relative Error of Estimated Spawning Stock Biomass"))
  dev.off()
  
  #Set original working directory and return the large ob
  setwd(originalwd)
  # print(mysimstudy)
  return(mysimstudy)
}
#############################################################################
