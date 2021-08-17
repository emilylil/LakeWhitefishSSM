#WFM_03_2018_TMB_SSM_Functions.R

# DATA SECTION
############################################################################
setdata <- function(retroyear=2017,ESS_W_T=1,ESS_W_G=1)
{
  fyear <- 1986
  lyear <- 2017
  lryear <- retroyear
  
  fage <- 4
  lage <- 15
  
  targ_age <- 4
  # asymptoteage <- 9
  asymptoteage <- 15
  
  years <- c(fyear:lyear)
  ryears <- c(fyear:lryear)
  ages <- c(fage:lage)
  asymptoteages <- c(fage:asymptoteage)
  
  sp_time <- 0.838
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
  
  lnmedM <- - 1.60944
  sdM <- 0.1
  
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
  in_N_SampT <- round(in_NtildeT*ESS_W_T)
  
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
  in_N_SampG <- round(in_NtildeG*ESS_W_G)
  
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
  
  return(data<-list(fyear=fyear,lyear=lyear,lryear=lryear,
                    fage=fage,lage=lage,targ_age=targ_age,
                    asymptoteage=asymptoteage,
                    years=years,ryears=ryears,ages=ages,
                    asymptoteages=asymptoteages,
                    sp_time=sp_time,
                    in_watage=as.matrix(in_watage),
                    in_latage=as.matrix(in_latage),
                    in_mat=as.matrix(in_mat),
                    lnmedM=lnmedM,
                    sdM=sdM,
                    in_obs_PAT=as.matrix(in_obs_PAT),in_NtildeT=in_NtildeT,in_N_SampT=in_N_SampT,
                    in_harv_wgtT=in_harv_wgtT,in_mnwgtT=in_mnwgtT,in_effortT=in_effortT,
                    in_obs_PAG=as.matrix(in_obs_PAG),in_NtildeG=in_NtildeG,in_N_SampG=in_N_SampG,
                    in_harv_wgtG=in_harv_wgtG,in_mnwgtG=in_mnwgtG,in_effortG=in_effortG,in_effort_adjust=in_effort_adjust,
                    in_Tharv_adjust=in_Tharv_adjust,in_Gharv_adjust=in_Gharv_adjust,
                    percent_female=percent_female,eggs_per_kg=eggs_per_kg,
                    reflenG=reflenG,reflenT=reflenT))
  
}
#############################################################################

#PARAMETER SECTION Recruitment (N1,j and Ni,1), and catchability (qi,j) are RE
#############################################################################
setparam <- function(data,agecompfit,log_sig = -2.70663320551,log_sdSR = -1.279150914,log_sd_logeffortT = -0.99251027,log_sd_logeffortG = -1.128061467,logit_rhoalphaT = 1.755746654,logit_rhoalphaG = 1.920252761,log_sd_logcatchT = -0.99251027,log_sd_logcatchG = -1.128061467,logit_rhoalphacatchT = 1.755746654,logit_rhoalphacatchG = 1.920252761)
{
  # As much as possible (where it matches with the ADMB file) the starting values
  # match the estimated values in the ADMB version from PAR file
  
  log_sig <- log_sig #Equivalent of variance = 0.07 for obs error
  log_sdSR <-   log_sdSR
  log_sd_logeffortT <- log_sd_logeffortT
  log_sd_logeffortG <- log_sd_logeffortG
  logit_rhoalphaT <-  logit_rhoalphaT
  logit_rhoalphaG <- logit_rhoalphaG
  lnM <- -1.661196184
  lnZavg <- -0.175479758
  
  log_qT <- matrix(rep(-3.81198897035,length(data$asymptoteages)*length(data$ryears)),nrow=length(data$ryears),ncol=length(data$asymptoteages))
  log_qG <- matrix(rep(-2.28728684774,length(data$ages)*length(data$ryears)),nrow=length(data$ryears),ncol=length(data$ages))
  
  log_rec <- c(13.5108817,13.13655618,12.21723511,10.7478461,
               9.409084668,8.816328425,13.02347741,12.43817197,
               12.66479096,13.61552429,13.84635009,13.99946412,
               13.64494878,13.45569927,14.25063155,14.07533313,
               14.03520818,13.95933381,13.96854919,13.86158275,
               13.64046765,13.56524758,13.38284896,13.90563459,
               14.20864664,14.03419619,13.82893998,13.63564438,
               13.49221651,13.23613482,13.05781846,12.93411147,
               12.91673307,12.85074624,13.10283799,12.65235975,
               12.71099085)
  log_rec <- log_rec[1:(data$lryear+data$lage-data$fage-6-data$fyear+1)] #Truncate the list according to how many recruitment events
  
  #Return a list of parameters
  if(agecompfit==1){
    # return(parameters <- list(log_sig=log_sig,log_sdSR=log_sdSR,
    #                    log_sd_logeffortT=log_sd_logeffortT,
    #                    log_sd_logeffortG=log_sd_logeffortG,
    #                    logit_rhoalphaT=logit_rhoalphaT,
    #                    logit_rhoalphaG=logit_rhoalphaG,
    #                    lnM=lnM,log_qT=log_qT,
    #                    log_qG=log_qG,
    #                    log_rec=log_rec,lnZavg=lnZavg))
    return(parameters <- list(log_sig=log_sig,log_sdSR=log_sdSR,
                              log_sd_logeffortT=log_sd_logeffortT,
                              log_sd_logeffortG=log_sd_logeffortG,
                              logit_rhoalphaT=logit_rhoalphaT,
                              logit_rhoalphaG=logit_rhoalphaG,
                              lnM=lnM,log_qT=log_qT,
                              log_qG=log_qG,
                              log_rec=log_rec))
  }
  
  log_sd_logcatchT <- log_sd_logcatchT
  log_sd_logcatchG <- log_sd_logcatchG
  logit_rhoalphacatchT <-  logit_rhoalphacatchT
  logit_rhoalphacatchG <- logit_rhoalphacatchG
  
  if(agecompfit==2){
    # return(parameters <- list(log_sig=log_sig,log_sdSR=log_sdSR,
    #                    log_sd_logeffortT=log_sd_logeffortT,
    #                    log_sd_logeffortG=log_sd_logeffortG,
    #                    logit_rhoalphaT=logit_rhoalphaT,
    #                    logit_rhoalphaG=logit_rhoalphaG,
    #                    lnM=lnM,log_qT=log_qT,
    #                    log_qG=log_qG,
    #                    log_rec=log_rec,lnZavg=lnZavg))
    return(parameters <- list(log_sdSR=log_sdSR,
                              log_sd_logeffortT=log_sd_logeffortT,
                              log_sd_logeffortG=log_sd_logeffortG,
                              logit_rhoalphaT=logit_rhoalphaT,
                              logit_rhoalphaG=logit_rhoalphaG,
                              log_sd_logcatchT=log_sd_logcatchT,
                              log_sd_logcatchG=log_sd_logcatchG,
                              logit_rhoalphacatchT=logit_rhoalphacatchT,
                              logit_rhoalphacatchG=logit_rhoalphacatchG,
                              lnM=lnM,log_qT=log_qT,
                              log_qG=log_qG,
                              log_rec=log_rec))
  }
  
  #This section for if you need the extra parameters for the cLGCP version
  log_std_log_gamT=rep(-2,length(data$ages))
  log_std_log_gamG=rep(-2,length(data$ages))
  log_gamT=matrix(0,nrow=length(data$ryears),ncol=length(data$ages))
  log_gamG=matrix(0,nrow=length(data$ryears),ncol=length(data$ages))
  
  if(agecompfit==3){
    return(parameters <- list(log_sig=log_sig,log_sdSR=log_sdSR,
                              log_sd_logeffortT=log_sd_logeffortT,
                              log_sd_logeffortG=log_sd_logeffortG,
                              logit_rhoalphaT=logit_rhoalphaT,
                              logit_rhoalphaG=logit_rhoalphaG,
                              lnM=lnM,log_qT=log_qT,
                              log_qG=log_qG,
                              log_rec=log_rec,lnZavg=lnZavg,
                              log_std_log_gamT=log_std_log_gamT,
                              log_std_log_gamG=log_std_log_gamG,
                              log_gamT=log_gamT,
                              log_gamG=log_gamG))
  }
  
  
}
#############################################################################

#BOUNDS SECTION 
#############################################################################
setbounds <- function(agecompfit)
{

  #Upper Bounds
  log_sig_upper <- 5
  log_sdSR_upper <- 5
  log_sd_logeffortT_upper <- 5
  log_sd_logeffortG_upper <- 5
  logit_rhoalphaT_upper <- 100
  logit_rhoalphaG_upper <- 100
  
  log_sd_logcatchT_upper <- 5
  log_sd_logcatchG_upper <- 5
  logit_rhoalphacatchT_upper <- 100
  logit_rhoalphacatchG_upper <- 100
  
  lnM_upper <- 5
  lnZavg_upper <- 5
  
  # When they aren't specified as random effects, the process error of q and rec need bounds:
  # log_qT_upper <- matrix(rep(1,length(ages)*length(years)),nrow=length(years),ncol=length(ages))
  # log_qG_upper <- matrix(rep(1,length(ages)*length(years)),nrow=length(years),ncol=length(ages))
  # 
  # log_rec_upper <- rep(15,lyear+lage-fage-6-fyear+1)
  
  # upper_bounds <- c(log_sig_upper=log_sig_upper,log_sdSR_upper=log_sdSR_upper,
  #                   log_sd_logeffortT_upper=log_sd_logeffortT_upper,
  #                   log_sd_logeffortG_upper=log_sd_logeffortG_upper,
  #                   logit_rhoalphaT_upper=logit_rhoalphaT_upper,
  #                   logit_rhoalphaG_upper=logit_rhoalphaG_upper,
  #                   lnM_upper=lnM_upper,lnZavg_upper=lnZavg_upper)
  if(agecompfit==1)
  {
    upper_bounds <- c(log_sig_upper=log_sig_upper,log_sdSR_upper=log_sdSR_upper,
                    log_sd_logeffortT_upper=log_sd_logeffortT_upper,
                    log_sd_logeffortG_upper=log_sd_logeffortG_upper,
                    logit_rhoalphaT_upper=logit_rhoalphaT_upper,
                    logit_rhoalphaG_upper=logit_rhoalphaG_upper,
                    lnM_upper=lnM_upper)
  }
  if(agecompfit==2)
  {
    upper_bounds <- c(log_sdSR_upper=log_sdSR_upper,
                      log_sd_logeffortT_upper=log_sd_logeffortT_upper,
                      log_sd_logeffortG_upper=log_sd_logeffortG_upper,
                      logit_rhoalphaT_upper=logit_rhoalphaT_upper,
                      logit_rhoalphaG_upper=logit_rhoalphaG_upper,
                      log_sd_logcatchT_upper=log_sd_logcatchT_upper,
                      log_sd_logcatchG_upper=log_sd_logcatchG_upper,
                      logit_rhoalphacatchT_upper=logit_rhoalphacatchT_upper,
                      logit_rhoalphacatchG_upper=logit_rhoalphacatchG_upper,
                      lnM_upper=lnM_upper)
  }
  
  #Lower Bounds
  log_sig_lower <- -10
  log_sdSR_lower <- -75
  log_sd_logeffortT_lower <- -20
  log_sd_logeffortG_lower <- -20
  logit_rhoalphaT_lower <- -100
  logit_rhoalphaG_lower <- -100
  
  log_sd_logcatchT_lower <- -20
  log_sd_logcatchG_lower <- -20
  logit_rhoalphacatchT_lower <- -100
  logit_rhoalphacatchG_lower <- -100
  
  lnM_lower <- -5
  lnZavg_lower <- -5
  
  # When they aren't specified as random effects, the process error of q and rec need bounds:
  # log_qT_lower <- matrix(rep(-20,length(ages)*length(years)),nrow=length(years),ncol=length(ages))
  # log_qG_lower <- matrix(rep(-20,length(ages)*length(years)),nrow=length(years),ncol=length(ages))
  
  # log_rec_lower <- rep(-15,lyear+lage-fage-6-fyear+1)
  
  # lower_bounds <- c(log_sig_lower=log_sig_lower,log_sdSR_lower=log_sdSR_lower,
  #                   log_sd_logeffortT_lower=log_sd_logeffortT_lower,
  #                   log_sd_logeffortG_lower=log_sd_logeffortG_lower,
  #                   logit_rhoalphaT_lower=logit_rhoalphaT_lower,
  #                   logit_rhoalphaG_lower=logit_rhoalphaG_lower,
  #                   lnM_lower=lnM_lower,lnZavg_lower=lnZavg_lower)
  
  if(agecompfit==1)
  {
    lower_bounds <- c(log_sig_lower=log_sig_lower,log_sdSR_lower=log_sdSR_lower,
                    log_sd_logeffortT_lower=log_sd_logeffortT_lower,
                    log_sd_logeffortG_lower=log_sd_logeffortG_lower,
                    logit_rhoalphaT_lower=logit_rhoalphaT_lower,
                    logit_rhoalphaG_lower=logit_rhoalphaG_lower,
                    lnM_lower=lnM_lower)
  }
  if(agecompfit==2)
  {
    lower_bounds <- c(log_sdSR_lower=log_sdSR_lower,
                      log_sd_logeffortT_lower=log_sd_logeffortT_lower,
                      log_sd_logeffortG_lower=log_sd_logeffortG_lower,
                      logit_rhoalphaT_lower=logit_rhoalphaT_lower,
                      logit_rhoalphaG_lower=logit_rhoalphaG_lower,
                      log_sd_logcatchT_lower=log_sd_logcatchT_lower,
                      log_sd_logcatchG_lower=log_sd_logcatchG_lower,
                      logit_rhoalphacatchT_lower=logit_rhoalphacatchT_lower,
                      logit_rhoalphacatchG_lower=logit_rhoalphacatchG_lower,
                      lnM_lower=lnM_lower)
  }
  
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
  
  # reffects <- c("log_rec")
  if(agecompfit==1){reffects <- c("log_rec","log_qG","log_qT")}
  if(agecompfit==2){reffects <- c("log_rec","log_qG","log_qT")}
  if(agecompfit==3){reffects <- c("log_rec","log_qG","log_qT","log_gamT","log_gamG")}
  
  # MAPPING:
  if(agecompfit==1){mymap <- mymapinput
                    #mymap<-c()
                    }
  if(agecompfit==2){mymap <- c()}
  # mymap = list(log_sig=as.factor(NA))
  # Add mapping such that log_std_log_gamT and G are not age-varying
  # Could add even more nuance if you think there's groupings for certain ages (1-3, 4-6, etc.)
  # mymap <- list(log_sig=as.factor(NA),log_std_log_gamT=factor(rep("all"),length(data$ages)),log_std_log_gamG=factor(rep("all"),length(data$ages)))
  
  #Make the TMB object (Builds the bridge between DLL and R functions and objects)
  # No random effects, no map:
  # obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB")
  # Yes random effects, no map:
  # obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB",random=reffects)
  # No random effects, yes map:
  # obj <- MakeADFun(data,parameters,DLL="WFM03_2018_TMB", map=mymap)
  # Yes random effects, yes map:
  obj <- MakeADFun(data,parameters,DLL=modelname,random=reffects, map=mymap)
  
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
  fit<-tryCatch({fit <- nlminb(obj$par, obj$fn, obj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)},
          warning=function(w){NA},
          error=function(e){NA},
          finally=function(f){NA}
          )
  # print(fit)
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
  sdr<-tryCatch({sdr <- sdreport(obj)},
                warning=function(w){NA},
                error=function(e){NA},
                finally=function(f){NA}
  )
  if(is.na(sdr))
  {
    print("Something messed up making the sdreport")
  }
  else
  {
    # Secondary convergence message:
    print("Did the model converge: ")
    print(sdr$pdHess)
    # # Look at sdr:
    # summary(sdr)
    # head(summary(sdr))
  }
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
savegraphspresentation<-function(checkagainstADMB=F,oridinalwd,newwd,sdr,data)
{
  prestheme <- theme(axis.title=element_text(size=40),axis.text=element_text(size=40),
                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                     panel.background = element_blank(), axis.line = element_line(colour = "black"),
                     axis.text.x = element_text(color="black",size=30),
                     axis.text.y = element_text(color="black",size=30,angle=90,hjust=0.5))
  
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
           prestheme) 
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
           prestheme) #For presentations
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
           prestheme) 
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
           prestheme) #For presentations
    dev.off()
    #############################################################################
    
    #SELECTIVITY CURVES
    #############################################################################
    #SSM
    FTmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
    FTmatrix.max <- apply(FTmatrix,1,max)
    AdjustedqTmatrix <- FTmatrix/FTmatrix.max
    AdjustedqT.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqTmatrix))
    
    jpeg('SelectivityTrapNetSSM.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Normalized Catchability")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    FGmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
    FGmatrix.max <- apply(FGmatrix,1,max)
    AdjustedqGmatrix <- FGmatrix/FGmatrix.max
    AdjustedqG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqGmatrix))
    
    jpeg('SelectivityGillNetSSM.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedqG.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Normalized Catchability")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    #SCA
    SelT.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelT"),2]
    SelTmatrix.admb<-matrix(SelT.admb,nrow=length(ryears),ncol=length(ages))
    SelTmatrix.matrix.max <- apply(SelTmatrix.admb,1,max)
    AdjustedSelTmatrix.admb <- SelTmatrix.admb/SelTmatrix.matrix.max
    AdjustedSelT.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(AdjustedSelTmatrix.admb))
    jpeg('SelectivityTrapNetSCAAdjusted.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedSelT.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Selectivity")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    SelT.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelT"),2]
    SelTmatrix.admb<-matrix(SelT.admb,nrow=length(ryears),ncol=length(ages))
    SelTmatrix.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(SelTmatrix.admb))
    jpeg('SelectivityTrapNetSCA.jpg',width=1200,height=600)
    plot(ggplot(data=SelTmatrix.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Selectivity")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    SelG.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelG"),2]
    SelGmatrix.admb<-matrix(SelG.admb,nrow=length(ryears),ncol=length(ages))
    SelGmatrix.matrix.max <- apply(SelGmatrix.admb,1,max)
    AdjustedSelGmatrix.admb <- SelGmatrix.admb/SelGmatrix.matrix.max
    AdjustedSelG.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(AdjustedSelGmatrix.admb))
    jpeg('SelectivityGillNetSCAAdjusted.jpg',width=1200,height=600)
    plot(ggplot(data=AdjustedSelG.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Selectivity")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    SelG.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="SelG"),2]
    SelGmatrix.admb<-matrix(SelG.admb,nrow=length(ryears),ncol=length(ages))
    SelGmatrix.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),selectivity=c(SelGmatrix.admb))
    jpeg('SelectivityGillNetSCA.jpg',width=1200,height=600)
    plot(ggplot(data=SelGmatrix.admb.df,aes(x=age,y=selectivity,color=as.factor(year)))+
           geom_line(size=1.25)+
           theme_bw()+
           xlab("Ages")+
           ylab("Selectivity")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    #############################################################################
    
    #CATCHABILITY
    #############################################################################
    #Specify color palette for catchability plots
    coul<-brewer.pal(10,"Spectral")
    
    #SSM
    qG <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qG"),1])
    qGmatrix<-matrix(qG,nrow=length(ryears),ncol=length(ages))
    qG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix))
    jpeg('CatchabilityGillNetSSM.jpg',width=1200,height=600)
    plot(ggplot(data=qG.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    qT <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qT"),1])
    qTmatrix<-matrix(qT,nrow=length(ryears),ncol=length(asymptoteages))
    qT.df <- data.frame(year=rep(ryears,length(asymptoteages)),age=rep(asymptoteages,each=length(ryears)),catchability=c(qTmatrix))
    jpeg('CatchabilityTrapNetSSM.jpg',width=1200,height=600)
    plot(ggplot(data=qT.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(asymptoteages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Trap Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    #SCA
    qG.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="qG"),2]
    qGmatrix.admb <- SelGmatrix.admb*0
    for(i in 1:nrow(SelGmatrix.admb))
    {
      qGmatrix.admb[i,]<-SelGmatrix.admb[i,]*qG.admb[i]
    }
    qG.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix.admb))
    jpeg('CatchabilityGillNetSCA.jpg',width=1200,height=600)
    plot(ggplot(data=qG.admb.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    qT.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="qT"),2]
    qTmatrix.admb <- SelTmatrix.admb*0
    for(i in 1:nrow(SelTmatrix.admb))
    {
      qTmatrix.admb[i,]<-SelTmatrix.admb[i,]*qT.admb[i]
    }
    qT.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qTmatrix.admb))
    jpeg('CatchabilityTrapNetSCA.jpg',width=1200,height=600)
    plot(ggplot(data=qT.admb.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme+theme(legend.text = element_text( size = 24)))
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
           scale_fill_manual(values=c("blueviolet","blue","lightblue"))+
           prestheme) #For Presentations
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
           scale_fill_grey()+
           prestheme) #For presentations
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
           prestheme) #For presentations
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(data=data.frame(years,PredG.admb),aes(x=years,y=PredG.admb),color="darkgoldenrod1",linetype=2,size=2)+
           geom_line(data=data.frame(ryears,PredG),aes(x=ryears,y=PredG),color="red",linetype=2,size=2)+
           theme_bw()+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           # ggtitle("Observed and Predicted Lake Whitefish Gill Net Harvest in WFM03, Yellow-Obs, Blue-SSM, Black-ADMB")+ #For self
           prestheme) #For presentations
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
           # ggtitle("Estimated lake whitefish abundance in WFM03")+
           prestheme)
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
           # ggtitle("Estimated Lake Whitefish Biomass in WFM03")+
           prestheme)
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
           # ggtitle("Estimated Lake Whitefish Spawning Stock Biomass in WFM03")+
           prestheme)
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
           # ggtitle("Estimated Lake Whitefish Age-4 Recruits in WFM03")+
           prestheme)
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
           # ggtitle("Selectivity Patterns by Year and Age of Lake Whitefish in WFM03")+
           prestheme)
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
           # ggtitle("Selectivity Patterns by Year and Age of Lake Whitefish in WFM03")+
           prestheme)
    dev.off()
    #############################################################################
    
    #CATCHABILITY
    #############################################################################
    #Specify color palette for catchability plots
    coul<-brewer.pal(10,"Spectral")
    
    qG <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qG"),1])
    qGmatrix<-matrix(qG,nrow=length(ryears),ncol=length(ages))
    qG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix))
    jpeg('CatchabilityGillNet.jpg',width=1200,height=600)
    plot(ggplot(data=qG.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme)
    dev.off()
    
    qT <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qT"),1])
    qTmatrix<-matrix(qT,nrow=length(ryears),ncol=length(asymptoteages))
    qT.df <- data.frame(year=rep(ryears,length(asymptoteages)),age=rep(asymptoteages,each=length(ryears)),catchability=c(qTmatrix))
    jpeg('CatchabilityTrapNet.jpg',width=1200,height=600)
    plot(ggplot(data=qT.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           theme_bw()+
           scale_color_manual(values=colorRampPalette(coul)(length(asymptoteages)))+
           xlab("Years")+
           ylab("Catchability")+
           # ggtitle("Catchability of Trap Net by Age and Year of Lake Whitefish in WFM03")+
           prestheme)
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
           # ggtitle("Estimated Mortality (Natural, Trap, Gill) by year in WFM03")+
           scale_fill_grey()+
           prestheme)
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
           # ggtitle("Observed (solid) and Estimated (dashed) Lake Whitefish Trap Net Harvest in WFM03")+
           prestheme)
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=600)
    plot(ggplot()+geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(data=data.frame(ryears,PredG),aes(x=ryears,y=PredG),color="black",linetype=2,size=2)+
           theme_bw()+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           # ggtitle("Observed (solid) and Estimated (dashed) Lake Whitefish Gill Net Harvest in WFM03")+
           prestheme)
    dev.off()
    #############################################################################
    
  }
  setwd(originalwd)
}
#############################################################################

# MAKING ALL THE GRAPHS FOR AN INDIVIDUAL RUN
#############################################################################
savegraphspublication<-function(checkagainstADMB=F,oridinalwd,newwd,sdr,data)
{
  pubtheme <- theme_classic()+theme(axis.title=element_text(size=32),axis.text=element_text(size=32),
                    text = element_text(family="serif"),
                    legend.text=element_text(size=32),
                    legend.title=element_text(size=32),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(color="black",size=24),
                    axis.text.y = element_text(color="black",size=24,angle=90,hjust=0.5))

  # Something I found in Glassic et al. 2019
  # pubtheme <- theme_classic()+theme (axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 10, b = 0, l = 0), colour = "black"),
  #                    axis.title.x = element_text(size = 14, margin = margin(t = 10, r = 0, b = 0, l = 0), colour = "black"),
  #                    #set the font type
  #                    text = element_text(family="Times New Roman"),
  #                    #modify plot title, the B in this case
  #                    plot.title = element_text(face="bold",family = "Arial"),
  #                    #position the legend on the figure
  #                    legend.position = c(1,0.5),
  #                    #adjust size of text for legend
  #                    legend.text = element_text(size = 10),
  #                    #margin for the plot
  #                    plot.margin = unit(c(0, 0, 0, 0), "cm"),
  #                    #set size of the tick marks for y-axis
  #                    axis.ticks.y = element_line(size = 0.5),
  #                    #set size of the tick marks for x-axis
  #                    axis.ticks.x = element_line(size = 0.5),
  #                    #adjust length of the tick marks
  #                    axis.ticks.length = unit(0.2,"cm"),
  #                    #set size and location of the tick labels for the y axis
  #                    axis.text.y = element_text(colour = "black", size = 14, angle = 0, vjust = 0.5, hjust = 1,
  #                                               margin = margin(t = 0, r = 5, b = 0, l = 0)),
  #                    #set size and location of the tick labels for the x axis
  #                    axis.text.x = element_text(colour = "black", size = 14, angle = 0, vjust = 0, hjust = 0.5,
  #                                         margin = margin(t = 5, r = 0, b = 0, l = 0)),
  #                    #set the axis size, color, and end shape
  #                     axis.line = element_line(colour = "black", size = 0.5, lineend = "square"))
  
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
  
  Trap <- data$in_effortT
  Gill <- data$in_effortG
  Effort.df <- data.frame(Year=rep(ryears,length(ages)),Fishery=rep(c("Trap","Gill"),each=length(ryears)),Effort=c(Trap,Gill))
  
  jpeg('Effort.jpg',width=1200,height=800)
  plot(ggplot(data=Effort.df,aes(x=Year,y=Effort,color=as.factor(Fishery)))+
         geom_line(size=3)+
         geom_point(size=3)+
         xlab("Years")+
         ylab("Effort (Millions of Feet or 100's of Lifts)")+
         pubtheme+theme(legend.text = element_text( size = 24))+
         scale_color_manual(values=c('black','azure4'))+
         labs(color="Fishery"))
  dev.off()
  
  if(checkagainstADMB==T)
  {
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
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,5000)+
           theme(legend.text = element_text( size = 24))+
           labs(color="Model")+
           annotate(geom="text",x=ryears[1], y=5000, label="A",size=24,fontface="bold")+
           pubtheme+scale_color_manual(values=c('black','azure4'))
  
    Recruitment<-ggplot(data=Recruitment.df,aes(x=Year,y=Recruitment,color=as.factor(Model)))+
           geom_line(aes(linetype=Model, color=Model),size=3)+
           geom_point(aes(color=Model))+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,1800)+
           theme(legend.text = element_text( size = 24))+
           labs(color="Model")+
           annotate(geom="text",x=ryears[1], y=1800, label="B",size=24,fontface="bold")+
           pubtheme+scale_color_manual(values=c('black','azure4'))
    
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
           ylab("Spawning Stock Biomass (x1000) lbs")+ylim(0,7000)+
           theme(legend.text = element_text( size = 24))+
           labs(color="Model")+
           pubtheme+scale_color_manual(values=c('black','azure4')))
    dev.off()
    #############################################################################
    
    #SELECTIVITY CURVES
    #############################################################################
    #SSM
    
    FTmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
    FTmatrix.max <- apply(FTmatrix,1,max)
    AdjustedqTmatrix <- FTmatrix/FTmatrix.max
    AdjustedqT.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqTmatrix))
    
    coul<-brewer.pal(11,"RdBu")
    
    SelectivityTrapNetSSM<-ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           xlab("Ages")+
           ylab("Normalized Catchability (100's of lifts * year)")+
           scale_color_manual(values=colorRampPalette(coul)(length(years)))+
           labs(color="Year")+
           pubtheme

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
           xlab("Ages")+
           ylab("Normalized Catchability (millions of feet * year)")+
           scale_color_manual(values=colorRampPalette(coul)(length(years)))+
           labs(color="Year")+
           pubtheme
    
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
           xlab("Ages")+
           ylab("Selectivity")+
           scale_color_manual(values=colorRampPalette(coul)(length(years)))+
           labs(color="Year")+
           pubtheme
    
    Selectivitylegend <- cowplot::get_legend(SelectivityGillNetSCAAdjusted)
    
    SelectivityGillNetSCAAdjusted <- SelectivityGillNetSCAAdjusted + theme(legend.position="none")
    SelectivityTrapNetSCAAdjusted <- SelectivityTrapNetSCAAdjusted + theme(legend.position="none")
    SelectivityGillNetSSM <- SelectivityGillNetSSM + theme(legend.position="none")
    SelectivityTrapNetSSM <- SelectivityTrapNetSSM + theme(legend.position="none")
    
    jpeg('Selectivity.jpg',width=2400,height=1800)
    grid.arrange(SelectivityGillNetSCAAdjusted,
                 SelectivityGillNetSSM,
                 Selectivitylegend,
                 SelectivityTrapNetSCAAdjusted,
                 SelectivityTrapNetSSM,
                 ncol=3,
                 layout_matrix = rbind(c(1,2,3), c(4,5,3)),
                 widths = c(4, 4, 0.75), heights = c(3,3),)
    dev.off() 
    
    
    #############################################################################
    
    #CATCHABILITY
    #############################################################################
    #Specify color palette for catchability plots
    coul<-brewer.pal(10,"Spectral")
    
    #SSM
    qG <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qG"),1])
    qGmatrix<-matrix(qG,nrow=length(ryears),ncol=length(ages))
    qG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix))
    jpeg('CatchabilityGillNetSSM.jpg',width=1200,height=800)
    plot(ggplot(data=qG.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability (millions of feet * year)")+
           ylim(0,0.125)+
           labs(color="Age")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    qT <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qT"),1])
    qTmatrix<-matrix(qT,nrow=length(ryears),ncol=length(asymptoteages))
    qT.df <- data.frame(year=rep(ryears,length(asymptoteages)),age=rep(asymptoteages,each=length(ryears)),catchability=c(qTmatrix))
    jpeg('CatchabilityTrapNetSSM.jpg',width=1200,height=800)
    plot(ggplot(data=qT.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(asymptoteages)))+
           xlab("Years")+
           ylab("Catchability (100's of lifts * year)")+
           ylim(0,0.04)+
           labs(color="Age")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    #SCA
    qG.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="qG"),2]
    qGmatrix.admb <- SelGmatrix.admb*0
    for(i in 1:nrow(SelGmatrix.admb))
    {
      qGmatrix.admb[i,]<-SelGmatrix.admb[i,]*qG.admb[i]
    }
    qG.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix.admb))
    jpeg('CatchabilityGillNetSCA.jpg',width=1200,height=800)
    plot(ggplot(data=qG.admb.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability (millions of feet * year)")+
           ylim(0,0.125)+
           labs(color="Age")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    qT.admb <- ADMBdat.table[which(ADMBdat.table[,1]=="qT"),2]
    qTmatrix.admb <- SelTmatrix.admb*0
    for(i in 1:nrow(SelTmatrix.admb))
    {
      qTmatrix.admb[i,]<-SelTmatrix.admb[i,]*qT.admb[i]
    }
    qT.admb.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qTmatrix.admb))
    jpeg('CatchabilityTrapNetSCA.jpg',width=1200,height=800)
    plot(ggplot(data=qT.admb.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability (100's of lifts * year)")+
           ylim(0,0.04)+
           labs(color="Age")+
           # ggtitle("Catchability of Gill Net by Age and Year of Lake Whitefish in WFM03")+
           pubtheme+theme(legend.text = element_text( size = 24)))
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
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           scale_fill_manual(values=c("deepskyblue","deepskyblue4","darkgrey"))+
           annotate(geom="text",x=ryears[1], y=1.5, label="B",size=24,fontface="bold")+
           labs(fill="Mortality Type")+
           pubtheme+theme(legend.position = c(0.85,0.6)) #For Presentations
    
    MortalityADMB<-ggplot(Z.df.admb,aes(years,morts))+
           geom_bar(stat="identity",aes(fill=Z.type.admb))+
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           scale_fill_grey()+
           annotate(geom="text",x=ryears[1], y=1.5, label="A",size=24,fontface="bold")+
           labs(fill="Mortality Type")+
           pubtheme+theme(legend.position = c(0.85,0.6))
                     
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
      
    jpeg('PredictedTrapHarvest.jpg',width=1200,height=800)
    plot(ggplot(data=PredT.df,aes(x=Year,y=Prediction,color=as.factor(Model)))+
           geom_line(data=data.frame(years,ObsT),aes(x=years,y=ObsT),color="black",size=2)+
           geom_line(aes(color=Model),size=2,linetype="dotted")+
           scale_color_manual(values=c('red','blue'))+
           xlab("Years")+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           theme(legend.text = element_text( size = 24))+
           labs(color="Model")+
           pubtheme)
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=800)
    plot(ggplot(data=PredG.df,aes(x=Year,y=Prediction,color=as.factor(Model)))+
           geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(aes(color=Model),size=2,linetype="dotted")+
           scale_color_manual(values=c('red','blue'))+
           xlab("Years")+
           ylab("Harvest (x1,000)")+ylim(0,600)+
           theme(legend.text = element_text( size = 24))+
           labs(color="Model")+
           pubtheme)
    dev.off()
    #############################################################################
    
  }
  
  else
  {
    #POPULATION SIZE
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Abundance<- apply(Nmatrix,1,sum)/1000
    
    jpeg('Abundance.jpg',width=1200,height=800)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,Abundance),aes(x=ryears,y=Abundance),color="black",size=2)+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,5000)+
           pubtheme)
    dev.off()
    #############################################################################
    
    #SPAWNING STOCK BIOMASS
    #############################################################################
    SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
    
    jpeg('SSBiomass.jpg',width=1200,height=800)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,SP_BIO),aes(x=ryears,y=SP_BIO),color="black",size=2)+
           xlab("Years")+
           ylab("Spawning Stock Biomass (x1000) lbs")+ylim(0,7000)+
           pubtheme)
    dev.off()
    #############################################################################
    
    #RECRUITMENT
    #############################################################################
    Nmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="N"),1],nrow=length(ryears),ncol=length(ages))
    Recruitment <- Nmatrix[,1]/1000
    
    jpeg('Recruitment.jpg',width=1200,height=800)
    plot(ggplot()+
           geom_line(data=data.frame(ryears,Recruitment),aes(x=ryears,y=Recruitment),color="black",size=2)+
           xlab("Years")+
           ylab("Number of fish (x1000)")+ylim(0,1800)+
           pubtheme)
    dev.off()
    #############################################################################
    
    #SELECTIVITY CURVES
    #############################################################################
    FTmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FT"),1],nrow=length(ryears),ncol=length(ages))
    FTmatrix.max <- apply(FTmatrix,1,max)
    AdjustedqTmatrix <- FTmatrix/FTmatrix.max
    AdjustedqT.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqTmatrix))
    
    jpeg('SelectivityTrapNet.jpg',width=1200,height=800)
    plot(ggplot(data=AdjustedqT.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           xlab("Ages")+
           ylab("Normalized Catchability (100's of lifts * year)")+
           labs(color="Year")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    FGmatrix<-matrix(summary(sdr)[which(row.names(summary(sdr))=="FG"),1],nrow=length(ryears),ncol=length(ages))
    FGmatrix.max <- apply(FGmatrix,1,max)
    AdjustedqGmatrix <- FGmatrix/FGmatrix.max
    AdjustedqG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(AdjustedqGmatrix))
    
    jpeg('SelectivityGillNet.jpg',width=1200,height=800)
    plot(ggplot(data=AdjustedqG.df,aes(x=age,y=catchability,color=as.factor(year)))+
           geom_line(size=1.25)+
           xlab("Ages")+
           ylab("Normalized Catchability (millions of feet * year)")+
           labs(color="Year")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    #############################################################################
    
    #CATCHABILITY
    #############################################################################
    #Specify color palette for catchability plots
    coul<-brewer.pal(10,"Spectral")
    
    qG <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qG"),1])
    qGmatrix<-matrix(qG,nrow=length(ryears),ncol=length(ages))
    qG.df <- data.frame(year=rep(ryears,length(ages)),age=rep(ages,each=length(ryears)),catchability=c(qGmatrix))
    jpeg('CatchabilityGillNet.jpg',width=1200,height=800)
    plot(ggplot(data=qG.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(ages)))+
           xlab("Years")+
           ylab("Catchability (millions of feet * year)")+
           labs(color="Age")+
           pubtheme+theme(legend.text = element_text( size = 24)))
    dev.off()
    
    qT <- exp(summary(sdr)[which(row.names(summary(sdr))=="log_qT"),1])
    qTmatrix<-matrix(qT,nrow=length(ryears),ncol=length(asymptoteages))
    qT.df <- data.frame(year=rep(ryears,length(asymptoteages)),age=rep(asymptoteages,each=length(ryears)),catchability=c(qTmatrix))
    jpeg('CatchabilityTrapNet.jpg',width=1200,height=800)
    plot(ggplot(data=qT.df,aes(x=year,y=catchability,color=as.factor(age)))+
           geom_line(size=1.25)+
           scale_color_manual(values=colorRampPalette(coul)(length(asymptoteages)))+
           xlab("Years")+
           ylab("Catchability (100's of lifts * year)")+
           labs(color="Age")+
           pubtheme+theme(legend.text = element_text( size = 24)))
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
           geom_bar(stat="identity",aes(fill=Z.type))+
           ylab("Mortality")+
           ylim(0,1.5)+
           xlab("Years")+
           scale_fill_grey()+
           pubtheme)
    dev.off()                  
    #############################################################################
    
    #OBSERVED VERSUS PREDICTED HARVEST
    #############################################################################
    ObsT<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsT"),2]/1000
    ObsG<-ADMBdat.table[which(ADMBdat.table[,1]=="ObsG"),2]/1000
    
    PredT <- summary(sdr)[which(row.names(summary(sdr))=="CT"),1]/1000
    PredG <- summary(sdr)[which(row.names(summary(sdr))=="CG"),1]/1000
    
    jpeg('PredictedTrapHarvest.jpg',width=1200,height=800)
    plot(ggplot()+geom_line(data=data.frame(years,ObsT),aes(x=years,y=ObsT),color="black",size=2)+
           geom_line(data=data.frame(ryears,PredT),aes(x=ryears,y=PredT),color="black",linetype=2,size=2)+
           xlab("Years")+
           ylab("Harvest (x1,000) lbs")+ylim(0,600)+
           pubtheme)
    dev.off()
    
    jpeg('PredictedGillHarvest.jpg',width=1200,height=800)
    plot(ggplot()+geom_line(data=data.frame(years,ObsG),aes(x=years,y=ObsG),color="black",size=2)+
           geom_line(data=data.frame(ryears,PredG),aes(x=ryears,y=PredG),color="black",linetype=2,size=2)+
           xlab("Years")+
           ylab("Harvest (x1,000) lbs")+ylim(0,600)+
           pubtheme)
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
  pubtheme <- theme(axis.title=element_text(size=32),axis.text=element_text(size=32),
                    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                    panel.background = element_blank(), axis.line = element_line(colour = "black"),
                    axis.text.x = element_text(color="black",size=24),
                    axis.text.y = element_text(color="black",size=24,angle=90,hjust=0.5))
  
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
  
  jpeg("Gill Net Catch Residual.jpg",width=1200,height=600)
  plot(ggplot()+geom_point(data=data.frame(residCG,ryears),aes(x=ryears,y=residCG),size=3,color="red")+
         geom_line(data=data.frame(ryears,zeroline),aes(x=ryears,y=zeroline),size=1)+
         theme_bw()+
         ggtitle("Gill Net Catch Residuals")+
         xlab("Years")+
         ylab("Residual")+
         pubtheme)
  dev.off()
  
  jpeg("Trap Net Catch Residual.jpg",width=1200,height=600)
  plot(ggplot()+geom_point(data=data.frame(residCT,ryears),aes(x=ryears,y=residCT),size=3,color="red")+
         geom_line(data=data.frame(ryears,zeroline),aes(x=ryears,y=zeroline),size=1)+
         theme_bw()+
         ggtitle("Trap Net Catch Residuals")+
         xlab("Years")+
         ylab("Residual")+
         pubtheme)
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
         ylab("Residual")+
         pubtheme)
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
         ylab("Residual")+
         pubtheme)
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
         ylab("Residual")+
         pubtheme)
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
         ylab("Residual")+
         pubtheme)
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
  hist(residCT,main="",xlab="",ylab="",cex.axis=3)
  hist(residCG,main="",xlab="",ylab="",cex.axis=3)
  hist(residPAT,main="",xlab="",ylab="",cex.axis=3)
  hist(residPAG,main="",xlab="",ylab="",cex.axis=3)
  hist(resid_effT,main="",xlab="",ylab="",cex.axis=3)
  hist(resid_effG,main="",xlab="",ylab="",cex.axis=3)
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
# saveretroplots(wds[2],originalwd,sdr,retrostart,retroend,data)
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
  #Count how many times the simulated data didn't converge
  noconverge=0
  
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
  
  Abundanceabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  Recruitmentabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  FGabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  FTabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  Biomassabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  SPBiomassabsoluteerror <-matrix(NA,nrow=length(ryears),ncol=numsims)
  
  setwd(simresultswd)
  sink("SimResults.txt",append=T)
  cat("Start of a new set of simulation results: ")
  cat("\n")
  cat("Total number of simulations (intended): ")
  cat(numsims)
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
    
    #Slow down and look at the data, trying to find what kind of simulated data messes the code
    # print(simres)
    # readline(prompt="press [enter] to continue")
    
    #Specify random effects, depending on age comp likelihood
    if(agecompfit==1){reffects <- c("log_rec","log_qG","log_qT")}
    if(agecompfit==3){reffects <- c("log_rec","log_qG","log_qT","log_gamT","log_gamG")}
    
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
    simfit <- fitmodel(simobj,bounds)
    # simfit <- nlminb(simobj$par, simobj$fn, simobj$gr,lower=bounds$lower_bounds,upper=bounds$upper_bounds)
    # simfit <- nlminb(simobj$par, simobj$fn, simobj$gr)

    if(is.na(simfit))
    {
      noconverge=noconverge+1
    }
    else
    {
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
      Abundanceabsoluteerror[,i]<-(simAbundance-Abundance)
      
      #Recruitment
      Recruitment <- Nmatrix[,1]/1000
      simRecruitment <- simNmatrix[,1]/1000
      Recruitmentrelativeerror[,i]<-(simRecruitment-Recruitment)/Recruitment
      Recruitmentabsoluteerror[,i]<-(simRecruitment-Recruitment)
      
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
      FGabsoluteerror[,i]<-(simFG_Avg-FG_Avg)
      FTabsoluteerror[,i]<-(simFT_Avg-FT_Avg)
      
      #Biomass
      BIOMASS <- summary(sdr)[which(row.names(summary(sdr))=="BIOMASS"),1]/1000*2.20462
      SP_BIO <-summary(sdr)[which(row.names(summary(sdr))=="SP_BIO"),1]/1000*2.20462
      simBIOMASS <- summary(simsdr)[which(row.names(summary(simsdr))=="BIOMASS"),1]/1000*2.20462
      simSP_BIO <-summary(simsdr)[which(row.names(summary(simsdr))=="SP_BIO"),1]/1000*2.20462
      Biomassrelativeerror[,i]<-(simBIOMASS-BIOMASS)/BIOMASS
      SPBiomassrelativeerror[,i]<-(simSP_BIO-SP_BIO)/SP_BIO
      Biomassabsoluteerror[,i]<-(simBIOMASS-BIOMASS)
      SPBiomassabsoluteerror[,i]<-(simSP_BIO-SP_BIO)
      
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
  }
    
    #Just to be sure the simulation process begins afresh, remove all previous objects
    if(is.na(simfit))
    {
      rm("simres","simdata","simobj","simfit")
    }
    else
    {
      rm("simres","simdata","simobj","simfit","simsdr")
    }
  }
  
  setwd(simresultswd)
  sink("SimResults.txt",append=T)
  cat("\n")
  cat("\n")
  cat("Number of times model didn't converge: ")
  cat(noconverge)
  sink()
  setwd(originalwd)
  
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
  
  
  # Calculate and Plot boxplot of N Error
  Abundanceabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    Abundanceabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Abundanceabsoluteerror[i,]
    Abundanceabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Abundanceabsoluteerror.df)=c("Difference","Years")
  jpeg("AbundanceAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Abundancerelativeerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated N"))
  dev.off()
  
  # Calculate and Plot boxplot of Recruitment Error
  Recruitmentabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    Recruitmentabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Recruitmentabsoluteerror[i,]
    Recruitmentabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Recruitmentabsoluteerror.df)=c("Difference","Years")
  jpeg("RecruitmentAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Recruitmentabsoluteerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated Recruitment"))
  dev.off()
  
  # Calculate and Plot boxplot of Gill Net Mortality Error
  FGabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    FGabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- FGabsoluteerror[i,]
    FGabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(FGabsoluteerror.df)=c("Difference","Years")
  jpeg("FGAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=FGabsoluteerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated Gill Net Mortality"))
  dev.off()
  
  # Calculate and Plot boxplot of Trap Net Mortality Error
  FTabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    FTabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- FTabsoluteerror[i,]
    FTabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(FTabsoluteerror.df)=c("Difference","Years")
  jpeg("FTAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=FTabsoluteerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated Trap Net Mortality"))
  dev.off()
  
  # Calculate and Plot boxplot of Biomass Error
  Biomassabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    Biomassabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- Biomassabsoluteerror[i,]
    Biomassabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(Biomassabsoluteerror.df)=c("Difference","Years")
  jpeg("BiomassAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=Biomassabsoluteerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated Biomass"))
  dev.off()
  
  # Calculate and Plot boxplot of Biomass Error
  SPBiomassabsoluteerror.df=data.frame()
  for(i in 1:length(ryears)){
    SPBiomassabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),1] <- SPBiomassabsoluteerror[i,]
    SPBiomassabsoluteerror.df[((i*numsims)-(numsims-1)):(i*numsims),2] <- ryears[i]
  }
  names(SPBiomassabsoluteerror.df)=c("Difference","Years")
  jpeg("SPBiomassAbsoluteErrorBoxplot.jpg",width=400,height=200)
  plot(ggplot(data=SPBiomassabsoluteerror.df,aes(y=Difference,group=Years))+
         geom_boxplot()+
         scale_fill_grey()+
         xlab("Years")+
         ylab("Absolute Error of Estimated Spawning Stock Biomass"))
  dev.off()
  
  #Set original working directory and return the large ob
  setwd(originalwd)
  # print(mysimstudy)
  return(mysimstudy)
}
#############################################################################

