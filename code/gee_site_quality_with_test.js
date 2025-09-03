// OPTIMIZED VERSION - Process one state at a time to avoid memory issues

// 1. Load boundaries and cropland mask FIRST
var states = ee.FeatureCollection("TIGER/2018/States");
var plantingDatesDict = ee.Dictionary({
  //'Texas': 152,    // 31 May 
  'South Dakota': 160,    // June 8
  'North Dakota': 153,    // June 1
  'Minnesota': 146,    // May 24
 'Nebraska': 161, // June 9 
  'Colorado': 165, // June 13 
 'Kansas': 164 // June 12 
});

// SELECT PARAMETER SET - CHANGE THIS TO USE DIFFERENT MODEL CONFIGURATIONS
var parameterSet = 'fixef_noTX_noYr'; // Options: 'ranef_noTX', 'fixef_noTX', 
//'ranef_includeTX', 'fixef_includeTX', 'ranef_noTX_noYr', 'fixef_noTX_noYr'

// PARAMETER SET DEFINITIONS
var paramSets = {
'ranef_noTX': {
    // Scaling factors
    tmeanMean: 0, tmeanStdDev: 0,
    vpdMean: 1.461327, vpdStdDev: 1.154439,
    gddMean: 0, gddStdDev: 0,
    tmaxMean: 14.771894, tmaxStdDev: 13.412419,
    tminMean: 0.95805, tminStdDev: 11.627014,
    prMean: 0, prStdDev: 0,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 12.000039, daylengthStdDev: 2.328596,

    // Regression coefficients
    intercept: -5564.914116, yearcoef: 4.659917,
    tmaxLinear_1: 1418.273632, tmaxQuadratic_1: -1001.485261,
    tmaxLinear_2: 0, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 0, tmeanQuadratic_1: 0,
    tmeanLinear_2: 0, tmeanQuadratic_2: 0,
    gddLinear_1: 0, gddQuadratic_1: 0,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: 13.54485, vpdQuadratic_1: -63.28191,
    vpdLinear_2: -107.296434, vpdQuadratic_2: -216.518321,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: -1729.317937, tminQuadratic_1: 1643.22374,
    tminLinear_2: 5053.249672, tminQuadratic_2: -3179.54297,
    tminLinear_3: -6125.805857, tminQuadratic_3: 4072.989986,
    tminLinear_4: 9288.186104, tminQuadratic_4: -5154.47105,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: 0, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 0,
    daylengthLinear: 1150.017558, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 10,
    tmean2_start: 10, tmean2_end: 10,
    tmax1_start: 52, tmax1_end: 72,
    tmax2_start: 10, tmax2_end: 10,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 17, vpd1_end: 23,
    vpd2_start: 122, vpd2_end: 135,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 10, gdd1_end: 10,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 10, pr_end: 10,
    tmin1_start: 10, tmin1_end: 93,
    tmin2_start: 73, tmin2_end: 100,
    tmin3_start: 73, tmin3_end: 114,
    tmin4_start: 10, tmin4_end: 121,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 10,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 17, dayl_end: 23
},

'fixef_noTX': {
    // Scaling factors
    tmeanMean: 7.864972, tmeanStdDev: 12.279119,
    vpdMean: 1.461327, vpdStdDev: 1.154439,
    gddMean: 5.913104, gddStdDev: 5.503491,
    tmaxMean: 14.771894, tmaxStdDev: 13.412419,
    tminMean: 0.95805, tminStdDev: 11.627014,
    prMean: 1.45715, prStdDev: 4.730427,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -3031.897732, yearcoef: 5.929157,
    tmaxLinear_1: 1847.50779, tmaxQuadratic_1: -910.879385,
    tmaxLinear_2: 3967.9293, tmaxQuadratic_2: -2107.159592,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 2716.868282, tmeanQuadratic_1: -1350.814522,
    tmeanLinear_2: 0, tmeanQuadratic_2: 0,
    gddLinear_1: 1069.201969, gddQuadratic_1: -629.076237,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: -292.498347, vpdQuadratic_1: 283.314823,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: -1284.18939, tminQuadratic_1: 742.811294,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: 0, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 177.844161,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 10, tmean2_end: 10,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 142,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 101, vpd1_end: 114,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 129, pr_end: 135,
    tmin1_start: 87, tmin1_end: 121,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 10,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'ranef_includeTX': {
    // Scaling factors
    tmeanMean: 8.116955, tmeanStdDev: 12.347567,
    vpdMean: 0, vpdStdDev: 0,
    gddMean: 0, gddStdDev: 0,
    tmaxMean: 15.005608, tmaxStdDev: 13.439846,
    tminMean: 1.228301, tminStdDev: 11.736643,
    prMean: 1.47631, prStdDev: 4.834507,
    rsdsWMean: 316.504961, rsdsStdDev: 126.357022,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2577.143882, yearcoef: 7.155662,
    tmaxLinear_1: 1133.512667, tmaxQuadratic_1: -734.773485,
    tmaxLinear_2: 3264.978514, tmaxQuadratic_2: -1641.562178,
    tmaxLinear_3: 60.080819, tmaxQuadratic_3: -278.475263,
    tmeanLinear_1: 2608.130428, tmeanQuadratic_1: -1315.895134,
    tmeanLinear_2: 0, tmeanQuadratic_2: 0,
    gddLinear_1: 0, gddQuadratic_1: 0,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: 0, vpdQuadratic_1: 0,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 250.98122, tminQuadratic_1: -5.613063,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: -624.521827, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 180.770083,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 10, tmean2_end: 10,
    tmax1_start: 52, tmax1_end: 72,
    tmax2_start: 66, tmax2_end: 86,
    tmax3_start: 122, tmax3_end: 128,
    vpd1_start: 10, vpd1_end: 10,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 10, gdd1_end: 10,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 65,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'fixef_includeTX': {
    // Scaling factors
    tmeanMean: 0, tmeanStdDev: 0,
    vpdMean: 1.475844, vpdStdDev: 1.160122,
    gddMean: 6.045973, gddStdDev: 5.585659,
    tmaxMean: 15.005608, tmaxStdDev: 13.439846,
    tminMean: 1.228301, tminStdDev: 11.736643,
    prMean: 1.47631, prStdDev: 4.834507,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -859.794098, yearcoef: 6.510618,
    tmaxLinear_1: 2364.612968, tmaxQuadratic_1: -1220.769429,
    tmaxLinear_2: 0, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 0, tmeanQuadratic_1: 0,
    tmeanLinear_2: 0, tmeanQuadratic_2: 0,
    gddLinear_1: 315.35667, gddQuadratic_1: 0,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: -97.265708, vpdQuadratic_1: -87.743035,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 168.041572, tminQuadratic_1: 0,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: 0, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 144.043563,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 10,
    tmean2_start: 10, tmean2_end: 10,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 10, tmax2_end: 10,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 10, vpd1_end: 72,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 38, gdd1_end: 44,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 10,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'ranef_noTX_noYr': {
    // Scaling factors
    tmeanMean: 7.864972, tmeanStdDev: 12.279119,
    vpdMean: 0, vpdStdDev: 0,
    gddMean: 5.913104, gddStdDev: 5.503491,
    tmaxMean: 14.771894, tmaxStdDev: 13.412419,
    tminMean: 0.95805, tminStdDev: 11.627014,
    prMean: 1.45715, prStdDev: 4.730427,
    rsdsWMean: 315.699584, rsdsStdDev: 126.592896,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2760.672093, yearcoef: 0,
    tmaxLinear_1: 2214.635522, tmaxQuadratic_1: -1133.204376,
    tmaxLinear_2: 0, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 2412.06453, tmeanQuadratic_1: -1238.359935,
    tmeanLinear_2: -1349.483559, tmeanQuadratic_2: 839.548467,
    gddLinear_1: 2258.184267, gddQuadratic_1: -1144.924158,
    gddLinear_2: 261.684878, gddQuadratic_2: 0,
    vpdLinear_1: 0, vpdQuadratic_1: 0,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 334.208291, tminQuadratic_1: 76.499895,
    tminLinear_2: -132.400194, tminQuadratic_2: -644.604503,
    tminLinear_3: 1162.447227, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: -818.233426, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 148.105911,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 87, tmean2_end: 114,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 10, tmax2_end: 10,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 10, vpd1_end: 10,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 38, gdd2_end: 44,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 24, tmin2_end: 58,
    tmin3_start: 24, tmin3_end: 51,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 65,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'fixef_noTX_noYr': {
    // Scaling factors
    tmeanMean: 7.864972, tmeanStdDev: 12.279119,
    vpdMean: 1.461327, vpdStdDev: 1.154439,
    gddMean: 5.913104, gddStdDev: 5.503491,
    tmaxMean: 14.771894, tmaxStdDev: 13.412419,
    tminMean: 0.95805, tminStdDev: 11.627014,
    prMean: 1.45715, prStdDev: 4.730427,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2493.730668, yearcoef: 0,
    tmaxLinear_1: 2093.377527, tmaxQuadratic_1: -943.076688,
    tmaxLinear_2: 531.605243, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 2547.599743, tmeanQuadratic_1: -1215.834995,
    tmeanLinear_2: 0, tmeanQuadratic_2: 0,
    gddLinear_1: 1100.956957, gddQuadratic_1: -697.320305,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: -228.773442, vpdQuadratic_1: 264.283764,
    vpdLinear_2: 309.501732, vpdQuadratic_2: -148.579975,
    vpdLinear_3: -51.096332, vpdQuadratic_3: -246.288991,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 272.646121, tminQuadratic_1: 0,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: 0, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 154.918241,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 10, tmean2_end: 10,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 65,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 101, vpd1_end: 114,
    vpd2_start: 80, vpd2_end: 86,
    vpd3_start: 122, vpd3_end: 135,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 10,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'ranef_includeTX_noYr': {
    // Scaling factors
    tmeanMean: 8.116955, tmeanStdDev: 12.347567,
    vpdMean: 0, vpdStdDev: 0,
    gddMean: 6.045973, gddStdDev: 5.585659,
    tmaxMean: 15.005608, tmaxStdDev: 13.439846,
    tminMean: 1.228301, tminStdDev: 11.736643,
    prMean: 0, prStdDev: 0,
    rsdsWMean: 316.504961, rsdsStdDev: 126.357022,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2423.534748, yearcoef: 0,
    tmaxLinear_1: 1785.027826, tmaxQuadratic_1: -900.323685,
    tmaxLinear_2: 0, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 2472.458367, tmeanQuadratic_1: -1313.943824,
    tmeanLinear_2: -1821.377794, tmeanQuadratic_2: 0,
    gddLinear_1: 3625.025431, gddQuadratic_1: -1027.911598,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: 0, vpdQuadratic_1: 0,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 337.311217, tminQuadratic_1: 0,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: -182.477195, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 0,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 45, tmean2_end: 100,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 10, tmax2_end: 10,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 10, vpd1_end: 10,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 38, gdd1_end: 100,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 10, pr_end: 10,
    tmin1_start: 66, tmin1_end: 142,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 129, rsds1_end: 135,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
},

'fixef_includeTX_noYr': {
    // Scaling factors
    tmeanMean: 8.116955, tmeanStdDev: 12.347567,
    vpdMean: 1.475844, vpdStdDev: 1.160122,
    gddMean: 6.045973, gddStdDev: 5.585659,
    tmaxMean: 15.005608, tmaxStdDev: 13.439846,
    tminMean: 1.228301, tminStdDev: 11.736643,
    prMean: 1.47631, prStdDev: 4.834507,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2250.315456, yearcoef: 0,
    tmaxLinear_1: 2106.271061, tmaxQuadratic_1: -996.784112,
    tmaxLinear_2: 553.436298, tmaxQuadratic_2: 0,
    tmaxLinear_3: 0, tmaxQuadratic_3: 0,
    tmeanLinear_1: 2349.913953, tmeanQuadratic_1: -1172.916023,
    tmeanLinear_2: -1517.068116, tmeanQuadratic_2: 1005.613766,
    gddLinear_1: 1921.578818, gddQuadratic_1: -1112.260151,
    gddLinear_2: 0, gddQuadratic_2: 0,
    vpdLinear_1: 274.178285, vpdQuadratic_1: -124.958296,
    vpdLinear_2: 0, vpdQuadratic_2: 0,
    vpdLinear_3: 0, vpdQuadratic_3: 0,
    vpdLinear_4: 0, vpdQuadratic_4: 0,
    tminLinear_1: 285.283577, tminQuadratic_1: 0,
    tminLinear_2: 0, tminQuadratic_2: 0,
    tminLinear_3: 0, tminQuadratic_3: 0,
    tminLinear_4: 0, tminQuadratic_4: 0,
    tminLinear_5: 0, tminQuadratic_5: 0,
    rsdsLinear_1: 0, rsdsQuadratic_1: 0,
    rsdsLinear_2: 0, rsdsQuadratic_2: 0,
    pptLinear: 162.7707,
    daylengthLinear: 0, daylengthQuadratic: 0,

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 87, tmean2_end: 114,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 65,
    tmax3_start: 10, tmax3_end: 10,
    vpd1_start: 80, vpd1_end: 86,
    vpd2_start: 10, vpd2_end: 10,
    vpd3_start: 10, vpd3_end: 10,
    vpd4_start: 10, vpd4_end: 10,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 10, gdd2_end: 10,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 10, tmin2_end: 10,
    tmin3_start: 10, tmin3_end: 10,
    tmin4_start: 10, tmin4_end: 10,
    tmin5_start: 10, tmin5_end: 10,
    rsds1_start: 10, rsds1_end: 10,
    rsds2_start: 10, rsds2_end: 10,
    dayl_start: 10, dayl_end: 10
}
};

// Get the selected parameter set
var params = paramSets[parameterSet];
if (!params) {
  throw new Error('Invalid parameter set: ' + parameterSet + '. Please check the parameterSet variable.');
}

// Add configurable sample point and determine which state contains it
var SAMPLE_LON = -100.2; // Edit as needed
var SAMPLE_LAT = 45.99;   // Edit as needed
var samplePoint = ee.Geometry.Point([SAMPLE_LON, SAMPLE_LAT]);
var sampleState = states.filterBounds(samplePoint).first();
print('Sample point state:', sampleState.get('NAME'));

// Filter states early and get geometry
var targetStates = states.filter(ee.Filter.inList('NAME', plantingDatesDict.keys()));
var studyArea = targetStates.geometry().bounds();

// Create cropland mask early and clip to study area
var nlcd = ee.Image('USGS/NLCD_RELEASES/2019_REL/NLCD/2019')
    .select('landcover')
    .clip(studyArea); // Clip NLCD first
var croplandMask = nlcd.eq(82);

// Test if sample point is within cropland mask
var maskValueAtSample = croplandMask.reduceRegion({
    reducer: ee.Reducer.first(),
    geometry: samplePoint,
    scale: 30
});

var isCropland = ee.Number(maskValueAtSample.get('landcover'));
isCropland.evaluate(function(value) {
    if (value === 0 || value === null) {
        print('WARNING: Sample point at', SAMPLE_LON, SAMPLE_LAT, 'is NOT in cropland area and will be masked out!');
        print('Consider choosing a different sample point location.');
    } else {
        print('Sample point at', SAMPLE_LON, SAMPLE_LAT, 'is within cropland area.');
    }
});

// 2. Load DAYMET data for daylength
var daymetCollection = ee.ImageCollection('NASA/ORNL/DAYMET_V4')
  .filterBounds(studyArea)
  .select('dayl'); // Day length in seconds

// 2. Load and preprocess MACAv2 data with early clipping
var macaBase = ee.ImageCollection('IDAHO_EPSCOR/MACAv2_METDATA')
  .filterBounds(studyArea) // Filter spatially first
  // these have no rh in them so leave off for now
  .filter(ee.Filter.neq('model', 'CCSM4')) // Exclude CCSM4 model
  .filter(ee.Filter.neq('model', 'NorESM1-M')) // Exclude NorESM1-M model
  .map(function(image) {
    // Clip each image to study area immediately
    image = image.clip(studyArea);
    
    // Calculate tmean as the average of tasmax and tasmin
    var tasmax = image.select('tasmax');
    var tasmin = image.select('tasmin');
    var tmean = tasmax.add(tasmin).divide(2);
    
    var temp_c = tasmax.subtract(273.15); // Convert from Kelvin to Celsius
    
    // Calculate VPD (vapor pressure deficit)
    var rh = image.select('rhsmax').add(image.select('rhsmin')).divide(2);
    
    // Calculate saturation vapor pressure (es) - using max temp
    var es = ee.Image(0.611).multiply(
      temp_c.gte(0).multiply(
        // Over liquid water (T >= 0°C)
        temp_c.multiply(17.27).divide(temp_c.add(237.3)).exp()
      ).add(
        temp_c.lt(0).multiply(
          // Over ice (T < 0°C)
          temp_c.multiply(21.875).divide(temp_c.add(265.5)).exp()
        )
      )
    );
    
    // Calculate VPD - Corrected formula: VPD = es * (1 - rh/100)
    var vpd = es.multiply(ee.Image(1).subtract(rh.divide(100)));
    
    // Calculate GDD_modave
    var tbase = 6.7;
    var tmax = 26;
    var tmaxdegc = tasmax.subtract(273.15); // Convert to Celsius
    var tmindegc = tasmin.subtract(273.15); // Convert to Celsius
    
    var tmax_constrained = tmaxdegc.min(tmax);
    var tmin_constrained = tmindegc.max(tbase);
    var GDD_modave = tmax_constrained.add(tmin_constrained).divide(2).subtract(tbase).max(0);
    
    /*
    // Calculate daylength (photoperiod) in hours
    var date = ee.Date(image.get('system:time_start'));
    var dayOfYear = date.getRelative('day', 'year');
    
    // Get latitude from image coordinates
    var pixelLatLon = image.select('tasmax').pixelLonLat();
    var latitude = pixelLatLon.select('latitude');
    
    // Calculate solar declination angle
    var solarDeclination = ee.Image(23.45).multiply(
      ee.Image(2).multiply(ee.Image(Math.PI)).multiply(
        ee.Image(284).add(dayOfYear).divide(365)
      ).sin()
    ).multiply(Math.PI / 180); // Convert to radians
    
    // Convert latitude to radians
    var latitudeRad = latitude.multiply(Math.PI / 180);
    
    // Calculate hour angle at sunrise/sunset
    var hourAngle = latitudeRad.tan().multiply(solarDeclination.tan()).multiply(-1).acos();
    
    // Calculate daylength in hours
    var daylength = hourAngle.multiply(2).multiply(24).divide(2 * Math.PI);
    
    // Add all new bands to the image and apply cropland mask immediately
    return image.addBands([
      tmean.rename('tmean'),
      vpd.rename('vpd'),
      GDD_modave.rename('GDD_modave'),
      daylength.rename('daylength')
    ]).updateMask(croplandMask); // Apply mask early
  });
*/

    // Add tmean, vpd, and GDD_modave bands to the image and apply cropland mask immediately
    return image.addBands([
      tmean.rename('tmean'),
      vpd.rename('vpd'),
      GDD_modave.rename('GDD_modave')
    ]).updateMask(croplandMask); // Apply mask early
  });

// Separate collections for historical and future scenarios
var macaHistorical = macaBase.filter(ee.Filter.eq('scenario', 'historical'));
var macaFuture = macaBase.filter(ee.Filter.eq('scenario', 'rcp45'));

// 3. Define year ranges, planting dates, and scaling factors
var currentStartYear = 1990;
var currentEndYear = 2010;
var futureStartYear = 2030;
var futureEndYear = 2050;


// 4. Generic function to compute multi-year ensemble mean for any MACA variable with provided scaling
/**
 * Computes multi-year mean of a MACA variable over a specified date range and geographic area.
 * Returns the processed variable along with scaled version and, if needed for downstream processing,
 * may include mean and stddev bands as constants.
 * 
 * @param {ee.Feature} state - Geographic boundary feature to clip the data to
 * @param {number} startYear - Starting year for the analysis period
 * @param {number} endYear - Ending year for the analysis period
 * @param {ee.ImageCollection} macaCollection - MACA climate data collection
 * @param {number} startDOY - Starting day of year (1-366) for seasonal window
 * @param {number} endDOY - Ending day of year (1-366) for seasonal window
 * @param {string} macaBandName - Name of the band/variable to extract from MACA collection
 * @param {string} outputBaseName - Base name for output bands
 * @param {number} scaleMean - Mean value for scaling normalization
 * @param {number} scaleStdDev - Standard deviation for scaling normalization
 * @param {boolean} [convertKelvinToCelsius=false] - Whether to convert temperature from Kelvin to Celsius
 * 
 * @returns {ee.Image} Multi-band image containing:
 *   - Original processed variable (outputBaseName)
 *   - Scaled/normalized variable (outputBaseName + '_scaled')
 *   - May also include scaling mean (outputBaseName + '_mean') and stddev (outputBaseName + '_stddev') bands if needed for downstream processing
 * 
 * @example
 * // Compute summer temperature for Montana from 1980-2010
 * var montana = ee.FeatureCollection('TIGER/2018/States')
 *   .filter(ee.Filter.eq('NAME', 'Montana'))
 *   .first();
 * 
 * var macaCollection = ee.ImageCollection('NASA/NEX-GDDP');
 * 
 * var summerTemp = computeMultiYearMACAVariable(
 *   montana,           // state boundary
 *   1980,             // start year
 *   2010,             // end year
 *   macaCollection,   // MACA data
 *   152,              // June 1st (day of year)
 *   243,              // August 31st (day of year)
 *   'tasmax',         // maximum temperature band
 *   'summer_tmax',    // output base name
 *   25.5,             // scaling mean (°C)
 *   5.2,              // scaling std dev (°C)
 *   true              // convert Kelvin to Celsius
 * );
 */
function computeMultiYearMACAVariable(state, startYear, endYear, macaCollection, startDOY, endDOY, macaBandName, outputBaseName, scaleMean, scaleStdDev, convertKelvinToCelsius) {
    // Set default for temperature conversion
    convertKelvinToCelsius = convertKelvinToCelsius !== undefined ? convertKelvinToCelsius : false;
    
    // Create list of years
    var years = ee.List.sequence(startYear, endYear);
    
    // Get variable images for each year
    var yearlyImages = years.map(function(year) {
        // Filter MACAv2 for the specific year and date range
        var yearStart = ee.Date.fromYMD(year, 1, 1);
        var yearEnd = ee.Date.fromYMD(year, 12, 31);
        
        var variableWindow = macaCollection
            .select(macaBandName)
            .filterDate(yearStart, yearEnd)
            .filter(ee.Filter.dayOfYear(startDOY, endDOY))
            .mean() // Mean across days within the window and Ensemble mean across models
            .clip(state);
        
        return variableWindow;
    });
    
    // Convert to ImageCollection and compute mean across years
    var yearlyCollection = ee.ImageCollection.fromImages(yearlyImages);
    var multiYearMean = yearlyCollection.mean();
    
    // Convert from Kelvin to Celsius if needed (for temperature variables)
    var processedVariable = convertKelvinToCelsius ? 
        multiYearMean.subtract(273.15) : 
        multiYearMean;
    
    // Create scaled version using provided scaling factors
    var variableScaled = processedVariable.subtract(scaleMean).divide(scaleStdDev);
    
    return processedVariable.rename(outputBaseName)
        .addBands(variableScaled.rename(outputBaseName + '_scaled'));

}

// Function to compute multi-year ensemble mean daylength from DAYMET for each state with provided scaling
function computeMultiYearDaylengthDaymet(state, startYear, endYear, startDOY, endDOY, scaleMean, scaleStdDev) {
  // For DAYMET, use the available years (1980-present for historical, use climatology for future)
  
  // Declare effective year range variables locally
  var effectiveStartYear = startYear;
  var effectiveEndYear = endYear;

  // If future period, use recent historical data as proxy (last 20 years available in DAYMET)
  if (startYear >= 2020) {
    effectiveStartYear = 2000;
    effectiveEndYear = 2020;
  }
  
  // Create list of years
  var years = ee.List.sequence(effectiveStartYear, effectiveEndYear);

  // Get daylength images for each year from DAYMET
  var yearlyImages = years.map(function(year) {
    var yearStart = ee.Date.fromYMD(year, 1, 1);
    var yearEnd = ee.Date.fromYMD(year, 12, 31);

    var daylengthWindow = daymetCollection
      .filterDate(yearStart, yearEnd)
      .filter(ee.Filter.dayOfYear(startDOY, endDOY))
      .mean() // Mean across days within the window
      .clip(state);

    // Convert from seconds to hours
    var daylengthHours = daylengthWindow.divide(3600);

    return daylengthHours;
  });

  // Convert to ImageCollection and compute mean across years
  var yearlyCollection = ee.ImageCollection.fromImages(yearlyImages);
  var multiYearMean = yearlyCollection.mean();

  // Create scaled version using provided scaling factors
  var daylengthScaled = multiYearMean.subtract(scaleMean).divide(scaleStdDev);

  return multiYearMean.rename('daylength')
    .addBands(daylengthScaled.rename('daylength_scaled'))
    .addBands(ee.Image.constant(scaleMean).rename('daylength_mean'))
    .addBands(ee.Image.constant(scaleStdDev).rename('daylength_stddev'));
}

// Function to process a single state - UPDATED to return image and support optional export
function processState(state, doExport) {
  // Default doExport to true if not specified
  doExport = (doExport === undefined) ? true : doExport;
  
  var stateName = state.getString('NAME');
  var plantingDOY = ee.Number(plantingDatesDict.get(stateName));
  
  print('Processing state:', stateName);
  
  // Initialize all variables as null
  var currentTmean_1 = null, currentTmean_2 = null;
  var currentTmax_1 = null, currentTmax_2 = null, currentTmax_3 = null;
  var currentVPD_1 = null, currentVPD_2 = null, currentVPD_3 = null, currentVPD_4 = null;
  var currentGDD_1 = null, currentGDD_2 = null;
  var currentPR = null;
  var currentTmin_1 = null, currentTmin_2 = null, currentTmin_3 = null, currentTmin_4 = null, currentTmin_5 = null;
  var currentRsds_1 = null, currentRsds_2 = null;
  var currentDaylength = null;

  var futureTmean_1 = null, futureTmean_2 = null;
  var futureTmax_1 = null, futureTmax_2 = null, futureTmax_3 = null;
  var futureVPD_1 = null, futureVPD_2 = null, futureVPD_3 = null, futureVPD_4 = null;
  var futureGDD_1 = null, futureGDD_2 = null;
  var futurePR = null;
  var futureTmin_1 = null, futureTmin_2 = null, futureTmin_3 = null, futureTmin_4 = null, futureTmin_5 = null;
  var futureRsds_1 = null, futureRsds_2 = null;
  var futureDaylength = null;

  // Only compute variables if their corresponding coefficients are non-zero

  // Tmean variables
  if (params.tmeanLinear_1 !== 0 || params.tmeanQuadratic_1 !== 0) {
      currentTmean_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                      plantingDOY.add(params.tmean1_start), plantingDOY.add(params.tmean1_end),
                                                                                      'tmean','tmean',
                                                                                      params.tmeanMean, params.tmeanStdDev, true);
      futureTmean_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmean1_start), plantingDOY.add(params.tmean1_end),
                                                                                   'tmean','tmean',
                                                                                   params.tmeanMean, params.tmeanStdDev, true);
  }

  if (params.tmeanLinear_2 !== 0 || params.tmeanQuadratic_2 !== 0) {
      currentTmean_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                      plantingDOY.add(params.tmean2_start), plantingDOY.add(params.tmean2_end),
                                                                                      'tmean','tmean',
                                                                                      params.tmeanMean, params.tmeanStdDev, true);
      futureTmean_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmean2_start), plantingDOY.add(params.tmean2_end),
                                                                                   'tmean','tmean',
                                                                                   params.tmeanMean, params.tmeanStdDev, true);
  }

  // Tmax variables
  if (params.tmaxLinear_1 !== 0 || params.tmaxQuadratic_1 !== 0) {
      currentTmax_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmax1_start), plantingDOY.add(params.tmax1_end),
                                                                                  'tasmax', 'tmax',
                                                                                  params.tmaxMean, params.tmaxStdDev, true);
      futureTmax_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmax1_start), plantingDOY.add(params.tmax1_end),
                                                                                   'tasmax', 'tmax',	
                                                                                   params.tmaxMean, params.tmaxStdDev, true);
  }

  if (params.tmaxLinear_2 !== 0 || params.tmaxQuadratic_2 !== 0) {
      currentTmax_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmax2_start), plantingDOY.add(params.tmax2_end),
                                                                                  'tasmax', 'tmax',
                                                                                  params.tmaxMean, params.tmaxStdDev, true);
      futureTmax_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmax2_start), plantingDOY.add(params.tmax2_end),
                                                                                   'tasmax', 'tmax',
                                                                                   params.tmaxMean, params.tmaxStdDev, true);
  }

  if (params.tmaxLinear_3 !== 0 || params.tmaxQuadratic_3 !== 0) {
      currentTmax_3 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmax3_start), plantingDOY.add(params.tmax3_end),
                                                                                  'tasmax', 'tmax',
                                                                                  params.tmaxMean, params.tmaxStdDev, true);
      futureTmax_3 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmax3_start), plantingDOY.add(params.tmax3_end),
                                                                                   'tasmax', 'tmax',	
                                                                                   params.tmaxMean, params.tmaxStdDev, true);
  }

  // VPD variables
  if (params.vpdLinear_1 !== 0 || params.vpdQuadratic_1 !== 0) {
      currentVPD_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.vpd1_start), plantingDOY.add(params.vpd1_end), 
                                                                                  'vpd', 'vpd',
                                                                                  params.vpdMean, params.vpdStdDev, false);
      futureVPD_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.vpd1_start), plantingDOY.add(params.vpd1_end), 
                                                                                   'vpd', 'vpd',
                                                                                   params.vpdMean, params.vpdStdDev, false);
  }

  if (params.vpdLinear_2 !== 0 || params.vpdQuadratic_2 !== 0) {
      currentVPD_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.vpd2_start), plantingDOY.add(params.vpd2_end), 
                                                                                  'vpd', 'vpd',
                                                                                  params.vpdMean, params.vpdStdDev, false);
      futureVPD_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.vpd2_start), plantingDOY.add(params.vpd2_end), 
                                                                                   'vpd', 'vpd',
                                                                                   params.vpdMean, params.vpdStdDev, false);
  }

  if (params.vpdLinear_3 !== 0 || params.vpdQuadratic_3 !== 0) {
      currentVPD_3 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.vpd3_start), plantingDOY.add(params.vpd3_end), 
                                                                                  'vpd', 'vpd',
                                                                                  params.vpdMean, params.vpdStdDev, false);
      futureVPD_3 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.vpd3_start), plantingDOY.add(params.vpd3_end), 
                                                                                   'vpd', 'vpd',
                                                                                   params.vpdMean, params.vpdStdDev, false);
  }
 if (params.vpdLinear_4 !== 0 || params.vpdQuadratic_4 !== 0) {
      currentVPD_4 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.vpd4_start), plantingDOY.add(params.vpd4_end), 
                                                                                  'vpd', 'vpd',
                                                                                  params.vpdMean, params.vpdStdDev, false);
      futureVPD_4 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.vpd4_start), plantingDOY.add(params.vpd4_end), 
                                                                                   'vpd', 'vpd',
                                                                                   params.vpdMean, params.vpdStdDev, false);
  }
  if (params.gddLinear_1 !== 0 || params.gddQuadratic_1 !== 0) {
      currentGDD_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                       plantingDOY.add(params.gdd1_start), plantingDOY.add(params.gdd1_end), 
                                                                                       'GDD_modave', 'GDD_modave',
                                                                                       params.gddMean, params.gddStdDev, false);
      futureGDD_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                      plantingDOY.add(params.gdd1_start), plantingDOY.add(params.gdd1_end), 
                                                                                      'GDD_modave', 'GDD_modave',
                                                                                       params.gddMean, params.gddStdDev, false);
  }

  if (params.gddLinear_2 !== 0 || params.gddQuadratic_2 !== 0) {
      currentGDD_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                       plantingDOY.add(params.gdd2_start), plantingDOY.add(params.gdd2_end), 
                                                                                       'GDD_modave', 'GDD_modave',
                                                                                       params.gddMean, params.gddStdDev, false);
      futureGDD_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                      plantingDOY.add(params.gdd2_start), plantingDOY.add(params.gdd2_end), 
                                                                                      'GDD_modave', 'GDD_modave',
                                                                                       params.gddMean, params.gddStdDev, false);
  }

  // Precipitation
  if (params.pptLinear !== 0) {
      currentPR = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                  plantingDOY.add(params.pr_start), plantingDOY.add(params.pr_end),
                                                                  'pr', 'pr',
                                                                  params.prMean, params.prStdDev, false);
      futurePR = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                               plantingDOY.add(params.pr_start), plantingDOY.add(params.pr_end), 
                                                               'pr', 'pr',
                                                                  params.prMean, params.prStdDev, false);
  }

  // Tmin variables
  if (params.tminLinear_1 !== 0 || params.tminQuadratic_1 !== 0) {
      currentTmin_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmin1_start), plantingDOY.add(params.tmin1_end),
                                                                                  'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
      futureTmin_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmin1_start), plantingDOY.add(params.tmin1_end), 
                                                                                   'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
  }
  if (params.tminLinear_2 !== 0 || params.tminQuadratic_2 !== 0) {
      currentTmin_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmin2_start), plantingDOY.add(params.tmin2_end), 
                                                                                  'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
      futureTmin_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmin2_start), plantingDOY.add(params.tmin2_end), 
                                                                                   'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
  }

  if (params.tminLinear_3 !== 0 || params.tminQuadratic_3 !== 0) {
      currentTmin_3 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmin3_start), plantingDOY.add(params.tmin3_end), 
                                                                                  'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
      futureTmin_3 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmin3_start), plantingDOY.add(params.tmin3_end), 
                                                                                   'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
  }

  if (params.tminLinear_4 !== 0 || params.tminQuadratic_4 !== 0) {
      currentTmin_4 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmin4_start), plantingDOY.add(params.tmin4_end), 
                                                                                  'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
      futureTmin_4 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmin4_start), plantingDOY.add(params.tmin4_end), 
                                                                                   'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
  }

  if (params.tminLinear_5 !== 0 || params.tminQuadratic_5 !== 0) {
      currentTmin_5 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.tmin5_start), plantingDOY.add(params.tmin5_end), 
                                                                                  'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
      futureTmin_5 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.tmin5_start), plantingDOY.add(params.tmin5_end), 
                                                                                   'tasmin', 'tmin',
                                                                                  params.tminMean, params.tminStdDev, true);
  }
                                                                          


  // Solar radiation variables
  if (params.rsdsLinear_1 !== 0 || params.rsdsQuadratic_1 !== 0) {
      currentRsds_1 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.rsds1_start), plantingDOY.add(params.rsds1_end), 
                                                                                  'rsds', 'rsds',
                                                                                  params.rsdsWMean, params.rsdsStdDev, false);
      futureRsds_1 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.rsds1_start), plantingDOY.add(params.rsds1_end), 
                                                                                   'rsds', 'rsds',
                                                                                   params.rsdsWMean, params.rsdsStdDev, false);
  }

  if (params.rsdsLinear_2 !== 0 || params.rsdsQuadratic_2 !== 0) {
      currentRsds_2 = computeMultiYearMACAVariable(state, currentStartYear, currentEndYear, macaHistorical, 
                                                                                  plantingDOY.add(params.rsds2_start), plantingDOY.add(params.rsds2_end),
                                                                                  'rsds', 'rsds',
                                                                                  params.rsdsWMean, params.rsdsStdDev, false);
      futureRsds_2 = computeMultiYearMACAVariable(state, futureStartYear, futureEndYear, macaFuture, 
                                                                                   plantingDOY.add(params.rsds2_start), plantingDOY.add(params.rsds2_end), 
                                                                                   'rsds', 'rsds',
                                                                                   params.rsdsWMean, params.rsdsStdDev, false);
  }

  // Daylength
  if (params.daylengthLinear !== 0 || params.daylengthQuadratic !== 0) {
      currentDaylength = computeMultiYearDaylengthDaymet(state, currentStartYear, currentEndYear, 
                                                                                                          plantingDOY.add(params.dayl_start), plantingDOY.add(params.dayl_end), 
                                                                                                          params.daylengthMean, params.daylengthStdDev);
      // daylength not expected to change
      futureDaylength = computeMultiYearDaylengthDaymet(state, currentStartYear, currentEndYear, 
                                                                                                       plantingDOY.add(params.dayl_start), plantingDOY.add(params.dayl_end), 
                                                                                                       params.daylengthMean, params.daylengthStdDev);
  }


  // Calculate site quality for current period using variables with non-zero params
  var currentSiteQuality = ee.Image(params.intercept);


  // Tmean terms
  if (params.tmeanLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmean_1.select('tmean_scaled').multiply(params.tmeanLinear_1));
  }
  if (params.tmeanQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmean_1.select('tmean_scaled').pow(2).multiply(params.tmeanQuadratic_1));
  }
  if (params.tmeanLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmean_2.select('tmean_scaled').multiply(params.tmeanLinear_2));
  }
  if (params.tmeanQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmean_2.select('tmean_scaled').pow(2).multiply(params.tmeanQuadratic_2));
  }

  // Tmax terms
  if (params.tmaxLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_1.select('tmax_scaled').multiply(params.tmaxLinear_1));
  }
  if (params.tmaxQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_1.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_1));
  }
  if (params.tmaxLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_2.select('tmax_scaled').multiply(params.tmaxLinear_2));
  }
  if (params.tmaxQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_2.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_2));
  }
  if (params.tmaxLinear_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_3.select('tmax_scaled').multiply(params.tmaxLinear_3));
  }
  if (params.tmaxQuadratic_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmax_3.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_3));
  }

  // VPD terms
  if (params.vpdLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_1.select('vpd_scaled').multiply(params.vpdLinear_1));
  }
  if (params.vpdQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_1.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_1));
  }
  if (params.vpdLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_2.select('vpd_scaled').multiply(params.vpdLinear_2));
  }
  if (params.vpdQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_2.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_2));
  }
  if (params.vpdLinear_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_3.select('vpd_scaled').multiply(params.vpdLinear_3));
  }
  if (params.vpdQuadratic_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_3.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_3));
  }
  if (params.vpdLinear_4 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_4.select('vpd_scaled').multiply(params.vpdLinear_4));
  }
  if (params.vpdQuadratic_4 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentVPD_4.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_4));
  }

  // GDD terms
  if (params.gddLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentGDD_1.select('GDD_modave_scaled').multiply(params.gddLinear_1));
  }
  if (params.gddQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentGDD_1.select('GDD_modave_scaled').pow(2).multiply(params.gddQuadratic_1));
  }
  if (params.gddLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentGDD_2.select('GDD_modave_scaled').multiply(params.gddLinear_2));
  }
  if (params.gddQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentGDD_2.select('GDD_modave_scaled').pow(2).multiply(params.gddQuadratic_2));
  }

  // Tmin terms
  if (params.tminLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_1.select('tmin_scaled').multiply(params.tminLinear_1));
  }
  if (params.tminQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_1.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_1));
  }
  if (params.tminLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_2.select('tmin_scaled').multiply(params.tminLinear_2));
  }
  if (params.tminQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_2.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_2));
  }
  if (params.tminLinear_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_3.select('tmin_scaled').multiply(params.tminLinear_3));
  }
  if (params.tminQuadratic_3 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_3.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_3));
  }
  if (params.tminLinear_4 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_4.select('tmin_scaled').multiply(params.tminLinear_4));
  }
  if (params.tminQuadratic_4 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_4.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_4));
  }
  if (params.tminLinear_5 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_5.select('tmin_scaled').multiply(params.tminLinear_5));
  }
  if (params.tminQuadratic_5 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentTmin_5.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_5));
  }

  // Solar radiation terms
  if (params.rsdsLinear_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentRsds_1.select('rsds_scaled').multiply(params.rsdsLinear_1));
  }
  if (params.rsdsQuadratic_1 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentRsds_1.select('rsds_scaled').pow(2).multiply(params.rsdsQuadratic_1));
  }
  if (params.rsdsLinear_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentRsds_2.select('rsds_scaled').multiply(params.rsdsLinear_2));
  }
  if (params.rsdsQuadratic_2 !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentRsds_2.select('rsds_scaled').pow(2).multiply(params.rsdsQuadratic_2));
  }

  // Precipitation term
  if (params.pptLinear !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentPR.select('pr_scaled').multiply(params.pptLinear));
  }

  // Daylength terms
  if (params.daylengthLinear !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentDaylength.select('daylength_scaled').multiply(params.daylengthLinear));
  }
  if (params.daylengthQuadratic !== 0) {
      currentSiteQuality = currentSiteQuality.add(currentDaylength.select('daylength_scaled').pow(2).multiply(params.daylengthQuadratic));
  }

  // Calculate site quality for future period using the same logic
  var futureSiteQuality = ee.Image(params.intercept);

  // Tmean terms
  if (params.tmeanLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmean_1.select('tmean_scaled').multiply(params.tmeanLinear_1));
  }
  if (params.tmeanQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmean_1.select('tmean_scaled').pow(2).multiply(params.tmeanQuadratic_1));
  }
  if (params.tmeanLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmean_2.select('tmean_scaled').multiply(params.tmeanLinear_2));
  }
  if (params.tmeanQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmean_2.select('tmean_scaled').pow(2).multiply(params.tmeanQuadratic_2));
  }

  // Tmax terms
  if (params.tmaxLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_1.select('tmax_scaled').multiply(params.tmaxLinear_1));
  }
  if (params.tmaxQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_1.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_1));
  }
  if (params.tmaxLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_2.select('tmax_scaled').multiply(params.tmaxLinear_2));
  }
  if (params.tmaxQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_2.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_2));
  }
  if (params.tmaxLinear_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_3.select('tmax_scaled').multiply(params.tmaxLinear_3));
  }
  if (params.tmaxQuadratic_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmax_3.select('tmax_scaled').pow(2).multiply(params.tmaxQuadratic_3));
  }

  // VPD terms
  if (params.vpdLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_1.select('vpd_scaled').multiply(params.vpdLinear_1));
  }
  if (params.vpdQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_1.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_1));
  }
  if (params.vpdLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_2.select('vpd_scaled').multiply(params.vpdLinear_2));
  }
  if (params.vpdQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_2.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_2));
  }
  if (params.vpdLinear_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_3.select('vpd_scaled').multiply(params.vpdLinear_3));
  }
  if (params.vpdQuadratic_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_3.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_3));
  }
  if (params.vpdLinear_4 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_4.select('vpd_scaled').multiply(params.vpdLinear_4));
  }
  if (params.vpdQuadratic_4 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureVPD_4.select('vpd_scaled').pow(2).multiply(params.vpdQuadratic_4));
  }

  // GDD terms
  if (params.gddLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureGDD_1.select('GDD_modave_scaled').multiply(params.gddLinear_1));
  }
  if (params.gddQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureGDD_1.select('GDD_modave_scaled').pow(2).multiply(params.gddQuadratic_1));
  }
  if (params.gddLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureGDD_2.select('GDD_modave_scaled').multiply(params.gddLinear_2));
  }
  if (params.gddQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureGDD_2.select('GDD_modave_scaled').pow(2).multiply(params.gddQuadratic_2));
  }

  // Tmin terms
  if (params.tminLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_1.select('tmin_scaled').multiply(params.tminLinear_1));
  }
  if (params.tminQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_1.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_1));
  }
  if (params.tminLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_2.select('tmin_scaled').multiply(params.tminLinear_2));
  }
  if (params.tminQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_2.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_2));
  }
  if (params.tminLinear_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_3.select('tmin_scaled').multiply(params.tminLinear_3));
  }
  if (params.tminQuadratic_3 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_3.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_3));
  }
  if (params.tminLinear_4 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_4.select('tmin_scaled').multiply(params.tminLinear_4));
  }
  if (params.tminQuadratic_4 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_4.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_4));
  }
  if (params.tminLinear_5 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_5.select('tmin_scaled').multiply(params.tminLinear_5));
  }
  if (params.tminQuadratic_5 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureTmin_5.select('tmin_scaled').pow(2).multiply(params.tminQuadratic_5));
  }

  // Solar radiation terms
  if (params.rsdsLinear_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureRsds_1.select('rsds_scaled').multiply(params.rsdsLinear_1));
  }
  if (params.rsdsQuadratic_1 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureRsds_1.select('rsds_scaled').pow(2).multiply(params.rsdsQuadratic_1));
  }
  if (params.rsdsLinear_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureRsds_2.select('rsds_scaled').multiply(params.rsdsLinear_2));
  }
  if (params.rsdsQuadratic_2 !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureRsds_2.select('rsds_scaled').pow(2).multiply(params.rsdsQuadratic_2));
  }

  // Precipitation term
  if (params.pptLinear !== 0) {
      futureSiteQuality = futureSiteQuality.add(futurePR.select('pr_scaled').multiply(params.pptLinear));
  }

  // Daylength terms
  if (params.daylengthLinear !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureDaylength.select('daylength_scaled').multiply(params.daylengthLinear));
  }
  if (params.daylengthQuadratic !== 0) {
      futureSiteQuality = futureSiteQuality.add(futureDaylength.select('daylength_scaled').pow(2).multiply(params.daylengthQuadratic));
  }
    
  // Calculate difference
  var siteQualityDifference = futureSiteQuality.subtract(currentSiteQuality).rename('site_quality_difference');
  currentSiteQuality = currentSiteQuality.rename('current_site_quality');
  futureSiteQuality = futureSiteQuality.rename('future_site_quality');

  // Get state bounds
  var stateBounds = state.geometry().bounds();
  
  // Only perform exports if doExport is true
  if (doExport) {
    // Export site quality difference for this state with parameter set name
    Export.image.toDrive({
      image: siteQualityDifference,
      description: 'SiteQualityDifference_' + parameterSet + '_' + stateName.getInfo().replace(' ', '_'),
      region: stateBounds,
      scale: 4000,
      maxPixels: 1e9,
      fileFormat: 'GeoTIFF'
    });
  }

  // Combine all scaled input bands and outputs into a single multiband image for return
  var allBands = ee.Image().rename('placeholder');
  
  // Add each scaled band if it exists (current period)
  if (currentTmean_1) allBands = allBands.addBands(currentTmean_1.select('tmean_scaled').rename('current_tmean_1'));
  if (currentTmean_2) allBands = allBands.addBands(currentTmean_2.select('tmean_scaled').rename('current_tmean_2'));
  if (currentTmax_1) allBands = allBands.addBands(currentTmax_1.select('tmax_scaled').rename('current_tmax_1'));
  if (currentTmax_2) allBands = allBands.addBands(currentTmax_2.select('tmax_scaled').rename('current_tmax_2'));
  if (currentTmax_3) allBands = allBands.addBands(currentTmax_3.select('tmax_scaled').rename('current_tmax_3'));
  if (currentVPD_1) allBands = allBands.addBands(currentVPD_1.select('vpd_scaled').rename('current_vpd_1'));
  if (currentVPD_2) allBands = allBands.addBands(currentVPD_2.select('vpd_scaled').rename('current_vpd_2'));
  if (currentVPD_3) allBands = allBands.addBands(currentVPD_3.select('vpd_scaled').rename('current_vpd_3'));
  if (currentVPD_4) allBands = allBands.addBands(currentVPD_4.select('vpd_scaled').rename('current_vpd_4'));
  if (currentGDD_1) allBands = allBands.addBands(currentGDD_1.select('GDD_modave_scaled').rename('current_gdd_1'));
  if (currentGDD_2) allBands = allBands.addBands(currentGDD_2.select('GDD_modave_scaled').rename('current_gdd_2'));
  if (currentPR) allBands = allBands.addBands(currentPR.select('pr_scaled').rename('current_pr'));
  if (currentTmin_1) allBands = allBands.addBands(currentTmin_1.select('tmin_scaled').rename('current_tmin_1'));
  if (currentTmin_2) allBands = allBands.addBands(currentTmin_2.select('tmin_scaled').rename('current_tmin_2'));
  if (currentTmin_3) allBands = allBands.addBands(currentTmin_3.select('tmin_scaled').rename('current_tmin_3'));
  if (currentTmin_4) allBands = allBands.addBands(currentTmin_4.select('tmin_scaled').rename('current_tmin_4'));
  if (currentTmin_5) allBands = allBands.addBands(currentTmin_5.select('tmin_scaled').rename('current_tmin_5'));
  if (currentRsds_1) allBands = allBands.addBands(currentRsds_1.select('rsds_scaled').rename('current_rsds_1'));
  if (currentRsds_2) allBands = allBands.addBands(currentRsds_2.select('rsds_scaled').rename('current_rsds_2'));
  if (currentDaylength) allBands = allBands.addBands(currentDaylength.select('daylength_scaled').rename('current_daylength'));
  
  // Add each scaled band if it exists (future period)
  if (futureTmean_1) allBands = allBands.addBands(futureTmean_1.select('tmean_scaled').rename('future_tmean_1'));
  if (futureTmean_2) allBands = allBands.addBands(futureTmean_2.select('tmean_scaled').rename('future_tmean_2'));
  if (futureTmax_1) allBands = allBands.addBands(futureTmax_1.select('tmax_scaled').rename('future_tmax_1'));
  if (futureTmax_2) allBands = allBands.addBands(futureTmax_2.select('tmax_scaled').rename('future_tmax_2'));
  if (futureTmax_3) allBands = allBands.addBands(futureTmax_3.select('tmax_scaled').rename('future_tmax_3'));
  if (futureVPD_1) allBands = allBands.addBands(futureVPD_1.select('vpd_scaled').rename('future_vpd_1'));
  if (futureVPD_2) allBands = allBands.addBands(futureVPD_2.select('vpd_scaled').rename('future_vpd_2'));
  if (futureVPD_3) allBands = allBands.addBands(futureVPD_3.select('vpd_scaled').rename('future_vpd_3'));
  if (futureVPD_4) allBands = allBands.addBands(futureVPD_4.select('vpd_scaled').rename('future_vpd_4'));
  if (futureGDD_1) allBands = allBands.addBands(futureGDD_1.select('GDD_modave_scaled').rename('future_gdd_1'));
  if (futureGDD_2) allBands = allBands.addBands(futureGDD_2.select('GDD_modave_scaled').rename('future_gdd_2'));
  if (futurePR) allBands = allBands.addBands(futurePR.select('pr_scaled').rename('future_pr'));
  if (futureTmin_1) allBands = allBands.addBands(futureTmin_1.select('tmin_scaled').rename('future_tmin_1'));
  if (futureTmin_2) allBands = allBands.addBands(futureTmin_2.select('tmin_scaled').rename('future_tmin_2'));
  if (futureTmin_3) allBands = allBands.addBands(futureTmin_3.select('tmin_scaled').rename('future_tmin_3'));
  if (futureTmin_4) allBands = allBands.addBands(futureTmin_4.select('tmin_scaled').rename('future_tmin_4'));
  if (futureTmin_5) allBands = allBands.addBands(futureTmin_5.select('tmin_scaled').rename('future_tmin_5'));
  if (futureRsds_1) allBands = allBands.addBands(futureRsds_1.select('rsds_scaled').rename('future_rsds_1'));
  if (futureRsds_2) allBands = allBands.addBands(futureRsds_2.select('rsds_scaled').rename('future_rsds_2'));
  if (futureDaylength) allBands = allBands.addBands(futureDaylength.select('daylength_scaled').rename('future_daylength'));

  // Add site quality outputs
  allBands = allBands.addBands(currentSiteQuality)
                    .addBands(futureSiteQuality)
                    .addBands(siteQualityDifference);

  // Remove placeholder band
  allBands = allBands.select(allBands.bandNames().slice(1));

  return allBands;
}

// Process each state individually
var statesList = targetStates.toList(targetStates.size());
var numStates = statesList.size().getInfo();

for (var i = 0; i < numStates; i++) {
  var state = ee.Feature(statesList.get(i));
  processState(state, true);
}

print('Processing complete for parameter set:', parameterSet);
print('Check Tasks tab for export status.');
print('Number of states processed:', numStates);
print('Total exports:', numStates, '(1 per state: site quality difference only)');

// Visualize the cropland mask and sample point on the interactive map
Map.centerObject(samplePoint, 10);

// Add cropland mask to map
Map.addLayer(croplandMask, {
    min: 0, 
    max: 1, 
    palette: ['white', 'green']
}, 'Cropland Mask');

// Add sample point to map
Map.addLayer(samplePoint, {
    color: 'red'
}, 'Sample Point');

// Add state boundaries for context
Map.addLayer(targetStates, {
    color: 'blue',
    fillColor: '00000000'
}, 'Study States');

// After standard exports, process sample point state with no exports
// and extract values at the sample point for CSV export
var pointResults = processState(sampleState, false);

// Sample all bands at the sample point
var sampledValues = pointResults.reduceRegion({
  reducer: ee.Reducer.first(),
  geometry: samplePoint,
  scale: 4000,
  maxPixels: 1e9
});

// Convert to a feature with properties for CSV export
var sampleFeature = ee.Feature(samplePoint, sampledValues)
  .set('parameterSet', parameterSet)
  .set('latitude', SAMPLE_LAT)
  .set('longitude', SAMPLE_LON)
  .set('state', sampleState.get('NAME'));

// Export the single row CSV
Export.table.toDrive({
  collection: ee.FeatureCollection([sampleFeature]),
  description: 'SinglePixel_' + parameterSet + '_' + SAMPLE_LAT + '_' + SAMPLE_LON,
  fileFormat: 'CSV'
});

print('Scheduled export of sample point data at:', SAMPLE_LON, SAMPLE_LAT);