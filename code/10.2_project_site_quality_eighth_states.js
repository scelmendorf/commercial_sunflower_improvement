// GEE Site Quality - Configurable for Historical or Future Climate Export
// Simplified script to export site quality for specified time period to reduce memory usage
// this one does BNU-ESM 3 & 8 closest/ best for both but does not have the additional date filter
// this one has the above modifications but also the right date range for future 

// Define a function to split a state into eight chunks (2x2x2 grid)
function splitStateIntoEighths(state) {
  var geometry = state.geometry();
  var bounds = geometry.bounds();
  var coords = ee.List(bounds.coordinates().get(0));
  
  // Get min and max latitude and longitude
  var lats = coords.map(function(coord) {
    return ee.List(coord).get(1);
  });
  var lons = coords.map(function(coord) {
    return ee.List(coord).get(0);
  });
  
  var minLat = ee.Number(lats.reduce(ee.Reducer.min()));
  var maxLat = ee.Number(lats.reduce(ee.Reducer.max()));
  var latQuarter1 = minLat.add(maxLat.subtract(minLat).multiply(0.25));
  var latMid = minLat.add(maxLat.subtract(minLat).multiply(0.5));
  var latQuarter3 = minLat.add(maxLat.subtract(minLat).multiply(0.75));
  
  var minLon = ee.Number(lons.reduce(ee.Reducer.min()));
  var maxLon = ee.Number(lons.reduce(ee.Reducer.max()));
  var lonQuarter1 = minLon.add(maxLon.subtract(minLon).multiply(0.25));
  var lonMid = minLon.add(maxLon.subtract(minLon).multiply(0.5));
  var lonQuarter3 = minLon.add(maxLon.subtract(minLon).multiply(0.75));
  
  // Create eight bounding boxes
  var boxes = [
    // Bottom row (South)
    ee.Geometry.Rectangle([minLon, minLat, lonQuarter1, latQuarter1]),      // SW1
    ee.Geometry.Rectangle([lonQuarter1, minLat, lonMid, latQuarter1]),       // SW2
    ee.Geometry.Rectangle([lonMid, minLat, lonQuarter3, latQuarter1]),       // SE1
    ee.Geometry.Rectangle([lonQuarter3, minLat, maxLon, latQuarter1]),       // SE2
    // Top row (North)
    ee.Geometry.Rectangle([minLon, latQuarter3, lonQuarter1, maxLat]),       // NW1
    ee.Geometry.Rectangle([lonQuarter1, latQuarter3, lonMid, maxLat]),       // NW2
    ee.Geometry.Rectangle([lonMid, latQuarter3, lonQuarter3, maxLat]),       // NE1
    ee.Geometry.Rectangle([lonQuarter3, latQuarter3, maxLon, maxLat])        // NE2
  ];
  
  // Clip state geometry to create eight chunks
  var chunks = boxes.map(function(box) {
    return geometry.intersection(box);
  });
  
  return chunks;
}

// Process state function - Export sum of all scaled variables multiplied by coefficients
function processStateCurrentOnly(state) {
  var stateName = state.getString('NAME');
  var plantingDOY = ee.Number(plantingDatesDict.get(stateName));
  
  print('Processing state for', timePeriodLabel, 'climate (', activeStartYear, '-', activeEndYear, '):', stateName);
  
  // Check if state should be split (e.g., Texas, large states)
  var largeStates = ['Texas', 'North Dakota', 'Minnesota', 'South Dakota', 'Nebraska']; // Add states that need splitting
  
  var shouldSplit = largeStates.indexOf(stateName.getInfo()) !== -1;
  
  if (shouldSplit) {
    print('Splitting', stateName, 'into eight chunks');
    var chunks = splitStateIntoEighths(state);
    
    // Process each chunk
    var chunkNames = ['SW1', 'SW2', 'SE1', 'SE2', 'NW1', 'NW2', 'NE1', 'NE2'];
    for (var c = 0; c < 8; c++) {
      var chunkState = ee.Feature(state).setGeometry(chunks[c]);
      processHalf(chunkState, stateName.getInfo() + '_' + chunkNames[c], plantingDOY);
    }
  } else {
    // Process whole state
    processHalf(state, stateName.getInfo(), plantingDOY);
  }
}


// Load boundaries and cropland mask
var states = ee.FeatureCollection("TIGER/2018/States");
var plantingDatesDict = ee.Dictionary({
  //'Texas': 152,    // 31 May 
  //'South Dakota': 160,    // June 8
  'North Dakota': 153,    // June 1
  //'Minnesota': 146, //,    // May 24
  //'Nebraska': 161, // June 9 
  //'Colorado': 165, // June 13 
  //'Kansas': 164, // June 12 
  //'Oklahoma': 158 // No info but split difference btw Kansas and TX
});

// SELECT PARAMETER SET - CHANGE THIS TO USE DIFFERENT MODEL CONFIGURATIONS
var parameterSet = 'ranef_includeTX_noYr'; // Options: 'ranef_includeTX', 'ranef_noTX',
//'fixef_includeTX'. 'fixef_noTX', 
// 'ranef_noTX_noYr', 'fixef_noTX_noYr',
//'ranef_includeTX_noYr', 'fixef_includeTX_noYr'

//done 'ranef_noTX_noYr', fixef_includeTX, fixef_noTX,
//fixef_noTX_noYr, fixef_includeTX_noYr, ranef_includeTX,
// ranef_noTX


// SELECT TIME PERIOD
var timePeriod = 'current'; // Options: 'current' (1990-2010) or 'future' (2030-2050)

// PARAMETER SET DEFINITIONS
// FROM gee_parameter_sets_aic.js
// first changing NULL to "mean" (GEE doesn't like the NULL)
// and removing ,,
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
    intercept: -5546.543156, yearcoef: 3.657757,
    tmaxLinear_1: 1524.213195, tmaxQuadratic_1: -1089.062941, tmaxForm_1: "mean",
    tmaxLinear_2: 0, tmaxQuadratic_2: 0, tmaxForm_2: "mean",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 0, tmeanQuadratic_1: 0, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 0, gddQuadratic_1: 0, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: -18.423086, vpdQuadratic_1: -52.838642, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 5420.842322, tminQuadratic_1: -1792.481615, tminForm_1: "mean",
    tminLinear_2: 331.754946, tminQuadratic_2: 0, tminForm_2: "min",
    tminLinear_3: 4180.582505, tminQuadratic_3: -2688.117021, tminForm_3: "mean",
    tminLinear_4: -3620.532396, tminQuadratic_4: 2365.534338, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: 0, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 0, pptForm_1: "mean",
    daylengthLinear: 1083.089794, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 150, tmean1_end: 150,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 52, tmax1_end: 72,
    tmax2_start: 150, tmax2_end: 150,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 17, vpd1_end: 23,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 150, gdd1_end: 150,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 150, pr_end: 150,
    tmin1_start: 10, tmin1_end: 93,
    tmin2_start: 129, tmin2_end: 142,
    tmin3_start: 73, tmin3_end: 100,
    tmin4_start: 73, tmin4_end: 114,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 150, rsds1_end: 150,
    rsds2_start: 150, rsds2_end: 150,
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
    intercept: -1588.573545, yearcoef: 7.722674,
    tmaxLinear_1: 1961.974389, tmaxQuadratic_1: -884.267767, tmaxForm_1: "mean",
    tmaxLinear_2: 494.789988, tmaxQuadratic_2: 0, tmaxForm_2: "max",
    tmaxLinear_3: 33.766411, tmaxQuadratic_3: -459.428758, tmaxForm_3: "mean",
    tmeanLinear_1: 2170.946003, tmeanQuadratic_1: -1079.116538, tmeanForm_1: "mean",
    tmeanLinear_2: -2991.462833, tmeanQuadratic_2: 1826.125113, tmeanForm_2: "mean",
    gddLinear_1: 2220.968425, gddQuadratic_1: -1232.580119, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: -396.904594, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: 397.061003, vpdQuadratic_2: -177.55543, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 334.5858, tminQuadratic_1: 0, tminForm_1: "min",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: 0, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 144.749791, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 80, tmean2_end: 114,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 65,
    tmax3_start: 122, tmax3_end: 135,
    vpd1_start: 45, vpd1_end: 72,
    vpd2_start: 80, vpd2_end: 86,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 150, rsds1_end: 150,
    rsds2_start: 150, rsds2_end: 150,
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
    intercept: -2578.466463, yearcoef: 7.292034,
    tmaxLinear_1: 1046.760953, tmaxQuadratic_1: -690.601424, tmaxForm_1: "mean",
    tmaxLinear_2: 3396.497517, tmaxQuadratic_2: -1683.500667, tmaxForm_2: "mean",
    tmaxLinear_3: 3.436788, tmaxQuadratic_3: -410.923678, tmaxForm_3: "mean",
    tmeanLinear_1: 2544.027761, tmeanQuadratic_1: -1298.063062, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 0, gddQuadratic_1: 0, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: 0, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 297.658724, tminQuadratic_1: 65.264033, tminForm_1: "mean",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: -610.272318, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 155.684092, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 52, tmax1_end: 72,
    tmax2_start: 66, tmax2_end: 86,
    tmax3_start: 122, tmax3_end: 135,
    vpd1_start: 150, vpd1_end: 150,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 150, gdd1_end: 150,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 10, rsds1_end: 65,
    rsds2_start: 150, rsds2_end: 150,
    dayl_start: 10, dayl_end: 10
},

'fixef_includeTX': {
    // Scaling factors
    tmeanMean: 0, tmeanStdDev: 0,
    vpdMean: 1.475844, vpdStdDev: 1.160122,
    gddMean: 6.045973, gddStdDev: 5.585659,
    tmaxMean: 15.005608, tmaxStdDev: 13.439846,
    tminMean: 0, tminStdDev: 0,
    prMean: 0, prStdDev: 0,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -912.645074, yearcoef: 6.386316,
    tmaxLinear_1: 2445.998145, tmaxQuadratic_1: -1220.641117, tmaxForm_1: "mean",
    tmaxLinear_2: 0, tmaxQuadratic_2: 0, tmaxForm_2: "mean",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 0, tmeanQuadratic_1: 0, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 324.259796, gddQuadratic_1: 0, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: -168.50893, vpdQuadratic_1: -69.29158, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 0, tminQuadratic_1: 0, tminForm_1: "mean",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: 0, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 0, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 150, tmean1_end: 150,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 150, tmax2_end: 150,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 10, vpd1_end: 72,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 38, gdd1_end: 44,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 150, pr_end: 150,
    tmin1_start: 150, tmin1_end: 150,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 150, rsds1_end: 150,
    rsds2_start: 150, rsds2_end: 150,
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
    intercept: -2359.402263, yearcoef: 0,
    tmaxLinear_1: 2222.494614, tmaxQuadratic_1: -1153.670368, tmaxForm_1: "mean",
    tmaxLinear_2: 0, tmaxQuadratic_2: 0, tmaxForm_2: "mean",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 3093.059724, tmeanQuadratic_1: -1523.046036, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 1230.630953, gddQuadratic_1: -670.177289, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: 0, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 236.653538, tminQuadratic_1: 0, tminForm_1: "mean",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: -825.920128, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 163.490025, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 150, tmax2_end: 150,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 150, vpd1_end: 150,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 10, rsds1_end: 65,
    rsds2_start: 150, rsds2_end: 150,
    dayl_start: 10, dayl_end: 10
},

'fixef_noTX_noYr': {
    // Scaling factors
    tmeanMean: 7.864972, tmeanStdDev: 12.279119,
    vpdMean: 1.461327, vpdStdDev: 1.154439,
    gddMean: 5.913104, gddStdDev: 5.503491,
    tmaxMean: 14.771894, tmaxStdDev: 13.412419,
    tminMean: 0, tminStdDev: 0,
    prMean: 1.45715, prStdDev: 4.730427,
    rsdsWMean: 0, rsdsStdDev: 0,
    daylengthMean: 0, daylengthStdDev: 0,

    // Regression coefficients
    intercept: -2842.830804, yearcoef: 0,
    tmaxLinear_1: 2271.843605, tmaxQuadratic_1: -1057.305183, tmaxForm_1: "mean",
    tmaxLinear_2: 588.245376, tmaxQuadratic_2: 0, tmaxForm_2: "max",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 2517.939537, tmeanQuadratic_1: -1248.429445, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 1568.752026, gddQuadratic_1: -846.832527, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: -432.759347, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: -153.985173, vpdQuadratic_2: 203.02974, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 0, tminQuadratic_1: 0, tminForm_1: "mean",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: 0, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 166.03096, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 65,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 45, vpd1_end: 72,
    vpd2_start: 101, vpd2_end: 114,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 129, pr_end: 135,
    tmin1_start: 150, tmin1_end: 150,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 150, rsds1_end: 150,
    rsds2_start: 150, rsds2_end: 150,
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
    intercept: -2926.71279, yearcoef: 0,
    tmaxLinear_1: 1870.778372, tmaxQuadratic_1: -1016.504997, tmaxForm_1: "mean",
    tmaxLinear_2: 0, tmaxQuadratic_2: 0, tmaxForm_2: "mean",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 2693.524424, tmeanQuadratic_1: -1392.268341, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 2602.399999, gddQuadratic_1: -1113.959648, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: 0, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 358.074323, tminQuadratic_1: 0, tminForm_1: "min",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: -779.027094, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: -191.387504, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 0, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 150, tmax2_end: 150,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 150, vpd1_end: 150,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 38, gdd1_end: 100,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 150, pr_end: 150,
    tmin1_start: 66, tmin1_end: 142,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 10, rsds1_end: 65,
    rsds2_start: 129, rsds2_end: 135,
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
    intercept: -2404.468738, yearcoef: 0,
    tmaxLinear_1: 2168.263136, tmaxQuadratic_1: -1035.543458, tmaxForm_1: "mean",
    tmaxLinear_2: 580.43272, tmaxQuadratic_2: 0, tmaxForm_2: "max",
    tmaxLinear_3: 0, tmaxQuadratic_3: 0, tmaxForm_3: "mean",
    tmeanLinear_1: 2454.205142, tmeanQuadratic_1: -1254.914155, tmeanForm_1: "mean",
    tmeanLinear_2: 0, tmeanQuadratic_2: 0, tmeanForm_2: "mean",
    gddLinear_1: 1155.843881, gddQuadratic_1: -708.499016, gddForm_1: "mean",
    gddLinear_2: 0, gddQuadratic_2: 0, gddForm_2: "mean",
    vpdLinear_1: -405.895242, vpdQuadratic_1: 0, vpdForm_1: "mean",
    vpdLinear_2: 0, vpdQuadratic_2: 0, vpdForm_2: "mean",
    vpdLinear_3: 0, vpdQuadratic_3: 0, vpdForm_3: "mean",
    vpdLinear_4: 0, vpdQuadratic_4: 0, vpdForm_4: "mean",
    tminLinear_1: 216.951471, tminQuadratic_1: 0, tminForm_1: "min",
    tminLinear_2: 0, tminQuadratic_2: 0, tminForm_2: "mean",
    tminLinear_3: 0, tminQuadratic_3: 0, tminForm_3: "mean",
    tminLinear_4: 0, tminQuadratic_4: 0, tminForm_4: "mean",
    tminLinear_5: 0, tminQuadratic_5: 0, tminForm_5: "mean",
    rsdsLinear_1: 0, rsdsQuadratic_1: 0, rdsForm_1: "mean",
    rsdsLinear_2: 0, rsdsQuadratic_2: 0, rdsForm_2: "mean",
    pptLinear: 168.892109, pptForm_1: "mean",
    daylengthLinear: 0, daylengthQuadratic: 0, daylengthForm_1: "mean",

    // Time windows (relative to planting DOY)
    tmean1_start: 10, tmean1_end: 30,
    tmean2_start: 150, tmean2_end: 150,
    tmax1_start: 66, tmax1_end: 72,
    tmax2_start: 38, tmax2_end: 65,
    tmax3_start: 150, tmax3_end: 150,
    vpd1_start: 45, vpd1_end: 72,
    vpd2_start: 150, vpd2_end: 150,
    vpd3_start: 150, vpd3_end: 150,
    vpd4_start: 150, vpd4_end: 150,
    gdd1_start: 73, gdd1_end: 100,
    gdd2_start: 150, gdd2_end: 150,
    pr_start: 129, pr_end: 135,
    tmin1_start: 136, tmin1_end: 142,
    tmin2_start: 150, tmin2_end: 150,
    tmin3_start: 150, tmin3_end: 150,
    tmin4_start: 150, tmin4_end: 150,
    tmin5_start: 150, tmin5_end: 150,
    rsds1_start: 150, rsds1_end: 150,
    rsds2_start: 150, rsds2_end: 150,
    dayl_start: 10, dayl_end: 10
}
};

// Get the selected parameter set
var params = paramSets[parameterSet];

// Filter states and get geometry
var targetStates = states.filter(ee.Filter.inList('NAME', plantingDatesDict.keys()));
var studyArea = targetStates.geometry().bounds();

// Create cropland mask
var nlcd = ee.Image('USGS/NLCD_RELEASES/2019_REL/NLCD/2019')
    .select('landcover')
    .clip(studyArea);
var croplandMask = nlcd.eq(82);

// Load DAYMET data for daylength
var daymetCollection = ee.ImageCollection('NASA/ORNL/DAYMET_V4')
  .filterBounds(studyArea)
  .select('dayl');

// Define time periods FIRST
var currentStartYear = 1990;
var currentEndYear = 2010;
var futureStartYear = 2030;
var futureEndYear = 2050;

// Set active time period based on selection
var activeStartYear = timePeriod === 'future' ? futureStartYear : currentStartYear;
var activeEndYear = timePeriod === 'future' ? futureEndYear : currentEndYear;
var activeScenario = timePeriod === 'future' ? 'rcp45' : 'historical';
var timePeriodLabel = timePeriod === 'future' ? 'Future' : 'Current';

// Determine which derived bands are needed
var needsTmean = params.tmeanLinear_1 !== 0 || params.tmeanQuadratic_1 !== 0;
var needsVpd = params.vpdLinear_1 !== 0 || params.vpdQuadratic_1 !== 0;
var needsGdd = params.gddLinear_1 !== 0 || params.gddQuadratic_1 !== 0;

// SINGLE macaCollection definition - with memory optimizations for future
var macaCollection = ee.ImageCollection('IDAHO_EPSCOR/MACAv2_METDATA')
  .filterBounds(studyArea)
  .filter(ee.Filter.eq('scenario', activeScenario))
  //.filter(ee.Filter.neq('model', 'CCSM4'))
  //.filter(ee.Filter.neq('model', 'NorESM1-M'))
  .filter(ee.Filter.inList('model', ['BNU-ESM', 'HadGEM2-CC365', 'CNRM-CM5', 'GFDL-ESM2G', 'bcc-csm1-1-m', 'bcc-csm1-1', 'IPSL-CM5A-LR', 'MIROC5', 'CanESM2', 'MIROC-ESM']))
  // Add single model filter for future to reduce memory usage
  //.filter(timePeriod === 'future' ? ee.Filter.eq('model', 'BNU-ESM') : ee.Filter.lt('system:time_start', ee.Date('2100-01-01').millis()))
  .map(function(image) {
    var clipped = image.clip(studyArea).updateMask(croplandMask);
    
    var bands = [clipped.select(['tasmax', 'tasmin', 'rhsmax', 'rhsmin', 'rsds', 'pr'])];
    
    if (needsTmean) {
      var tasmax = clipped.select('tasmax');
      var tasmin = clipped.select('tasmin');
      var tmean = tasmax.add(tasmin).divide(2);
      bands.push(tmean.rename('tmean'));
    }
    
    if (needsVpd) {
      var tasmax = clipped.select('tasmax');
      var temp_c = tasmax.subtract(273.15);
      var rh = clipped.select('rhsmax').add(clipped.select('rhsmin')).divide(2);
      
      var es = ee.Image(0.611).multiply(
        temp_c.gte(0).multiply(
          temp_c.multiply(17.27).divide(temp_c.add(237.3)).exp()
        ).add(
          temp_c.lt(0).multiply(
            temp_c.multiply(21.875).divide(temp_c.add(265.5)).exp()
          )
        )
      );
      
      var vpd = es.multiply(ee.Image(1).subtract(rh.divide(100)));
      bands.push(vpd.rename('vpd'));
    }
    
    if (needsGdd) {
      var tasmax = clipped.select('tasmax');
      var tasmin = clipped.select('tasmin');
      var tbase = 6.7;
      var tmax = 26;
      var tmaxdegc = tasmax.subtract(273.15);
      var tmindegc = tasmin.subtract(273.15);
      
      var tmax_constrained = tmaxdegc.min(tmax);
      var tmin_constrained = tmindegc.max(tbase);
      var GDD_modave = tmax_constrained.add(tmin_constrained).divide(2).subtract(tbase).max(0);
      bands.push(GDD_modave.rename('GDD_modave'));
    }
    
    return ee.Image.cat(bands);
  });

// Function to compute multi-year MACA variable - with memory optimization
function computeMultiYearMACAVariable(state, startYear, endYear, macaCollection, startDOY, endDOY, macaBandName, outputBaseName, scaleMean, scaleStdDev, convertKelvinToCelsius, varForm) {
  convertKelvinToCelsius = convertKelvinToCelsius !== undefined ? convertKelvinToCelsius : false;
  varForm = varForm !== undefined ? varForm : 'mean';
  
  var filteredCollection = macaCollection
    .filterBounds(state.geometry())
    .filterDate(ee.Date.fromYMD(startYear, 1, 1), ee.Date.fromYMD(endYear, 12, 31))
    .filter(ee.Filter.dayOfYear(startDOY, endDOY))
    .select(macaBandName);
  
  var models = filteredCollection.aggregate_array('model').distinct();
  // Use fewer years for future scenarios to reduce memory
  var yearStep = timePeriod === 'future' ? 2 : 1; // Every 2nd year for future
  var years = ee.List.sequence(startYear, endYear, yearStep);
  
  var modelYearStats = models.map(function(model) {
    var modelData = filteredCollection.filter(ee.Filter.eq('model', model));
    
    var yearlyStats = years.map(function(year) {
      var yearData = modelData.filter(ee.Filter.calendarRange(year, year, 'year'));
      
      return varForm === 'min' ? yearData.min() :
             varForm === 'max' ? yearData.max() :
             yearData.mean();
    });
    
    return ee.ImageCollection.fromImages(yearlyStats).mean();
  });
  
  var result = ee.ImageCollection.fromImages(modelYearStats).mean().clip(state);
  
  var processed = convertKelvinToCelsius ? result.subtract(273.15) : result;
  var scaled = scaleStdDev !== 0 ?
    processed.subtract(scaleMean).divide(scaleStdDev) :
    processed.subtract(scaleMean);
  
  return processed.rename(outputBaseName)
    .addBands(scaled.rename(outputBaseName + '_scaled'));
}

// Daylength function - adjusted for future periods
function computeMultiYearDaylengthDaymet(state, startYear, endYear, startDOY, endDOY, scaleMean, scaleStdDev) {
  // For future periods, use current DAYMET data (daylength doesn't change significantly with climate)
var effectiveStartYear = startYear >= 2020 ? 2000 : startYear;
var effectiveEndYear = startYear >= 2020 ? 2020 : endYear;

var result = daymetCollection
.filterDate(ee.Date.fromYMD(effectiveStartYear, 1, 1), ee.Date.fromYMD(effectiveEndYear, 12, 31))
.filter(ee.Filter.dayOfYear(startDOY, endDOY))
.mean()
.clip(state)
.divide(3600);

var scaled = result.subtract(scaleMean).divide(scaleStdDev);

return result.rename('daylength')
.addBands(scaled.rename('daylength_scaled'));
}



// Helper function to process a state or state chunk
function processHalf(stateGeom, exportName, plantingDOY) {
  var variables = {};
  var stateBounds = stateGeom.geometry().bounds();
  var termsList = [];
  
  // Tmean1
  if (params.tmeanLinear_1 !== 0 || params.tmeanQuadratic_1 !== 0) {
    variables.tmean1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmean1_start), plantingDOY.add(params.tmean1_end),
      'tmean', 'tmean', params.tmeanMean, params.tmeanStdDev, true, params.tmeanForm_1
    );
    
    var tmeanScaled = variables.tmean1.select('tmean_scaled');
    var tmeanLinearTerm = tmeanScaled.multiply(params.tmeanLinear_1);
    var tmeanQuadraticTerm = tmeanScaled.pow(2).multiply(params.tmeanQuadratic_1);
    var tmeanTotalTerm = tmeanLinearTerm.add(tmeanQuadraticTerm);
    termsList.push(tmeanTotalTerm);
  }
  
  // Tmax1
  if (params.tmaxLinear_1 !== 0 || params.tmaxQuadratic_1 !== 0) {
    variables.tmax1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmax1_start), plantingDOY.add(params.tmax1_end),
      'tasmax', 'tmax', params.tmaxMean, params.tmaxStdDev, true, params.tmaxForm_1
    );
    
    var tmaxScaled = variables.tmax1.select('tmax_scaled');
    var tmaxLinearTerm = tmaxScaled.multiply(params.tmaxLinear_1);
    var tmaxQuadraticTerm = tmaxScaled.pow(2).multiply(params.tmaxQuadratic_1);
    var tmaxTotalTerm = tmaxLinearTerm.add(tmaxQuadraticTerm);
    termsList.push(tmaxTotalTerm);
  }
  
  // Tmax2
  if (params.tmaxLinear_2 !== 0 || params.tmaxQuadratic_2 !== 0) {
    variables.tmax2 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmax2_start), plantingDOY.add(params.tmax2_end),
      'tasmax', 'tmax', params.tmaxMean, params.tmaxStdDev, true, params.tmaxForm_2
    );
    
    var tmax2Scaled = variables.tmax2.select('tmax_scaled');
    var tmax2LinearTerm = tmax2Scaled.multiply(params.tmaxLinear_2);
    var tmax2QuadraticTerm = tmax2Scaled.pow(2).multiply(params.tmaxQuadratic_2);
    var tmax2TotalTerm = tmax2LinearTerm.add(tmax2QuadraticTerm);
    termsList.push(tmax2TotalTerm);
  }
  
  // Tmax3
  if (params.tmaxLinear_3 !== 0 || params.tmaxQuadratic_3 !== 0) {
    variables.tmax3 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmax3_start), plantingDOY.add(params.tmax3_end),
      'tasmax', 'tmax', params.tmaxMean, params.tmaxStdDev, true, params.tmaxForm_3
    );
    
    var tmax3Scaled = variables.tmax3.select('tmax_scaled');
    var tmax3LinearTerm = tmax3Scaled.multiply(params.tmaxLinear_3);
    var tmax3QuadraticTerm = tmax3Scaled.pow(2).multiply(params.tmaxQuadratic_3);
    var tmax3TotalTerm = tmax3LinearTerm.add(tmax3QuadraticTerm);
    termsList.push(tmax3TotalTerm);
  }
  
  // VPD1
  if (params.vpdLinear_1 !== 0 || params.vpdQuadratic_1 !== 0) {
    variables.vpd1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.vpd1_start), plantingDOY.add(params.vpd1_end),
      'vpd', 'vpd', params.vpdMean, params.vpdStdDev, false, params.vpdForm_1
    );
    
    var vpdScaled = variables.vpd1.select('vpd_scaled');
    var vpdLinearTerm = vpdScaled.multiply(params.vpdLinear_1);
    var vpdQuadraticTerm = vpdScaled.pow(2).multiply(params.vpdQuadratic_1);
    var vpdTotalTerm = vpdLinearTerm.add(vpdQuadraticTerm);
    termsList.push(vpdTotalTerm);
  }
  
  // GDD1
  if (params.gddLinear_1 !== 0 || params.gddQuadratic_1 !== 0) {
    variables.gdd1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.gdd1_start), plantingDOY.add(params.gdd1_end),
      'GDD_modave', 'gdd', params.gddMean, params.gddStdDev, false, params.gddForm_1
    );
    
    var gddScaled = variables.gdd1.select('gdd_scaled');
    var gddLinearTerm = gddScaled.multiply(params.gddLinear_1);
    var gddQuadraticTerm = gddScaled.pow(2).multiply(params.gddQuadratic_1);
    var gddTotalTerm = gddLinearTerm.add(gddQuadraticTerm);
    termsList.push(gddTotalTerm);
  }
  
  // Tmin1
  if (params.tminLinear_1 !== 0 || params.tminQuadratic_1 !== 0) {
    variables.tmin1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmin1_start), plantingDOY.add(params.tmin1_end),
      'tasmin', 'tmin', params.tminMean, params.tminStdDev, true, params.tminForm_1
    );
    
    var tmin1Scaled = variables.tmin1.select('tmin_scaled');
    var tmin1LinearTerm = tmin1Scaled.multiply(params.tminLinear_1);
    var tmin1QuadraticTerm = tmin1Scaled.pow(2).multiply(params.tminQuadratic_1);
    var tmin1TotalTerm = tmin1LinearTerm.add(tmin1QuadraticTerm);
    termsList.push(tmin1TotalTerm);
  }
  
  // Tmin2
  if (params.tminLinear_2 !== 0 || params.tminQuadratic_2 !== 0) {
    variables.tmin2 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmin2_start), plantingDOY.add(params.tmin2_end),
      'tasmin', 'tmin', params.tminMean, params.tminStdDev, true, params.tminForm_2
    );
    
    var tmin2Scaled = variables.tmin2.select('tmin_scaled');
    var tmin2LinearTerm = tmin2Scaled.multiply(params.tminLinear_2);
    var tmin2QuadraticTerm = tmin2Scaled.pow(2).multiply(params.tminQuadratic_2);
    var tmin2TotalTerm = tmin2LinearTerm.add(tmin2QuadraticTerm);
    termsList.push(tmin2TotalTerm);
  }
  
  // Tmin3
  if (params.tminLinear_3 !== 0 || params.tminQuadratic_3 !== 0) {
    variables.tmin3 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.tmin3_start), plantingDOY.add(params.tmin3_end),
      'tasmin', 'tmin', params.tminMean, params.tminStdDev, true, params.tminForm_3
    );
    
    var tmin3Scaled = variables.tmin3.select('tmin_scaled');
    var tmin3LinearTerm = tmin3Scaled.multiply(params.tminLinear_3);
    var tmin3QuadraticTerm = tmin3Scaled.pow(2).multiply(params.tminQuadratic_3);
    var tmin3TotalTerm = tmin3LinearTerm.add(tmin3QuadraticTerm);
    termsList.push(tmin3TotalTerm);
  }
  
  // Solar radiation (RSDS)
  if (params.rsdsLinear_1 !== 0 || params.rsdsQuadratic_1 !== 0) {
    variables.rsds1 = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.rsds1_start), plantingDOY.add(params.rsds1_end),
      'rsds', 'rsds', params.rsdsWMean, params.rsdsStdDev, false, params.rsdsForm_1
    );
    
    var rsdsScaled = variables.rsds1.select('rsds_scaled');
    var rsdsLinearTerm = rsdsScaled.multiply(params.rsdsLinear_1);
    var rsdsQuadraticTerm = rsdsScaled.pow(2).multiply(params.rsdsQuadratic_1);
    var rsdsTotalTerm = rsdsLinearTerm.add(rsdsQuadraticTerm);
    termsList.push(rsdsTotalTerm);
  }
  
  // Precipitation
  if (params.pptLinear !== 0) {
    variables.ppt = computeMultiYearMACAVariable(
      stateGeom, activeStartYear, activeEndYear, macaCollection, 
      plantingDOY.add(params.pr_start), plantingDOY.add(params.pr_end),
      'pr', 'pr', params.prMean, params.prStdDev, false, params.pptForm_1
    );
    
    var pptScaled = variables.ppt.select('pr_scaled');
    var pptLinearTerm = pptScaled.multiply(params.pptLinear);
    termsList.push(pptLinearTerm);
  }
  
  // Daylength
  if (params.daylengthLinear !== 0 || params.daylengthQuadratic !== 0) {
    variables.daylength = computeMultiYearDaylengthDaymet(
      stateGeom, activeStartYear, activeEndYear, 
      plantingDOY.add(params.dayl_start), plantingDOY.add(params.dayl_end), 
      params.daylengthMean, params.daylengthStdDev
    );
    
    var daylengthScaled = variables.daylength.select('daylength_scaled');
    var daylengthLinearTerm = daylengthScaled.multiply(params.daylengthLinear);
    var daylengthQuadraticTerm = daylengthScaled.pow(2).multiply(params.daylengthQuadratic);
    var daylengthTotalTerm = daylengthLinearTerm.add(daylengthQuadraticTerm);
    termsList.push(daylengthTotalTerm);
  }
  
  // Sum all terms and add intercept to get site quality without year effect
  var sumOfTerms = ee.Image(0);
  for (var i = 0; i < termsList.length; i++) {
    sumOfTerms = sumOfTerms.add(termsList[i]);
  }
  
  var siteQuality = ee.Image(params.intercept).add(sumOfTerms);
  
  Export.image.toDrive({
    image: siteQuality,
    description: timePeriodLabel + '_SiteQuality_Sum_' + parameterSet + '_' + exportName.replace(' ', '_'),
    region: stateBounds,
    scale: 4000,
    maxPixels: 1e9,
    fileFormat: 'GeoTIFF'
  });
}

// Process each state
var statesList = targetStates.toList(targetStates.size());
var numStates = statesList.size().getInfo();

for (var i = 0; i < numStates; i++) {
  var state = ee.Feature(statesList.get(i));
  processStateCurrentOnly(state);
}

print('Processing complete - exporting sum of all scaled variables with coefficients plus intercept');
print('Parameter set:', parameterSet);
print('Time period:', timePeriodLabel, '(' + activeStartYear + '-' + activeEndYear + ')');
print('Scenario:', activeScenario);
print('Output: Site quality = intercept + sum of all variable terms');
print('Check Tasks tab for export status.');
print('Number of states processed:', numStates);