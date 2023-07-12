//extract_s1s2_rasters_netherlands_gee.js

//example of date range and spatial functions

var start = '2018-01-01'
var end = '2018-12-31'
var roi = ee.FeatureCollection('USDOS/LSIB_SIMPLE/2017').filter(ee.Filter.eq('country_co','NL'));


//import required functions for S1/S2 

function maskcloud2(image) {
  var QA60 = image.select(['QA60']);
	var clouds = QA60.bitwiseAnd(1<<10).or(QA60.bitwiseAnd(1<<11));
  return image.updateMask(clouds.not());
  }
function addNDVI(image) {
  var ndvi = image.normalizedDifference(['B8', 'B4']).rename('NDVI');
  return image.addBands(ndvi);
  }

var s1grd = ee.ImageCollection('COPERNICUS/S1_GRD').filterDate(start, end).filterBounds(roi).select(['VH','VV']).filter(ee.Filter.eq('instrumentMode','IW'));
var s2sr = ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED').filterDate(start, end).filterBounds(roi).select(['B2', 'B3', 'B4', 'B5', 'B8','QA60']).map(maskcloud2).map(addNDVI);


//extract metrics 

var reducers = ee.Reducer.mean().combine({
  reducer2: ee.Reducer.median(),
  sharedInputs: true}).combine({
  reducer2: ee.Reducer.stdDev(),
  sharedInputs: true}).combine({
  reducer2: ee.Reducer.min(),
  sharedInputs: true}).combine({
  reducer2: ee.Reducer.max(),
  sharedInputs: true}).combine({
  reducer2: ee.Reducer.percentile([10,90]),
  sharedInputs: true})


//select bands

var filteredS1GRD = ee.ImageCollection(s1grd.select(['VH','VV']))
  var filteredS2SR = ee.ImageCollection(s2sr.select('NDVI'))


//subset image collection for images from coordinates of feat

var reducedBACK = ee.Image(filteredS1GRD.reduce({reducer: reducers}))
var reducedNDVI = ee.Image(filteredS2SR.reduce({reducer: reducers}))


//export images to drive 

var BACK = reducedBACK.multiply(1e4).toInt()
var NDVI = reducedNDVI.multiply(1e4).toInt()

Export.image.toDrive({
  image: BACK,
  region: roi,
  crs: 'EPSG: 28992',
  scale: 10,
  maxPixels: 1e13,
  description: 'BACK',
  folder: 'BACK'
});

Export.image.toDrive({
  image: NDVI,
  region: roi,
  crs: 'EPSG: 28992',
  scale: 10,
  maxPixels: 1e13,
  description: 'NDVI',
  folder: 'NDVI'
});
