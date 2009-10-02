-- | EXIF tags and their interpretation
module Dragonfly.ImageGallery.Exif where

-- | EXIF tag
manufacturer :: String
manufacturer = "Make"

-- | Interpretation of 'manufacturer'
makeI :: String
makeI = "Make:"

-- | EXIF tag
model :: String        
model = "Model"

-- | Interpretation of 'model'
modelI :: String        
modelI = "Camera:"

-- | EXIF tag
focalLength :: String
focalLength = "FocalLength"

-- | Interpretation of 'focalLength'
focalLengthI  :: String
focalLengthI = "Focal length:"

-- | EXIF tag
dtOriginal :: String        
dtOriginal = "DateTimeOriginal"

-- | Interpretation of 'dtOriginal'
dtOriginalI = "Taken on:"

-- | EXIF tag
fNumber :: String        
fNumber = "FNumber"

-- | Interpretation of 'fNumber'
fNumberI :: String        
fNumberI = "Aperture:"

-- | EXIF tag
exposure :: String
exposure = "ExposureTime"

-- | Interpretation of 'exposure'
exposureI :: String
exposureI = "Exposure time:"

-- | EXIF tag
iso :: String
iso = "ISOSpeedRatings"

-- | Interpretation of 'iso'
isoI :: String
isoI = "ISO:"

-- | EXIF tag
flash :: String
flash = "Flash"

-- | Interpretation of 'flash'
flashI :: String
flashI = "Flash status:"

-- | EXIF tag
meter :: String
meter = "MeteringMode"

-- | Interpretation of 'meter'
meterI :: String
meterI = "Metering mode:"

-- | EXIF tag
exposureMode :: String
exposureMode = "ExposureMode"

-- | Interpretation of 'exposureMode'
exposureModeI :: String
exposureModeI = "Exposure mode:"

-- | EXIF tag
colourSpace :: String
colourSpace = "ColorSpace"

-- | Interpretation of 'colorSpace'
colourSpaceI :: String
colourSpaceI = "Colour space:"

-- | EXIF tag
whiteBalance :: String
whiteBalance = "WhiteBalance"

-- | Interpretation of 'whiteBalance'
whiteBalanceI :: String
whiteBalanceI = "White balance:"

-- | EXIF tag
userComment :: String
userComment = "UserComment"

-- | Interpretation of 'userComment'
userCommentI :: String
userCommentI = "User's comment:"

-- | EXIF tag
gpsLatRef :: String
gpsLatRef = "InteroperabilityIndex"

-- | Interpretation of 'gpsLatRef'
gpsLatRefI :: String
gpsLatRefI = "GPS Lattitude reference:"

-- | EXIF tag
gpsLat :: String
gpsLat = "InteroperabilityVersion"

-- | Interpretation of 'gpsLat'
gpsLatI :: String
gpsLatI = "GPS Lattitude:"

-- | EXIF tag
gpsLongRef :: String
gpsLongRef = "GPSLongitudeRef"

-- | Interpretation of 'gpsLongRef'
gpsLongRefI :: String
gpsLongRefI = "GPS Longitude reference:"

-- | EXIF tag
gpsLong :: String
gpsLong = "GPSLongitude"

-- | Interpretation of 'gpsLong'
gpsLongI :: String
gpsLongI = "GPS Longitude:"

-- | EXIF tag
gpsAltRef :: String
gpsAltRef = "GPSAltitudeRef"

-- | Interpretation of 'gpsAltRef'
gpsAltRefI :: String
gpsAltRefI = "GPS Altitude reference:"

-- | EXIF tag
gpsAlt :: String
gpsAlt = "GPSAltitude"

-- | Interpretation of 'gpsAlt'
gpsAltI :: String
gpsAltI = "GPS Altitude:"
