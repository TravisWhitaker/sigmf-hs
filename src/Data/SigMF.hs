{-# LANGUAGE DeriveAnyClass
           , DeriveGeneric
           , DerivingStrategies
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , OverloadedStrings
           , RecordWildCards
           #-}

module Data.SigMF (
    Field(..)
  , Endianness(..)
  , WideType(..)
  , Type(..)
  , DatasetFormat(..)
  , Extension(..)
  , Global(..)
  , Capture(..)
  , Annotation(..)
  , Recording(..)
  , encodeRecording
  , decodeRecording
  , Stream(..)
  , mkStream
  , Collection(..)
  , encodeCollection
  , decodeCollection
  , Bearing(..)
  , CartesianPoint(..)
  , CalType(..)
  , Calibration(..)
  , spatialExtension
  , SpatialGlobal(..)
  , SpatialCapture(..)
  , emptySpatialCapture
  , SpatialAnnotation(..)
  , emptySpatialAnnotation
  ) where

import Control.Applicative

import Control.DeepSeq

import Crypto.Hash

import qualified Data.Aeson        as J
import qualified Data.Aeson.Key    as J
import qualified Data.Aeson.KeyMap as J
import qualified Data.Aeson.Types  as J

import Data.Aeson ((.:), (.:?), (.=), (.!=))

import qualified Data.Attoparsec.Text as A

import Data.ByteArray.Encoding

import qualified Data.ByteString as BS

import Data.List (sortOn)

import Data.Scientific

import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import Data.Time.Clock

import qualified Data.Vector as V

import Data.Word

import GHC.Generics

import qualified Linear as L

import System.FilePath

apInAE :: A.Parser a -> T.Text -> J.Parser a
apInAE p t = case A.parseOnly (p <* A.endOfInput) t of
            Left e  -> fail e
            Right x -> pure x

noNullPairs :: [J.Pair] -> [J.Pair]
noNullPairs = filter (\(_, v) -> not (isNull v))
    where isNull J.Null = True
          isNull _      = False

-- | The field of sample values.
data Field = -- | Real-valued samples.
             Real
           | -- | Complex-valued samples.
             Complex
           deriving ( Eq
                    , Ord
                    , Read
                    , Show
                    , Generic
                    , NFData
                    )

parseField :: A.Parser Field
parseField = asum [ A.char 'r' *> pure Real
                  , A.char 'c' *> pure Complex
                  ]

textField :: Field -> T.Text
textField Real    = "r"
textField Complex = "c"

-- | Sample word endianness.
data Endianness = -- | Little-endian samples.
                  LE
                | -- | Big-engian samples.
                  BE
                deriving ( Eq
                         , Ord
                         , Read
                         , Show
                         , Generic
                         , NFData
                         )

parseEndianness :: A.Parser Endianness
parseEndianness = asum [ A.string "_le" *> pure LE
                       , A.string "_be" *> pure BE
                       ]

textEndianness :: Endianness -> T.Text
textEndianness LE = "_le"
textEndianness BE = "_be"

-- | Width and interpretation of multi-byte samples.
data WideType = -- | 32-bit float samples.
                Float
              | -- | 32-bit signed integer samples.
                Int32
              | -- | 16-bit signed integer samples.
                Int16
              | -- | 32-bit unsigned integer samples.
                Word32
              | -- | 16-bit unsigned integer samples.
                Word16
              deriving ( Eq
                       , Ord
                       , Read
                       , Show
                       , Generic
                       , NFData
                       )

parseWideType :: A.Parser WideType
parseWideType = asum [ A.string "f32" *> pure Float
                     , A.string "i32" *> pure Int32
                     , A.string "i16" *> pure Int16
                     , A.string "u32" *> pure Word32
                     , A.string "u16" *> pure Word16
                     ]

textWideType :: WideType -> T.Text
textWideType Float  = "f32"
textWideType Int32  = "i32"
textWideType Int16  = "i16"
textWideType Word32 = "u32"
textWideType Word16 = "u16"

-- | Width and interpretation of samples.
data Type = -- | 8-bit signed integer samples
            Int8
          | -- | 8-bit unsigned integer samples.
            Word8
          | -- | Multibyte samples.
            Wide WideType Endianness
          deriving ( Eq
                   , Ord
                   , Read
                   , Show
                   , Generic
                   , NFData
                   )

parseType :: A.Parser Type
parseType = asum [ A.string "i8" *> pure Int8
                 , A.string "u8" *> pure Word8
                 , (Wide <$> parseWideType <*> parseEndianness)
                 ]

textType :: Type -> T.Text
textType Int8        = "i8"
textType Word8       = "u8"
textType (Wide wt e) = textWideType wt <> textEndianness e

-- | Field, width, and interpretation of samples.
data DatasetFormat = DatasetFormat {
    datasetFormatField :: Field
  , datasetFormatType  :: Type
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

parseDatasetFormat :: A.Parser DatasetFormat
parseDatasetFormat = DatasetFormat <$> parseField <*> parseType

textDatasetFormat :: DatasetFormat -> T.Text
textDatasetFormat (DatasetFormat f t) = textField f <> textType t

-- | A (potentially optional) extension to the specification used in this
--   dataset.
data Extension = Extension {
    extensionName     :: T.Text
  , extensionVersion  :: T.Text
  , extensionOptional :: Bool
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Extension where
    parseJSON = J.withObject "extension" $ \o ->
        Extension <$> o .: "name"
                  <*> o .: "version"
                  <*> o .: "optional"

instance J.ToJSON Extension where
    toJSON Extension{..} =
        J.object $ noNullPairs
            [ "name" .= extensionName
            , "version" .= extensionVersion
            , "optional" .= extensionOptional
            ]

-- | All of these fields are "global" in the sense that they apply to all
--   captures and annotations in a recording.
data Global = Global {
    -- | This field is required according to the specification.
    globalDatasetFormat :: Maybe DatasetFormat
  , globalSampleRate    :: Maybe Double
    -- | This field is required according to the specification.
  , globalVersion       :: Maybe T.Text
  , globalNumChannels   :: Maybe Word64
  , globalSha512        :: Maybe T.Text
  , globalOffset        :: Maybe Word64
  , globalDescription   :: Maybe T.Text
  , globalAuthor        :: Maybe T.Text
  , globalMetaDOI       :: Maybe T.Text
  , globalDataDOI       :: Maybe T.Text
  , globalRecorder      :: Maybe T.Text
  , globalLicense       :: Maybe T.Text
  , globalHW            :: Maybe T.Text
  , globalDataset       :: Maybe T.Text
  , globalTrailingBytes :: Maybe Word64
  , globalMetadataOnly  :: Maybe Bool
  , globalExtensions    :: [Extension]
    -- | Name of the containing .sigmf-collection, if present.
  , globalCollection    :: Maybe T.Text
    -- | Optional spatial extension fields.
  , globalSpatialGlobal :: Maybe SpatialGlobal
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Global where
    parseJSON = J.withObject "global" $ \o ->
        Global <$> (o .:? "core:datatype" >>=
                       (sequence . fmap (apInAE parseDatasetFormat))
                   )
               <*> o .:? "core:sample_rate"
               <*> o .:? "core:version"
               <*> o .:? "core:num_channels"
               <*> o .:? "core:sha512"
               <*> o .:? "core:offset"
               <*> o .:? "core:description"
               <*> o .:? "core:author"
               <*> o .:? "core:meta_doi"
               <*> o .:? "core:data_doi"
               <*> o .:? "core:recorder"
               <*> o .:? "core:license"
               <*> o .:? "core:hw"
               <*> o .:? "core:dataset"
               <*> o .:? "core:trailing_bytes"
               <*> o .:? "core:metadata_only"
               <*> (o .:? "core:extensions" .!= [])
               <*> o .:? "core:collection"
               <*> (do sne <- o .:? "spatial:num_elements"
                       sci <- o .:? "spatial:channel_index"
                       case (sne, sci) of
                           (Nothing, Nothing) -> pure Nothing
                           (Just ne, Just ci) -> pure (Just (SpatialGlobal ne ci))
                           _ -> fail "Either all spatial attrs must be present, or none."
                   )

instance J.ToJSON Global where
    toJSON Global{..} =
        J.object $ noNullPairs
            [ "core:datatype" .= fmap textDatasetFormat globalDatasetFormat
            , "core:sample_rate" .= globalSampleRate
            , "core:version" .= globalVersion
            , "core:num_channels" .= globalNumChannels
            , "core:sha512" .= globalSha512
            , "core:offset" .= globalOffset
            , "core:description" .= globalDescription
            , "core:author" .= globalAuthor
            , "core:meta_doi" .= globalMetaDOI
            , "core:data_doi" .= globalDataDOI
            , "core:recorder" .= globalRecorder
            , "core:license" .= globalLicense
            , "core:hw" .= globalHW
            , "core:dataset" .= globalDataset
            , "core:trailing_bytes" .= globalTrailingBytes
            , "core:metadata_only" .= globalMetadataOnly
            , "core:extensions" .= globalExtensions
            , "core:collection" .= globalCollection
            ] ++ (case globalSpatialGlobal of
                    Nothing -> []
                    Just (SpatialGlobal{..}) ->
                        [ "spatial:num_elements" .= spatialGlobalNumElements
                        , "spatial:channel_index" .= spatialGlobalChannelIndex
                        ]
                 )

-- | Metadata for each of the contiguous captures in the corresponding
--   .sigmf-data file.
data Capture = Capture {
    -- | Index of the starting sample in the .sigmf-data file. This field is
    --   required by the standard.
    captureSampleStart :: Maybe Word64
    -- | Meaning not super clear, see spec for details.
  , captureGlobalIndex :: Maybe Word64
    -- | This is always zero for conforming .sigmf-data files.
  , captureHeaderBytes :: Maybe Word64
  , captureFrequency   :: Maybe Double
  , captureDateTime    :: Maybe UTCTime
    -- | Spatial extension fields.
  , captureSpatialCapture :: SpatialCapture
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Capture where
    parseJSON = J.withObject "capture" $ \o ->
        Capture <$> o .:? "core:sample_start"
                <*> o .:? "core:global_index"
                <*> o .:? "core:header_bytes"
                <*> o .:? "core:frequency"
                <*> o .:? "core:datetime"
                <*> (SpatialCapture <$> o .:? "spatial:aperture_azimuth"
                                    <*> o .:? "spatial:aperture_bearing"
                                    <*> o .:? "spatial:emitter_bearing"
                                    <*> (o .:? "spatial:element_geometry" .!= [])
                                    <*> o .:? "spatial:phase_offset"
                                    <*> o .:? "spatial:calibration"
                    )

instance J.ToJSON Capture where
    toJSON Capture{..} =
        let SpatialCapture{..} = captureSpatialCapture
        in J.object $ noNullPairs
            [ "core:sample_start" .= captureSampleStart
            , "core:global_index" .= captureGlobalIndex
            , "core:header_bytes" .= captureHeaderBytes
            , "core:frequency" .= captureFrequency
            , "core:datetime" .= captureDateTime
            , "spatial:aperture_azimuth" .= spatialCaptureApertureAzimuth
            , "spatial:aperture_bearing" .= spatialCaptureApertureBearing
            , "spatial:emitter_bearing" .= spatialCaptureEmitterBearing
            , "spatial:phase_offset" .= spatialCapturePhaseOffset
            , "spatial:calibration" .= spatialCaptureCalibration
            ]

data Annotation = Annotation {
    -- | Sample index in the .sigmf-data file at the start of this annotation.
    --   This is required in the specification.
    annotationSampleStart       :: Maybe Word64
    -- | Annotation applies to the end of the data set if missing.
  , annotationSampleCount       :: Maybe Word64
  , annotationGenerator         :: Maybe T.Text
  , annotationLabel             :: Maybe T.Text
  , annotationComment           :: Maybe T.Text
  , annotationFreqLowerEdge     :: Maybe Double
  , annotationFreqUpperEdge     :: Maybe Double
    -- | deprecated
  , annotationLatitude          :: Maybe Double
    -- | deprecated
  , annotationLongitude         :: Maybe Double
    -- | Spatial extension fields.
  , annotationSpatialAnnotation :: SpatialAnnotation
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Annotation where
    parseJSON = J.withObject "annotation" $ \o ->
        Annotation <$> o .: "core:sample_start"
                   <*> o .:? "core:sample_count"
                   <*> o .:? "core:generator"
                   <*> o .:? "core:label"
                   <*> o .:? "core:comment"
                   <*> o .:? "core:freq_lower_edge"
                   <*> o .:? "core:freq_upper_edge"
                   <*> o .:? "core:latitude"
                   <*> o .:? "core:longitude"
                   <*> (SpatialAnnotation <$> o .:? "spatial:signal_azimuth"
                                          <*> o .:? "spatial:signal_bearing"
                       )

instance J.ToJSON Annotation where
    toJSON Annotation{..} =
        let SpatialAnnotation{..} = annotationSpatialAnnotation
        in J.object $ noNullPairs $
            [ "core:sample_start" .= annotationSampleStart
            , "core:sample_count" .= annotationSampleCount
            , "core:generator" .= annotationGenerator
            , "core:label" .= annotationLabel
            , "core:comment" .= annotationComment
            , "core:freq_lower_edge" .= annotationFreqLowerEdge
            , "core:freq_upper_edge" .= annotationFreqUpperEdge
            , "core:latitude" .= annotationLatitude
            , "core:longitude" .= annotationLongitude
            , "spatial:signal_azimuth" .= spatialAnnotationSignalAzimuth
            , "spatial:signal_bearing" .= spatialAnnotationSignalBearing
            ]

-- | Top-level recording fields, there should be one of these in a .sigmf-meta
--   file for each .sigmf-data file with the same basename.
data Recording = Recording {
    recordingGlobal      :: Global
  , recordingCaptures    :: [Capture]
  , recordingAnnotations :: [Annotation]
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Recording where
    parseJSON = J.withObject "top-level" $ \o ->
        Recording <$> o .: "global"
                  <*> fmap (sortOn captureSampleStart) (o .: "captures")
                  <*> o .: "annotations"

instance J.ToJSON Recording where
    toJSON Recording{..} =
        J.object $ noNullPairs
            [ "global" .= recordingGlobal
            , "captures" .= (sortOn captureSampleStart) recordingCaptures
            , "annotations" .= recordingAnnotations
            ]

-- | Encode to a file.
encodeRecording :: FilePath -> Recording -> IO ()
encodeRecording = J.encodeFile

-- | Decode from a file.
decodeRecording :: FilePath -> IO (Either String Recording)
decodeRecording = J.eitherDecodeFileStrict'

-- | The name of a recording/dataset in a collection, along with the hash of its
--   metadata.
data Stream = Stream {
    streamBaseName :: T.Text
  , streamHash :: Digest SHA512
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | Given the path to a .sigmf-meta file, compute the `Stream` for a `Collection`.
mkStream :: FilePath -> IO Stream
mkStream fp = (Stream (T.pack (takeBaseName fp)) . hash) <$> BS.readFile fp

instance J.FromJSON Stream where
    parseJSON x = kvStream x <|> arrStream x
        where kvStream y = flip (J.withObject "stream-kv") y $ \o ->
                case J.toList o of
                    [(k, v)] -> flip (J.withText "stream-hash") v $ \t ->
                        Stream (J.toText k) <$> tryBases (T.encodeUtf8 t)
                    _ -> fail "stream must be singleton key/value pair"
              tryBases t = asum $
                  fmap (tryBase t) [Base16, Base32, Base64, Base64URLUnpadded]
              tryBase t b =
                  case convertFromBase b t of
                      Left e -> fail e
                      Right sha -> case digestFromByteString (sha :: BS.ByteString) of
                         Nothing -> fail "wrong digest byte length"
                         Just d -> pure d
              arrStream y = flip (J.withArray "stream-arr") y $ \a ->
                  if V.length a /= 2
                  then fail "stream must be tuple"
                  else flip (J.withText "stream-key") (a V.! 0) $ \k ->
                          flip (J.withText "stream-hash") (a V.! 1) $ \t ->
                              Stream k <$> tryBases (T.encodeUtf8 t)

instance J.ToJSON Stream where
    toJSON Stream{..} = J.Array $ fmap J.String $ V.fromList
        [streamBaseName, T.decodeUtf8 (convertToBase Base16 streamHash)]

-- | The top-level for a .sigmf-collection file, which associates several
--   'Recording's into a group. To make one of these, first have your program
--   emit the .sigmf-meta files for all of your 'Recording's, then, use
--   'mkStream' on each to construct the 'Stream's, before constructing and
--   serializing the 'Collection'.
data Collection = Collection {
    -- | Specification version. This is required by the specification.
    collectionVersion :: Maybe T.Text
  , collectionDescription :: Maybe T.Text
  , collectionAuthor :: Maybe T.Text
  , collectionCollectionDOI :: Maybe T.Text
  , collectionLicense :: Maybe T.Text
  , collectionExtensions :: [Extension]
  , collectionStreams :: [Stream]
  , collectionSpatialElementGeometry :: [CartesianPoint]
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Collection where
    parseJSON = J.withObject "top-level" $ \o -> o .: "collection" >>= \co ->
        Collection <$> co .: "core:version"
                   <*> co .:? "core:description"
                   <*> co .:? "core:author"
                   <*> co .:? "core:collection_doi"
                   <*> co .:? "core:license"
                   <*> (co .:? "core:extensions" .!= [])
                   <*> (co .:? "core:streams" .!= [])
                   <*> (co .:? "spatial:element_geometry" .!= [])

instance J.ToJSON Collection where
    toJSON Collection{..} = J.object $ noNullPairs
        [ "core:version" .= collectionVersion
        , "core:description" .= collectionDescription
        , "core:author" .= collectionAuthor
        , "core:collection_doi" .= collectionCollectionDOI
        , "core:license" .= collectionLicense
        , "core:extensions" .= collectionExtensions
        , "core:streams" .= collectionStreams
        , "spatial:element_geometry" .= collectionSpatialElementGeometry
        ]

-- | Encode to a file.
encodeCollection :: FilePath -> Collection -> IO ()
encodeCollection = J.encodeFile

-- | Decode from a file.
decodeCollection :: FilePath -> IO (Either String Collection)
decodeCollection = J.eitherDecodeFileStrict'

-- ** Spatial Extension

-- | The 'Extension' required for the spatial fields.
spatialExtension :: Extension
spatialExtension =
    let extensionName = "spatial"
        extensionVersion = "v1.0.0"
        extensionOptional = False
    in Extension{..}

-- | All angles are degrees, see the SigMF "spatial" extension spec for
--   coordinate system diagram. Azimuth and elevation are navigation convention,
--   i.e. degrees east of north, degrees above the horizon.
data Bearing = Bearing {
    bearingAzimuth :: Maybe Double
  , bearingElevation :: Maybe Double
  , bearingRange :: Maybe Double
  , bearingAzError :: Maybe Double
  , bearingElError :: Maybe Double
  , bearingRangeError :: Maybe Double
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )
instance J.FromJSON Bearing where
    parseJSON = J.withObject "bearing" $ \o ->
        Bearing <$> o .:? "azimuth"
                <*> o .:? "elevation"
                <*> o .:? "range"
                <*> o .:? "az_error"
                <*> o .:? "el_error"
                <*> o .:? "range_error"

instance J.ToJSON Bearing where
    toJSON Bearing{..} = J.object $ noNullPairs
        [ "azimuth" .= bearingAzimuth
        , "elevation" .= bearingElevation
        , "range" .= bearingRange
        , "az_error" .= bearingAzError
        , "el_error" .= bearingElError
        , "range_error" .= bearingRangeError
        ]

newtype CartesianPoint = CartesianPoint (Maybe (L.V3 Double))
                       deriving ( Eq
                                , Ord
                                , Read
                                , Show
                                , Generic
                                )
                       deriving newtype (NFData)

instance J.FromJSON CartesianPoint where
    parseJSON = J.withObject "cartesian_point" $ \o ->
        case J.lookup "point" o of
            Nothing -> case J.lookup "unknown" o of
                Nothing -> fail "cartesian_point must have point or unknown"
                Just _ -> pure (CartesianPoint Nothing)
            Just p -> flip (J.withArray "cartesian_point") p $ \a ->
                case V.toList a of
                    [x, y, z] ->
                        flip (J.withScientific "x") x $ \x' ->
                            flip (J.withScientific "y") y $ \y' ->
                                flip (J.withScientific "z") z $ \z' ->
                                    pure (CartesianPoint (Just
                                        (fmap toRealFloat (L.V3 x' y' z'))))
                    _ -> fail "cartesian_point.point must have three elements"

instance J.ToJSON CartesianPoint where
    toJSON (CartesianPoint Nothing) = J.object ["unknown" .= True]
    toJSON (CartesianPoint (Just (L.V3 x y z))) =
        J.Array $ fmap J.toJSON $ V.fromList
            [x, y, z]

data CalType = CalTone
             | CalCrossCorrelation
             | CalReference
             | CalOther
             deriving ( Eq
                      , Ord
                      , Read
                      , Show
                      , Generic
                      , NFData
                      )

instance J.FromJSON CalType where
    parseJSON = J.withText "caltype" $ \t ->
        case t of "tone" -> pure CalTone
                  "xcorr" -> pure CalCrossCorrelation
                  "ref" -> pure CalReference
                  "other" -> pure CalOther
                  _ -> fail ("Unknown caltype: " <> T.unpack t)

instance J.ToJSON CalType where
    toJSON CalTone = J.String "tone"
    toJSON CalCrossCorrelation = J.String "xcorr"
    toJSON CalReference = J.String "ref"
    toJSON CalOther = J.String "other"

data Calibration = Calibration {
    calibrationCalType :: CalType
  , calibrationBearing :: Maybe Bearing
  , calibrationCalGeometry :: Maybe CartesianPoint
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

instance J.FromJSON Calibration where
    parseJSON = J.withObject "calibration" $ \o ->
        Calibration <$> o .: "caltype"
                    <*> o .:? "bearing"
                    <*> o .:? "cal_geometry"

instance J.ToJSON Calibration where
    toJSON Calibration{..} = J.object $ noNullPairs
        [ "caltype" .= calibrationCalType
        , "bearing" .= calibrationBearing
        , "cal_geometry" .= calibrationCalGeometry
        ]

data SpatialGlobal = SpatialGlobal {
    spatialGlobalNumElements :: Word64
  , spatialGlobalChannelIndex :: Word64
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

data SpatialCapture = SpatialCapture {
    spatialCaptureApertureAzimuth :: Maybe Double
  , spatialCaptureApertureBearing :: Maybe Bearing
  , spatialCaptureEmitterBearing :: Maybe Bearing
  , spatialCaptureElementGeometry :: [CartesianPoint]
  , spatialCapturePhaseOffset :: Maybe Double
  , spatialCaptureCalibration :: Maybe Calibration
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | The empty 'SpatialCapture', for convenience of construction.
emptySpatialCapture :: SpatialCapture
emptySpatialCapture =
    let spatialCaptureApertureAzimuth = Nothing
        spatialCaptureApertureBearing = Nothing
        spatialCaptureEmitterBearing = Nothing
        spatialCaptureElementGeometry = []
        spatialCapturePhaseOffset = Nothing
        spatialCaptureCalibration = Nothing
    in SpatialCapture{..}

data SpatialAnnotation = SpatialAnnotation {
    spatialAnnotationSignalAzimuth :: Maybe Double
  , spatialAnnotationSignalBearing :: Maybe Bearing
  } deriving ( Eq
             , Ord
             , Read
             , Show
             , Generic
             , NFData
             )

-- | The empty 'SpatialAnnotation', for convenience of construction.
emptySpatialAnnotation :: SpatialAnnotation
emptySpatialAnnotation =
    let spatialAnnotationSignalAzimuth = Nothing
        spatialAnnotationSignalBearing = Nothing
    in SpatialAnnotation{..}
