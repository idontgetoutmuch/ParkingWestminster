
I had a fun weekend analysing car parking data in
[Westminster](http://www.westminster.gov.uk) at the [Future Cities
Hackathon](http://futurecitieshackathon.com) along with

* Amit Nandi
* Bart Baddeley
* Jackie Steinitz
* [Ian Ozsvald](https://twitter.com/ianozsvald)
* Mateusz Łapsa-Malawski

Our analysis gained an honourable mention. Ian has produced a great
[write-up](http://ianozsvald.com/2013/10/07/future-cities-hackathon-ds_ldn-oct-2013-on-parking-usage-inefficiencies)
of our analysis with fine watercolour maps and Bart's time-lapse video
of parking behaviour.

We mainly used Python, [Pandas](http://pandas.pydata.org) and Excel
for the actual analysis and [QGIS](http://qgis.org/en/site) for the maps.

I thought it would be an interesting exercise to recreate some of the analysis in Haskell.

A Haskell Implementation
========================

First some pragmas and imports.

> {-# OPTIONS_GHC -Wall                    #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# OPTIONS_GHC -fno-warn-orphans        #-}
>
> {-# LANGUAGE ScopedTypeVariables         #-}
> {-# LANGUAGE OverloadedStrings           #-}
> {-# LANGUAGE ViewPatterns                #-}
> {-# LANGUAGE DeriveTraversable           #-}
> {-# LANGUAGE DeriveFoldable              #-}
> {-# LANGUAGE DeriveFunctor               #-}
>
> module Main (main) where
>
> import Database.Shapefile
>
> import Data.Binary.Get
> import qualified Data.ByteString.Lazy as BL
> import qualified Data.ByteString as B
> import Data.Binary.IEEE754
> import Data.Word ( Word32 )
> import Data.Csv hiding ( decode, lookup )
> import Data.Csv.Streaming
> import qualified Data.Vector as V
> import Data.Time
> import qualified Data.Text as T
> import Data.Char
> import qualified Data.Map.Strict as Map
> import Data.Int( Int64 )
> import Data.Maybe ( fromJust, isNothing )
>
> import Control.Applicative
> import Control.Monad
>
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
>
> import System.FilePath
> import System.Directory
> import System.Locale
>
> import Data.Traversable ( Traversable )
> import qualified Data.Traversable as Tr
> import Data.Foldable ( Foldable )

A type synonym to make typing some of our functions a bit more
readable (and easier to modify e.g. if we want to use Cairo).

> type Diag = Diagram SVG R2

The paths to all our data.

SHP files are [shape files](http://en.wikipedia.org/wiki/Shapefile), a
fairly old but widespread map data format that was originally produced
by a company called ESRI.

The
[polygons](http://data.london.gov.uk/datastore/package/i-trees-canopy-ward-data)
for the outline of the wards in Westminster. Surely there is a better
place to get this rather than using tree canopy data.

The [polyline
data](http://download.geofabrik.de/europe/great-britain.html) for all
the roads (and other stuff) in the UK. We selected out all the roads
in a bounding box for London. Even so plotting these takes about a
minute.

> prefix :: FilePath
> prefix = "/Users/dom"
>
> dataDir :: FilePath
> dataDir = "Downloadable/DataScienceLondon"
>
> borough :: FilePath
> borough = "WestMinster"
>
> parkingBorough :: FilePath
> parkingBorough = "ParkingWestminster"
>
> flGL :: FilePath
> flGL = prefix </> dataDir </> "GreaterLondonRoads.shp"
>
> flParkingCashless :: FilePath
> flParkingCashless = "ParkingCashlessDenorm.csv"

The data for payments are contained in a CSV file so we create a
record in which to keep the various fields contained therein.

> data Payment = Payment
>                { _amountPaid       :: LaxDouble
>                ,  paidDurationMins :: Int
>                ,  startDate        :: UTCTime
>                , _startDay         :: DayOfTheWeek
>                , _endDate          :: UTCTime
>                , _endDay           :: DayOfTheWeek
>                , _startTime        :: TimeOfDay
>                , _endTime          :: TimeOfDay
>                , _designationType  :: T.Text
>                ,  hoursOfControl   :: T.Text
>                , _tariff           :: T.Text
>                , _maxStay          :: T.Text
>                , spaces            :: Maybe Int
>                , _street           :: T.Text
>                , _xCoordinate      :: Maybe Double
>                , _yCoordinate      :: Maybe Double
>                , latitude          :: Maybe Double
>                , longitude         :: Maybe LaxDouble
>                }
>   deriving Show
>
> data DayOfTheWeek = Monday
>                   | Tuesday
>                   | Wednesday
>                   | Thursday
>                   | Friday
>                   | Saturday
>                   | Sunday
>   deriving (Read, Show, Enum)

We need to be able to parse the day of the week.

> instance FromField DayOfTheWeek where
>   parseField s = read <$> parseField s

The field containing the longitude has values of the form -.1. The CSV
parser for *Double* will reject this so we create our own datatype
with a more relaxed parser.

> newtype LaxDouble = LaxDouble { laxDouble :: Double }
>   deriving Show
>
> instance FromField LaxDouble where
>   parseField = fmap LaxDouble . parseField . addLeading
>
>     where
>
>       addLeading :: B.ByteString -> B.ByteString
>       addLeading bytes =
>             case B.uncons bytes of
>               Just (c -> '.', _)    -> B.cons (o '0') bytes
>               Just (c -> '-', rest) -> B.cons (o '-') (addLeading rest)
>               _ -> bytes
>
>       c = chr . fromIntegral
>       o = fromIntegral . ord

We need to be able to parse dates and times.

> instance FromField UTCTime where
>   parseField s = do
>     f <- parseField s
>     case parseTime defaultTimeLocale "%F %X" f of
>       Nothing -> fail "Unable to parse UTC time"
>       Just g  -> return g
>
> instance FromField TimeOfDay where
>   parseField s = do
>     f <- parseField s
>     case parseTime defaultTimeLocale "%R" f of
>       Nothing -> fail "Unable to parse time of day"
>       Just g  -> return g

Finally we can write a parser for our record.

> instance FromRecord Payment where
>   parseRecord v
>          | V.length v == 18
>          = Payment <$>
>            v .!  0 <*>
>            v .!  1 <*>
>            v .!  2 <*>
>            v .!  3 <*>
>            v .!  4 <*>
>            v .!  5 <*>
>            v .!  6 <*>
>            v .!  7 <*>
>            v .!  8 <*>
>            v .!  9 <*>
>            v .! 10 <*>
>            v .! 11 <*>
>            v .! 12 <*>
>            v .! 13 <*>
>            v .! 14 <*>
>            v .! 15 <*>
>            v .! 16 <*>
>            v .! 17
>          | otherwise     = mzero

To make the analysis simpler, we only look at what might be a typical
day, a Thursday in February.

> selectedDay :: UTCTime
> selectedDay = case parseTime defaultTimeLocale "%F %X" "2013-02-28 00:00:00" of
>   Nothing -> error "Unable to parse UTC time"
>   Just t  -> t

It turns out that there are a very limited number of different sorts
of hours of control so rather than parse this and calculate the number
of control minutes per week, we can just create a simple look up table
by hand.

> hoursOfControlTable :: [(T.Text, [Int])]
> hoursOfControlTable = [
>     ("Mon - Fri 8.30am - 6.30pm"                      , [600, 600, 600, 600, 600,   0,   0])
>   , ("Mon-Fri 10am - 4pm"                             , [360, 360, 360, 360, 360,   0,   0])
>   , ("Mon - Fri 8.30-6.30 Sat 8.30 - 1.30"            , [600, 600, 600, 600, 600, 300,   0])
>   , ("Mon - Sat 8.30am - 6.30pm"                      , [600, 600, 600, 600, 600, 600,   0])
>   , ("Mon-Sat 11am-6.30pm "                           , [450, 450, 450, 450, 450, 450,   0])
>   , ("Mon - Fri 8.00pm - 8.00am"                      , [720, 720, 720, 720, 720,   0,   0])
>   , ("Mon - Fri 8.30am - 6.30pm "                     , [600, 600, 600, 600, 600,   0,   0])
>   , ("Mon - Fri 10.00am - 6.30pm\nSat 8.30am - 6.30pm", [510, 510, 510, 510, 510, 600,   0])
>   , ("Mon-Sun 10.00am-4.00pm & 7.00pm - Midnight"     , [660, 660, 660, 660, 660, 660, 660])
>   ]

Now we create a record in which to record the statistics in which we are interested:

* Number of times a lot is used.

* Number of usage minutes. In reality this is the amount of minutes
purchased and people often leave a bay before their ticket expires so
this is just a proxy.

* The hourse of control for the lot.

* The number of bays in the lot.

N.B. The !'s are *really* important otherwise we get a space leak. In
more detail, these are strictness annotations which force the record
to be evaluated rather than be carried around unevaluated (taking up
unnecessary space) until needed.

> data LotStats = LotStats { usageCount       :: !Int
>                          , usageMins        :: !Int64
>                          , usageControlTxt  :: !T.Text
>                          , usageSpaces      :: !(Maybe Int)
>                          }
>   deriving Show

As we work our way through the data we need to update our statistics.

> updateStats :: LotStats -> LotStats -> LotStats
> updateStats s1 s2 = LotStats { usageCount = (usageCount s1) + (usageCount s2)
>                              , usageMins  = (usageMins s1) +  (usageMins s2)
>                              , usageControlTxt  = usageControlTxt s2
>                              , usageSpaces = usageSpaces s2
>                              }
>
> initBayCountMap :: Map.Map (Pair Double) LotStats
> initBayCountMap = Map.empty


We are going to be working with co-ordinates which are pairs of
numbers so we need a data type in which to keep them.

> data Pair a = Pair { xPair :: !a, yPair :: !a }
>   deriving (Show, Eq, Ord, Functor, Foldable, Traversable)
>
> getPair :: Get a -> Get (a,a)
> getPair getPart = do
>     x <- getPart
>     y <- getPart
>     return (x,y)
>
> getBBox :: Get a -> Get (BBox a)
> getBBox getPoint = do
>     bbMin <- getPoint
>     bbMax <- getPoint
>     return (BBox bbMin bbMax)
>
> myBBox :: Get (BBox (Double, Double))
> myBBox = do
>   shpFileBBox <- getBBox (getPair getFloat64le)
>   return shpFileBBox
>
> getRecs :: t -> BL.ByteString ->
>            (BBox (Double, Double),
>             Word32,
>             Word32,
>             [Word32],
>             [(Double, Double)])
> getRecs _n = runGet $ do
>   _ <- getShapeType32le
>   bb <- myBBox
>   -- FIXME: But what if there is more than one part? This will FAIL!!!
>   nParts <- getWord32le
>   nPoints <- getWord32le
>   parts <- replicateM (fromIntegral nParts) getWord32le
>   points <- replicateM (fromIntegral nPoints) (getPair getFloat64le)
>   return (bb, nParts, nPoints, parts, points)
>
>
> getBBs :: BL.ByteString -> BBox (Double, Double)
> getBBs = runGet $ do
>   _ <- getShapeType32le
>   myBBox
>
> isInBB :: (Ord a, Ord b) => BBox (a, b) -> BBox (a, b) -> Bool
> isInBB bbx bby = ea >= eb && wa <= wb &&
>                  sa >= sb && na <= nb
>   where
>     (ea, sa) = bbMin bbx
>     (wa, na) = bbMax bbx
>     (eb, sb) = bbMin bby
>     (wb, nb) = bbMax bby
>
> makeBorder :: (Ord a, Ord b) => BBox (a, b) -> BBox (a, b) -> BBox (a, b)
> makeBorder bbx bby = BBox { bbMin = (min ea eb, min sa sb)
>                           , bbMax = (max wa wb, max na nb)
>                           }
>   where
>     (ea, sa) = bbMin bbx
>     (wa, na) = bbMax bbx
>     (eb, sb) = bbMin bby
>     (wb, nb) = bbMax bby
>
> recsOfInterest :: BBox (Double, Double) -> [ShpRec] -> [ShpRec]
> recsOfInterest bb = filter (flip isInBB bb . getBBs . shpRecData)
>
> processWard :: [ShpRec] -> FilePath -> IO ([ShpRec], ([(Double, Double)], BBox (Double, Double)))
> processWard recDB fileName = do
>   input <- BL.readFile $ prefix </> dataDir </> borough </> fileName
>   let (hdr, recs) = runGet getShpFile input
>       ns          = map (shpRecSizeBytes . shpRecHdr) $ recs
>       bb          = shpFileBBox hdr
>   let (_, _, _, _, ps)  = head $ zipWith getRecs ns  (map shpRecData recs)
>   return $ (recsOfInterest bb recDB, (ps, bb))
>
> colouredLine :: Double -> Colour Double -> [(Double, Double)] -> Diag
> colouredLine thickness lineColour xs = (fromVertices $ map p2 xs) #
>                                        lw thickness #
>                                        lc lineColour
>
> bayDots :: [Pair Double] -> [Double] -> Diag
> bayDots xs bs = position (zip (map p2 $ map toPair xs) dots)
>   where dots     = map (\b -> circle 0.0005 # fcA (blend b c1 c2) # lw 0.0) bs
>         toPair p = (xPair p, yPair p)
>         c1       = darkgreen  `withOpacity` 0.7
>         c2       = lightgreen `withOpacity` 0.7
>
> main :: IO ()
> main = do
>   parkingCashlessCsv <- BL.readFile $
>                         prefix </>
>                         dataDir </>
>                         parkingBorough </>
>                         flParkingCashless
>
>   let loop m rs = case rs of
>         Cons u rest -> case u of
>           Left err ->  error err
>           Right val -> case Tr.sequence $ Pair (laxDouble <$> longitude val) (latitude val) of
>             Nothing -> loop m rest
>
>             -- If the latitude and longitude were recorded then we can update our
>             -- stats. To make the analysis simpler, we only record a single day.
>
>             Just v  -> if startDate val == selectedDay
>                        then loop (Map.insertWith updateStats v delta m) rest
>                        else loop m rest
>               where
>                 delta = LotStats { usageCount = 1
>                                  , usageMins  = fromIntegral $ paidDurationMins val
>                                  , usageControlTxt = hoursOfControl val
>                                  , usageSpaces     = spaces val
>                                  }
>         Nil mErr x  -> if BL.null x
>                        then m
>                        else error $ "Nil: " ++ show mErr ++ " " ++ show x
>
>   let bayCountMap = loop initBayCountMap (decode False parkingCashlessCsv)
>
>   -- Calculate the available parking minutes for a lot on our chosen
>   -- day (a Thursday).
>
>   let vals = Map.elems bayCountMap
>
>       availableMinsThu :: [Maybe Double]
>       availableMinsThu =
>         map (fmap fromIntegral) $
>         map (fmap (!!(fromEnum Thursday))) $
>         map (flip lookup hoursOfControlTable) $
>         map usageControlTxt vals
>
>       availableMinsThu' :: [Maybe Double]
>       availableMinsThu' = zipWith f availableMinsThu
>                                     (map (fmap fromIntegral . usageSpaces) vals)
>         where
>           f x y = (*) <$> x <*> y
>
>       actualMinsThu :: [Double]
>       actualMinsThu =
>         map fromIntegral $
>         map usageMins vals
>
>       usage :: [Maybe Double]
>       usage = zipWith f actualMinsThu availableMinsThu'
>         where
>           f x y = (/) <$> pure x <*> y
>
>   let parkBayCoords :: [Pair Double]
>       parkBayCoords = Map.keys bayCountMap
>
>   mapM_ putStrLn $ map show parkBayCoords
>   mapM_ putStrLn $ map show usage
>
>   fs <- getDirectoryContents $ prefix </> dataDir </> borough
>   let gs = map (uncurry addExtension) $
>            filter ((==".shp"). snd) $
>            map splitExtension fs
>   putStrLn $ show gs
>
>   inputGL <- BL.readFile flGL
>   let (hdrGL, recsGL) = runGet getShpFile inputGL
>       nsGL            = map (shpRecSizeBytes . shpRecHdr) recsGL
>   putStrLn $ show $ shpFileBBox hdrGL
>
>   rps <- mapM (processWard recsGL) gs
>
>   let bbWestminster = foldr makeBorder (BBox (read "Infinity", read "Infinity") (read "-Infinity", read "-Infinity")) $ map (snd . snd) rps
>
>   putStrLn $ show bbWestminster
>
>   let (ea, sa) = bbMin bbWestminster
>       (wa, na) = bbMax bbWestminster
>       wmHeight = na - sa
>       wmWidth  = wa - ea
>
>   putStrLn $ show wmWidth
>   putStrLn $ show wmHeight
>
>   let recsFiltered = concat $ map fst rps
>
>   let xs = zipWith getRecs nsGL (map shpRecData recsFiltered)
>       f (_, _, _, _, ps) = ps
>       p = map (colouredLine 0.0001 blue . f) xs
>
>   let wmBackground = translateX (wmWidth / 2.0) $
>                      translateY (wmHeight / 2.0) $
>                      scaleX 1.1 $
>                      scaleY 1.1 $
>                      rect wmWidth wmHeight # fcA (yellow `withOpacity` 0.1) # lw 0.0
>
>       wmStreets =  translateX (negate ea) $
>                    translateY (negate sa) $
>                    mconcat p
>
>       wmParking = translateX (negate ea) $
>                   translateY (negate sa) $
>                   bayDots parkBayCoords (map fromJust usage)
>
>   defaultMain $
>     wmBackground
>     <> translateX (negate ea) (translateY (negate sa) (mconcat (zipWith (colouredLine 0.0003) (repeat navajowhite) (map (fst . snd) rps))))
>     <> wmStreets
>     <> wmParking

http://www.bbc.co.uk/news/uk-england-london-19732371

Observations
============

* We appear to need to use *ghc -O2* otherwise we get a spaceleak.

* We didn't explicitly need the equivalent of pandas. It would be
interesting to go through the Haskell and Python code and see where we
used pandas and what the equivalent was in Haskell.

* Python and R seem more forgiving about data formats e.g. they handle
-.1 where Haskell doesn't. Perhaps this should be in the Haskell
equivalent of pandas.
