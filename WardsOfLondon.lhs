
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
> import Data.List ( nub )
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
>
> type Diag = Diagram SVG R2
>
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
>
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
> colouredLine :: Colour Double -> [(Double, Double)] -> Diag
> colouredLine lineColour xs = (fromVertices $ map p2 xs) # lw 0.0001 # lc lineColour
>
> bayDots :: [Pair Double] -> Diag
> bayDots xs = position (zip (map p2 $ map toPair xs) (repeat dot))
>   where dot      = circle 0.001 # fc green # lw 0.0
>         toPair p = (xPair p, yPair p)
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
> recsOfInterest :: BBox (Double, Double) -> [ShpRec] -> [ShpRec]
> recsOfInterest bb = filter (flip isInBB bb . getBBs . shpRecData)
>
> processWard :: [ShpRec] -> FilePath -> IO ([ShpRec], [(Double, Double)])
> processWard recDB fileName = do
>   input <- BL.readFile $ prefix </> dataDir </> borough </> fileName
>   let (hdr, recs) = runGet getShpFile input
>       ns          = map (shpRecSizeBytes . shpRecHdr) $ recs
>       bb          = shpFileBBox hdr
>   putStrLn $ show bb
>   let (_, _, _, _, ps)  = head $ zipWith getRecs ns  (map shpRecData recs)
>   return $ (recsOfInterest bb recDB, ps)
>
> _columnNames :: [String]
> _columnNames = [ "amount paid"
>                , "paid duration mins"
>                , "start date"
>                , "start day"
>                , "end date"
>                , "end day"
>                , "start time"
>                , "end time"
>                , "DesignationType"
>                , "Hours of Control"
>                , "Tariff"
>                , "Max Stay"
>                , "Spaces"
>                , "Street"
>                , "x coordinate"
>                , "y coordinate"
>                , "latitude"
>                , "longitude"
>                ]
>
> data DayOfTheWeek = Monday
>                   | Tuesday
>                   | Wednesday
>                   | Thursday
>                   | Friday
>                   | Saturday
>                   | Sunday
>   deriving (Read, Show, Enum)
>
> instance FromField DayOfTheWeek where
>   parseField s = read <$> parseField s
>
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
>
> data Payment = Payment
>                { _amountPaid       :: LaxDouble
>                , paidDurationMins  :: Int
>                , _startDate        :: UTCTime
>                , _startDay         :: DayOfTheWeek
>                , _endDate          :: UTCTime
>                , _endDay           :: DayOfTheWeek
>                , _startTime        :: TimeOfDay
>                , _endTime          :: TimeOfDay
>                , _designationType  :: T.Text
>                , hoursOfControl   :: T.Text
>                , _tariff           :: T.Text
>                , _maxStay          :: T.Text
>                , _spaces           :: Maybe Int
>                , _street           :: T.Text
>                , _xCoordinate      :: Maybe Double
>                , _yCoordinate      :: Maybe Double
>                , latitude          :: Maybe Double
>                , longitude         :: Maybe LaxDouble
>                }
>   deriving Show
>
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
>
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

It turns out that there are a very limited number of hours of control
so rather than parse this and calculate the number of control minutes
per week, we can just create a simple look up table by hand.

> hoursOfControlTable :: [(T.Text, Int)]
> hoursOfControlTable = [
>     ("Mon - Fri 8.30am - 6.30pm"                      , 50 * 60)
>   , ("Mon-Fri 10am - 4pm"                             , 30 * 60)
>   , ("Mon - Fri 8.30-6.30 Sat 8.30 - 1.30"            , 55 * 60)
>   , ("Mon - Sat 8.30am - 6.30pm"                      , 60 * 60)
>   , ("Mon-Sat 11am-6.30pm "                           , 45 * 60)
>   , ("Mon - Fri 8.00pm - 8.00am"                      , 60 * 60)
>   , ("Mon - Fri 8.30am - 6.30pm "                     , 50 * 60)
>   , ("Mon - Fri 10.00am - 6.30pm\nSat 8.30am - 6.30pm", 85 * 30 + 10 * 60)
>   , ("Mon-Sun 10.00am-4.00pm & 7.00pm - Midnight"     , 77 * 60)
>   ]

The !'s are *really* important otherwise we get a space leak. In more
detail, these are strictness annotations which force the record to be
evaluated rather than be carried around unevaluated (taking up
unnecessary space) until needed.

> data LotStats = LotStats { usageCount       :: !Int
>                          , usageMins        :: !Int64
>                          , usageControlTxt  :: !T.Text
>                          , usageControlMins :: !Int
>                          }
>   deriving Show
>
> updateStats :: LotStats -> LotStats -> LotStats
> updateStats s1 s2 = LotStats { usageCount = (usageCount s1) + (usageCount s2)
>                              , usageMins  = (usageMins s1) +  (usageMins s2)
>                              , usageControlTxt = usageControlTxt s2
>                              , usageControlMins = y
>                              }
>   where
>     y = case lookup (usageControlTxt s2) hoursOfControlTable of
>           Nothing -> error $ "Looked up " ++ show (usageControlTxt s2)
>           Just x  -> x
>
> bayCountMap :: Map.Map (Pair Double) LotStats
> bayCountMap = Map.empty
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
>             Just v  -> loop (Map.insertWith updateStats v delta m) rest
>               where
>                 delta = LotStats { usageCount = 1
>                                  , usageMins  = fromIntegral $ paidDurationMins val
>                                  , usageControlTxt = hoursOfControl val
>                                  , usageControlMins = 0
>                                  }
>         Nil mErr x  -> if BL.null x
>                        then m
>                        else error $ "Nil: " ++ show mErr ++ " " ++ show x
>
>   let baz = loop bayCountMap (decode False parkingCashlessCsv)
>
>   mapM_ putStrLn $ nub $ map T.unpack $ map usageControlTxt $ Map.elems baz
>   mapM_ putStrLn $ map show $ map usageControlMins $ Map.elems baz
>
>   error "Stop here"
>
>   let parkBayCoords :: [Pair Double]
>       parkBayCoords = Map.keys baz
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
>   let recsFiltered = concat $ map fst rps
>
>   let xs = zipWith getRecs nsGL (map shpRecData recsFiltered)
>       f (_, _, _, _, ps) = ps
>       p = map (colouredLine blue . f) xs
>
>   defaultMain $
>     mconcat (zipWith colouredLine (cycle [red, yellow]) (map snd rps)) <>
>     mconcat p <>
>     bayDots parkBayCoords

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
