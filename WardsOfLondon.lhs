
I had a fun weekend analysing car parking data in Westminster (in the
UK for clarity) at the [Future Cities
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
for the actual analysis.

I thought it would be an interesting exercise to recreate some of the analysis in Haskell.

> {-# OPTIONS_GHC -Wall                    #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# OPTIONS_GHC -fno-warn-orphans        #-}
>
> {-# LANGUAGE ScopedTypeVariables         #-}
> {-# LANGUAGE OverloadedStrings           #-}
> {-# LANGUAGE ViewPatterns                #-}
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
> import Data.Csv
> import qualified Data.Vector as V
> import Data.Time
> import qualified Data.Text as T
> import Data.Char
>
> import Control.Monad
>
> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
>
> import System.FilePath
> import System.Directory
> import System.Locale
>
> type Diag = Diagram Cairo R2
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
> flParkingCashless = "ParkingCashlessDenormHead.csv"
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
> bayDots :: V.Vector (Double, Double) -> Diag
> bayDots xs = position (zip (map p2 $ V.toList xs) (repeat dot))
>   where dot       = circle 0.001 # fc green
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
>                { _amountPaid       :: Float
>                , _paidDurationMins :: Int
>                , _startDate        :: UTCTime
>                , _startDay         :: DayOfTheWeek
>                , _endDate          :: UTCTime
>                , _endDay           :: DayOfTheWeek
>                , _startTime        :: TimeOfDay
>                , _endTime          :: TimeOfDay
>                , _designationType  :: T.Text
>                , _hoursOfControl   :: T.Text
>                , _tariff           :: Float
>                , _maxStay          :: T.Text
>                , _spaces           :: Int
>                , _street           :: T.Text
>                , _xCoordinate      :: Double
>                , _yCoordinate      :: Double
>                , latitude         :: Double
>                , longitude        :: LaxDouble
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
>
> main :: IO ()
> main = do
>   parkingCashlessCsv <- BL.readFile $
>                         prefix </>
>                         dataDir </>
>                         parkingBorough </>
>                         flParkingCashless
>
>   parkBayCoords <- case decode False parkingCashlessCsv of
>     Left err -> error err
>     Right v -> V.forM v $
>                \(v :: Payment) ->
>                do let x = laxDouble $ longitude v
>                       y = latitude v
>                   putStrLn $ "(" ++ (show x) ++ ", " ++ (show y) ++ ")"
>                   return (x, y)
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
>     -- mconcat p <>
>     bayDots parkBayCoords

http://www.bbc.co.uk/news/uk-england-london-19732371
