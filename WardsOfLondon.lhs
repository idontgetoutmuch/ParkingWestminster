> {-# OPTIONS_GHC -Wall                     #-}
> {-# OPTIONS_GHC -fno-warn-name-shadowing  #-}
>
> module Main (main) where
>
> import Database.Shapefile
>
> import Data.Binary.Get
> import qualified Data.ByteString.Lazy as BL
> import Data.Binary.IEEE754
> import Control.Monad
> import Diagrams.Prelude
> import Diagrams.Backend.Cairo.CmdLine
> import System.FilePath
> import Data.Word( Word32 )
>
> prefix :: FilePath
> prefix = "/Users/dom"
>
> prefix1 :: FilePath
> prefix1 = "Downloadable/DataScienceLondon"
>
> prefix2 :: FilePath
> prefix2 = "WestMinster"
>
> flGrove :: FilePath
> flGrove = prefix </> prefix1 </> prefix2 </> "Westminster_AbbeyRoad.shp"
>
> flGrove1 :: FilePath
> flGrove1 = prefix </> prefix1 </> prefix2 </> "Westminster_Bayswater.shp"

> flGL :: FilePath
> flGL = prefix </> prefix1 </> "GreaterLondonRoads.shp"

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
> foo :: Colour Double -> [(Double, Double)] -> Diagram Cairo R2
> foo lineColour xs = (fromVertices $ map p2 xs) # lw 0.0001 # lc lineColour
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
> main :: IO ()
> main = do
>   inputGrove <- BL.readFile flGrove
>   let (hdrGrove, recsGrove) = runGet getShpFile inputGrove
>       ns                    = map (shpRecSizeBytes . shpRecHdr) $ recsGrove
>       gwBB                  = shpFileBBox hdrGrove
>   putStrLn $ show gwBB
>
>   inputGrove1 <- BL.readFile flGrove1
>   let (hdrGrove1, recsGrove1) = runGet getShpFile inputGrove1
>       ns1                     = map (shpRecSizeBytes . shpRecHdr) $ recsGrove1
>       gwBB1                   = shpFileBBox hdrGrove1
>   putStrLn $ show gwBB1
>
>   let (_, _, _, _, ps)  = head $ zipWith getRecs ns  (map shpRecData $ recsGrove)
>   let (_, _, _, _, ps1) = head $ zipWith getRecs ns1 (map shpRecData $ recsGrove1)
>
>   inputGL <- BL.readFile flGL
>   let (hdrGL, recsGL) = runGet getShpFile inputGL
>       nsGL            = map (shpRecSizeBytes . shpRecHdr) recsGL
>   putStrLn $ show $ shpFileBBox hdrGL
>
>   let recsFiltered = recsOfInterest gwBB  recsGL ++
>                      recsOfInterest gwBB1 recsGL
>
>   let xs = zipWith getRecs nsGL (map shpRecData recsFiltered)
>       f (_, _, _, _, ps) = ps
>       p :: [Diagram Cairo R2]
>       p = map (foo blue . f) xs
>
>   defaultMain (foo red ps <> foo yellow ps1 <> mconcat p)
