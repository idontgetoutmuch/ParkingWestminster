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
> import Data.Word( Word32 )
> import Control.Monad
> import Diagrams.Prelude
> import Diagrams.Backend.SVG.CmdLine
> import System.FilePath
> import System.Directory
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
> foo :: Colour Double -> [(Double, Double)] -> Diagram SVG R2
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
> processWard :: [ShpRec] -> FilePath -> IO ([ShpRec], [(Double, Double)])
> processWard recDB fileName = do
>   input <- BL.readFile $ prefix </> prefix1 </> prefix2 </> fileName
>   let (hdr, recs) = runGet getShpFile input
>       ns          = map (shpRecSizeBytes . shpRecHdr) $ recs
>       bb          = shpFileBBox hdr
>   putStrLn $ show bb
>   let (_, _, _, _, ps)  = head $ zipWith getRecs ns  (map shpRecData $ recs)
>   return $ (recsOfInterest bb recDB, ps)
>
> main :: IO ()
> main = do
>   fs <- getDirectoryContents $ prefix </> prefix1 </> prefix2
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
>       p = map (foo blue . f) xs
>
>   defaultMain (mconcat (zipWith foo (cycle [red, yellow]) (map snd rps)) <> mconcat p)
