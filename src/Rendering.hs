module Rendering(renderImages, renderPixbuf) where
import Graphics.UI.Gtk (initGUI)
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.Pixbuf (pixbufNewFromFile, pixbufGetWidth, pixbufGetHeight, Pixbuf (..))
import Graphics.UI.Gtk.Cairo (setSourcePixbuf)
import Control.Monad.IO.Class (MonadIO)
import Data.List.Split (chunksOf)


data ImageMatrix = ImageMatrix {
        matrixWidth :: Double,
        matrixHeight :: Double,
        imagesHorizontally :: Int,
        imagesVertically :: Int
    }

scaleNeededFor :: ImageMatrix -> Double -> Double -> Double
scaleNeededFor matrix imageWidth imageHeight = min widthScale heightScale
    where
        shouldBeWidth = (matrixWidth matrix) / (fromIntegral $ imagesHorizontally matrix)
        shouldBeHeight = (matrixHeight matrix) / (fromIntegral $ imagesVertically matrix)
        widthScale = shouldBeWidth / (imageWidth)
        heightScale = shouldBeHeight / (imageHeight)

renderPixbuf :: MonadIO m => Double -> Double -> Double -> Surface -> Pixbuf -> Double -> Double -> m ()
renderPixbuf width height scaling surface pbImageA xOffset yOffset = renderWith surface $ do
        -- rectangle xOffset yOffset width height
        save
        translate xOffset yOffset
        scale scaling scaling
        setSourcePixbuf pbImageA 0 0
        paint
        restore


locationsInMatrix :: ImageMatrix -> [(Double, Double)]
locationsInMatrix matrix = coords
    where
        widthPerImage = matrixWidth matrix / (fromIntegral $ imagesHorizontally matrix)
        heightPerImage = matrixHeight matrix / (fromIntegral $ imagesVertically matrix)
        coords = [(widthPerImage * (fromIntegral x), heightPerImage * (fromIntegral y)) | y <- [0..(imagesVertically matrix) -1], x <- [0..(imagesHorizontally matrix - 1)]]


combineImages :: ImageMatrix -> [IO Pixbuf] -> String -> Surface -> IO ()
combineImages matrix images outputImageName surface = do
    firstImage <- head images
    imageWidth <- fmap fromIntegral (pixbufGetWidth firstImage)
    imageHeight <- fmap fromIntegral (pixbufGetHeight firstImage)

    let scaling = scaleNeededFor matrix imageWidth imageHeight
    renderWith surface $ do
        setSourceRGB 1 1 1
        rectangle 0 0 (matrixHeight matrix) (matrixHeight matrix)
        paint
    let place = renderPixbuf imageWidth imageHeight scaling surface :: Pixbuf -> Double -> Double -> IO()
    pbImageA <- head images
    let imagesAndLocations = zip (locationsInMatrix matrix) images
    putStrLn ("Placing " ++ (show $ length imagesAndLocations))
    mapM (\((x,y), ioImage) -> do
            img <- ioImage
            putStrLn ("Placing image at " ++ (show x) ++ " " ++ (show y))
            place img x y
            return ()
        ) imagesAndLocations
    --place pbImageA 600 60
    surfaceWriteToPNG surface outputImageName

renderImagesOnto :: Int -> Int -> [String] -> String -> IO()
renderImagesOnto width height imageFileNames outputImageName =     withImageSurface FormatRGB24 width height (combineImages (ImageMatrix (fromIntegral width) (fromIntegral height) 6 8) (map pixbufNewFromFile imageFileNames) outputImageName)


renderImages :: Int -> Int -> [String] -> IO()
renderImages width height imageFileNames = do
        initGUI -- Initialize GDK for jpeg loading
        mapM_ (\(images, output) -> 
            renderImagesOnto width height images output) imagesWithResultNames
    where
        imagesWithResultNames = zip (chunksOf 48 imageFileNames) outputNames
        outputNames = [x : ".png" | x <- ['a'..'z']]







