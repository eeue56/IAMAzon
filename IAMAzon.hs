module IAMAzon
where
    import Data.List
    import Data.Maybe

    data Dimensions = Lengths {small :: Double, middle :: Double, large :: Double} deriving (Show, Eq)

    data AmazonCategory  = Media | NonMedia deriving (Show, Eq)

    data AmazonItem = AmazonItem { 
        price :: Double,
        category :: AmazonCategory,
        profitMarkupDesired :: Maybe Double,
        percentageMarkupDesired :: Maybe Double,
        dimension :: Dimensions,
        monthsInStorageSummer :: Int,
        monthsInStorageWinter :: Int,
        weight :: Double,
        shippingCost :: Maybe Double 
    } deriving (Show, Eq)


    createDimension :: [Double] -> Maybe Dimensions
    createDimension xs 
        | length xs == 3 = Just $ Lengths small middle large
        | otherwise = Nothing
            where
                small = sorted !! 0
                middle = sorted !! 1
                large = sorted !! 2
                sorted = sort xs

    isOversized :: Double -> Double -> Double -> AmazonItem -> Bool
    isOversized lengthLimit widthLimit heightLimit item = 
        lengthLimit >= largeLength && 
        widthLimit >= middleLength &&
        heightLimit >= smallLength 
            where
                dimensions = dimension item
                largeLength = large dimensions
                middleLength = middle dimensions
                smallLength = small dimensions

    isOverweight :: Double -> AmazonItem -> Bool
    isOverweight weightLimit item = weight item < weightLimit