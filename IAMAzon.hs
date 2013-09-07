module IAMAzon
where
    import Data.Maybe

    data Dimensions = Lengths [Double]

    data AmazonCategory  = Media | NonMedia

    data AmazonItem = AmazonItem { 
        Price :: Double,
        Category :: AmazonCategory,
        ProfitMarkupDesired :: Maybe Double
        PercentageMarkupDesired :: Maybe Double,
        Dimension :: Dimensions,
        MonthsInStorageSummer :: Int,
        MonthsInStorageWinter :: Int,
        Weight :: Double,
        ShippingCost :: Maybe Double 
    }