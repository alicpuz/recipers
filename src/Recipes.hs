module Recipes ( Unit(..), Ingredient(..), Recipe(..), RecipeDatabase(..), IngredientDatabase(..)) where

    import Database.PostgreSQL.Simple.ToField (ToField(..))
    import Database.PostgreSQL.Simple.FromField ( FromField (fromField), returnError, ResultError (..) )
    import Database.PostgreSQL.Simple.FromRow (FromRow(..), field)
    import qualified Data.Text as T
    import Control.Applicative ()
    import Data.Int (Int64)


-- ingredients types

    data Unit = TSP | TBSP | CUP | ML | L | G | KG | PCS | CLOVE | SLICE | CAN | PINCH | HANDFUL deriving (Show, Read, Enum, Bounded, Eq)
    instance ToField Unit where
        toField = toField . show
    
    instance FromField Unit where
        fromField f bs = do
            text <- fromField f bs
            case T.unpack (T.toLower text) of
                "tsp" -> pure TSP
                "tbsp" -> pure TBSP
                "cup" -> pure CUP
                "ml" -> pure ML
                "l" -> pure L
                "g" -> pure G
                "kg" -> pure KG
                "pcs" -> pure PCS
                "clove" -> pure CLOVE
                "slice" -> pure SLICE
                "can" -> pure CAN
                "pinch" -> pure PINCH
                "handful" -> pure HANDFUL
                _ -> returnError ConversionFailed f "unknown unit"

    data Ingredient = Ingredient 
        {
            ingName :: String, 
            quantity :: Double,
            unit :: Unit
        } deriving (Show)
    
    instance FromRow Ingredient where
        fromRow = Ingredient <$> field <*> field <*> ( read <$> field )

    data IngredientDatabase = IngredientDatabase 
        {
            idIgDB :: Int64,
            ingNameDB :: String, 
            quantityDB :: Double,
            unitDB :: Unit
        } deriving (Show)
    
    instance FromRow IngredientDatabase where
        fromRow = IngredientDatabase <$> field <*> field <*> field <*> ( read <$> field )

-- recipes types

    data Recipe = Recipe
        {
            name :: String,
            portions :: Maybe Int,
            calories :: Maybe Int,
            ingredients :: [Ingredient],
            description :: String
        } deriving (Show)
    
    data RecipeDatabase = RecipeDatabase
        {
            idDB :: Int64,
            nameDB :: String,
            portionsDB :: Maybe Int,
            caloriesDB :: Maybe Int,
            descriptionDB :: String
        } deriving(Show)

    instance FromRow RecipeDatabase where
        fromRow = RecipeDatabase <$> field <*> field <*> field <*> field <*> field
