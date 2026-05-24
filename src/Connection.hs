{-# LANGUAGE OverloadedStrings #-}

module Connection (localPG, startConnection, addRecipe, deleteRecipe, searchRecipe, getAllRecipes, updateStringR, updateIntR, deleteIngredient, addIngredient, updateIngredientName, updateIngredientQuantity, updateIngredientUnit) where

    import Database.PostgreSQL.Simple
    import Recipes
    import Control.Monad
    import Data.Int (Int64)
    import Data.String (fromString)


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------- DATABASE CONNECTION --------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    localPG :: ConnectInfo
    localPG = defaultConnectInfo
        { connectHost = "localhost"
        , connectDatabase = "recipes"
        , connectUser = "postgres"
        }
    
    startConnection :: IO Connection
    startConnection = connect localPG


------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- TABLE INGREDIENTS --------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    addIngredients :: Connection -> Int -> [Ingredient] -> IO()
    addIngredients connection recipeId ingredients = do
        forM_ ingredients $ \ ingredient -> do
            execute connection
                "insert into ingredients (recipe_id, name, quantity, unit) values (?, ?, ?, ?)"
                (recipeId, ingName ingredient, quantity ingredient, unit ingredient)
    
    deleteIngredient :: Connection  -> IngredientDatabase -> IO()
    deleteIngredient  connection ingredient = do
        void $ execute connection
            "delete from ingredients where id = ? "
            (Only (idIgDB ingredient))

    addIngredient :: Connection -> Int64 -> Ingredient -> IO()
    addIngredient  connection recipeId ingredient = do
        void $ execute connection
            "insert into ingredients (recipe_id, name, quantity, unit) values (?, ?, ?, ?)"
            (recipeId, ingName ingredient, quantity ingredient, unit ingredient)
    
    updateIngredientName :: Connection -> Int64 -> String -> IO ()
    updateIngredientName connection ingredientId newName = do
        void $ execute connection
            "update ingredients set name = ? where id = ?"
            (newName, ingredientId)

    updateIngredientQuantity :: Connection -> Int64 -> Double -> IO ()
    updateIngredientQuantity connection ingredientId newQuantity = do
        void $ execute connection
            "update ingredients set quantity = ? where id = ?"
            (newQuantity, ingredientId)

    updateIngredientUnit :: Connection -> Int64 -> Unit -> IO ()
    updateIngredientUnit connection ingredientId newUnit = do
        void $ execute connection
            "update ingredients set unit = ? where id = ?"
            (show newUnit, ingredientId)


------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- TABLE RECIPES ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    addRecipe :: Connection -> Recipe -> IO ()
    addRecipe connection recipe = do
        [Only recipeId] <- query connection
            "insert into recipes (name, portions, calories, description) values (?, ?, ?, ?) returning id"
            (name recipe, portions recipe, calories recipe, description recipe)

        addIngredients connection recipeId (ingredients recipe)
    
    deleteRecipe :: Connection -> Int64 -> IO ()
    deleteRecipe connection i = do 
        void $ execute connection
            "delete from recipes where id = ?"
            (Only i)
    
    --updates

    updateStringR :: Connection -> Int64 -> String -> String -> IO ()
    updateStringR connection id col text = do
        let queryString = "UPDATE recipes SET " ++ col ++ " = ? WHERE id = ?"
            query = fromString queryString
        void $ execute connection query (text, id)

    updateIntR :: Connection -> Int64 -> String -> Int -> IO ()
    updateIntR connection id col i = do
        let queryString = "UPDATE recipes SET " ++ col ++ " = ? WHERE id = ?"
            query = fromString queryString
        void $ execute connection query (i, id)
    
    -- deleteRecipe :: Connection -> String -> IO ()
    -- deleteRecipe connection name = do 
    --     _ <- execute connection
    --         "delete from recipes where name ilike ?"
    --         (Only name)
    
    searchRecipe :: Connection -> String -> IO [ (RecipeDatabase, [IngredientDatabase]) ]
    searchRecipe connection name = do
        let pattern = "%" ++ name ++ "%"
        recipes <- query connection
            "select * from recipes where name ilike ?"
            (Only pattern)
        recipeAndIng <- mapM (\ recipe -> do
            ingredients <- query connection
                "select id, name, quantity, unit from ingredients where recipe_id = ?"
                (Only (idDB recipe))
            return (recipe, ingredients)) recipes
        
        return recipeAndIng
    
    getAllRecipes :: Connection -> IO [ (RecipeDatabase, [IngredientDatabase]) ]
    getAllRecipes connection = do
        recipes <- query connection
            "select * from recipes" ()
        recipeAndIng <- mapM (\recipe -> do
            ingredients <- query connection
                "select id, name, quantity, unit from ingredients where recipe_id = ?"
                (Only (idDB recipe))
            return (recipe, ingredients)) recipes
        
        return recipeAndIng
