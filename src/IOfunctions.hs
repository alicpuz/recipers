module IOfunctions (options, choices, printIngredients, showRecipe) where

    import Text.Read (readMaybe)
    import Connection
    import Recipes
    import Control.Monad

    options :: IO()
    options = do
        putStrLn "Choose what you want to do"
        putStrLn "1. Search recipe"
        putStrLn "2. Add recipe"
    
    choices :: IO()
    choices = do
        n <- getLine
        case (readMaybe n :: Maybe Int) of
            Just 1 -> putStrLn "opcja 1"
            Just 2 -> putStrLn "opcja 2"
            _ -> putStrLn "Niepoprawna opcja"
    
    printIngredients :: [Ingredient] -> IO()
    printIngredients i = do
        mapM_ (\ing ->
            putStrLn ((ingName ing) ++ "   " ++ show (quantity ing) ++ " " ++ show (unit ing))) i

    showRecipe :: (RecipeDatabase, [Ingredient]) -> IO()
    showRecipe (r, i) = do
        putStrLn (nameDB r)
        putStrLn (maybe "?" show (portionsDB r) ++ " portions   " ++ maybe "?" show (caloriesDB r) ++ " kcal")
        printIngredients i
        putStrLn (descriptionDB r)
