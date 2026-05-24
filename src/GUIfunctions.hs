{-# LANGUAGE OverloadedStrings #-}

module GUIfunctions (showMainView, showSearchResults) where

    import qualified Graphics.UI.Threepenny as UI
    import Graphics.UI.Threepenny.Core
    import Recipes
    import Control.Monad
    import Connection
    import Database.PostgreSQL.Simple
    import Text.Read (readMaybe)
    import Data.IORef ( modifyIORef, newIORef, readIORef, writeIORef )
    import Data.Maybe (isNothing, catMaybes)
    import Data.List

    clearWindow :: Window -> UI ()
    clearWindow window = do
        body <- getBody window
        void $ element body # set children []


------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- MAIN VIEW ----------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    showMainView :: Window -> Connection -> UI ()
    showMainView window connection = do
        clearWindow window
        content <- UI.div
        searchInput <- UI.input
        searchBtn <- UI.button # set UI.text "Search" # set UI.style [("margin-left", "20px")]
        addBtn <- UI.button # set UI.text "Add new recipe" # set UI.style [("margin-top", "20px")]
        allRecipes <- liftIO $ getAllRecipes connection
        showSearchResults window connection allRecipes content

        _ <- getBody window # set UI.style [
            ("display", "flex")
            , ("flex-direction", "column")
            , ("align-items", "center")
            , ("justify-content", "center")]

        _ <- getBody window #+ [column
            [row [element searchInput
            , element searchBtn]
            , element addBtn
            , element content]]

        on UI.click searchBtn $ \_ -> do
            userInput <- get value searchInput
            results <- liftIO $ searchRecipe connection userInput
            void $ element content # set children []
            showSearchResults window connection results content

        on UI.click addBtn $ \_ -> do
            clearWindow window
            showAddView window connection


------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- SEARCH VIEW --------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    showSearchResults :: Window -> Connection -> [(RecipeDatabase, [IngredientDatabase])] -> Element -> UI ()
    showSearchResults window con res sA = do
        forM_ res $ \(r, i) -> do
            tit <- UI.h3 #+ [string (nameDB r)]
            info <- UI.p #+ [string (showInfo r)]
            ing <- mapM (\ind -> UI.li #+ [string (showIngredient ind)]) i
            ingList <- UI.ul #+ map element ing
            des <- UI.p # set text (descriptionDB r)
            deleteRecipeBtn <- UI.button # set UI.text "Delete" # set UI.style [("color", "red"), ("margin-left", "20px")]
            updateRecipeBtn <- UI.button # set UI.text "Edit" # set UI.style [("margin-left", "20px")]

            searchBlock <- UI.div #+
                [row [element tit
                , element updateRecipeBtn, element deleteRecipeBtn]
                , element info
                , element ingList
                , element des]
                # set UI.style [
                    ("border", "2px solid rgba(132, 98, 19, 1)")
                    , ("border-color", "rgba(132, 98, 19, 1)")
                    , ("border-radius", "10px")
                    , ("padding", "10px")
                    , ("margin-top", "20px")
                    , ("background-color", "rgba(155, 114, 20, 0.52)"), ("width", "40vw")]

            _ <- element sA #+ [element searchBlock]

            on UI.click deleteRecipeBtn $ \_ -> do
                liftIO $ deleteRecipe con (idDB r)
                resu <- liftIO $ getAllRecipes con
                showMainView window con

            on UI.click updateRecipeBtn $ \_ -> do
                clearWindow window
                showEditView window con (r, i)

------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------- EDIT VIEW ----------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    showEditView :: Window -> Connection -> (RecipeDatabase, [IngredientDatabase]) -> UI ()
    showEditView window connection (r, i) = do

        -- elements --

        nDB <- UI.input # set value (nameDB r) # set UI.style [("margin-top", "10px"), ("font-size", "16px")]
        pDB <- UI.input # set value (maybe "" show (portionsDB r)) # set UI.style [("margin-top", "10px"), ("font-size", "16px")]
        cDB <- UI.input # set value (maybe "" show (caloriesDB r)) # set UI.style [("margin-top", "10px"), ("font-size", "16px")]
        dDB <- UI.textarea # set value (descriptionDB r) # set UI.style [("margin-top", "10px"), ("font-size", "16px"), ("width", "100%")]
        saveBtn <- UI.button # set UI.text "Save" # set UI.style [("margin-top", "40px"), ("font-size", "16px")]
        message <- UI.p # set UI.style [("color", "red")]
        iDB <- UI.div # set UI.style [("margin-top", "10px")]
        addIngBtn <- UI.button # set UI.text "Add ingredient" # set UI.style [("margin-top", "5px"), ("font-size", "14px")]
        igToDeleteRef <- liftIO $ newIORef []
        ingredientInputsRef <- liftIO $ newIORef []

        -- ingredients inputs --

        iInputs <- forM i $ \ig -> do
            igN <- UI.input # set value (ingNameDB ig) # set UI.style [("margin-top", "5px"), ("margin-right", "20px"), ("font-size", "16px")]
            igQ <- UI.input # set value (show (quantityDB ig)) # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            igU <- UI.select # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            igDelete <- UI.button # set text "Delete" # set UI.style [("color", "red"), ("margin-left", "20px")]

            _ <- element igU #+
                map (\un -> UI.option # set UI.text (show un) # set UI.value (show un))
                [TSP, TBSP, CUP, ML, L, G, KG, PCS, CLOVE, SLICE, CAN, PINCH, HANDFUL]
            _ <- element igU # set value (show (unitDB ig))

            iRow <- row [element igN, element igQ, element igU, element igDelete]

            _ <- element iDB #+ [element iRow]

            on UI.click igDelete $ \_ -> do
                
                _ <- element igN # set UI.enabled False
                _ <- element igQ # set UI.enabled False
                _ <- element igU # set UI.enabled False
                _ <- element igDelete # set UI.text "Marked for deletion"
                liftIO $ modifyIORef igToDeleteRef (++ [ig])
            return (igN, igQ, igU)
        
        -- add new ingredients --

        on UI.click addIngBtn $ \_ -> do
            n <- UI.input # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            q <- UI.input # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            u <- UI.select # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            _ <- element u #+
                map (\un -> UI.option # set UI.text (show un) # set UI.value (show un))
                [TSP, TBSP, CUP, ML, L, G, KG, PCS, CLOVE, SLICE, CAN, PINCH, HANDFUL]

            igDel <- UI.button # set text "Delete" # set UI.style [("color", "red"), ("margin-left", "20px")]

            rowElem <- row [element n, element q, element u]-- , element igDel]

            void $ element iDB #+ [element rowElem]

            on UI.click igDel $ \_ -> do
                _ <- element n # set value ""
                _ <- element q # set value ""
                _ <- element u # set value ""

                _ <- element rowElem # set UI.style [("display", "none")]
                return ()
            
            liftIO $ modifyIORef ingredientInputsRef (++ [(n, q, u)])

        editBlock <- UI.div # set UI.style [
                    ("border", "2px solid rgba(132, 98, 19, 1)")
                    , ("border-color", "rgba(132, 98, 19, 1)")
                    , ("border-radius", "10px")
                    , ("padding", "20px")]
        
        element editBlock #+ [column
            [ row [ UI.span #+ [UI.string "Name: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element nDB]
            , row [ UI.span #+ [UI.string "Portions: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element pDB]
            , row [ UI.span #+ [UI.string "Calories: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element cDB]
            , UI.span #+ [UI.string "Ingredients: "] # set UI.style [("margin-top", "20px"), ("font-size", "16px")]
            , element iDB
            , element addIngBtn
            , element dDB
            , element saveBtn
            , element message]]

        void $ getBody window #+ [element editBlock]

        -- save changes --

        on UI.click saveBtn $ \_ -> do

            accepted <- liftIO $ newIORef True

            nI <- get value nDB
            pI <- get value pDB
            let maybePInt = readMaybe pI :: Maybe Int
            cI <- get value cDB
            let maybeCInt = readMaybe cI :: Maybe Int
            dI <- get value dDB

            -- data checking --

            when (nI == "") $ do
                _ <- element message # set UI.text "You need to provide a name!"
                liftIO $ writeIORef accepted False
            case maybePInt of
                Just n -> when (Just n /= portionsDB r) (liftIO $ updateIntR connection (idDB r) "portions" n)
                Nothing -> when (pI /= "") (do
                    _ <- element message # set UI.text "Incorrect data in the 'portions' field!"
                    liftIO $ writeIORef accepted False)
            case maybeCInt of
                Just n -> when (Just n /= caloriesDB r) (liftIO $ updateIntR connection (idDB r) "calories" n)
                Nothing -> when (cI /= "") (do
                    _ <- element message # set UI.text "Incorrect data in the 'calories' field!"
                    liftIO $ writeIORef accepted False)
            when (dI == "") ( do
                _ <- element message # set UI.text "You need to provide a description!"
                liftIO $ writeIORef accepted False)
            
            igToAddRef <- liftIO $ newIORef []
            ingredientInputs <- liftIO $ readIORef ingredientInputsRef

            nonEmptyIngredients <- filterM (\(n, q, u) -> do
                nVal <- get value n
                qVal <- get value q
                uVal <- get value u
                return $ any (/= "") [nVal, qVal, uVal]) ingredientInputs

            parsedIngredients <- forM nonEmptyIngredients $ \(n, q, u) -> do
                nVal <- get value n
                qVal <- get value q
                uVal <- get value u
                let mQ = readMaybe qVal :: Maybe Double
                    mU = readMaybe uVal :: Maybe Unit
                return $ case (nVal, mQ, mU) of
                    ("", _, _)       -> Nothing
                    (_, Nothing, _)  -> Nothing
                    (_, _, Nothing)  -> Nothing
                    (_, Just q', Just u') -> Just (Ingredient nVal q' u')

            parsedIngredients <- forM nonEmptyIngredients $ \(n, q, u) -> do
                nVal <- get value n
                qVal <- get value q
                uVal <- get value u
                let mQ = readMaybe qVal :: Maybe Double
                    mU = readMaybe uVal :: Maybe Unit

                return $ case (nVal, mQ, mU) of
                    ("", _, _)       -> Nothing
                    (_, Nothing, _)  -> Nothing
                    (_, _, Nothing)  -> Nothing
                    (_, Just q', Just u') -> Just (Ingredient nVal q' u')

            when (any isNothing parsedIngredients) $ do
                _ <- element message # set UI.text "Some ingredient data is incomplete or invalid!"
                liftIO $ writeIORef accepted False
            
            let ingList = catMaybes parsedIngredients

            when (null iInputs && null ingList) (do
                _ <- element message # set UI.text "You need to add the ingredients!"
                liftIO $ writeIORef accepted False)


            a <- liftIO $ readIORef accepted
            when a ( do
                when (nI /= nameDB r) (liftIO $ updateStringR connection (idDB r) "name" nI)
                igToDelete <- liftIO $ readIORef igToDeleteRef
                liftIO $ forM_ igToDelete $ \ingredient ->
                    deleteIngredient connection ingredient
                liftIO $ forM_ ingList $ \ingredient ->
                    addIngredient connection (idDB r) ingredient
                when (dI /= descriptionDB r) (liftIO $ updateStringR connection (idDB r) "description" dI)

                forM_ (zip i iInputs) $ \(ingredient, (igN, igQ, igU)) -> do
                    newName <- get value igN
                    newQuantityStr <- get value igQ
                    let maybeNewQuantity = readMaybe newQuantityStr :: Maybe Double
                    newUnitStr <- get value igU
                    let maybeNewUnit = readMaybe newUnitStr :: Maybe Unit

                    when (newName /= ingNameDB ingredient) $
                        liftIO $ updateIngredientName connection (idIgDB ingredient) newName

                    case maybeNewQuantity of
                        Just q -> when (q /= quantityDB ingredient) $
                            liftIO $ updateIngredientQuantity connection (idIgDB ingredient) q
                        Nothing -> return () 

                    case maybeNewUnit of
                        Just u -> when (u /= unitDB ingredient) $
                            liftIO $ updateIngredientUnit connection (idIgDB ingredient) u
                        Nothing -> return ()

                showMainView window connection)


------------------------------------------------------------------------------------------------------------------------
------------------------------------------------- ADD RECIPE VIEW ------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------

    showAddView :: Window -> Connection -> UI ()
    showAddView window connection = do
        nDB <- UI.input # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
        pDB <- UI.input # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
        cDB <- UI.input # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
        ingBlock <- UI.div # set UI.style [("margin-top", "20px"), ("margin-bottom", "20px"), ("font-size", "16px")]
        iL <- UI.ul
        addIngBtn <- UI.button # set UI.text "Add ingredient"
        dDB <- UI.textarea # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
        saveBtn <- UI.button # set UI.text "Save" # set UI.style [("margin-top", "40px"), ("font-size", "16px"), ("align", "right")]
        message <- UI.p # set UI.style [("color", "red")]
        moreDetMsg <- UI.p

        ingredientsRef <- liftIO $ newIORef []
        ingredientInputsRef <- liftIO $ newIORef []

        addBlock <- UI.div # set UI.style [
                    ("border", "2px solid rgba(132, 98, 19, 1)")
                    , ("border-color", "rgba(132, 98, 19, 1)")
                    , ("border-radius", "10px")
                    , ("padding", "20px")]

        element addBlock #+ [column
            [ row [ UI.span #+ [UI.string "Name: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element nDB]
            , row [ UI.span #+ [UI.string "Portions: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element pDB]
            , row [ UI.span #+ [UI.string "Calories: "] # set UI.style [("margin-right", "10px"), ("font-size", "16px")]
            , element cDB]
            , element ingBlock #+ [UI.span #+ [UI.string "Ingredients: "] # set UI.style [("margin-top", "20px"), ("font-size", "16px")], element iL, element addIngBtn, element moreDetMsg]
            , UI.span #+ [UI.string "Description: "] # set UI.style [("margin-top", "20px"), ("font-size", "16px")]
            , element dDB
            , element saveBtn
            , element message]] 

        void $ getBody window #+ [element addBlock]

        -- adding inputs for ingredients --

        on UI.click addIngBtn $ \_ -> do
            n <- UI.input # set (attr "placeholder") "Ingredient name" # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            q <- UI.input # set (attr "placeholder") "Quantity" # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            u <- UI.select # set UI.style [("margin-top", "10px"), ("margin-right", "20px"), ("font-size", "16px")]
            _ <- element u #+
                map (\un -> UI.option # set UI.text (show un) # set UI.value (show un))
                [TSP, TBSP, CUP, ML, L, G, KG, PCS, CLOVE, SLICE, CAN, PINCH, HANDFUL]
            _ <- element ingBlock #+ [row [
                element n
                , element q
                , element u]]
            
            liftIO $ modifyIORef ingredientInputsRef (++ [(n, q, u)])

        -- save recipe --

        on UI.click saveBtn $ \_ -> do

            accepted <- liftIO $ newIORef True

            nI <- get value nDB
            pI <- get value pDB
            let maybePInt = readMaybe pI :: Maybe Int
            cI <- get value cDB
            let maybeCInt = readMaybe cI :: Maybe Int
            dI <- get value dDB

            -- data checking

            when (nI == "") $ do
                _ <- element message # set UI.text "You need to provide a name!"
                liftIO $ writeIORef accepted False

            when (maybePInt == Nothing) (
                when (pI /= "") ( do
                    _ <- element message # set UI.text "Incorrect data in the 'portions' field!"
                    liftIO $ writeIORef accepted False
                ))

            when (maybeCInt == Nothing) (
                when (cI /= "") ( do
                    _ <- element message # set UI.text "Incorrect data in the 'calories' field!"
                    liftIO $ writeIORef accepted False
                ))

            when (dI == "") ( do
                _ <- element message # set UI.text "You need to provide a description!"
                liftIO $ writeIORef accepted False)
            
            ingredientInputs <- liftIO $ readIORef ingredientInputsRef

            parsedIngredients <- forM ingredientInputs $ \(n, q, u) -> do
                nVal <- get value n
                qVal <- get value q
                uVal <- get value u
                let mQ = readMaybe qVal :: Maybe Double
                    mU = readMaybe uVal :: Maybe Unit
                return $ case (nVal, mQ, mU) of
                    ("", _, _)       -> Nothing
                    (_, Nothing, _)  -> Nothing
                    (_, _, Nothing)  -> Nothing
                    (_, Just q', Just u') -> Just (Ingredient nVal q' u')

            when (any isNothing parsedIngredients) $ do
                _ <- element message # set UI.text "Some ingredient data is incomplete or invalid!"
                liftIO $ writeIORef accepted False
            
            let ingList = catMaybes parsedIngredients
            
            when (null ingList) (do 
                _ <- element message # set UI.text "You need to add the ingredients!"
                liftIO $ writeIORef accepted False)

            a <- liftIO $ readIORef accepted
            when (a) ( do
                let newRecipe = Recipe nI maybePInt maybeCInt ingList dI
                liftIO $ addRecipe connection newRecipe
                showMainView window connection)

    -- display help --

    showInfo :: RecipeDatabase -> String
    showInfo rd = maybe "?" show (portionsDB rd) ++ " portions   " ++ maybe "?" show (caloriesDB rd) ++ " kcal"

    showIngredient :: IngredientDatabase -> String
    showIngredient i = (ingNameDB i) ++ "   " ++ show (quantityDB i) ++ " " ++ show (unitDB i)