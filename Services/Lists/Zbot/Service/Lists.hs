module Zbot.Service.Lists (
    Lists
,   lists
)   where

import Zbot.Core.Bot
import Zbot.Core.Service
import Zbot.Extras.Command

import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Char (isNumber)
import Safe (readMay)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as M
import qualified Data.Text as T


newtype Lists = MkLists (M.Map T.Text [(T.Text, Bool)])

-- | The lists service allows users to keep lists of things. It can be queried
-- with the !list command.
lists :: Bot m => Service m Lists
lists = Service {
        initial     = MkLists M.empty
    ,   serialize   = serializeLists
    ,   deserialize = deserializeLists
    ,   name        = "Zbot.Service.Lists"
    ,   process     = onCommand "!list" listsCommand
    }

serializeLists :: Lists -> Maybe BS.ByteString
serializeLists (MkLists m) = Just $ BS.fromString $ show m

deserializeLists :: BS.ByteString -> Maybe Lists
deserializeLists bs = MkLists <$> readMay (BS.toString bs)

listsCommand :: Bot m => Reply m -> T.Text -> MonadService Lists m ()
listsCommand reply args = listsAction (T.words args)
    where
        listsAction ["show"]                = showLists
        listsAction ["show", list]          = showList list
        listsAction ["add", list]           = addList list
        listsAction ("add":"at":i:list:xs)  = addElemAt i list xs
        listsAction ("add":list:xs)         = addElem list xs
        listsAction ["rm", list]            = rmList list
        listsAction ("rm":list:xs)          = rmElem list xs
        listsAction ("check":list:e:xs)     = checkOffElem list (e:xs)
        listsAction ["flush", list]         = flushList list
        listsAction _                       = help

        -- The usage message, in case no arguments are passed.
        help = mapM_ (lift . reply) [
                  "!list show [list]"
              ,   "!list add [at index] list [element]"
              ,   "!list rm list [element]"
              ,   "!list check list element"
              ,   "!list flush list"
              ]

        -- Displays the list of lists.
        showLists = do
            (MkLists listMap) <- get
            let speakKeys = lift . reply <$> M.keys listMap
            case speakKeys of
                [] -> lift $ reply "Such missing! Much not lists! Wow."
                _  -> sequence_ speakKeys

        -- Displays the elements in a given list l.
        -- If l does not exist, exits cleanly.
        showList l = do
            (MkLists listMap) <- get
            let result = M.lookup l listMap
            lift $ case result of
                Nothing -> reply $ T.concat ["There is no list '", l, "'."]
                Just xs -> case xs of
                    []  ->  reply "Much empty. Such void. Wow!"
                    _   ->  mapM_ (reply . (\(i, (n, b)) -> T.concat [
                                    showText i, ") "
                                ,   if b then st n else n
                                ]))
                        $   zip [(1 :: Int)..] xs
            where
                st = ("\204\182" `T.append`)

        -- Makes a new empty list l.
        -- Adds it to the list of lists.
        addList l = do
            (MkLists listMap) <- get
            let result = M.member l listMap
            if result
                then lift $ reply
                          $  T.concat ["There is already a list '", l, "'."]
                else do
                    put (MkLists $ M.insert l [] listMap)
                    lift $ reply
                         $ T.concat ["'", l, "' added to the list of lists."]

        -- Appends an existing list l with a new string xs.
        -- If l does not exist, exits cleanly.
        -- If xs aleady exists within l, exits cleanly.
        addElem l xs = do
            (MkLists listMap) <- get
            let result = M.lookup l listMap
            let x = T.unwords xs
            case result of
                Nothing -> lift $ reply
                                $ T.concat ["There is no list '", l, "'."]
                Just ls ->  do
                    let result2 = lookup x ls
                    case result2 of
                        Nothing -> do
                            put $ MkLists $ M.update
                                    (\lx -> return $ lx ++ [(x, False)])
                                    l
                                    listMap
                            lift $ reply
                                 $ T.concat ["'", x, "' added to '", l ,"'." ]
                        Just _  -> lift $ reply $ T.concat [
                                        "'", x, "' already exists within '"
                                    ,   l, "'."
                                    ]

        -- Inserts a new string xs into list l at index i.
        -- If l does not exist, exits cleanly.
        -- If xs already exists within l, exits cleanly.
        -- If i can not be parsed as an number, exits cleanly.
        addElemAt i l xs = do
            (MkLists listMap) <- get
            let result = M.lookup l listMap
            let x = T.unwords xs
            case result of
                Nothing -> lift $ reply
                                $ T.concat ["There is no list '", l, "'."]
                Just ls -> do
                    let b = T.all isNumber i
                    if b then lift $ reply $ T.concat [
                                      "There is no index '", i
                                  ,   "' into list '", l, "'."
                                  ]
                         else do
                            let result2 = lookup x ls
                            case result2 of
                                Nothing -> do
                                    let i' = readText i - 1
                                    let f y z = y ++ [(x,False)] ++ z
                                    put $ MkLists
                                        $ M.update
                                            (return . uncurry f . splitAt i')
                                            l
                                            listMap
                                    lift $ reply
                                         $ T.concat [
                                                "'", x, "' added to '", l
                                            ,   "' at index '", i, "'."
                                            ]
                                Just _  -> lift $ reply
                                                $ T.concat [
                                                        "'", x
                                                    ,   "' already exists "
                                                    ,   "within '", l, "'."
                                                    ]

        -- Removes a list l from the list of lists.
        -- If l does not exist, exits cleanly.
        rmList l = do
            (MkLists listMap) <- get
            let result = M.member l listMap
            lift $ reply
                 $ if result
                    then T.concat ["Deleting list '", l, "'."]
                    else T.concat ["There is no list '", l, "'."]
            put $ MkLists $ M.delete l listMap

        -- Removes an element xs from an existing list l.
        -- If either l or xs don't exist, exits cleanly.
        -- xs can be either the string stored, or its index.
        rmElem l xs = do
            (MkLists listMap) <- get
            let result = M.lookup l listMap
            let x = T.unwords xs
            case result of
                Nothing -> lift $ reply
                                $ T.concat ["There is no list '", l, "'."]
                Just ls -> do
                    --Safe due to short circuit.
                    let result1 =   lookup x ls
                    let result2 =   (,) result1
                                $   T.all isNumber x
                                &&  readText x
                                <=  length ls
                    case result2 of
                        (Nothing,False) -> lift $ reply
                                                $ T.concat [
                                                        "'", x
                                                    ,   "' does not exist "
                                                    ,   "within list '", l, "'."
                                                    ]
                        _           ->  do
                            put $ MkLists $ remove x l listMap
                            lift $ reply
                                 $ T.concat [
                                        "'", x, "' removed from '",  l, "'."
                                    ]
            where
                -- Helper function for removing element, regardless of
                -- whether or not 'rmElem' is passed the string to be
                -- deleted or its current index.
                remove  :: T.Text
                        -> T.Text
                        -> M.Map T.Text [(T.Text, Bool)]
                        -> M.Map T.Text [(T.Text, Bool)]
                remove x key dic
                    | T.all isNumber x =
                        let i = readText x
                        in  M.update
                                (\lx -> return $ take (i-1) lx ++ drop i lx)
                                key
                                dic
                    | otherwise = M.update
                                    (return . filter (\(n, _) -> n /= x))
                                    key
                                    dic

        -- Checks an element l off from an existing list l.
        -- If either l or xs don't exist, exits cleanly.
        -- xs can be either the string stored, or its index.
        -- Will un-checkoff an already checked off element.
        checkOffElem l xs = do
            (MkLists listMap) <- get
            let result = if null xs then Nothing else M.lookup l listMap
            let x = T.unwords xs
            case result of
                Nothing -> lift $ reply
                                $ T.concat ["There is no list '", l, "'."]
                Just ls -> do
                    --Safe due to short circuit.
                    let result1 =   lookup x ls
                    let result2 =   (,) result1
                                $   T.all isNumber x
                                &&  readText x
                                <=  length ls
                    case result2 of
                        (Nothing, False) -> lift $ reply
                                                 $ T.concat [
                                                        "'", x, "' does not "
                                                    ,   "exist within list '", l
                                                    ,   "'."
                                                    ]
                        _                ->  do
                            put $ MkLists $ check x l listMap
                            lift $ reply
                                 $ T.concat [
                                        "'", x, "' checked off from list '", l
                                    , "'."
                                    ]
            where
                -- Flips the checked flag.
                flip' (str, b) = (str, not b)
                -- Helper function for flipping checked bit.
                -- Accepts both the name of elements to check off,
                -- or their indexes in a given list.
                check :: T.Text
                      -> T.Text
                      -> M.Map T.Text [(T.Text, Bool)]
                      -> M.Map T.Text [(T.Text, Bool)]
                check x key dic
                    | T.all isNumber x = do
                        let i = readText x
                        let start = take (i - 1)
                        let end' = drop (i-1)
                        let mid = flip' . head . end'
                        let end = tail . end'
                        let f ls = return $ start ls ++ [mid ls] ++ end ls
                        M.update f key dic
                    | otherwise        = do
                        let f' t@(n,_) = if n == x then flip' t else t
                        let f = return . map f'
                        M.update f key dic

        -- Flushes some list, l. Flushing removes
        -- all checked off elements. Exits cleanly,
        -- if no list l exists.
        flushList l = do
            (MkLists listMap) <- get
            let result = M.lookup l listMap
            case result of
                Nothing -> lift $ reply
                                $ T.concat ["There is no list '", l, "'."]
                Just _  -> do
                    let f = filter (\(_,b) -> not b)
                    put $ MkLists $ M.map f listMap
                    lift $ reply $ T.concat ["Flushing '", l, "'."]

readText :: Read a => T.Text -> a
readText = read . T.unpack

showText :: Show a => a -> T.Text
showText = T.pack . show
