import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.Word
import Text.ParserCombinators.Parsec ( Parser, parse, many, oneOf
                                     , noneOf, between, char, (<|>))

-- zipper
data ListZipper a = ListZipper { getLeft  :: [a]
                               , getValue :: a
                               , getRight :: [a]
                               } deriving Show
modifyValue :: (a -> a) -> ListZipper a -> ListZipper a
modifyValue f (ListZipper ls x rs) = ListZipper ls (f x) rs

forward :: ListZipper a -> ListZipper a
forward (ListZipper ls x (r:rs)) = ListZipper (x:ls) r rs

backward :: ListZipper a -> ListZipper a
backward (ListZipper (l:ls) x rs) = ListZipper ls l (x:rs)

-- brainfuck
data BFIns = Next | Prev | Inc | Dec | Read | Write | Loop [BFIns]
             deriving Show
type BFCell = Word8
type BFState = ListZipper BFCell
type Brainfuck = StateT BFState IO

emptyState :: BFState
emptyState = ListZipper zeroes 0 zeroes
    where zeroes = repeat 0

eval :: BFIns -> Brainfuck ()
eval Next = modify forward
eval Prev = modify backward
eval Inc = modify $ modifyValue (+1)
eval Dec = modify . modifyValue $ subtract 1
eval Write = gets getValue >>= liftIO . putStr . return . chr . fromEnum
eval Read = liftIO getChar >>= modify . modifyValue . const . toEnum . ord
eval loop@(Loop inside) = gets getValue >>= executeLoop
    where executeLoop val | val == 0 = return ()
                          | otherwise = mapM_ eval $ inside ++ [loop]

runBF :: [BFIns] -> IO BFState
runBF = flip execStateT emptyState . mapM_ eval

-- parser
comment :: Parser (Maybe BFIns)
comment = noneOf "]" >> return Nothing

simpleIns :: Parser (Maybe BFIns)
simpleIns = oneOf "<>+-.," >>= \ins -> return . Just $ case ins of
        '<' -> Next
        '>' -> Prev
        '+' -> Inc
        '-' -> Dec
        '.' -> Write
        ',' -> Read

loop :: Parser (Maybe BFIns)
loop = between (char '[') (char ']') (program >>= return . Just . Loop)

program :: Parser [BFIns]
program = liftM catMaybes $ many $ simpleIns <|> loop <|> comment

-- utility
parseAndRunBF :: String -> IO ()
parseAndRunBF str = do let parseRes = parse program "bf" str
                       case parseRes of
                          Left err -> putStrLn ("Syntax error: " ++ show err)
                          Right ins -> runBF ins >> putStrLn ""
