{-
    Haskell brainfuck interpreter
    @author Patrick Kage
-}

-- Dependencies
import Data.Char
import Data.List
import System.IO
import Data.Maybe
import Control.Monad
import System.Environment

-- Type declarations
data Operation = MemInc
               | MemDec
               | PtrInc
               | PtrDec
               | Input
               | Output
               | LStart
               | LEnd
               | Nop
               | ProgramEnd
               deriving (Eq, Show)

data ExecutionState = Running
                    | Stopped
                    deriving (Eq, Show)

type Program = [Operation]
type Memory  = [Int]
type Pointer = Int
type Stack   = [Int]

-- Final interpreter type
data Interpreter = Interpreter { program :: Program
                               , execptr :: Pointer
                               , memory  :: Memory
                               , memptr  :: Pointer
                               , stack   :: Stack
                               , state   :: ExecutionState
                               } deriving (Show)

-- debuggy stuff
showOperation :: Operation -> String
showOperation MemInc = "+"
showOperation MemDec = "-"
showOperation PtrInc = ">"
showOperation PtrDec = "<"
showOperation Input  = ","
showOperation Output = "."
showOperation LStart = "["
showOperation LEnd = "]"
showOperation Nop = " "
showOperation ProgramEnd = "#"

showProgram :: Program -> String
showProgram = concatMap showOperation

-- Program operations
charToOperation :: Char -> Operation
charToOperation '+' = MemInc
charToOperation '-' = MemDec
charToOperation '>' = PtrInc
charToOperation '<' = PtrDec
charToOperation ',' = Input
charToOperation '.' = Output
charToOperation '[' = LStart
charToOperation ']' = LEnd
charToOperation _   = Nop

stringToProgram :: String -> Program
stringToProgram = filter (/= Nop) . map charToOperation

programRead :: Program -> Pointer -> Operation
programRead prog ptr = prog !! ptr

fastforward :: Program -> Pointer -> Pointer
fastforward = fastforward' (-1)

fastforward' :: Int -> Program -> Pointer -> Pointer
fastforward' cnt prog ptr
    | op == ProgramEnd       = error "unmatched loop!"
    | op == LEnd && cnt == 0 = ptr
    | op == LEnd             = fastforward' (cnt - 1) prog (ptr + 1)
    | op == LStart           = fastforward' (cnt + 1) prog (ptr + 1)
    | otherwise              = fastforward' cnt       prog (ptr + 1)
    where
        op = programRead prog ptr

-- Stack operations
stackPush :: Stack -> Pointer -> Stack
stackPush st p = p : st

stackPop :: Stack -> Stack
stackPop [] = []
stackPop x  = tail x

stackPeek :: Stack -> Pointer
stackPeek [] = error "stack bottomed out"
stackPeek x  = head x

stackCreate :: Stack
stackCreate = []



-- Memory Operations
memoryRead :: Memory -> Pointer -> Int
memoryRead mem ptr = mem !! ptr

memoryWrite :: Memory -> Pointer -> Int -> Memory
memoryWrite mem ptr val = first ++ [val] ++ tail second
    where
        (first, second) = splitAt ptr mem

memoryIncrement :: Memory -> Pointer -> Memory
memoryIncrement mem ptr = memoryWrite mem ptr $ memoryRead mem ptr + 1

memoryDecrement :: Memory -> Pointer -> Memory
memoryDecrement mem ptr = memoryWrite mem ptr $ memoryRead mem ptr - 1

memoryCreate :: Memory
memoryCreate = repeat 0



-- Interpreter Operations
interpreterCreate :: String -> Interpreter
interpreterCreate str = Interpreter {
                            program = stringToProgram str ++ [ProgramEnd],
                            memory  = memoryCreate,
                            stack   = stackCreate,
                            execptr = 0,
                            memptr  = 0,
                            state   = Running
                        }
interpreterTick :: Interpreter -> Maybe Char -> (Interpreter, Maybe Char)
interpreterTick int inp
    | next == MemInc = (int {
                                memory  = memoryIncrement (memory int) (memptr int),
                                execptr = execptr int + 1
                            },
                        Nothing)
    | next == MemDec = (int {
                                memory  = memoryDecrement (memory int) (memptr int),
                                execptr = execptr int + 1
                            },
                        Nothing)
    | next == PtrInc = (int {
                                memptr  = memptr int  + 1,
                                execptr = execptr int + 1
                            },
                        Nothing)
    | next == PtrDec = (int {
                                memptr  = memptr int  - 1,
                                execptr = execptr int + 1
                            },
                        Nothing)
    | next == Input  = (int {
                                memory  = memoryWrite (memory int) (memptr int) $ ord (fromMaybe '\0' inp),
                                execptr = execptr int + 1
                            },
                        Nothing)
    | next == Output = (int {
                                execptr = execptr int + 1
                            },
                        Just $ chr (memoryRead (memory int) (memptr int))
                       )
    | next == LStart = if 0 /= memoryRead (memory int) (memptr int) then
                       (int {
                                stack   = stackPush (stack int) $ execptr int,
                                execptr = execptr int + 1
                            },
                        Nothing) else
                       (int {
                                execptr = fastforward (program int) (execptr int) + 1
                            },
                        Nothing)
    | next == LEnd    = if 0 == memoryRead (memory int) (memptr int) then
                       (int {
                                stack   = stackPop (stack int),
                                execptr = execptr int + 1
                            },
                        Nothing) else
                       (int {
                                execptr = stackPeek (stack int) + 1
                            },
                        Nothing)
    | next == ProgramEnd = (int { state = Stopped }, Nothing)
    | otherwise = (int, Nothing)
    where
        next = programRead (program int) (execptr int)

interpreterPeek :: Interpreter -> Operation
interpreterPeek int = programRead (program int) (execptr int)

-- Initialization
main = do
    args <- getArgs
    prog <- readFile (last args)

    let int = interpreterCreate prog

    progloop int

-- program loop
progloop int = do
--    print int
    (int, out) <- if interpreterPeek int == Input then do
        c <- getChar
        return $ interpreterTick int (Just c)
    else
        return $ interpreterTick int Nothing

    when (isJust out) $ do
        putChar (fromMaybe '\0' out)
        hFlush stdout

    when (state int /= Stopped) $ progloop int
