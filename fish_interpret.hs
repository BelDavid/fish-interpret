import System.IO
import System.Environment
import System.Random

-------   Workaround from getChar, so program does not wait for return ('\n' or '\r') when reading single char
import Data.Char
import Foreign.C.Types
getHiddenChar = fmap (chr.fromEnum) c_getch
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
-------

debugMode :: Bool
debugMode = False -- enables debugging output

type Line = [Char]
type CodeMap = [Line]
type Code = (CodeMap, Int, Int) -- instructions in 2D array, width (#columns), height (#rows) 

type Value = Int

type Register = Maybe Value

type Stack = [Value]
type Stacks = [(Stack,Register)]

-- direction
data Dir = DirUp | DirDown | DirLeft | DirRight
    deriving Eq

instance Show Dir where
    show dir = case dir of 
        DirUp   -> "^"
        DirDown   -> "v"
        DirRight   -> ">"
        DirLeft   -> "<"

-- Instruction Pointer
data IP = IP Int Int Dir
    deriving Eq

instance Show IP where
    show (IP a b dir) = "[" ++ (show a) ++ "," ++ (show b) ++ "," ++ (show dir) ++ "]"

data Instruction = 
    NOP
    | ChangeDir Dir
    | MirrorX
    | MirrorY
    | MirrorSlash
    | MirrorBackslash
    | MirrorHash
    | ChangeDirRandom
    | Trampoline
    | TrampolineConditional
    | Jump

    | PushNum Value
    | Addition
    | Subtraction
    | Multiplication
    | Division
    | Modulo
    | Equals
    | GreaterThan
    | LessThan

    | QuoteSingle
    | QuoteDouble

    | DuplicateTopValue
    | RemoveTopValue
    | SwapTwoTopValues
    | ShiftTopThreeValues
    | ShiftStackRight
    | ShiftStackLeft
    | ReverseStack
    | PushLength
    | CreateNewStack
    | RemoveStack

    | OutputAsNumber 
    | OutputAsChar
    | ReadInput 

    | ToggleRegister
    | ReadFromCode
    | WriteToCode
    | Terminate 
    deriving (Show, Eq)

data ProcessingMode = ModeInstructions | ModeCharReadingSingle | ModeCharReadingDouble
    deriving (Show, Eq)

-- Runtime Value
data RV = 
    RVCode Code
    | RVStacks Stacks 
    | RVIP IP 
    | RVBool Bool
    | RVInt Int
    | RVIO (Runtime -> IO Runtime)
    | RVPM ProcessingMode

-- Runtime = 0: Code, 1: Stacks, 2: Instruction Pointer, 3: isRunning, 4: ignoreNextInstruction, 5: randNum(0,3), 6: ioOperation, 7: Processing mode
type Runtime = [RV]

--------------------------------------------------------------------------------

---- GETTERS AND SETTERS FOR RUNTIME ----
getFromRuntime :: Int -> Runtime -> RV
getFromRuntime i runtime = runtime !! i

setInRuntime :: RV -> Int -> Runtime -> Runtime
setInRuntime rv i runtime = take i runtime ++ [rv] ++ drop (i + 1) runtime

--  Code
getCode :: Runtime -> Code
getCode runtime = let RVCode code = getFromRuntime 0 runtime in code

setCode :: Code -> Runtime -> Runtime
setCode code = setInRuntime (RVCode code) 0

-- Stacks
getStacks :: Runtime -> Stacks
getStacks runtime = let RVStacks stacks = getFromRuntime 1 runtime in stacks

setStacks :: Stacks -> Runtime -> Runtime
setStacks stacks = setInRuntime (RVStacks stacks) 1

-- Instruction Pointer
getIP :: Runtime -> IP
getIP runtime = let RVIP ip = getFromRuntime 2 runtime in ip

setIP :: IP -> Runtime -> Runtime
setIP ip = setInRuntime (RVIP ip) 2

-- Is running
getIsRunning :: Runtime -> Bool
getIsRunning runtime = let RVBool isRunning = getFromRuntime 3 runtime in isRunning

setIsRunning :: Bool -> Runtime -> Runtime
setIsRunning isRunning = setInRuntime (RVBool isRunning) 3

-- Ignore next instruction 
getIgnoreNextInstruction :: Runtime -> Bool
getIgnoreNextInstruction runtime = let RVBool ignoreNextInstruction = getFromRuntime 4 runtime in ignoreNextInstruction 

setIgnoreNextInstruction :: Bool -> Runtime -> Runtime
setIgnoreNextInstruction ignoreNextInstruction = setInRuntime (RVBool ignoreNextInstruction) 4

-- rand(0, 3)
getRandNum0_3 :: Runtime -> Int
getRandNum0_3 runtime = let RVInt randNum0_3 = getFromRuntime 5 runtime in randNum0_3

setRandNum0_3 :: Int -> Runtime -> Runtime
setRandNum0_3 randNum0_3 = setInRuntime (RVInt randNum0_3) 5

-- IO operation
getIOoperation :: Runtime -> (Runtime -> IO Runtime)
getIOoperation runtime = let RVIO io = getFromRuntime 6 runtime in io

setIOoperation :: (Runtime -> IO Runtime) -> Runtime -> Runtime
setIOoperation io = setInRuntime (RVIO io) 6

executeIOoperation :: Runtime -> IO Runtime
executeIOoperation runtime = getIOoperation runtime $ runtime

-- Processing mode
getProcessingMode :: Runtime -> ProcessingMode
getProcessingMode runtime = let RVPM pm = getFromRuntime 7 runtime in pm

setProcessingMode :: ProcessingMode -> Runtime -> Runtime
setProcessingMode pm = setInRuntime (RVPM pm) 7

-- Code
getCodeMap :: Code -> CodeMap
getCodeMap (codemap, _, _) = codemap

setCodeMap :: CodeMap -> Code -> Code
setCodeMap codemap (_, width, height) = (codemap, width, height)

-- Stacks
getTopValue :: Stacks -> Maybe Value
getTopValue ((x:_, _):_) = Just x
getTopValue _ = Nothing

popNValues :: Int -> Stacks -> Maybe ([Value], Stacks)
popNValues n ((xx, r):ss) = 
    if length xx >= n
    then Just (take n xx, ((drop n xx, r):ss))
    else Nothing

pushValues :: [Value] -> Stacks -> Stacks
pushValues values ((xx, r):ss) = ((values ++ xx, r):ss)

--------------------------------------------------------------------------------

errorMessage :: String
errorMessage = "something smells fishy..."

debug :: String -> IO ()
debug str = 
    if debugMode 
    then (if not . null $ str then putStr "[DEBUG] " else return ()) >> putStrLn str 
    else return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            process filePath
        _ -> putStrLn "Invalid arguments, only path to Fish source code expected!"

-- Parsing input from One string into list of strings by delimiter '\n'
parseInput :: String -> [String]
parseInput [] = []
parseInput str = 
    let line = takeWhile (/= '\n') str
    in line : parseInput (drop (length line +1) str)

-- makes all lines equal length with 'extendLines' function
adjustCode :: [String] -> Code
adjustCode lines =
    let width = foldl (\ll x -> max (length x) ll) 0 lines -- longest line
    in (map (extendLines width) lines, width, length lines)

-- extends line by adding '\0' to a certain length
extendLines :: Int -> Line -> Line
extendLines l line = line ++ replicate (l - length line) '\0'

-- adds empty ['\0'] lines 
addLines :: Int -> Int -> [Line]
addLines count width = replicate count (replicate width '\0')

-- core function
process :: String -> IO ()
process filePath = do
    debug "Initializing"
    code_str <- readFile filePath

    let code = adjustCode . parseInput $ code_str
    let runtime = [RVCode code, RVStacks [([],Nothing)], RVIP (IP 0 0 DirRight), RVBool True, RVBool False, RVInt 0, RVIO return, RVPM ModeInstructions]
    debug "Running"
    run runtime

    debug "Finished"

--------------------------------------------------------------------------------

-- recursive function processing code
run :: Runtime -> IO ()
run runtime = 
    if not . getIsRunning $ runtime
    then debug "Stopped"
    else do
        let pm = getProcessingMode runtime
        if pm == ModeInstructions
        then do
            let maybe_instr = getInstruction runtime
            rN0_3 <- getStdRandom (randomR (0, 3))
            let runtime1 = setRandNum0_3 rN0_3 runtime
            case maybe_instr of
                Nothing    -> run $ moveIP runtime1
                Just instr ->
                    if not . getIgnoreNextInstruction $ runtime1
                    then do
                        if instr /= NOP then debug $ (show $ getIP runtime1) ++ " Executing " ++ (show instr) else return ()
                        let runtime2 = execInstr instr runtime1
                        runtime3 <- executeIOoperation runtime2
                        run $ moveIP . (setIOoperation (\r -> return r)) $ runtime3
                    else do
                        debug $ (show $ getIP runtime1) ++ " Ignoring " ++ (show instr)
                        run $ moveIP (setIgnoreNextInstruction False runtime1)
        else do
            let char = getCharAtPos (getCode runtime) (getIP runtime)
            if (char == '\'' && pm == ModeCharReadingSingle) || (char == '\"' && pm == ModeCharReadingDouble)
            then run $ moveIP (setProcessingMode ModeInstructions runtime)
            else run $ moveIP (setStacks (pushValues [ord char] (getStacks runtime)) runtime)

-- moves IP according to it's direction
moveIP :: Runtime -> Runtime
moveIP runtime =
    let (codemap, width, height) = getCode runtime
    in case getIP runtime of 
        (IP row column DirUp) ->
            if row == 0
            then setIP (IP (height-1) column DirUp) runtime
            else setIP (IP (row - 1) column DirUp) runtime
        (IP row column DirDown) ->
            if row+1 == height
            then setIP (IP 0 column DirDown) runtime
            else setIP (IP (row + 1) column DirDown) runtime
        (IP row column DirLeft) ->
            if column == 0
            then setIP (IP row (width-1) DirLeft) runtime
            else setIP (IP row (column - 1) DirLeft) runtime
        (IP row column DirRight) ->
            if column+1 == width
            then setIP (IP row 0 DirRight) runtime
            else setIP (IP row (column + 1) DirRight) runtime

instructionsMap :: [(Char, Instruction)]
instructionsMap = [
    ('\0', NOP),
    (' ', NOP),
    ('^', ChangeDir DirUp),
    ('v', ChangeDir DirDown),
    ('<', ChangeDir DirLeft),
    ('>', ChangeDir DirRight),
    ('|', MirrorX),
    ('_', MirrorY),
    ('/', MirrorSlash),
    ('\\', MirrorBackslash),
    ('#', MirrorHash),
    ('x', ChangeDirRandom),
    ('!', Trampoline),
    ('?', TrampolineConditional),
    ('.', Jump),

    ('0', PushNum 0),
    ('1', PushNum 1),
    ('2', PushNum 2),
    ('3', PushNum 3),
    ('4', PushNum 4),
    ('5', PushNum 5),
    ('6', PushNum 6),
    ('7', PushNum 7),
    ('8', PushNum 8),
    ('9', PushNum 9),
    ('a', PushNum 10),
    ('b', PushNum 11),
    ('c', PushNum 12),
    ('d', PushNum 13),
    ('e', PushNum 14),
    ('f', PushNum 15),

    ('+', Addition),
    ('-', Subtraction),
    ('*', Multiplication),
    (',', Division),
    ('%', Modulo),
    ('=', Equals),
    (')', GreaterThan),
    ('(', LessThan),

    ('\'', QuoteSingle),
    ('\"', QuoteDouble),

    (':', DuplicateTopValue),
    ('~', RemoveTopValue),
    ('$', SwapTwoTopValues),
    ('@', ShiftTopThreeValues),
    ('}', ShiftStackRight),
    ('{', ShiftStackLeft),
    ('r', ReverseStack),
    ('l', PushLength),
    ('[', CreateNewStack),
    (']', RemoveStack),

    ('o', OutputAsChar),
    ('n', OutputAsNumber),
    ('i', ReadInput),

    ('&', ToggleRegister),
    ('g', ReadFromCode),
    ('p', WriteToCode),
    (';', Terminate)
    ]


-- gets char under instruction pointer
getCharAtPos :: Code -> IP -> Char
getCharAtPos (codemap, width, height) (IP row column _) = 
    if row < height && column < width
    then (codemap !! row) !! column
    else '\0'
    
-- returns proper Instruction from Instruction map based on char under IP
getInstruction :: Runtime -> Maybe Instruction
getInstruction runtime = lookup (getCharAtPos (getCode runtime) (getIP runtime)) instructionsMap 
    

-- EXECUTE INSTRUCTION
execInstr :: Instruction -> Runtime -> Runtime
-- Movement and execution
execInstr NOP runtime = runtime
execInstr (ChangeDir dir) runtime = case getIP runtime of (IP x y _) -> setIP (IP x y dir) runtime
execInstr MirrorX runtime = case getIP runtime of
    (IP x y DirLeft) -> setIP (IP x y DirRight) runtime 
    (IP x y DirRight) -> setIP (IP x y DirLeft) runtime
    _ -> runtime 
execInstr MirrorY runtime = case getIP runtime of
    (IP x y DirUp) -> setIP (IP x y DirDown) runtime
    (IP x y DirDown) -> setIP (IP x y DirUp) runtime
    _ -> runtime
execInstr MirrorSlash runtime = case getIP runtime of
    (IP x y DirUp) -> setIP (IP x y DirRight) runtime
    (IP x y DirDown) -> setIP (IP x y DirLeft) runtime
    (IP x y DirLeft) -> setIP (IP x y DirDown) runtime 
    (IP x y DirRight) -> setIP (IP x y DirUp) runtime
execInstr MirrorBackslash runtime = case getIP runtime of
    (IP x y DirUp) -> setIP (IP x y DirLeft) runtime
    (IP x y DirDown) -> setIP (IP x y DirRight) runtime
    (IP x y DirLeft) -> setIP (IP x y DirUp) runtime 
    (IP x y DirRight) -> setIP (IP x y DirDown) runtime
execInstr MirrorHash runtime = case getIP runtime of 
    (IP x y DirUp) -> setIP (IP x y DirDown) runtime
    (IP x y DirDown) -> setIP (IP x y DirUp) runtime
    (IP x y DirLeft) -> setIP (IP x y DirRight) runtime 
    (IP x y DirRight) -> setIP (IP x y DirLeft) runtime
execInstr ChangeDirRandom runtime = case getIP runtime of
    (IP x y _) -> case getRandNum0_3 runtime of 
        0 -> setIP (IP x y DirUp) runtime
        1 -> setIP (IP x y DirDown) runtime
        2 -> setIP (IP x y DirLeft) runtime
        3 -> setIP (IP x y DirRight) runtime

execInstr Trampoline runtime = setIgnoreNextInstruction True runtime
execInstr TrampolineConditional runtime = popNValuesAndExec 1 (\[val] stacks -> 
    let runtime1 = (setStacks stacks runtime)
    in if val == 0
    then setIgnoreNextInstruction True runtime1
    else runtime1
    ) runtime
execInstr Jump runtime = popNValuesAndExec 2 (\[row,column] stacks ->
    let (_, width, height) = getCode runtime
    in if column < width && row < height
        then let (IP _ _ dir) = getIP runtime
            in setIP (IP row column dir) (setStacks stacks runtime)
        else printErrorAndExit runtime
    ) runtime

-- Literals and operators
execInstr (PushNum num) runtime = setStacks (pushValues [num] (getStacks runtime)) runtime
execInstr Addition runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x + y] stacks) runtime ) runtime
execInstr Subtraction runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x - y] stacks) runtime ) runtime
execInstr Multiplication runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x * y] stacks) runtime ) runtime
execInstr Division runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x `div` y] stacks) runtime ) runtime
execInstr Modulo runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x `mod` y] stacks) runtime ) runtime
execInstr Equals runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks ((if x == y then pushValues [1] else pushValues [0]) stacks) runtime ) runtime
execInstr GreaterThan runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks ((if x > y then pushValues [1] else pushValues [0]) stacks) runtime ) runtime
execInstr LessThan runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks ((if x < y then pushValues [1] else pushValues [0]) stacks) runtime ) runtime

execInstr QuoteSingle runtime = setProcessingMode ModeCharReadingSingle runtime
execInstr QuoteDouble runtime = setProcessingMode ModeCharReadingDouble runtime

-- Stack manipulation
execInstr DuplicateTopValue runtime = let stacks = getStacks runtime in case getTopValue stacks of
    Just val -> setStacks (pushValues [val] stacks) runtime
    _ -> printErrorAndExit runtime
execInstr RemoveTopValue runtime = popNValuesAndExec 1 (\[_] stacks -> setStacks stacks runtime) runtime
execInstr SwapTwoTopValues runtime = popNValuesAndExec 2 (\[y,x] stacks -> setStacks (pushValues [x,y] stacks) runtime ) runtime
execInstr ShiftTopThreeValues runtime = popNValuesAndExec 3 (\[x,y,z] stacks -> setStacks (pushValues [y,z,x] stacks) runtime ) runtime
execInstr ShiftStackRight runtime = let ((x:xx,r):ss) = getStacks runtime in setStacks ((xx ++ [x], r):ss) runtime
execInstr ShiftStackLeft runtime = let ((xx,r):ss) = getStacks runtime in setStacks ((((last xx):init xx), r):ss) runtime
execInstr ReverseStack runtime = let ((xx,r):ss) = getStacks runtime in setStacks ((reverse xx, r):ss) runtime
execInstr PushLength runtime = let ((xx,r):ss) = getStacks runtime in setStacks (pushValues [length xx] ((xx,r):ss)) runtime
execInstr CreateNewStack runtime = popNValuesAndExec 1 (\[x] stacks -> 
    popNValuesAndExec x (\xx stacks1 -> 
        setStacks ((xx, Nothing):stacks1) runtime
        ) (setStacks stacks runtime)
    ) runtime
execInstr RemoveStack runtime = case getStacks runtime of
    (s:[]) -> printErrorAndExit runtime
    ((xx, r):ss) -> setStacks (pushValues xx ss) runtime


-- Input/output
execInstr OutputAsChar runtime = outputValue (return . chr) runtime
execInstr OutputAsNumber runtime = outputValue show runtime
execInstr ReadInput runtime = setIOoperation (\r -> do ch <- getHiddenChar; return (setStacks (pushValues [ord ch] (getStacks r)) r)) runtime

-- Reflection/miscellaneous
execInstr ToggleRegister runtime = case getStacks runtime of
    ((xx, Just r):ss) -> setStacks ((r:xx, Nothing):ss) runtime
    ((x:xx, Nothing):ss) -> setStacks ((xx, Just x):ss) runtime
    _ -> printErrorAndExit runtime
execInstr ReadFromCode runtime = popNValuesAndExec 2 (\[row,column] stacks -> 
    setStacks (pushValues [ord (getCharAtPos (getCode runtime) (IP row column DirRight))] stacks) runtime
    ) runtime
execInstr WriteToCode runtime = popNValuesAndExec 3 (\[row,column,v] stacks -> 
    setCode (editCharInCode row column (chr v) (getCode runtime)) (setStacks stacks runtime)
    ) runtime
execInstr Terminate runtime = setIsRunning False runtime


----- Helper functions -----

-- pops from stack and handles possible error (poping empty stack)
popNValuesAndExec :: Int -> ([Value] -> Stacks -> Runtime) -> Runtime -> Runtime
popNValuesAndExec n f runtime = case popNValues n (getStacks runtime) of
    Just (xx, stacks) -> f xx stacks
    Nothing -> printErrorAndExit runtime

printErrorAndExit :: Runtime -> Runtime
printErrorAndExit runtime = setIOoperation (\r -> do putStr errorMessage >> debug "" >> return r) (setIsRunning False runtime)

-- pops Value from stack and outputs to a stdout
outputValue :: (Value -> String) -> Runtime -> Runtime
outputValue f runtime = popNValuesAndExec 1 (\[x] stacks -> setIOoperation (\r -> do putStr (f x) >> debug "" >> return r) (setStacks stacks runtime)) runtime

editCharInCode :: Int -> Int -> Char -> Code -> Code
editCharInCode row column ch (codemap, width, height) = 
    let (newWidth, newHeight) = (max width (column + 1), max height (row + 1)) in
    let newcodemap = (map (extendLines newWidth) codemap) ++ (addLines (newHeight - height) newWidth) in --possible extending of codemap
    (replaceAtPos row (replaceAtPos column ch (newcodemap !! row)) newcodemap, newWidth, newHeight)

-- replaces element at certain position (index) in array
replaceAtPos :: Int -> a -> [a] -> [a]
replaceAtPos 0 v (x:xx) = v : xx
replaceAtPos n v (x:xx) = x : replaceAtPos (n-1) v xx