# Fish interpret

https://esolangs.org/wiki/Fish

<br>

You can run it either by directly calling function 'process :: String -> IO ()' and giving it a path to fish .code file as an argument.
> process "./helloworld.fish"

Or compiling it into an executable and calling it with a path to fish .code file as a parameter.
> fish-interpret.exe "./helloworld.fish"

<br>

Debugging output can be enabled by editing function 'debugMode :: Bool' to return True.
It then prints out what instruction it is executing and the position and the direction of the Instruction pointer.

<br>

### Core code hiearchy

```haskell
process    -- called directly or from main, reads and adjusts code, creates Runtime and calls 'run'
    -> run    -- recursive, each interation corecponds to one instruction
        -> execInstr    -- executes instruction
        -> moveIP    -- moves instruction pointer
```

