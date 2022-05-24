# Description

The ***clash-simulation-script*** is a small package for testing (simulating) Clash circuits. The key concept in the framework is the *SimScript* monad, which provides a mechanism for writing complex scenarios. In every simulation step, we can create stimuli for the circuit based on the results of previous steps. In other words, we have testing functions that look like the Clash prelude function *simulate*, but instead of a fixed array of circuit inputs, they expect a simulation program.

In addition to the simulation results, we can also get the log (not necessarily textual) that we created in our simulation program.

This description contains a real-life example that illustrates all features of the package. **If you want to be just informed about the package, see the Collatz test in the Tests directory.**

## Requirements

When I began programming in Clash, I quickly needed a testing mechanism that satisfied the following requirements:

- writing arbitrarily complex scenarios suitable for testing protocols, timing conditions, and performances (on register transfer level, of course);
- reusability of parts of the script in other scripts;
- using the usual Haskell mechanisms for writing testing scripts; an API that will be appropriate for less experienced Clash/Haskell programmers; avoiding complex toolchains for test execution;
- testing circuits that perform computations that are difficult to predict ahead of execution time;
- producing results suitable for use in haddock-style comments.

In addition to these functional requirements, there were requirements related to the effort needed to make the system, which should have been simple (as much as possible), small, and easy for future upgrades.

## Example

Instead of a small, complete example, we have decided to illustrate your framework for testing a real system - computing the SHA256 hash in a scenario where message blocks arrive asynchronously.  Unnecessary technical details will be omitted here. I hope the source code for the whole system will be released soon.

### Context

The circuit sha256 which we tested is based on message patterns. In any cycle, the circuit can obtain a command and a possible message block. The whole programme was written for the Arduino Vidor platform, with communication between the microcontroller and FPGA chip using the JTAG protocol. Because the circuit is called from the Verilog programme and your primary goal is that the code is understandable to novices in Haskell programming, we use the cmdRdy flag as the indicator of the new command in input. Adapting sha256 for use in SimScript is trivial:

```haskell
type CInp = (Bool, Command, BitVector 512)
type COut = Result

sha256circ :: (HiddenClockResetEnable dom)
     => Signal dom CInp
     -> Signal dom COut
sha256circ inp = sha256 cmdRdy cmd ibuff
  where
    (cmdRdy, cmd, ibuff) = unbundle inp
```

Commands that *sha256* understands are: *cmdGetStatus*, *cmdInit*, *cmdNewBlock*, *cmdLastBlock*, *cmdGetResult.* In the testing module are defined constants *getStatus*, *getResult* and *initProc* of type *CInp* for commands which don’t need message block (*ibuff*) parameter. Command cmdLastBlock has one parameter (arg1 field in record) - size of the last block (in bytes).

The result (type *Result*) has *resReady* (the indicator that the result is available), resStatus (global status of the circuit), and eight values (res1, … res8). All fields in *Command* and *Results* are *Unsigned 32* type.

### Testing data

For testing, we use five messages of different sizes (8, 32, 64, 96, and 160 bytes). Messages are created by concatenation of the string “@ABCDEFG”. 

```haskell
tests :: [TestData]
tests = [
    TestData   8 (msg  1) "759d378f139ee9e71d8225ee1882b96a3df6e57af705335af1722efdf2a6e4bc"
  , TestData  32 (msg  4) "48785ba2ead33b858c01fbcd364f3af4dac5011677a961d5adf84a433e8a83d7"
  , TestData  64 (msg  8) "e4b13445eb8558208bbea3d6c98c507ff84972bc411739922f6dc5ec1178a196"
  , TestData  96 (msg 12) "e75a6d080109157721e3fe4493b86c12f9f0fc0e801b38337f1833fe0a4df778"
  , TestData 160 (msg 20) "bc2d9095fe890eb21216afe6f16f4c708a782083ee1bacaaf44c229783d87d68"
  ]
```

Control hashes are computed using an online program.

### Test Script

Your main testing script is:

```haskell
test :: SimScript CInp COut [String]
test = script $ \exec continue -> do
  -- waiting to start initalise circuit
  void $ execUntil exec
                   (\r -> resStatus r /= resStsNotStarted)
                   getStatus
  -- for every test call procMsg script
  mapM_ (procMsg exec) tests
  continue
```

The script type is SimScript with (type) parameters:

- type of circuit input (*CInp*)
- type of circuit output (*COut*)
- type of simulation log (*[String]*)

The main script is initialised by calling the *script* function. The type of this function is somewhat unusual, especially for those who have not used *Cont* monad and the function *callCC*:

```haskell
script :: forall w c r. Monoid w 
			 => ((c -> SimScript_ c r w r) -> SimScript c r w -> SimScript c r w)
			 -> SimScript c r w
```

However, as we can see from the example, the use is simple. The function needed for calling the *script* is formed as a simple lambda expression. The first argument (named *exec*) provides a command to your circuit. The second argument (named *continue*, although it could be named *abort*) is a signal to the interpreter that the script is finished, and the simulation will continue repeating the last circuit input.

The script *test* calls a utility function execUntil, which simply executes exec with the specified input until the result does not satisfy the specified condition.

The script procMsg, which we call from the main script, performs the actual testing for a message.

```haskell
procMsg :: SimExecFn CInp COut [String] 
        -> TestData 
        -> SimScript_ CInp COut [String] ()
procMsg exec TestData{..} = do
  -- initialise new computation
  void $ exec initProc
  -- get starting simulation time
  tstart <- getSimTime
  forM_ [0 .. L.length tdMessageBlocks - 1] $ \ix -> do
    -- if is the last block in the message prepare cmdLastBlock with the size of the block,
    -- else call cmdNewBlock
    let cmd = if ix < L.length tdMessageBlocks - 1
              then defaultCommand cmdNewBlock
              else (defaultCommand cmdLastBlock) {
                        arg1 = let ll = tdMessageSize `mod` 64
                               in  if ll == 0 then 64 else ll }
    -- call command using execCmd script
    void $ execCmd exec (True, cmd, tdMessageBlocks L.!! ix)
  -- wait to end of computation
  void $ execUntil exec (\r -> res1 r == stsResultReady) getStatus
  -- get result
  rv <- exec getResult
  -- get end of computation time
  tstop <- getSimTime
  -- write result in log
  tell [show (showHsRes rv == tdExpected) <>
        " [" <> showHsRes rv <> "]   Execution time (clock ticks): " <>
        show (tstop - tstart)]
```

Note the return type of the function: *procMsg :: …  -> **SimScript_** CInp COut [String] ()*. Type *SimScript_* has additional, fourth, parameter - type of the return value. Subscripts always have return value type, even if they return ().

Function *getSimTime* returns simulation time - clock tick counter staring at the start of the simulation.

Function *tell* writes messages in the log. Similarly to function tell from Writer monad writing is performed using monoid *mappend*, in our example, the argument is a list of messages. 

All that remains is to write the function for sending the command:

```haskell
execCmd :: SimExecFn CInp COut [String]
        -> CInp
        -> SimScript_ CInp COut [String] COut
execCmd exec cmd = do
  -- waiting to be processing pipeline will be ready for new command
  void $ execUntil exec (\r -> res1 r /= stsBusy) getStatus
  rv <- exec cmd
  -- tt <- getSimTime
  -- tell["exec coommand res! " <> show rv <> "@" <> show tt]
  return rv
```

Two commented lines after sending the command are for logging time when the circuit is ready to accept a new message block, which can be useful for analysing block flow in the processing pipeline. 

We still have to perform our test. 

Functions for executing scripts are described in the section “Running simulation” below. For our test, we will use *execScriptN.* Running:

```haskell
clashi> mapM_ putStrLn $ execScriptN @System 1000 sha256circ test
```

will produce:

```haskell
True [759d37...22efdf2a6e4bc]   Execution time (clock ticks): 105
True [48785b...84a433e8a83d7]   Execution time (clock ticks): 105
True [e4b134...dc5ec1178a196]   Execution time (clock ticks): 170
True [e75a6d...833fe0a4df778]   Execution time (clock ticks): 170
True [bc2d90...c229783d87d68]   Execution time (clock ticks): 235
```

At first glance, it seems strange that the execution time for the third and fourth messages is the same, although the third has one block and the fourth has two blocks. The reason is that in SHA256, an additional block is generated in particular cases.

<aside>
💡 The performance of our implementation will be analysed when we release the complete source code of the project, but for now, we can notice that processing the first block requires 105 cycles and every subsequent one 65. Since computing the SHA256 hash requires 64 steps in the main loop, this indicates that we achieved optimal one step by cycle with maximum overlapping of hash computing and preparation of the next block.

</aside>

## API

### Stability

The API is an early experimental phase.

### Types

The following types are intended for the creation of simulation scripts:

```haskell
-- Top level simulation script
type SimScript c r w = ...
-- Simulation script

type SimScript_ c r w v = ...
-- Type of simlation script exec function

type SimExecFn c r w = (c -> SimScript_ c r w r)

-- Type of simlation script continue function
type SimContinueFn c r w = SimScript_ c r w ()
```

The type parameters have the following mining:

- c - input type for simulated circuit
- r - output type for simulated circuit
- w - the type of the simulation log
- v - the type of return value of subscript

### A script

How to create a script using the *script* function is described in the example. There we will repeat the function type:

```haskell
script :: forall w c r. Monoid w 
			 => ((c -> SimScript_ c r w r) -> SimScript c r w -> SimScript c r w)
			 -> SimScript c r w
```

The script function is based on callCC from the Cont monad and, consequently, uses the identical method for defining call-current-continuation functions.

```haskell
-- | Append to simulation log
tell :: (Monoid w) => w -> SimScript_ c r w ()

-- | Returns the current simulation time (clock ticks)
getSimTime :: SimScript_ c r w Int
```

### Running simulation

There are three functions for executing the script:

```haskell
execScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> SimScript c r w -> w

evalScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> SimScript c r w
            -> [r]

runScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
           => Int
           -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
           -> SimScript c r w
           -> ([r], w)
```

The first two parameters of all three functions are identical to the parameters of the Clash prelude function *simulateN* - a number of cycles to simulate and the circuit that is tested. The third parameter is the simulation script. Function execScriptN returns the accumulated log, evalScriptN returns a list of output from the circuit (one for every cycle), *runScriptN* - pairs of output list and the simulation log.

### Utility functions

Module Pragmata.Clash.SimScript.Utils, for now, defines only one function:

```haskell
-- | Execute the command until result satisifies specified condition
--   return value is pair of result (which satisfies condiotn) and
--   simulation time when is condition satisfied
execUntil :: SimExecFn c r w            -- ^ exec function
          -> (r -> Bool)                -- ^ condition
          -> c                          -- ^ command
          -> SimScript_ c r w (r, Int)
```

## Implementation

Your implementation follows the example of coroutine implementation using *ContT* monad and *callCC* function from:

[https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)