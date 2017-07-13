---
title: Watching process outputs with Haskell
tags: Haskell
---

When using *docker-compose*, I really like how the output printed by the
running containers gets interleaved in different colors. I wanted something
like that, but for arbitrary commands.

A recent [post on Reddit] actually reminded me of that.

[post on Reddit]: https://www.reddit.com/r/haskell/comments/41za4n/streaming_a_unix_tools_output_to_haskell/

I will call the program `race`, although there are not going to be any winners.

The idea of the program is simple: it will run each process in a separate
thread. This runner thread will read the output of the program (both `stdout`
and `stderr`, I just don't care) and send this to the main thread. This way,
there will always single thread writing the actual output.

The communication between threads will happen through
`Control.Concurrent.Chan`, and we will be sending simple messages:

```haskell
data Msg = Quit                 -- ^The process finished
         | Msg Int ByteString   -- ^A single line of output
```

The number in the message will indicate which child process is sending the line
and will be used to pick a color.

The thread for consuming these messages and printing the data is not too
complex. It's a loop that keeps track of the number of running processes. In
each iteration, we read a single message from the channel and depending on
what it is, we either decrement the process counter or print the message
wrapped in ANSI color sequences.

```haskell
printer :: Chan Msg -> Int -> IO ()
printer _ 0 = return ()
printer chan num = do
    msg <- readChan chan
    case msg of
        Quit -> printer chan (num - 1)
        Msg i d -> do
            B.putStr $ colored i d
            reader chan num

colored :: Int -> ByteString -> ByteString
colored i d = let col = colors !! i
              in "\ESC[" <> col <> ";1m" <> d <> "\ESC[0m\n"
  where
    colors = cycle ["36", "35", "32", "33", "34", "31"]
```

Reading the output of a random program is a bit more involved. As input, we
will submit the channel, process number and a string with the command.

There is a very [helpful tutorial] for `Data.Conduit.Process` which contains an
example. We can adapt this to our needs.

[helpful tutorial]: https://github.com/snoyberg/conduit/blob/master/PROCESS.md

The biggest hurdle here is that *glibc* will by default line-buffer standard
output if it goes to interactive terminal, but as soon as we redirect it to a
pipe, it gets fully buffered in 4 KiB blocks.

This is definitely not what we want. There are a few ways to mitigate this:
the easiest is to use `stdbuf` executable, which modifies the buffering mode.
The problem with it is that it does not work for all cases, especially for
commands involving piping data between multiple processes.

Another attempt that I made was to use a pseudoterminal. This approach is
however quite complex, and I failed to get it running reliably.

In the end I settled for `script` command. By using the `-c` argument it can
run a complex command and it handles buffering just the way I wanted.

```haskell
runProcess :: Chan Msg -> Int -> String -> IO ()
runProcess chan' i cmd = do
    let cmd' = "script -qfc \"" <> cmd <> "\" /dev/null"
    (ClosedStream, fromProcess, fromProcessErr, cph) <-
        streamingProcess (shell cmd')

    let output h = CB.sourceHandle h $$ CB.lines =$ CL.mapM_
            (writeChan chan . Msg i)

    _ <- runConcurrently $
        Concurrently (output fromProcess) *>
        Concurrently (output fromProcessErr) *>
        Concurrently (waitForStreamingProcess cph)

    writeChan chan Quit
```

All that is left to do is to tie it all together: get command line arguments,
spawn a thread for each one and run the printer function in the main thread. We
will create the channel with one duplicate: the copy will be shared by all
worker threads.

```haskell
main :: IO ()
main = do
    args <- getArgs
    readEnd <- newChan
    writeEnd <- dupChan readEnd

    mapM_ (forkIO . uncurry (runProcess writeEnd)) (zip [0..] args)

    reader readEnd (length args)
```

The program can now be used like this:

```bash
$ race "python -m SimpleHTTPServer" "make rebuild-on-change"
```

Arguably, it is just a glorified wrapper for the `&` Bash functionality and
`wait` command. Nonetheless, I still consider it useful.

You can get [the whole project] including cabal file with list of dependencies
on GitHub.

[the whole project]: https://github.com/lubomir/race
