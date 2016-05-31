---
title: 'Writing Neovim plugin in Haskell'
author: Paul Meng
tags: [Haskell, Neovim]
abstract: |
    Haskell.SG 2016
...

What's neovim
--------------

* A fork of vim and pioneered the supports for the long-wanted features. (then
  we have vim 8.0)
* Features include async io, RPC architecture, job-control, integrated term.
* Modernize the C code for vim. Vim has been notorious for legacy and bad code.
* Unlike the contribution to vim, the initiator of neovim makes the contribution
  env much more friendly


RPC architecture
----------------

* The main functions of the features run in another process.
* Using Msgpack binary protocol to communicate with the plugin process.
* Abstract the communication with "channel"
* That means you can write the plugin in any programming language you prefer
* VimL is only for minimal glue. 


Neovim API
----------

* The official promises to maintain the python library binding while the active development
* However, you can use `nvim --api-info` to dump the api info in msgpack format

```bash
nvim --api-info | python -c 'import msgpack, sys, yaml; print yaml.dump(msgpack.unpackb(sys.stdin.read()))'
```

```
error_types:
  Exception: {id: 0}
  Validation: {id: 1}
functions:
- async: false
  can_fail: true
  name: buffer_line_count
  parameters:
  - [Buffer, buffer]
  return_type: Integer
```


Haskell Wrapper
---------------

* [nvim-hs](https://hackage.haskell.org/package/nvim-hs)
* [nvim-hs-contrib](https://hackage.haskell.org/package/nvim-hs-contrib)


REPL in ghci
------------

* Start the nvim by specifying `NVIM_LISTEN_ADDRESS`

```bash
NVIM_LISTEN_ADDRESS=/tmp/nvim nvim
```

* Then start ghci by setting the env var

```bash
NVIM_LISTEN_ADDRESS=/tmp/nvim stack ghci
```

```haskell
λ Right (tids, cfg) <- develMain Nothing

λ runNeovim' cfg $ vim_call_function "getqflist" []
 Right (Right (ObjectArray []))
```


Launching the plugin
--------------------

* Make a shell script and put the `stack exec` in it
```
PATH=`stack path --bin-path` stack exec nvim-hs-emoji-exe -- "$@"
```

* Let neovim start the process so that the channel is established
* Neovim provides `rpcstart` function call
* `remote#host#Register` and `remote#host#Reqreui` are VimL help function


Launching the plugin
--------------------

```
if has('nvim') " This way you can also put it in your vim config file
  call remote#host#Register('fibonacci', "*", rpcstart('/Users/mno2/Develop/haskell/nvim-fib/nvim-hs-devel.sh'))
  let haskellChannel = remote#host#Require('fibonacci')

  if haskellChannel < 1
    echom 'Failure to initialize the haskell channel for remote procedure calls'
    cq!
  endif
endif
```

Fibonacci Function
------------------

* Get the number `N` under the cursor, calculate the `N`th Fibonacci number.
* Using `Neovim` monad, which is `data Neovim r st a`

```haskell
module Fibonacci.Plugin (fibonacci) where

import Neovim

fibonacci :: Neovim r st String
fibonacci = do
  cw <- errOnInvalidResult $ vim_call_function "expand" $ [toObject "<cword>"]
  let n = read cw
  return $ show (fibs !! n)
  where
    fibs :: [Integer]
    fibs = 0:1:scanl1 (+) fibs
```

Autocomplete Emoji
------------------

* Vim has a built-in `omnifunc` mechanism for autocompletion
* Type some chars and press `Ctrl-X Ctrl-O`
* You implemented a function with the following spec

1. Two params: `findstart` and `base`
2. It would be first call with `findstart` as 1 and `base` as empty
3. Then it would be call with `findstart` as 0 and `base` as the prefix


Autocomplete Emoji
------------------

```haskell
emojicomplete :: Bool -> String -> Neovim r st (Either Int [String])
emojicomplete findstart base = do
  if findstart
     then do
       curr_line :: String <- errOnInvalidResult $ vim_call_function "getline" $ [toObject "."]
       curr_col :: Int <- errOnInvalidResult $ vim_call_function "col" $ [toObject "."]
       let prefix = take (curr_col-1) curr_line
       let reverse_idx = fromMaybe 0 (L.findIndex (== ' ') (reverse prefix))
       return $ Left (curr_col - reverse_idx - 1)
     else do
       let emoji = [":thumbsup:", ":thumbsdown:", ":smile:", ":banana:"]
       let ans = filter (L.isPrefixOf base) emoji
       return $ Right ans
```


Thank you
---------

* [Code is here](https://github.com/MnO2/nvim-hs-emoji)


