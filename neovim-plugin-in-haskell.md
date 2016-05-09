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

* `nvim --api-info` would dump the api info in msgpack format

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

* [http://hackage.haskell.org/package/nvim-hs](nvim-hs)
* [https://hackage.haskell.org/package/nvim-hs-contrib](nvim-hs)


Launching the plugin
--------------------

```
PATH=`stack path --bin-path` stack exec nvim-fib-exe -- "$@"
```


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


Plugin Functions
----------------

```
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

Complete function for Emoji
---------------------------


Thank you
---------

