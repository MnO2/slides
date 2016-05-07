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


Tab complete Emoji
-------------------

```haskell
```

Autocmd to compress  
--------------------

```haskell
```


Thank you
---------

