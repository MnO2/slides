---
title: 'Writing Atom plugin in Haskell'
author: Paul Meng
tags: [Haskell, Neovim]
abstract: |
    Haskell.SG 2016
...

What's Atom 
--------------

* Github's Editor
* Underlying is Electron, which is based on Webkit
* Programmable Editor
* Rich interactive environment like Emacs but with Javascript


Atom API
---------

* Much easier than Neovim.
* And REPL with the Developer Tool make it easy to see live result.
* API are put under `atom` namespace
* [Editor](https://atom.io/docs/api/v1.8.0/TextEditor)


Javascript call GHCJS
---------------------

* GHCJS doesn't explicitly support exporting function.
* It requires workaround to make the two side world communicate to each other.

```
foreign import javascript unsafe "renderMarkdown_ = $1"
    js_set_renderMarkdown :: Callback a -> IO ()
```


Markdown Preview 
----------------

* Live preview of the markdown content.
* With the help of `markdown` package. Implementing markdown2html is pretty
  easy.

```
renderMarkdown :: JSVal -> IO ()
renderMarkdown x = do
  let o = Object x
  rawMarkdownStr <- getProp "markdownStr" o
  let markdownStr = textFromJSVal rawMarkdownStr
  let htmlStr  = (unpack $ renderHtml $ markdown def $ fromChunks [markdownStr])
  setStringProp (JSS.pack "ret") (JSS.pack htmlStr) o
```

Compile to JS
-------------

* Compile to Javascript with GHCJS 0.2
* The target file size is pretty large

```
stack exec ghcjs -- -DGHCJS_BROWSER -o render.js render.hs
```

```
$  ls -lh lib/render.js
-rw-r--r--  1 mno2  staff   6.0M Jun 12 13:00 lib/render.js
```


Glue Code in JS
---------------

* Use the convention option object to pass the content to compiled JS
* Retrieve the return value from modified object

```
renderMarkdown(content) {
  var option = { markdownStr: content }
  window.renderMarkdown_(option);
  var html = option.ret;

  var outPath = path.resolve(path.join(os.tmpdir(), "star-platium" + ".html"));
  fs.writeFileSync(outPath, html, {});
  this.element.src = "file://" + outPath;
}
```

Launching the plugin
--------------------

```
atim install --dev .
atom --dev .
```


Thank you
---------

* [Code is here](https://github.com/MnO2/star-platium) 


