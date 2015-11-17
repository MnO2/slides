---
title: 'Refactoring with Free Monad'
author: Paul Meng
tags: [Haskell, Free Monad]
abstract: |
    Haskell.SG 2015
...

Making a Slackbot
--------------------

```
@iris: :help

iris: - help
      - load_metric
      ...
```

```
@iris: load_metrics

iris: loading now...
iris: 12345
```


Message Dispatch
-------------------

```haskell
dispatch :: ChannelId -> T.Text -> SlackAction ()
dispatch cid msg =
    case T.parse irisCmd "" msg of
      Left err -> sendMessage cid (T.pack "uncomprehensible command")
      Right cmd -> liftIO $ run cid cmd
```

Data type
---------

```haskell
data IrisCommand next = Help
                      | LoadMetric
                      deriving (Eq, Show)
```


Interpret in IO Monad
----------------------

```haskell
run :: ChannelId -> IrisCommand -> SlackAction ()
run cid (Help) = sendMessage cid (T.pack ":help")
run cid (LoadMetric) = do
  sendMessage cid (T.pack "loading now...")
  l <- liftIO getDailyMetric
  let resp = T.pack $ show l
  sendMessage cid resp
```


How to Test This?
-----------------

* It relies on `sendMessage`, which results to a call to Slack API.
* In Ruby, you can use Rspec to write `expect` a method call.
* It also connects to a database to fetch data, you have to setup a database and mock data.


How to Test This?
-----------------

* With Haskell it is hard to stub the message passing and its response without a runtime like Ruby.
* Setting up a testing database works as most imperaitve languages do, the only drawback is slowdown.
* It fallback to IO Monad. Is there an alternative before we use the last resort?

* How about.. test it semantically? With the structure of AST, we know how the thing would happen.


Thinking in Javascript
------------------------

* We are loading an array of `String`, and decoding them into `Function`
* It is able to be mapped over, so it's a `Functor`

```javascript
var commands_in_str = ["ReplyLoading", "LoadMetrics"];

//after parsing

replyLoading = function() {
    console.log("in replayLoading");
};

loadMetrics = function() {
    console.log("in loadMetrics");
};

// functor
var commands = [replyLoading, loadMetrics];
commands.map(function(f) { f.call(this); });

//cheating by using window
commands_in_str.map(function(fname) { window[fname].call(this); });
```



Thinking in Javascript
------------------------

* With a `Functor`, there exists an implicit `Structure` for Free
* We have an implicit `sequence` compose the evaluation order.

```javascript
// monad for free, visitor pattern
function node(type, next) {
    this.type = type;
    this.next = next ? next : null;
}

node.prototype.accept = function(visitorObj) {
    visitorObj.visit(this);

    if (this.next) this.next.accept(visitorObj);
};
```


Thinking in Javascript
------------------------

* `Functor` as Abstract Syntax Tree, and that stucture is actually a `Monad`. It's visitor pattern in OO term.

```javascript
function visitor() {
    var that = this;
    this.visit = function(tgt) {
        if (tgt.type == "replyLoading") {
             console.log("replyLoading");
        }

        if (tgt.type == "loadMetrics") {
            console.log("loadMetrics");
        }
    };

    this.walk = function(tgt) {
        tgt.accept(that);
    };
}

var head = new node("replyLoading", (new node("loadMetrics")));
(new visitor()).walk(head);
```

Data type in Haskell
--------------------

```haskell
data IrisCommand next = Help'
                      | LoadMetric'
                      | ReplyLoading' next
                      deriving (Functor, Eq, Show)

type IrisCommandM = Free IrisCommand

makeFree ''IrisCommand
```

Data type in Haskell
---------------------

* It looks a lot like a list strcuture on data type level

```haskell
data [a] = [] | a : [a]
```

```haskell
data Free f a = Pure a | Roll (f (Free f a))
```


Data type
---------

```haskell
--it needs to be a functor
instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Roll x) = Roll (fmap (fmap f) x)

--this is the same thing as (++) basically
concatFree :: Functor f => Free f (Free f a) -> Free f a
concatFree (Pure x) = x
concatFree (Roll y) = Roll (fmap concatFree y)

instance Functor f => Monad (Free f) where
  return = Pure
  x >>= f = concatFree (fmap f x)
```

Interpret in IO Monad
----------------------

```haskell
runIris :: ChannelId -> IrisCommandM () -> SlackAction ()
runIris cid = iterM run
  where
    run :: IrisCommand (SlackAction ()) -> SlackAction ()
    run (Help') = sendMessage cid (T.pack ":help")
    run (LoadMetric')= do
      l <- liftIO getDailyMetric
      let resp = T.pack $ show l
      sendMessage cid resp
    run (ReplyLoading' n) = do
      sendMessage cid (T.pack "loading now...")
      n
```

Interpret in Test
------------------

```haskell
type FakeResponse = [T.Text]

runIrisTest :: MonadState FakeResponse m => IrisCommandM () -> m ()
runIrisTest = iterM run
  where
    run :: MonadState FakeResponse m => IrisCommand (m ()) -> m ()
    run (Help') = modify (λl -> T.pack ":help" : l)
    run (LoadMetric') = modify (λl -> T.pack "123456" : l)
    run (ReplyLoading' n) = do
      modify (λl -> T.pack "loading now..." : l)
      n
```


Run with Hspec
---------------

```haskell
spec :: Spec
spec = do
    describe "dispatch" $ do•
      context "receive help" $ do
        it "return help result" $ do
          (runState (I.runIrisTest I.help') []) `shouldBe` ((), [":help"])

      context "receive load_metric" $ do
        it "return load_metric result" $ do
          (runState (I.runIrisTest (I.replyLoading' >> I.loadMetric')) []) `shouldBe` ((), ["123456", "loading now..."])
```


How to Test This?
-----------------

* Without resorting to external database
* Using local state to simulate the state and response.
* Test it semantically, on the command level, but not the side-effect comes with the commands.


Thank you
---------

