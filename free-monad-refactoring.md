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
@iris: knock knock

iris: who is it ?
```

```
@iris: :help

iris: - help
      - load_metric
      ...
```

```
@iris: load_metrics

iris: 12345
```


Message Dispatch
-------------------

```haskell
dispatch :: ChannelId -> T.Text -> SlackAction ()
dispatch cid msg =
    case T.parse irisCmd "" msg of
      Left err -> sendMessage cid (T.pack "uncomprehensible command")
      Right cmd -> liftIO $ runIris cid cmd
```

Interpret in IO Monad
----------------------

```haskell
runIris :: ChannelId -> IrisCommand -> IO ()
runIris cid = run
  where
    run :: IrisCommand -> SlackAction ()
    run (Help) = sendMessage cid (T.pack ":help")
    run (LoadMetric) = do
      sendMessage cid (T.pack "loading now...")
      let stateStore = stateSet UserState{} stateEmpty
      env0 <- liftIO $ initEnv stateStore ()
      l <- liftIO $ runHaxl env0 getDailyMMetric
      let resp = T.pack $ show l
      sendMessage cid resp
```


Data type
---------

```haskell
data IrisCommand next = Help
                      | LoadMetric
                      deriving (Functor)
```

How to Test This?
-----------------

* It relies on `sendMessage`, which results to a call to Slack API.
* In Ruby, you can use Rspec to write `expect` a method call.
* It also connects to Redshift to fetch data, you have to setup a database and mock data.



Thinking in Javascript
------------------------

* It might be easier to think in Javascript Array, to think it like a `Functor`

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
```


Thinking in Javascript
------------------------

* With a `Functor`, there exists a `Monad` for Free
* Using `>>=` to compose the evaluation order.
* `Functor` as Abstract Syntax Tree, and `Monad` defines the evaluation order

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

Data type
---------

```haskell
data IrisCommand next = Help'
                      | LoadMetric'
                      | ReplyLoading' next
                      deriving (Functor)

type IrisCommandM = Free IrisCommand

makeFree ''IrisCommand
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
      let stateStore = stateSet UserState{} stateEmpty
      env0 <- liftIO $ initEnv stateStore ()
      l <- liftIO $ runHaxl env0 getDailyMMetric
      let resp = T.pack $ show l
      sendMessage cid resp
    run (ReplyLoading' n) = do•
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
    run (Prf') = modify (λl -> T.pack "123456" : l)
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

      context "receive prf" $ do
        it "return prf result" $ do
          (runState (I.runIrisTest (I.replyLoading' >> I.prf')) []) `shouldBe` ((), ["123456", "loading now..."])
```


Thank you
---------

