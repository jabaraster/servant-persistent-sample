# servant-sample

HaskellでRESTサーバを作るためのライブラリ[Servant](https://haskell-servant.github.io)のDBアクセスフレームワークとして[Database.Persist](https://hackage.haskell.org/package/persistent/docs/Database-Persist.html)を使う方法を調査。

# 要件
* PostgreSQLを使う
* ConnectionPoolを使う
* SQLログが出力されること

# 参考

### [Servant + monad-logger でログを吐いてみる](https://qiita.com/lotz/items/c357a41d4432942d8054)

### [Haskell Servant 入門 (Database)](https://qiita.com/algas/items/5a6e570028d95b5dac1b)

### [haskell-servant/example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent)

persistのバージョンが古いのでシグニチャを変える必要はあるが、参考になった。


### [Using a custom monad](http://haskell-servant.readthedocs.io/en/stable/cookbook/using-custom-monad/UsingCustomMonad.html)

Servant本家のカスタムモナドの解説. Servantのバージョンには注意.

