При запуске с использованием Cabal перенесите папку [TagGame](./TagGame/) в корень папки [src](./../),
а код [Main.hs](./Main.hs) перенести в [Main.hs](./../Main.hs)

И добавить в [src.cabal](./../../src.cabal) следующее

```
other-modules:
        TagGame.Board,
        TagGame.Move,
        TagGame.Statistic,
        TagGame.Vector
```

под `executable src`