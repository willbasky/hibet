# Устройство hibet

`hibet` — переводчик с тибетского на английский. Вход — тибетский Unicode или
Wylie, выход — статьи из словарей. Конверсию письма он не делает: ходит в
`hewts`.

## Слои

```
hibet-cli/app/Main.hs
  → App.app          разбор команды, Ki-scope, фоновая загрузка Env
      Cli            команды, запуск
      Shell          диалог, getAnswer, вывод
      Pretty         терминальный вывод
  ─────────────────────────────────────────────────────────
hibet (движок)
      Translator     чистый getAnswer
      Dictionary     формат словаря, поиск
      Env            загрузка словарей и меток, MVar Env
      Label, Type, Utility
```

## Загрузка и фон

`effectful` снят (T24). Загрузка словарей — обычный `IO`:

- `makeEnv :: IO (Either HibetError Env)` — все словари;
- `makeEnvFor :: [Int] -> IO (Either HibetError Env)` — выбранные, data-root
  через resolver;
- `makeEnvForDataDir :: Maybe FilePath -> [Int] -> IO (Either HibetError Env)` —
  то же с явным `--data-dir`;
- `makeEnvFrom :: FilePath -> [Int] -> IO (Either HibetError Env)` — данные из
  произвольного каталога (тесты, сервер).

`resolveDataDir :: Maybe FilePath -> IO (Either HibetError FilePath)` ищет
корень в порядке `--data-dir` → `HIBET_DATA_DIR` → `$XDG_DATA_HOME/hibet/data`.
Кандидат годится, только если в нём есть `titles.toml` и `dicts/`; ошибка
перечисляет проверенные пути. `getDataFileName` больше не используется:
данные не входят в пакет.

Внутри — `BS.readFile`, `listDirectory`, `try`; ошибки IO заворачиваются в
`EffectError`. `App` запускает загрузку в фоне через `Ki.scoped` + `Ki.fork`;
поток-загрузчик кладёт `Env` в `MVar`, `readEnv` читает его. `scoped` гасит или
дожидается поток при выходе. Команды без словарей (`om`, `data status`)
не загружают `Env` вовсе.

Консольный вывод (`putColorDoc`, `pprint`) — обычный `IO` в `Pretty`.

## Загрузка

`makeEnvForDataDir`, `makeEnvFor` или `makeEnvFrom`:

1. resolver находит корень; `makeEnvFrom` получает его аргументом.
2. `getLabels` из `titles.toml` (tomland).
3. `key-aliases.tsv` — пары «легаси-ключ → канонический Wylie» (см.
   «Ключи и Unicode»).
4. `dicts/` — 59 файлов, около 103 МБ.
5. `makeDictionary`: строка `ключ|перевод`, дубликаты склеиваются.
6. `addAliases`: канонический ключ кладётся рядом с легаси с теми же статьями.
7. `parMap rdeepseq` по файлам.
8. `Env { dictionaryMeta :: [DictionaryMeta], labels :: Labels }`.

`readEnv :: MVar Env -> IO Env` — `readMVar`; `modifyEnv` и `putEnvMVar` —
запись.

## Поиск

`getAnswer query env`:

1. `detectScript`: есть символ `U+0F00–U+0FFF` → `Tibet`, иначе `Wylie`.
2. `boilQuery`:
   - `Wylie` → `hewts.convertWylieToUnicode defaultSettings`; ключ — `T.words`.
   - `Tibet` → `hewts.splitSentences` + `hewts.renderItems OutWylie` по
     `SyllableItem`; ключ — `T.words`.
3. `searchTranslation` — `HMS.lookup` по каждому словарю, последовательно:
   59 lookup дешевле sparks на 59 задач (замер T9).
4. `sortOutput` по `dictNumber`.

Ключ словаря — Wylie. Пунктуация и пробелы запроса в ключ не попадают.

## Ключи и Unicode

Ключи словарей хранят Wylie в написании источника, а юникод-запрос после hewts
даёт канонический Wylie. Для 6 100 из 348 433 ключей написания не совпадают
(`bha ga` против `b+ha ga`, `gnad` против `gand`), и точный поиск промахивался.

Нормализация вынесена из загрузки в данные: `data/key-aliases.tsv` хранит пары
«легаси-ключ ⇥ канонический ключ», а `addAliases` при загрузке кладёт
канонический ключ рядом с легаси с теми же статьями. Оба написания остаются
рабочими, вывод hewts не меняется; юникод-запрос находит и `bha ga`, и `gnad`.

Почему не канонизировать ключи при загрузке: 348 433 ключа через hewts — это
140 с CPU (≈0,4 мс на ключ), а полная загрузка `Env` была 0,28 с. Таблица
стоит одного чтения 259 КБ и ~348 тыс. lookups; запрос не дорожает.

Таблица производная: после обновления словарей её надо перегенерировать
(`just gen-key-aliases`, около 2,5 мин). `hibet-env`
проверяет каждую строку: ключ должен совпадать с тем, что даёт юникод-путь, а
оба написания — читаться в те же слоги. 26 ключей, у которых hewts меняет
слово, в таблицу не попадают.

## CLI

- `hibet` — интерактивный shell.
- `hibet --data-dir DIR …` — явный корень данных для любой команды.
- `hibet data status` — разрешённый data-root и версия из `manifest.toml`.
  План T25: `hibet data install --from DIR` и `hibet data update` с проверкой
  манифеста; данные остаются в основной репе, отдельной репы нет.
- `hibet shell -s ID…` — shell с выбранными словарями.
- `hibet show names` — список словарей.
- `hibet show meta [-d ID]` — описания словарей.
- `hibet om` — картинка (данные не нужны).

`:q` — выход, `:h` — история. Словари грузятся в фоне (`Ki.scoped` +
`Ki.fork`), поэтому shell открывается до окончания загрузки.

## Граница с hewts

Единственный модуль библиотеки с `import Convert` — `hibet/src/Translator.hs`.
Используются `convertWylieToUnicode`, `splitSentences`, `renderItems OutWylie`,
`defaultSettings`, `OutputFormat`, `SpellItem`. Своей конверсии в `hibet` нет.
Генератор ключей `hibet-update-dicts --gen-key-aliases` (пакет `hibet-tools`)
тоже ходит в hewts, но это offline-инструмент, а не часть приложения.

## Пакеты

`hibet` — движок (`Dictionary`, `Env`, `Translator`, `Type`, `Utility`,
`Label`); `hibet-cli` — консоль (`Cli`, `App`, `Shell`, `Pretty`, Haskeline,
optparse, exe `hibet`). Движок не зависит от консольных пакетов, поэтому
серверный API (`hibet-web`) сможет зависеть от `hibet` без Haskeline.

## Тесты

- `hibet-env` — загрузка словарей (`makeEnvFrom` на `test/env/data`), resolver,
  `getAnswer` на Wylie и Unicode, матрица Unicode-запросов (пунктуация,
  пробелы, смешанный текст, ошибки), инвариант таблицы `data/key-aliases.tsv`.
  Тесты с реальными словарями берут `../data` или `HIBET_DATA_DIR`; в распакованном
  `sdist` данных нет, и они помечаются pending, а не падают.
- `hibet-labels` — метки словарей.
- `hibet-pretty` — вывод (живёт в пакете `hibet-cli`).

## Данные

`data/dicts/*.txt` — 59 файлов, около 103 МБ; `data/titles.toml` —
метаданные; `data/key-aliases.tsv` — производная таблица ключей (259 КБ);
`data/manifest.toml` — версия набора и sha256 каждого файла; его генерирует и
сверяет с диском `hibet-tools` (`just gen-manifest`, `--check`). Это не
`data-files` пакета: `cabal sdist` содержит только код и fixture, а бинарь
ищет данные через `--data-dir`, `HIBET_DATA_DIR` или XDG. В браузер они не
грузятся: веб-переводчик может быть только серверным.
