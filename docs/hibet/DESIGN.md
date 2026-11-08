# Устройство hibet

`hibet` — переводчик с тибетского на английский. Вход — тибетский Unicode или
Wylie, выход — статьи из словарей. Конверсию письма он не делает: ходит в
`hewts`.

## Слои

```
app/Main.hs
  → App.app          разбор команды, Ki-scope, фоновая загрузка Env
      Cli            команды, запуск
      Shell          диалог, getAnswer, вывод
      Translator     чистый getAnswer
      Dictionary     формат словаря, поиск
      Env            загрузка словарей и меток, MVar Env
      Pretty, Label, Type, Utility
```

## Загрузка и фон

`effectful` снят (T24). Загрузка словарей — обычный `IO`:

- `makeEnv :: IO (Either HibetError Env)` — все словари;
- `makeEnvFor :: [Int] -> IO (Either HibetError Env)` — выбранные;
- `makeEnvFrom :: FilePath -> [Int] -> IO (Either HibetError Env)` — данные из
  произвольного каталога (тесты, сервер).

Внутри — `getDataFileName`, `BS.readFile`, `listDirectory`, `try`; ошибки
IO заворачиваются в `EffectError`. `App` запускает загрузку в фоне через
`Ki.scoped` + `Ki.fork`; поток-загрузчик кладёт `Env` в `MVar`, `readEnv`
читает его. `scoped` гасит или дожидается поток при выходе.

Консольный вывод (`putColorDoc`, `pprint`) — обычный `IO` в `Pretty`.

## Загрузка

`makeEnvFor` или `makeEnvFrom`:

1. `getLabels` из `stuff/titles.toml` (tomland).
2. `stuff/key-aliases.tsv` — пары «легаси-ключ → канонический Wylie» (см.
   «Ключи и Unicode»).
3. `dicts/` — 59 файлов, около 103 МБ.
4. `makeDictionary`: строка `ключ|перевод`, дубликаты склеиваются.
5. `addAliases`: канонический ключ кладётся рядом с легаси с теми же статьями.
6. `parMap rdeepseq` по файлам.
7. `Env { dictionaryMeta :: [DictionaryMeta], labels :: Labels }`.

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

Нормализация вынесена из загрузки в данные: `stuff/key-aliases.tsv` хранит пары
«легаси-ключ ⇥ канонический ключ», а `addAliases` при загрузке кладёт
канонический ключ рядом с легаси с теми же статьями. Оба написания остаются
рабочими, вывод hewts не меняется; юникод-запрос находит и `bha ga`, и `gnad`.

Почему не канонизировать ключи при загрузке: 348 433 ключа через hewts — это
140 с CPU (≈0,4 мс на ключ), а полная загрузка `Env` была 0,28 с. Таблица
стоит одного чтения 259 КБ и ~348 тыс. lookups; запрос не дорожает.

Таблица производная: после обновления словарей её надо перегенерировать
(`cabal exec -- runghc scripts/gen-key-aliases.hs`, около 2,5 мин). `hibet-env`
проверяет каждую строку: ключ должен совпадать с тем, что даёт юникод-путь, а
оба написания — читаться в те же слоги. 26 ключей, у которых hewts меняет
слово, в таблицу не попадают.

## CLI

- `hibet` — интерактивный shell.
- `hibet shell -s ID…` — shell с выбранными словарями.
- `hibet show names` — список словарей.
- `hibet show meta [-d ID]` — описания словарей.
- `hibet om` — картинка.

`:q` — выход, `:h` — история. Словари грузятся в фоне (`Ki.scoped` +
`Ki.fork`), поэтому shell открывается до окончания загрузки.

## Граница с hewts

Единственный модуль библиотеки с `import Convert` — `hibet/src/Translator.hs`.
Используются `convertWylieToUnicode`, `splitSentences`, `renderItems OutWylie`,
`defaultSettings`, `OutputFormat`, `SpellItem`. Своей конверсии в `hibet` нет.
Генератор ключей `scripts/gen-key-aliases.hs` тоже ходит в hewts, но это
offline-инструмент, а не часть приложения.

## Что меняется (T2)

`effectful` уже снят (T24): `Env` — значение, загрузка — `IO`, фон — `ki`.
T2 остаётся: `hibet` — движок (`Dictionary`, `Env`, `Translator`, `Type`,
`Utility`, `Label`), `hibet-cli` — консоль (`Cli`, `App`, `Shell`, `Pretty`,
Haskeline, optparse). Подробности — `../TASKS.md`, T2.

## Тесты

- `hibet-env` — загрузка словарей (`makeEnvFor`, `makeEnvFrom` на
  `test/env/data`), `getAnswer` на Wylie и Unicode, матрица Unicode-запросов
  (пунктуация, пробелы, смешанный текст, ошибки), инвариант таблицы
  `stuff/key-aliases.tsv`.
- `hibet-labels` — метки словарей.
- `hibet-pretty` — вывод.

## Данные

`dicts/*.txt` — 59 файлов, около 103 МБ; `stuff/titles.toml` — метаданные;
`stuff/key-aliases.tsv` — производная таблица ключей (259 КБ).
В браузер они не грузятся: веб-переводчик может быть только серверным.
