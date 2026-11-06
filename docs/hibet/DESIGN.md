# Устройство hibet

`hibet` — переводчик с тибетского на английский. Вход — тибетский Unicode или
Wylie, выход — статьи из словарей. Конверсию письма он не делает: ходит в
`hewts`.

## Слои

```
app/Main.hs
  → App.app
      Cli          разбор команды, запуск
      Translator   диалог, getAnswer
      Dictionary   формат словаря, поиск
      Env          загрузка словарей и меток
      Effects.*    IO-обвязка: Console, File, PrettyPrint
      Pretty, Label, Type, Utility
```

## Эффекты

`App.HibetEffects`:

```
Concurrent, Resource, Console, FileSystem, PrettyPrint,
Error HibetError, Log, IOE
```

`interpretHibet` снимает их по порядку: `runConcurrent` → `runResource` →
`runConsole` → `runFileSystemIO` → `runPrettyPrint` → `runError` → `runLog` →
`runEff`.

- `Console` — Haskeline (ввод, история, выход) и optparse (парсер команд).
- `FileSystem` — чтение `dicts/*.txt` и `stuff/titles.toml`, `Paths_hibet`.
- `PrettyPrint` — `prettyprinter` + ANSI + pager.
- `Env` живёт в `MVar`: `shell` читает его из параллельной ветки, пока
  `prepareEnv` грузит словари.

## Загрузка

`makeEnvFor ids`:

1. `getLabels` из `stuff/titles.toml` (tomland).
2. `dicts/` — 61 файл, около 103 МБ.
3. `makeDictionary`: строка `ключ|перевод`, дубликаты склеиваются.
4. `parMap rdeepseq` по файлам.
5. `Env { dictionaryMeta :: [DictionaryMeta], labels :: Labels }`.

`readEnv` — `readMVar`; `modifyEnv` и `putEnvMVar` — запись.

## Поиск

`getAnswer query env`:

1. `detectScript`: есть символ `U+0F00–U+0FFF` → `Tibet`, иначе `Wylie`.
2. `boilQuery`:
   - `Wylie` → `hewts.convertWylieToUnicode defaultSettings`; ключ — `T.words`.
   - `Tibet` → `hewts.splitSentences` + `hewts.renderItems OutWylie` по
     `SyllableItem`; ключ — `T.words`.
3. `searchTranslation` — `HMS.lookup` по каждому словарю, последовательно:
   61 lookup дешевле sparks на 61 задачу (замер T9).
4. `sortOutput` по `dictNumber`.

Ключ словаря — Wylie. Пунктуация и пробелы запроса в ключ не попадают.

## CLI

- `hibet` — интерактивный shell.
- `hibet shell -s ID…` — shell с выбранными словарями.
- `hibet show names` — список словарей.
- `hibet show meta [-d ID]` — описания словарей.
- `hibet om` — картинка.

`:q` — выход, `:h` — история. Словари грузятся в фоне (`withAsync prepareEnv`),
поэтому shell открывается до окончания загрузки.

## Граница с hewts

Единственный модуль с `import Convert` — `hibet/src/Translator.hs`. Используются
`convertWylieToUnicode`, `splitSentences`, `renderItems OutWylie`,
`defaultSettings`, `OutputFormat`, `SpellItem`. Своей конверсии в `hibet` нет.

## Что меняется (T2)

`hibet` станет движком, `hibet-cli` — консолью. Шов: `Env` как значение,
`getAnswer`/`translate` без Haskeline; `Cli`, `App`, `Effects.Console`,
`Effects.PrettyPrint`, `Pretty` уезжают в CLI. Подробности — `../TASKS.md`, T2.

## Тесты

- `hibet-env` — загрузка словарей, `getAnswer` на Wylie и Unicode, матрица
  Unicode-запросов (пунктуация, пробелы, смешанный текст, ошибки).
- `hibet-labels` — метки словарей.
- `hibet-pretty` — вывод.

## Данные

`dicts/*.txt` — 61 файл, около 103 МБ; `stuff/titles.toml` — метаданные.
В браузер они не грузятся: веб-переводчик может быть только серверным.
