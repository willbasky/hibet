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
      Render.Pretty  терминальный вывод
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

Внутри — `BS.readFile`, `listDirectory`; ошибка IO становится `DataUnreadable`
с именем файла. `App` запускает загрузку в фоне через `Ki.scoped` + `Ki.fork`;
поток-загрузчик кладёт `Env` в `MVar`, `readEnv` читает его. `scoped` гасит или
дожидается поток при выходе. Команды без словарей (`om`, `data status`,
`show names`/`show meta`) не загружают `Env` вовсе; `show` читает только
`titles.toml`.

Файлы словарей читаются строго, по одному зелёному потоку на файл
(`Ki.scoped` + `Ki.fork`), и только потом разбираются `parMap rdeepseq` (T8).
Загрузчик форсит `Env` до NF (`force`) до возврата из `makeEnvAt`: к моменту
`putMVar` разобраны все словари. Поэтому первый запрос после готовности `Env` стоит
столько же, сколько второй, а `:q` сразу не ждёт загрузку: `Ki.scoped` гасит
поток. `NFData` не помнит форсированность, так что `force` обходит хэш-таблицы
целиком: это около 0,03 с на фоне 0,25 с разбора и +0,03 с к немедленному
первому запросу.

Консольный вывод (`putColorDoc`, `pprint`) — обычный `IO` в `Render.Pretty`.

## Загрузка

`makeEnvForDataDir`, `makeEnvFor` или `makeEnvFrom`:

1. resolver находит корень; `makeEnvFrom` получает его аргументом.
2. `getLabels` из `titles.toml` (tomland).
3. `key-aliases.tsv` — пары «легаси-ключ → канонический Wylie» (см.
   «Ключи и Unicode»).
4. `dicts/` — 59 файлов, около 103 МБ; чтение строгое, один зелёный поток на
   файл (`Ki.scoped` + `Ki.fork`).
5. `makeDictionary`: строка `ключ|перевод`, дубликаты склеиваются.
6. `addAliases`: канонический ключ кладётся рядом с легаси с теми же статьями.
7. `parMap rdeepseq` по файлам: разбор остаётся на sparks (T8).
8. `force` до NF: разбор заканчивается в потоке-загрузчике, а не в первом
   запросе.
9. `Env { dictionaryMeta :: [DictionaryMeta], labels :: Labels }`.

`readEnv :: MVar Env -> IO Env` — `readMVar`; `modifyEnv` и `putEnvMVar` —
запись.

Почему так (T8, 16 ядер, GHC 9.10.3, тёплый кэш): строгое чтение всех файлов
по потоку на файл — 29 мс, ленивое через `parMap` — 40 мс, последовательное —
41 мс, то есть ленивый `BSL.readFile` под `parMap` чтение не распараллеливает.
Строгий `Text` ещё и дешевле ленивого: аллокации 746 МБ против 910. Полная
загрузка `Env`: 403 мс при старом чтении и `-A4m`, 386 мс при новом чтении и
`-A4m`, 253 мс при новом чтении и `-A16m` (холодный кэш — 259 мс); RSS 374 →
542 МБ (в основном nursery `-A16m`), `maximum residency` около 160 МБ. Разбор в зелёных потоках не выиграл
у `parMap` (283 против 272 мс на 8 ядрах), поэтому оставлен `parMap`.

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

Нормализация вынесена из загрузки в данные: `hibet-data/key-aliases.tsv` хранит пары
«легаси-ключ ⇥ канонический ключ», а `addAliases` при загрузке кладёт
канонический ключ рядом с легаси с теми же статьями. Оба написания остаются
рабочими, вывод hewts не меняется; юникод-запрос находит и `bha ga`, и `gnad`.

Почему не канонизировать ключи при загрузке: 348 433 ключа через hewts — это
140 с CPU (≈0,4 мс на ключ), а полная загрузка `Env` была 0,28 с. Таблица
стоит одного чтения 259 КБ и ~348 тыс. lookups; запрос не дорожает.

Таблица производная: после обновления словарей её надо перегенерировать
(`just generate-key-aliases`, около 2,5 мин). `hibet-env`
проверяет каждую строку: ключ должен совпадать с тем, что даёт юникод-путь, а
оба написания — читаться в те же слоги. 26 ключей, у которых hewts меняет
слово, в таблицу не попадают.

## Границы IO

`Text` — доменный тип: словари, `Env`, `Dictionary`, `Labels`, `Manifest`,
ответы и сообщения. `ByteString` — файлы, архивы и сеть. Текст из файла
читается строго (`BS.readFile`) и декодируется `TE.decodeUtf8'`; ошибка несёт
путь (`Io.readUtf8`, `Utf8Error`). Запись — `TE.encodeUtf8` + `BS.writeFile`
(`Io.writeUtf8`). Locale не участвует: под `LC_ALL=C` словарь читается так же,
как под UTF-8.

Консоль — исключение: `Data.Text.IO` кодирует в locale терминала, и это
правильно (Snoyman, «Beware of readFile»). Поэтому `TIO.putStr*`/`hPutStr*`
остаются в `Render.Pretty`, `Progress`, `App`, `Cli` и `Art`; `BS.hPut` на
stdout/stderr не используется. Внутри сообщения — `Text`; `String` остаётся
только у `FilePath`, `Show`/`Read`, исключений, `optparse-applicative` и URL
для `http-client`.

## Ошибки

Ошибки делим по назначению, а не по механизму:

- ожидаемый сбой (нет данных, битый UTF-8/TOML, плохой запрос) — значение
  `Either HibetError`;
- баг или нарушенный инвариант — исключение (`error`/`throw`), в обычном
  потоке не ловится;
- асинхронность (`UserInterrupt`, `ThreadKilled`) — исключение, проходит
  наверх;
- «не найдено» — `Maybe` или пустой список, не ошибка.

На границах нет нативных `ErrorCall`; `UnicodeException` заворачивается в
`Io.Utf8Error` и на файловой границе превращается в значение
(`Either HibetError`/`Either Text`). `HibetError` делится по причине:

- `DataNotFound [FilePath]` — не найден корень данных; `renderError`
  перечисляет проверенные пути и подсказывает `data install`/`update`;
- `DataUnreadable FilePath Text` — файл или каталог не читается: путь и
  сообщение IO в конструкторе;
- `DataInvalid FilePath Text` — файл прочитан, но не разбирается (UTF-8,
  TOML);
- `QueryInvalid Text` — hewts отверг запрос.

`Label.getLabels`, `Env.readAliases` и `Env.getFilesTexts` возвращают
`Either HibetError`; `Env.makeEnvAt` ловит `IOException` на каждом чтении и
возвращает `Left`, а не бросает. `Exception HibetError` остаётся транспортом на
границу CLI: `Cli.runDataStatus` и `Cli.runShowStatic` бросают его наверх,
`App` ловит.

`annotated-exception` не берём: он не превращает ожидаемые сбои в значения,
тянет `safe-exceptions` и `unliftio-core` и окупается только в глубоких
IO-стеках. Здесь стек короткий, API уже `Either`; с GHC 9.8/9.10 совместим,
отказ не из-за версий. Пересмотреть, если появится `hibet-web` с глубокими
middleware-стеками.

`ManifestError` — отдельный тип в `hibet-tools`, не в `hibet`: манифест не
читает движок. Поля `line` (строка исходника, когда известна), `fieldName`
(ключ манифеста) и `reason`, все `Text`; `renderManifestError` собирает из
них одну строку на границе `Install`/`Release`/`hibet-dataset`.

## CLI

- `hibet` — интерактивный shell.
- `hibet --data-dir DIR …` — явный корень данных для любой команды.
- `hibet data status` — разрешённый data-root и версия из `manifest.toml`.
- `hibet data install --from DIR` — поставить проверенный набор из каталога
  (клон репы, распакованный релиз) в `$XDG_DATA_HOME/hibet/data`.
- `hibet data update [--source URL|DIR]` — скачать `manifest.toml` и
  `hibet-data-<version>.tar.gz` из релиза, проверить sha256 и заменить набор.
  `--source` принимает `file://` и локальный каталог; без него берётся
  `releases/download/hibet-data` основной репы.
- Установка всегда идёт в XDG: `--data-dir` и `HIBET_DATA_DIR` задают, откуда
  читать при работе, но не куда ставить. Старый набор остаётся в `data.old`.
  Если один из них указывает на старый клон, `hibet` продолжит читать его,
  хотя `data update` положит новый набор в XDG.
- `hibet shell -s ID…` — shell с выбранными словарями.
- `hibet show names` — список словарей.
- `hibet show meta [-d ID]` — описания словарей.
- `hibet om` — картинка (данные не нужны).

`:q`, Ctrl-D или Esc — выход, `:h` — история. Esc включается, только когда в
`~/.haskeline` не выбран `editmode: Vi`: `Shell` дополняет пользовательские
`Prefs` строкой `bind: escape ctrl-a ctrl-k ctrl-d` (очистить строку и получить
EOF) и запускает ввод через `runInputTWithPrefs`, а не `initializeInput`.
`keyseqTimeoutMs` из 0.8.5 не даёт Esc-байту сработать раньше стрелки на
медленном вводе. В Vi-режиме Esc остаётся сменой режима. Словари грузятся в
фоне (`Ki.scoped` + `Ki.fork`), поэтому shell открывается до окончания
загрузки.

Ошибку, дошедшую до `App`, печатает `Type.renderError` — summary и detail без
`Show`, поэтому в выводе нет имени конструктора. Текст идёт в `stderr`,
процесс завершается кодом 1. Ловятся только `HibetError` и `IOException`
(последний через `displayException`); `UserInterrupt`, `ExitSuccess` и
`ErrorCall` проходят наверх, так что Ctrl+C и успешные команды не меняются.

`Shell` отличает плохой запрос от пустого ответа: `Left` печатает
`renderError` (например, `QueryInvalid`), «Nothing found» — только пустой
список статей.

## Граница с hewts

Единственный модуль библиотеки с `import Convert` — `hibet/src/Translator.hs`.
Используются `convertWylieToUnicode`, `splitSentences`, `renderItems OutWylie`,
`defaultSettings`, `OutputFormat`, `SpellItem`. Своей конверсии в `hibet` нет.
Генератор ключей `hibet-dataset generate-key-aliases` (пакет `hibet-tools`)
тоже ходит в hewts, но это offline-инструмент, а не часть приложения.

## Пакеты

`hibet` — движок (`Dictionary`, `Env`, `Translator`, `Type`, `Utility`,
`Label`); `hibet-cli` — консоль (`Cli`, `App`, `Shell`, `Render`, Haskeline,
optparse, exe `hibet`). `Manifest` живёт в `hibet-tools`: CLI и
maintainer-инструменты делят разбор `manifest.toml`, `ManifestError` и
проверку хэшей. Движок не зависит от консольных пакетов, поэтому
серверный API (`hibet-web`) сможет зависеть от `hibet` без Haskeline.

## Тесты

- `hibet-env` — загрузка словарей (`makeEnvFrom` на `test/env/data`), resolver,
  `getAnswer` на Wylie и Unicode, матрица Unicode-запросов (пунктуация,
  пробелы, смешанный текст, ошибки), инвариант таблицы `hibet-data/key-aliases.tsv`.
  Тесты с реальными словарями берут `../data` или `HIBET_DATA_DIR`; в распакованном
  `sdist` данных нет, и они помечаются pending, а не падают.
- `hibet-labels` — метки словарей.
- `hibet-render` — вывод (живёт в пакете `hibet-cli`).
- `hibet-cli-data` — установка: проверка манифеста, `data.new`/`data.old`,
  локальный `file://`-релиз, несовпадение хэша, лишний и небезопасный путь в
  архиве.

## Данные

`hibet-data/dicts/*.txt` — 59 файлов, около 103 МБ; `hibet-data/titles.toml` —
метаданные; `hibet-data/key-aliases.tsv` — производная таблица ключей (259 КБ);
`hibet-data/manifest.toml` — версия набора и sha256 каждого файла; его генерирует и
сверяет с диском `hibet-tools` (`just generate-manifest`, `hibet-dataset check`).
Это не
`data-files` пакета: `cabal sdist` содержит только код и fixture, а бинарь
ищет данные через `--data-dir`, `HIBET_DATA_DIR` или XDG. Установленный набор
лежит в `$XDG_DATA_HOME/hibet/data`; `hibet data install`/`update` собирают
его в `data.new` и оставляют прежний в `data.old`. В браузер данные не
грузятся: веб-переводчик может быть только серверным.

## Обновление данных

`hibet data update` печатает шаги в `stderr`: `manifest`, `download`,
`unpack`, `verify`, `switch`, `done`. В терминале это одна перерисовываемая
строка со спиннером `|/-\` и байтами/процентом; в pipe — одна строка на шаг
без анимации. Спиннер выключается при `NO_COLOR` или `TERM=dumb`. Архив
скачивается потоком (`withResponse` + `brRead`) ради прогресса и
прерываемости, но всё равно собирается в памяти: `Tar.read` нужен целый
архив. Для `file://` и локального каталога процент не рисуется, печатается
размер.

После удалённого `manifest.toml` читается `<target>/data/manifest.toml`; если
версии совпадают, архив не запрашивается, команда печатает
`data already up to date (<version>)` и выходит с 0. Хэши на этом шаге не
сверяются: битый набор чинится `hibet data install --from`.

Подготовка идёт в `data.new`; при ошибке или Ctrl+C (`UserInterrupt`) каталог
удаляется, активный `data` не трогается, исключение пробрасывается дальше —
выход с кодом 130. Переключение (`удалить data.old` → `data` → `data.new`)
выполняется под `uninterruptibleMask_`, чтобы отмена не оставила каталог без
активного набора.

## Релиз данных

`just data-release` запускает `hibet-dataset release`: тот проверяет
`hibet-data/manifest.toml` и собирает `dist/data-release/hibet-data-<version>.tar.gz`
вместе с `manifest.toml` на Haskell, без системного `tar`.
Артефакты публикуются в GitHub Releases основной репы под тегом `hibet-data`;
версия живёт в имени архива и в манифесте, а не в теге. `hibet data update`
по умолчанию читает `releases/download/hibet-data`, поэтому релиз данных не
зависит от кодовых релизов и от того, какой из них GitHub считает latest.
Отдельной репы `hibet-data` нет.
