# Архитектура репозитория

Четыре пакета и цепочка зависимостей: `hibet-cli → hibet → hewts`; отдельно
`hibet-tools → hibet` для обновления данных. Обратных зависимостей нет и не
будет: `hewts` не знает ни про словари, ни про перевод, ни про консоль, а
`hibet` не знает про Haskeline, optparse и HTTP.

## Пакеты

| Пакет | Что это | Зависит от |
|---|---|---|
| `hewts` | библиотека: конверсия Wylie ↔ Unicode, проверка правописания, диагностика | `base`, `text`, `bytestring`, `containers`, `megaparsec`, `bytestring-trie`, `unordered-containers` |
| `hibet` | движок: загрузка `Env`, resolver data-root, чистый `getAnswer` | `hewts`, `parallel`, `tomland` |
| `hibet-cli` | консоль: `App`, `Cli`, `Shell`, `Pretty`, `data install`/`update`, exe `hibet` | `hibet`, `haskeline`, `optparse-applicative`, `prettyprinter`, `pager`, `ki`, `http-client-tls`, `tar`, `zlib`, `crypton` |
| `hibet-tools` | maintainer-инструмент: обновление словарей и alias-таблицы, HTTP/NFC | `hibet`, `hewts`, `http-client-tls`, `unicode-transforms` |
| `hibet-web` | план: серверный API и веб-фронтенд | `hibet` |

`hibet-web` пока не существует; веб-переводчик описан в `hibet/ROADMAP.md`.

## Данные

Словари и метаданные — не часть пакета. Они лежат в `data/` в корне репы:

```
data/
├── dicts/*.txt         59 словарей, около 103 МБ
├── titles.toml         метаданные словарей
├── key-aliases.tsv     производная таблица ключей
└── manifest.toml       версия набора и sha256 каждого файла
```

`hibet` не объявляет `data-files`; `cabal sdist` содержит только код и маленький
тестовый fixture. Установленный бинарь находит данные так:

1. `--data-dir DIR` — явный корень;
2. `HIBET_DATA_DIR` — переменная окружения;
3. `$XDG_DATA_HOME/hibet/data` — активный набор.

Кандидат принимается, только если в нём есть `titles.toml` и `dicts/`. Ошибка
называет проверенные пути и подсказывает флаг и переменную. `hibet data status`
печатает разрешённый путь и версию из `manifest.toml`.

Ответственность разделена: `hibet-tools` — maintainer-only, обновляет `data/`
в репе и готовит манифест; `hibet data install --from DIR` и
`hibet data update` — пользовательские команды в `hibet-cli`, которые
проверяют манифест и ставят данные в XDG. Релиз
`hibet-data-<version>.tar.gz` публикуется в основной репе; отдельной репы
данных нет. Манифест читают только updater и команды CLI; библиотека `hibet`
его не требует.

Установленный набор лежит плоско, как `data/` в репе:
`$XDG_DATA_HOME/hibet/data/{dicts,titles.toml,key-aliases.tsv,manifest.toml}`.
Перед заменой активного набора данные собираются в `data.new`, а старый
набор уезжает в `data.old`. Обе команды не зависят от текущего каталога,
`--data-dir` и `HIBET_DATA_DIR` задают только источник для запуска, но не
цель установки.

Для разработки `just run` выставляет `HIBET_DATA_DIR=$PWD/data`; `nix develop`
делает то же через `shellHook`. Nix-пакет `hibet-cli` оборачивает бинарь и
передаёт ему данные из `./data`.

## Обновление словарей

`hibet-tools` (exe `hibet-update-dicts`) — maintainer-only замена
`scripts/update-dicts.sh`; он не устанавливает данные пользователю. Он
скачивает все upstream-файлы, нормализует их в
NFC, применяет две локальные правки (`Sera-Textbook-Def`, `JimValby`),
сравнивает с `data/dicts` и пишет только изменения. Счётчики те же, что у
bash-версии; `--check` ничего не пишет и завершается кодом 1 при отличиях.
`BASE_URL=file:///…` используется в тестах без сети, `REF` задаёт ветку.
После изменений инструмент в том же процессе вызывает генератор
`data/key-aliases.tsv` и пересобирает `data/manifest.toml`; отдельные режимы —
`just gen-key-aliases` (тоже обновляет манифест) и `just gen-manifest`.
`--check` дополнительно сверяет `data/manifest.toml` с файлами на диске:
пропавший или лишний файл, несовпадение размера или sha256 дают код 1.

## Установка данных

`hibet data install --from DIR` читает `DIR/manifest.toml`, проверяет размеры
и sha256 каждого файла, копирует только файлы из манифеста (плюс сам
`manifest.toml`) в `$XDG_DATA_HOME/hibet/data.new` и переключает `data`.
`hibet data update` делает то же с релизом: скачивает `manifest.toml`, затем
`hibet-data-<version>.tar.gz`, распаковывает только обычные файлы из
манифеста (ссылки и пути с `..` отвергаются) и сверяет хэши. `--source`
принимает HTTP URL, `file://` или локальный каталог; по умолчанию берётся
`releases/latest/download` основной репы. Сеть и tar живут в `hibet-cli`,
не в библиотеке `hibet`.

## Поток перевода

```
exe hibet (пакет hibet-cli)
  → App.app: Cli.parser через optparse execParser
      → Env.resolveDataDir: --data-dir → HIBET_DATA_DIR → XDG
      → Ki.scoped + Ki.fork: makeEnvForDataDir → MVar Env (в фоне)
      → Cli.runCommand
          → Shell.translator (Haskeline)
              → getAnswer (чистая)
                  → detectScript        U+0F00–0FFF → Tibet, иначе Wylie
                  → boilQuery
                      Wylie → hewts.convertWylieToUnicode
                      Tibet → hewts.splitSentences + hewts.renderItems OutWylie
                  → Dictionary.searchTranslation (HMS.lookup) по всем словарям
              → Shell.mkOutput: Dictionary.sortOutput по номеру словаря
              → Pretty.pprint
```

Ключ словаря — Wylie. Пунктуация и пробелы запроса в ключ не попадают: тибетская
ветка берёт только `SyllableItem` и печатает их в Wylie.

## Поток конверсии hewts

```
текст
  → Convert.Tokenizer.{Wylie,Unicode}   чтение: текст → [Token]
  → Convert.Grammar.*                  разбор: токены → [SpellItem]
  → Convert.Render.*                   печать: [SpellItem] → текст
        ↘ Convert.Diagnostic           находки: код, важность, Span, сообщение
```

Три слоя плюс азбука (`Convert.Token.*`) и общий канал диагностики. Публичная
дверь — модуль `Convert`: `Settings`, `InputFormat`, `convertWylieToUnicode`,
`convertUnicodeToWylie`, `checkSpelling`, `splitSentences`, `renderItems`.
Внутренние модули можно переименовывать, не ломая контракт.

## Границы

- **`hewts`** не делает IO, не читает файлы, не знает про перевод. Всё, что
  нужно снаружи, приходит текстом и `Settings`.
- **`hibet`** не содержит своей конверсии. Единственный модуль библиотеки с
  `import Convert` — `hibet/src/Translator.hs`; своя логика письма там
  отсутствует. Offline-генератор ключей в `hibet-tools` тоже ходит в hewts, но
  в приложение не входит.
- **`hibet-tools`** — maintainer-only: HTTP, NFC и запись словарей живут там,
  а не в библиотеке `hibet`.
- **`hibet`** не зависит от консоли: `haskeline`, `optparse-applicative`,
  `terminal-size`, `pager` и `prettyprinter-ansi-terminal` живут только в
  `hibet-cli`.
- **`hibet`** не использует effect system: загрузка словарей — обычный `IO`
  (`makeEnv*` возвращает `Either`), фоновая загрузка — `ki`.
- **Словари** (59 файлов, ~103 МБ) — данные репозитория и установки, не
  `data-files` пакета; `data/key-aliases.tsv` — производная таблица написаний
  ключей, `data/manifest.toml` — версия и хэши. В браузер они не едут:
  веб-конвертер собирается из одного `hewts`, веб-переводчик — сервер.

## Модули

### hewts

| Модуль | Роль |
|---|---|
| `Convert` | фасад: настройки, направления, проверка, разбиение |
| `Convert.Token`, `.Token.Unicode`, `.Token.Wylie` | азбука: знак, канон, написание |
| `Convert.Tokenizer.Unicode`, `.Wylie` | чтение текста в токены |
| `Convert.Grammar.*` | разбор: `Parser`, `Structure`, `Constraint`, `Superfix`, `Syllable`, `Sentence` |
| `Convert.Render.*` | печать в Unicode и Wylie, нотация |
| `Convert.Diagnostic` | канал находок |

### hibet

| Модуль | Роль |
|---|---|
| `Translator` | чистый `getAnswer`, `tibetToWylieKey`, граница с hewts |
| `Dictionary` | формат словаря, поиск, добавление alias-ключей |
| `Env` | загрузка словарей и меток в `IO`, `MVar Env` |
| `Label`, `Type`, `Utility` | метаданные, ошибки, мелочи |

### hibet-cli

| Модуль | Роль |
|---|---|
| `App` | разбор команды, `Ki`-scope, фоновая загрузка `Env` |
| `Cli` | команды `shell`, `show names`, `show meta`, `om`, `data status`/`install`/`update` |
| `Manifest` | разбор и проверка `manifest.toml` перед установкой |
| `Install` | скачивание, распаковка и замена `data`/`data.old` |
| `Shell` | диалог Haskeline, `getAnswer`, сборка вывода |
| `Pretty` | вывод в терминал и пейджер |

### hibet-tools

| Модуль | Роль |
|---|---|
| `Fetch` | HTTP-загрузка и `file://` для тестов |
| `Update` | карта upstream-файлов, NFC, локальные правки, сравнение и запись |
| `KeyAliases` | генератор `data/key-aliases.tsv` |
| `Manifest` | генерация и проверка `data/manifest.toml` (sha256 и размеры) |

## Сборка и проверки

- GHC 9.8.4 или 9.10.3, Cabal.
- `cabal build all` — четыре пакета.
- `cabal test all` — тесты `hewts`, `hibet`, `hibet-cli` и `hibet-tools`.
- `cabal run hewts-benchmark` — линейность разбора.
- `just update-dicts` — скачать upstream и обновить `data/dicts`, затем
  пересобрать `data/key-aliases.tsv` и `data/manifest.toml` в том же процессе.
- `just update-dicts-check` — то же без записи плюс сверка манифеста с диском;
  код 1, если есть отличия или манифест устарел. `BASE_URL=file:///path` и
  `REF=...` сохраняют интерфейс старого скрипта.
- `just gen-key-aliases` — только пересобрать `data/key-aliases.tsv`
  (около 2,5 мин) и обновить манифест.
- `just gen-manifest` — только пересобрать `data/manifest.toml` из текущего
  `data/`; версия набора — дата в формате `YYYY.MM.DD`.
- `just run` — запуск из репы с `HIBET_DATA_DIR=$PWD/data`.
- `hibet data status` — разрешённый data-root и версия набора.
- `hibet data install --from data` — поставить данные из репы в XDG;
  `hibet data update --source DIR` — из локального релиза без сети.
- `fourmolu` для `.hs`, `cabal-gild` для `.cabal`.
- Nix: `nix build`, `nix develop`; `justfile` — короткие команды.
