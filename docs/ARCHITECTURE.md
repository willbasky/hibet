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
| `hibet-tools` | maintainer-инструмент: обновление словарей, alias-таблица, манифест и релизный архив | `hibet`, `hewts`, `http-client-tls`, `unicode-transforms`, `optparse-applicative`, `tar`, `zlib` |
| `hibet-web` | план: серверный API и веб-фронтенд | `hibet` |

`hibet-web` пока не существует; веб-переводчик описан в `hibet/ROADMAP.md`.

## Данные

Словари и метаданные — не часть пакета. Они лежат в `hibet-data/` в корне репы:

```
hibet-data/
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
печатает разрешённый путь и версию из `manifest.toml`. Флаг и переменная
побеждают XDG: `hibet data update` всегда ставит набор в
`$XDG_DATA_HOME/hibet/data`, поэтому при старом переопределении приложение
продолжит читать старый путь.

Ответственность разделена: `hibet-tools` — maintainer-only, обновляет
`hibet-data/` в репе и готовит манифест; `hibet data install --from DIR` и
`hibet data update` — пользовательские команды в `hibet-cli`, которые
проверяют манифест и ставят данные в XDG. Релиз
`hibet-data-<version>.tar.gz` публикуется в основной репе; отдельной репы
данных нет. Манифест читают только updater и команды CLI; библиотека `hibet`
его не требует.

Установленный набор лежит плоско, как `hibet-data/` в репе:
`$XDG_DATA_HOME/hibet/data/{dicts,titles.toml,key-aliases.tsv,manifest.toml}`.
Перед заменой активного набора данные собираются в `data.new`, а старый
набор уезжает в `data.old`. Обе команды не зависят от текущего каталога,
`--data-dir` и `HIBET_DATA_DIR` задают только источник для запуска, но не
цель установки.

Для разработки `just run` выставляет `HIBET_DATA_DIR=$PWD/data`; `nix develop`
делает то же через `shellHook`. Nix-пакет `hibet-cli` оборачивает бинарь и
передаёт ему данные из `./data`.

## Обновление словарей

`hibet-tools` (exe `hibet-dataset`) — maintainer-only замена
`scripts/update-dicts.sh`; он не устанавливает данные пользователю. Он
скачивает все upstream-файлы, нормализует их в
NFC, применяет две локальные правки (`Sera-Textbook-Def`, `JimValby`),
сравнивает с `hibet-data/dicts` и пишет только изменения. Счётчики те же, что у
bash-версии; команда `check` ничего не пишет и завершается кодом 1 при отличиях.
`BASE_URL=file:///…` используется в тестах без сети, `REF` задаёт ветку.
После изменений инструмент в том же процессе вызывает генератор
`hibet-data/key-aliases.tsv` и пересобирает `hibet-data/manifest.toml`;
отдельные команды —
`hibet-dataset generate-key-aliases` (тоже обновляет манифест) и
`hibet-dataset generate-manifest` (обёртки: `just generate-key-aliases`,
`just generate-manifest`).
Команда `check` дополнительно сверяет `hibet-data/manifest.toml` с файлами на диске:
пропавший или лишний файл, несовпадение размера или sha256 дают код 1.
Команды и рабочий процесс maintainer'а — в `../hibet-tools/README.md`.

## Установка данных

`hibet data install --from DIR` читает `DIR/manifest.toml`, проверяет размеры
и sha256 каждого файла, копирует только файлы из манифеста (плюс сам
`manifest.toml`) в `$XDG_DATA_HOME/hibet/data.new` и переключает `data`.
`hibet data update` делает то же с релизом: скачивает `manifest.toml`, затем
`hibet-data-<version>.tar.gz`, распаковывает только обычные файлы из
манифеста (ссылки и пути с `..` отвергаются) и сверяет хэши. `--source`
принимает HTTP URL, `file://` или локальный каталог; по умолчанию берётся
`releases/download/hibet-data` основной репы. Сеть и tar живут в `hibet-cli`,
не в библиотеке `hibet`.

## Релиз данных

`just check-manifest` сверяет `hibet-data/manifest.toml` с `hibet-data/` без сети.
`just data-release` запускает `hibet-dataset release`: тот сам проверяет манифест,
собирает `dist/data-release/hibet-data-<version>.tar.gz` на Haskell (`tar` +
`zlib`, без системного `tar`) и копирует туда же `manifest.toml`.
`just data-publish` делает то же и публикует артефакты в GitHub Releases:
создаёт релиз или загружает новые файлы с `--clobber` и обновляет notes.
Оба файла живут под тегом `hibet-data`; этот тег не двигается, а `manifest.toml` и
версионный архив обновляются через `gh release upload --clobber`. Поэтому
`hibet data update` не зависит от того, какой релиз GitHub считает latest.
Старые версии архива остаются в том же релизе и годятся для Nix/AUR по
неизменяемому URL `.../releases/download/hibet-data/hibet-data-<version>.tar.gz`.

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
  `data-files` пакета; `hibet-data/key-aliases.tsv` — производная таблица
  написаний ключей, `hibet-data/manifest.toml` — версия и хэши. В браузер они не едут:
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
| `KeyAliases` | генератор `hibet-data/key-aliases.tsv` |
| `Manifest` | генерация и проверка `hibet-data/manifest.toml` (sha256 и размеры) |
| `Release` | проверка манифеста и сборка `hibet-data-<version>.tar.gz` |

## Сборка и проверки

- GHC 9.8.4 или 9.10.3, Cabal.
- `cabal build all` — четыре пакета.
- `cabal test all` — тесты `hewts`, `hibet`, `hibet-cli` и `hibet-tools`.
- `cabal run hewts-benchmark` — линейность разбора.
- `just update-dicts` — скачать upstream и обновить `hibet-data/dicts`, затем
  пересобрать `hibet-data/key-aliases.tsv` и `hibet-data/manifest.toml` в том же процессе.
- `just update-dicts-check` — то же без записи плюс сверка манифеста с диском;
  код 1, если есть отличия или манифест устарел. `BASE_URL=file:///path` и
  `REF=...` сохраняют интерфейс старого скрипта.
- `just generate-key-aliases` — только пересобрать `hibet-data/key-aliases.tsv`
  (около 2,5 мин) и обновить манифест.
- `just generate-manifest` — только пересобрать `hibet-data/manifest.toml` из
  текущего `hibet-data/`; версия набора — дата в формате `YYYY.MM.DD`.
- `just check-manifest` — сверить манифест с `hibet-data/` без сети.
- `just data-release` — собрать `dist/data-release/` для GitHub Releases
  (`hibet-dataset release` сам проверяет манифест перед сборкой).
- `just run` — запуск из репы с `HIBET_DATA_DIR=$PWD/data`.
- `hibet data status` — разрешённый data-root и версия набора.
- `hibet data install --from data` — поставить данные из репы в XDG;
  `hibet data update --source DIR` — из локального релиза без сети.
- `fourmolu` для `.hs`, `cabal-gild` для `.cabal`.
- Nix: `nix build`, `nix develop`; `justfile` — короткие команды.
