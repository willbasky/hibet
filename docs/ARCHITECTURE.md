# Архитектура репозитория

Три пакета и цепочка зависимостей: `hibet-cli → hibet → hewts`. Обратных
зависимостей нет и не будет: `hewts` не знает ни про словари, ни про перевод,
ни про консоль, а `hibet` не знает про Haskeline и optparse.

## Пакеты

| Пакет | Что это | Зависит от |
|---|---|---|
| `hewts` | библиотека: конверсия Wylie ↔ Unicode, проверка правописания, диагностика | `base`, `text`, `bytestring`, `containers`, `megaparsec`, `bytestring-trie`, `unordered-containers` |
| `hibet` | движок: загрузка `Env`, resolver data-root, чистый `getAnswer` | `hewts`, `parallel`, `tomland` |
| `hibet-cli` | консоль: `App`, `Cli`, `Shell`, `Pretty`, `data status`, exe `hibet` | `hibet`, `haskeline`, `optparse-applicative`, `prettyprinter`, `pager`, `ki` |
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
печатает разрешённый путь и версию из `manifest.toml`. Манифест читают только
CLI и будущий updater; библиотека `hibet` его не требует.

Для разработки `just run` выставляет `HIBET_DATA_DIR=$PWD/data`; `nix develop`
делает то же через `shellHook`. Nix-пакет `hibet-cli` оборачивает бинарь и
передаёт ему данные из `./data`.

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
  отсутствует. Offline-генератор ключей `scripts/gen-key-aliases.hs` тоже ходит
  в hewts, но в приложение не входит.
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
| `Cli` | команды `shell`, `show names`, `show meta`, `om` |
| `Shell` | диалог Haskeline, `getAnswer`, сборка вывода |
| `Pretty` | вывод в терминал и пейджер |

## Сборка и проверки

- GHC 9.8.4 или 9.10.3, Cabal.
- `cabal build all` — три пакета.
- `cabal test all` — тесты `hewts`, `hibet` и `hibet-cli`.
- `cabal run hewts-benchmark` — линейность разбора.
- `cabal exec -- runghc scripts/gen-key-aliases.hs` — пересобрать
  `data/key-aliases.tsv` после обновления словарей (около 2,5 мин).
- `data/manifest.toml` после обновления словарей надо перегенерировать вручную:
  генератор манифеста появится вместе с Haskell-инструментом T12 (T25, шаг 6).
- `just run` — запуск из репы с `HIBET_DATA_DIR=$PWD/data`.
- `hibet data status` — разрешённый data-root и версия набора.
- `fourmolu` для `.hs`, `cabal-gild` для `.cabal`.
- Nix: `nix build`, `nix develop`; `justfile` — короткие команды.
