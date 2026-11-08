# Архитектура репозитория

Три пакета и цепочка зависимостей: `hibet-cli → hibet → hewts`. Обратных
зависимостей нет и не будет: `hewts` не знает ни про словари, ни про перевод,
ни про консоль, а `hibet` не знает про Haskeline и optparse.

## Пакеты

| Пакет | Что это | Зависит от |
|---|---|---|
| `hewts` | библиотека: конверсия Wylie ↔ Unicode, проверка правописания, диагностика | `base`, `text`, `bytestring`, `containers`, `megaparsec`, `bytestring-trie`, `unordered-containers` |
| `hibet` | движок: словари, загрузка `Env`, чистый `getAnswer` | `hewts`, `parallel`, `tomland` |
| `hibet-cli` | консоль: `App`, `Cli`, `Shell`, `Pretty`, exe `hibet` | `hibet`, `haskeline`, `optparse-applicative`, `prettyprinter`, `pager`, `ki` |
| `hibet-web` | план: серверный API и веб-фронтенд | `hibet` |

`hibet-web` пока не существует; веб-переводчик описан в `hibet/ROADMAP.md`.

## Поток перевода

```
exe hibet (пакет hibet-cli)
  → App.app: Cli.parser через optparse execParser
      → Ki.scoped + Ki.fork: makeEnvFor → MVar Env (в фоне)
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
- **Словари** (59 файлов, ~103 МБ) — данные `hibet`; `stuff/key-aliases.tsv` —
  производная таблица написаний ключей. В браузер они не едут: веб-конвертер
  собирается из одного `hewts`, веб-переводчик — сервер.

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
  `hibet/stuff/key-aliases.tsv` после обновления словарей (около 2,5 мин).
- `fourmolu` для `.hs`, `cabal-gild` для `.cabal`.
- Nix: `nix build`, `nix develop`; `justfile` — короткие команды.
