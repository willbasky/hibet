# Архитектура репозитория

Два пакета и одна зависимость: `hibet → hewts`. Обратной нет и не будет:
`hewts` не знает ни про словари, ни про перевод, ни про консоль.

## Пакеты

| Пакет | Что это | Зависит от |
|---|---|---|
| `hewts` | библиотека: конверсия Wylie ↔ Unicode, проверка правописания, диагностика | `base`, `text`, `bytestring`, `containers`, `megaparsec`, `bytestring-trie`, `unordered-containers` |
| `hibet` | переводчик: словари, поиск, консольный цикл | `hewts`, `effectful`, `haskeline`, `optparse-applicative`, `prettyprinter` |
| `hibet-cli` | план T2: консоль отдельно от движка | `hibet` |
| `hibet-web` | план: серверный API и веб-фронтенд | `hibet` |

`hibet-cli` и `hibet-web` пока не существуют. Появление `hibet-cli` описано в
`TASKS.md` (T2), веб-переводчика — в `hibet/ROADMAP.md`.

## Поток перевода

```
hibet (exe)
  → Cli: разбор команды
  → App.app
      → prepareEnv: stuff/titles.toml + dicts/*.txt → Env
      → Translator.loopDialog (Haskeline)
          → getAnswer
              → detectScript        U+0F00–U+0FFF → Tibet, иначе Wylie
              → boilQuery
                  Wylie → hewts.convertWylieToUnicode
                  Tibet → hewts.splitSentences + hewts.renderItems OutWylie
              → Dictionary.searchTranslation (HMS.lookup) по всем словарям
              → Dictionary.sortOutput по номеру словаря
          → Pretty.mkOutput
      → Effects.PrettyPrint.pprint
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
- **`hibet`** не содержит своей конверсии. Единственный модуль с
  `import Convert` — `hibet/src/Translator.hs`; своя логика письма там
  отсутствует.
- **Словари** (61 файл, ~103 МБ) — данные `hibet`. В браузер они не едут:
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
| `App` | сборка эффектов и запуск |
| `Cli` | команды `shell`, `show names`, `show meta`, `om` |
| `Translator` | диалог, `getAnswer`, граница с hewts |
| `Dictionary` | формат словаря, поиск, сортировка ответов |
| `Env` | загрузка словарей и меток, `MVar Env` |
| `Effects.Console`, `.File`, `.PrettyPrint`, `.Common` | IO-обвязка |
| `Pretty`, `Label`, `Type`, `Utility` | вывод, метаданные, ошибки, мелочи |

## Сборка и проверки

- GHC 9.8.4 или 9.10.3, Cabal.
- `cabal build all` — оба пакета.
- `cabal test all` — тесты `hewts` и `hibet`.
- `cabal run hewts-benchmark` — линейность разбора.
- `fourmolu` для `.hs`, `cabal-gild` для `.cabal`.
- Nix: `nix build`, `nix develop`; `justfile` — короткие команды.
