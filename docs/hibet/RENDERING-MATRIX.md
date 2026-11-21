# Матрица рендеринга `hibet-cli`

Снимок: 21.11.2026, `master` `05b6fa0`; OSC 66 удалён. Документ описывает **фактическое**
поведение `hibet shell` и `hibet om`: что увидит пользователь при разном
сочетании сборки, установленных данных, тибетского шрифта и терминала.

Код, на который опирается документ:

- `hibet-cli/src/Cli.hs` — `commandNeedsEnv`, `runStatic`;
- `hibet-cli/src/Shell.hs` — `queryHeader`, `omHeader`, `largeHeader`,
  `pickHeader`;
- `hibet-cli/src/Render/Art.hs` — `findFont`, `dataFontFiles`,
  `renderQueryBlocks`, `warnIfNoFont`;
- `hibet/src/Env.hs` — `resolveDataDir`;
- `hibet-cli/src/Install.hs` — `installFromRelease`;
- T35 в `../TASKS.md` — шрифт в `hibet-data/fonts`.

## Обозначения

| Знак | Значение |
|---|---|
| **A** | сборка с `-f ascii-art` (брайлевский рендерер включён) |
| **P** | обычная сборка без флага (рендерер — заглушка) |
| **D+** | установлен датасет 2026.11.20 со шрифтом `fonts/BabelStoneTibetan.ttf` |
| **D0** | датасет есть, но без шрифта (старая версия или файл удалён) |
| **D−** | данных нет: `resolveDataDir` не находит `titles.toml` и `dicts/` |
| **F+** | `findFont` нашёл тибетский шрифт: env, data или system |
| **F−** | шрифта нет нигде |
| **T** | интерактивный терминал (kitty теперь не отличается) |
| **R** | stdout не терминал: файл, пайп |
| **fit** | все строки арта укладываются в `width - 4` и `height - 4` |

«Арт» — брайлевские ячейки из `Render.Braille`; «ASCII Om» — фиксированная
картинка из `Cli.om`; «plain» — обычный текст запроса.

## 1. `hibet shell`

`shell` требует данных: без них `Env` не загрузится. Наличие шрифта влияет
только на заголовок, но не на запуск.

| Сборка | Данные | Шрифт | Что произойдёт |
|---|---|---|---|
| A | D+ | F+ | Shell работает. Заголовок — брайль, если fit; иначе plain. Exit 0. |
| A | D+ | F− | `warnIfNoFont` пишет предупреждение в stderr, shell работает, заголовок plain. Exit 0. |
| A | D0 | F+ (system/env) | Shell работает, арт системным шрифтом; предупреждения нет. |
| A | D0 | F− | Предупреждение, shell работает, plain. |
| A | D− | F± | Печатает приглашение, затем `Dictionary data not found…`, exit 1. Предупреждение о шрифте — только если F−. |
| P | D+/D0 | любой | Shell работает, арт выключен, plain. Предупреждения нет. |
| P | D− | любой | `Dictionary data not found…`, exit 1. |

Важное: `shell` без данных завершается кодом 1 **даже на `:q`** (проверено).
Ошибка появляется после приглашения, потому что `prepareEnv` падает в
фоновом потоке. Сообщение предлагает `hibet data install --from DIR`,
`hibet data update`, `--data-dir=DIR` или `HIBET_DATA_DIR`.

## 2. `hibet om`

`om` **не проверяет данные** и не падает без них. Всё решают сборка, шрифт,
высота и ширина. kitty больше ничем не отличается: OSC 66 нет.

| Сборка | Данные | Шрифт | Tty | Что видно |
|---|---|---|---|---|
| A | D+ | F+ BabelStone | T/R, height 24 | `HIBET_OM_LINES=10`: ASCII-картинка Om (23 строки), не влезает. `HIBET_OM_LINES=6`: брайль (13 строк). При height ≥25 брайль и с дефолтом. |
| A | D−/D0 | F+ system Noto | T/R | С Noto дефолтные 10 строк дают 17 строк — брайль влезает. С другим шрифтом может не влезть. |
| A | любой | F− | T/R | Предупреждение + ASCII Om. Exit 0. |
| A | любой | F+ | T/R | Если арт шире `width-4` или выше `height-4` — тихий fallback, без предупреждения. |
| P | любой | любой | T/R | ASCII Om, без предупреждения. |

Проверено:

- `XDG_DATA_HOME=/tmp/empty hibet om` (системный Noto): брайль, 17 строк;
- `hibet om --data-dir ./hibet-data` (BabelStone): ASCII, 23 строки;
- то же с `HIBET_OM_LINES=6`: брайль, 13 строк;
- `HIBET_TIBETAN_FONT=/etc/hostname`: файл молча пропущен, использован
  системный шрифт, exit 0.

## 3. Откуда берётся шрифт

Порядок в `findFont`:

1. `HIBET_TIBETAN_FONT`;
2. `<data-root>/fonts/BabelStoneTibetan.ttf`, где data-root — это
   `--data-dir`, затем `HIBET_DATA_DIR`, затем `$XDG_DATA_HOME/hibet/data`;
3. `fc-match "Noto Serif Tibetan"`;
4. `/usr/share/fonts`, `/usr/local/share/fonts`, `~/.local/share/fonts`.

Каждый кандидат проходит `isTibetanFont`: файл должен читаться и содержать
глиф `ཀ`.

| Ситуация | Результат |
|---|---|
| `HIBET_TIBETAN_FONT` — валидный тибетский шрифт | Используется он, данные и система игнорируются. |
| `HIBET_TIBETAN_FONT` — несуществующий файл | Пропускается, идём дальше. |
| `HIBET_TIBETAN_FONT` — не шрифт/битый файл | Пропускается (проверено на текстовом файле). |
| `HIBET_TIBETAN_FONT` — валидный, но без `ཀ` | Пропускается. |
| `--data-dir` | Для `om` — только источник шрифта; для `shell` — ещё и данные. |
| `HIBET_DATA_DIR` | То же, если нет `--data-dir`. |
| `$XDG_DATA_HOME/hibet/data` | Последний data-root. |
| В data-root нет `fonts/BabelStoneTibetan.ttf` | Идём к `fc-match` и системным каталогам. |
| Файл в data переименован | Не найдётся: имя `BabelStoneTibetan.ttf` зашито. |
| Нет ни одного шрифта | Только в сборке A: предупреждение. В P — молча. |
| Шрифт есть, но арт не влез | Fallback без предупреждения. |

Системный обход берёт только файлы, в имени которых есть `tibet` (регистр не
важен) и расширение `.ttf/.otf/.ttc`. Поэтому, например, `Jomolhari.ttf` сам
по себе не найдётся, если fontconfig не подскажет его через `fc-match`.

## 4. Предупреждение и коды возврата

Предупреждение (`Render.Art.warnIfNoFont`, только сборка A):

```
no Tibetan font: Braille art is off; run `hibet data update` or set HIBET_TIBETAN_FONT
```

- Печатается один раз за запуск `shell` или `om`.
- Только если шрифта нет **вообще**; если есть системный, предупреждения нет,
  даже когда данные не установлены.
- Не печатается, если шрифт есть, но арт не влез по высоте/ширине.
- В сборке P `warnIfNoFont` — заглушка, предупреждения нет.

| Команда | Условие | Код |
|---|---|---|
| `hibet om` | любой fallback | 0 |
| `hibet shell` | данные есть | 0 |
| `hibet shell` | данных нет | 1 (`DataNotFound`) |
| `hibet data status` / `show` | данных нет | 1 (`DataNotFound`) |
| `hibet data update` | сеть/проверка | 0 или 1, к арту не относится |

## 5. Что легко упустить

1. **Сборка без `-f ascii-art` не рисует арт никогда.** `cabal install
   exe:hibet` из README — как раз такая; `just install` включает флаг
   автоматически, если найдены `freetype2`/`harfbuzz`.
2. **`hibet om` не проверяет данные.** D− для него не ошибка; данные влияют
   только через шрифт.
3. **`hibet shell` без данных падает**, даже если шрифт есть.
4. **Высота важнее наличия шрифта.** `pickHeader` требует `строк ≤ height-4`;
   BabelStone-арт `ༀ` при 10 строках не влезает в 24-строчный терминал,
   поэтому после установки данных `hibet om` по умолчанию показывает ASCII,
   а не брайль.
5. **При перенаправлении stdout** терминал считается 80×24; арт может
   печататься в файл, если fit.
6. **Старые данные без шрифта** — не ошибка: `shell` работает, арт берётся из
   системы; если системы нет — предупреждение.
7. **`--data-dir` с одним `fonts/`** достаточен для `om`, но `shell` на нём
   упадёт: нужны `titles.toml` и `dicts/`.
8. **`data status`/`show` без данных** — `DataNotFound`, exit 1.
9. **`data update` сравнивает только версию.** Второй релиз с той же датой
    пользователи не увидят: `installFromRelease` вернёт «already up to date».

## 6. Известные шероховатости

- `defaultOmLines = 10` плохо сочетается с BabelStone: на стандартном
  24-строчном терминале `hibet om` после установки данных уходит в ASCII.
  Кандидат на правку: дефолт 6 или высота от `terminalHeight`.
- Предупреждение означает «нет шрифта вообще», а не «нет данных». Если нужен
  сигнал «данные не установлены, использую системный шрифт» — это отдельная
  строка, сейчас её нет.
- `isTibetanFont` проверяет только `ཀ`: шрифт с частичным покрытием пройдёт
  проверку, но отдельные стеки могут выпасть.
- Путь к data-шрифту зашит: `fonts/BabelStoneTibetan.ttf`.

## 7. Как проверялось

Проверено на `master` `05b6fa0`:

- `shell` с пустым `XDG_DATA_HOME`/`HIBET_DATA_DIR`: приглашение,
  `Dictionary data not found…`, exit 1 (и на запросе, и на `:q`);
- `om` с пустым XDG и системным Noto: брайль, 17 строк, без предупреждения;
- `om --data-dir hibet-data`: ASCII Om, 23 строки; с `HIBET_OM_LINES=6` —
  брайль, 13 строк;
- `HIBET_TIBETAN_FONT=/etc/hostname`: файл пропущен, системный шрифт, exit 0.

Не проверялось локально: среда без единого тибетского шрифта (нельзя убрать
системный Noto). Эта клетка матрицы выведена из кода и помечена как
fallback-поведение.

## 8. Пользовательские и developer-команды

Пользователь обновляет **установленные** данные:

```sh
hibet data status
hibet data update
hibet data install --from DIR
```

Мейнтейнер работает с **репозиторной** копией `hibet-data/`:

```sh
just update-dicts        # скачать upstream в hibet-data/dicts
just update-dicts-check  # read-only сверка с upstream
just check-manifest      # манифест против hibet-data/
just data-release        # собрать архив датасета
just data-publish        # опубликовать в GitHub Releases
just update-bounds       # обновить bounds зависимостей
```

Рецепты `update-dicts*` и `data-*` не трогают `$XDG_DATA_HOME/hibet/data`.
`update-dicts-check` завершается с кодом 1 при отличиях от upstream — это
сигнал обновить датасет, а не ошибка установки.
