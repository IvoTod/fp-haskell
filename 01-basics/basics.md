<!--
    page_number:true
    *page_number:false
-->

Въведение в Haskell
Основни Типове
==


---

## Какво е Haskell

 - стриктно типуван
 - (lazy) pure functional language
 - c кратък, странен, но елегантен и удобен синтаксис

---

## Деклариране на променливи

```hs
i :: Int
i = 3

c :: Char
c = 'c'

d :: Double
d = 3.0
```

---

## Анатомия на дефинициите - сигнатурата


```hs
i :: Int
```

 - деклариране на променлива `i`
 - `::` оператор, който означава _има тип_
 - самият тип - Int

---

## Анатомия на дефинициите - променливата


```hs
i = 3
i = 4 -- compiltime error
```

 - в Haskell променливите не са "промeнливи", а дефиниция или наименование на стойност - един път зададена такава, тя не може да бъде мутирана или повторно задавана
 - операторът за присвояване е `=`

---

## Type Inference

Haskell сам може да открие типовете, без да му бъдат подадени. Програмата винаги е добре типувана (дори при изпуснати сигнатури) и ако има грешки или двусмислие ще бъдат хванати при компилация.
<br>

```hs
c' = 'e' -- inferred as Char
```

<br>**Добра практика**: винаги пишете сигнатурите на всички не-вгнездени дефиниции

---

## Основни типове

| Тип                        | Стойности              |
|----------------------------|------------------------|
| Boolean                    | `True`, `False`        |
| Int (32/64 bit)            | `0`, `1`, `2`, `3`, .. |
| Double                     | `0.0`, `0.5`, `1.33`   |
| Char                       | `'a'`, `'b'`, `'c'`    |
| Integer - произволно голям | `1234567890123456789..`       |

<br>**Забележка:** Синтаксисът на езика изисква типовете винаги да започват с главна буква. Променливите (било то константи или имена на функции), задължително започват с малка.

---

## Именоване на променливи

 - винаги започват с малка буква
 - състоят се от unicode букви, цифри и символът `'`
 - символът `'` (секонд) се използва са повторна/спомагателна дефиниция
 - по конвенция се ползва camelCase пред snake_case
<br>

```hs
fairRandom = 4

diceRoll   = 6
diceRoll'  = 3

theZCharacter = 'z'
```

---

## Булеви алгебра

Haskell използва познатите оператори за логичските операции.

`&&` - логическо и
`||` - логическо или
`not` - логическо отрицание, не е оператор, а функция

<br>

```hs
> True && False
False

> True || False
True

> not True
False
```

---

## Оператори за сравнение


| Оператор | Значение            |
|----------|---------------------|
| `==`     | равно / еднакво     |
| `/=`     | неравно / различно  |
| `<`      | по-малко            |
| `>`      | по-голямо           |
|`<=`      | по-малко или равно  |
|`>=`      | по-голямо или равно |


<br>**Забележка:** Сравнителните оператори в Haskell са същите като в повечето други езици, с изключение на различно - `/=`.

---

## Операции върху числа

Аритметичните операции са в голямата си част както в другите езици и притежават обичайните свойства - `+`, `-`, `*`

```hs
> 3 + 4 * 5
-- 3 + (4 * 5)
23

> 1 + (-2) - 3
0
```

<br>**Hint:** Обграждайте отрицателните числа в скоби (пр. `(-2)`). В противен случай на много места компилаторът ще се оплаква със странни грешки.

___

## Целочислено деление

Делението на цели числа става посредством функциите `div` и `quot`:

```hs
> quot 10 5
2

> quot (-4) 3
-1

> div (-4) 3
-2
```

<br>**Hint:** използвайте `quot` за (интуитивно) поведение като в C. Разликата между двата операторa е само при деление на числа с различни знаци.

---

## Нецелочислено деление

Операторът за нецелочислено деление е познатият `/`
```hs
> 3 / 4
0.75

> (-5) / 2
-2.5

> 10 / 2.5
4.0
```

---

## Работa с функции - извикване

 - извикването на функция става без скоби
 - параметрите се подавата в реда, в който функцията ги очаква, разделени с празно (whitespace)
 - извикването на функция е операцията с най-висок приоритет
 - ако е нужна промяна на приоритета се използват скоби

```hs
quot 4 2
> 2

quot 20 2 * 5
  -- (quot 20 2) * 5
> 50

quot 20 (2 * 5)
  -- quot 20 10
> 2
```

---

## Сигнатура на функция

```hs
quot :: Int -> Int -> Int
  |      |      |      |
 name   arg1   arg2  result
```

Сигнатурата на функция представлява изброени аргументите, които тя приема, последвани от резултатния тип, разделени със знака `->` помежду им.

**Как се чете:** `quot` е функция която приема два аргумнта от тип `Int` и връща като резултат `Int`.

---

```hs
parseNumber :: String -> Int
```

Е функция която приема `String` и връща `Int`.
<br>

```hs
mishmash :: Int -> Boolean -> String -> CustomType
```
```hs
> mishmash 10 False "string"
-- some value of type CustomType
```

---

## Дефиниране на функция

Синтаксис:
```hs
funtionName arg1 arg2 argN = definition
```

<br>Пример:
```hs
add3 :: Int -> Int -> Int -> Int
add3 x y z = x + y + z
```

---

## Декларативен стил - pattern matching

Hasekell набляга на така наречение декларативен стил на програмиране. Целта е имплементиране на функциите като дефиниции за различни случаи, а не като поредица от действия. Този подход прилича много на часовете по математика, където се описват какви са свойствата на дадена функция.

Средството, което Haskell предоставя, се нарича pattern matching.

---

## Pattern matching - стойности и променливи

Формулата за функцията на Fibonacci е:
<pre>
F<sub>0</sub> = 0
F<sub>1</sub> = 1
F<sub>n</sub> = F(n-1) + F(n-2)
</pre>

<br>Транслирана в код на Haskell:
```hs
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

---

## Pattern matching - стойности и променливи ...

```hs
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

<br>`fib 0`, `fib 1` правят pattern matching по съответна стойност. `0` и `1` са patterns - съответните константи.
<br>`fib n` - тук pattern е именоването на входа, т.е. декларираме че всеки друг вход ще наричаме `n`. Или погледнато по-просто, казваме че входът е променливата `n`, ако не е някой от предходните случаи.

---

<br>**Важно:** Отделните patterns се проверяват в реда, в който са дефинирани.

```hs
encodeBoolean :: Int -> Int
encodeBoolean 0 = 0
encodeBoolean n = 1

encodeBoolean' :: Int -> Int
encodeBoolean' n = 1
encodeBoolean' 0 = 0
```

```hs
> encodeBoolean 0
0

> encodeBoolean' 0
1
```

---

## Декларативнен стил - guards

В Haskell (и останалите функционални езици) липсват _statements_ (твърдения). Програмите се изграждат като вгнездени _expressions_ (изрази). Изразите винаги връщат резултат, за разлика от твърденията, чието основно предназначение е извършване на страничен ефект.

Това прави използването на конструкции като `if` неудобни, понеже задължително трябва да бъде съпроводен от `else` и двата клона да продуцират резултат. Идиоматичният Haskell използва механизъм, много подобен на Допустими Стойности като синтаксис, който едновременно решава горния проблем и предоставя приятен външен вид.

---

## Пример - guards

Формулата за функцията на Fibonacci, ДС стил:
<pre>
       ┃ 0                <i>ако n == 0</i>
ƒ(n) = ┫ 1                <i>ако n == 1</i>
       ┃ ƒ(n-1) + ƒ(n-2)  <i>ако n > 1</i>
</pre>

<br>Транслирана в код на Haskell:
```hs
fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | n >  1 = fib (n - 1) + fib (n - 2)
```

---

## Пример 2 - signum

Използването на guards е особено удобно, когато pattern matching не е възможен.

<br>

```hs
signum :: Int -> Int
signum x | x < 0  = -1
         | x == 0 = 0
         | x > 0  = 1
```

---

## Пример 3 - преразгледан fib

Крайното условие на guards е препоръчително да е най-общият случай, като преди това са проверени конкретните изключения. Обикновено се ползва константата `otherwise`, но тя е просто псевдоним за `True`.

<br>Дефиниция на `fib`, която поддържа и отрицателни числа:
```hs
fib' :: Int -> Int
fib' n | n <= 0    = 0
       | n == 1    = 1
       | otherwise = fib' (n - 1) + fib' (n - 2)
```

---

## guards - генерализиран синтаксис

```hs
funName param
    | bool-condition    = specific-definition
     ....
    | bool-condition-n  = specific-definition-n
    | otherwise         = most-general-definition
```

<br>**Забележка:** Критично важно е правилното подравняване! Компилаторът използва индентацията като маркер, че дефиницията на функцията продължава!

---

## Списъци (листове)

Едносвързаният списък е основна структира във функционалните езици.

<br>Причини:
 - евтино, без мутация добавяне в началото
 - позволява серийна работа (алтернатива на итератор)
 - натурално приляга на рекурсивни решения

---

## Списъци - синтаксис

Списъците изглеждат синтактично като масиви в другите езици, но имат съответните runtime характеристики.
<br>

```hs
emptyList :: [Int]
emptyList = []

listOfNums :: [Int]
listOfNums = [1, 2, 3, 4]
```

<br>За разлика от в C-образните езици, синтаксисът за типът списък в Haskell не е `Int[]`, а е `[Int]`, a литералите използват познатия синтаксис.

---

## Списъци - добавяне

Операцията по добавяне на нов елемент отпред на списъка се нарича `cons`. Произлиза от думата `construct` и е наследство от Lisp - първият език, който ги вкарва в масова употреба.

Haskell притежава удобен синтаксис - `value : list`
```hs
> 1 : [2, 3, 4]
[1, 2, 3, 4]

> 1 : 2 : 3 : 4 : []
[1, 2, 3, 4]
```

---

## Списъци

Операцията `cons` е дясно асоциативна и с нисък приоритет (т.е. скоби не са нужни).

<br>

```hs
1:2:3:4:[] == 1:(2:(3:(4:[]))) == [1, 2, 3, 4]

> 1 + 2 : 3 : quot 4 2 : []
  -- (1 + 2) : 3 : (quot 4 2) : []
[3, 3, 2]

```

<br>**Важно:** Всъщност синтаксисът `[1, 2, 3, 4]` е само удобство (_syntactic suggar_), който компилаторът свежда до `1:2:3:4:[]`.

---

## Списъци - други операции

Трите базови операции върху едносвързан списък са:
 - добавяне на елемент в началото (`cons`, `:`)
 - проверка дали списъкът е празен
 - разделяне на списъка на глава и останала част

---

## Pattern matching - листове

Най-удобният начин за проверка дали списъкът е празен и разделянето му на подчасти е с pattern matching. Синтаксисът е аналогичен като синтаксиса за конструиране.
<br>

```hs
firstOrDefault :: Int -> [Int] -> Int
firstOrDefault def []     = def -- empty list
firstOrDefault def (x:xs) = x

restOrEmpty :: [Int] -> [Int]
restOrEmpty []     = []
restOrEmpty (x:xs) = xs
```

---

## Пример - дължина на списък

Дължина на списък:
```hs
intListLength :: [Int] -> Int
intListLength []     = 0
intListLength (x:xs) = 1 + intListLength xs

intListLength []
> 0

intListLength [1, 2, 3, 4]
> 4
```

<br>**Hint:** Hasekell има вградена функция за изчисляване дължината на списък от прозволен тип - `length`.

---

## Типът String

Типът стринг е просто списък от `Char`. Логически е доста близък до този в C, с разликата че в Haskell не е null терминиран масив, а едносвързан списък.

<br>

```hs
str :: String
str = "string"

str' :: [Char]
str' = "string"

str'' :: [Char]
str'' = 's':'t':'r':'i':'n':'g':[]

str == str' && str == str''
> True
```

---

## Pattern magic

Patterns могат да гнездят! Те не са ограничени до едно ниво - с тях могат да се създават произволни комбинации (дори и с guards)!

<br>

```hs
-- Look a list of lists of Int
isFirstDoubleZeroArray :: [[Int]] -> Bool
isFirstDoubleZeroArray ([0,0] : rest) = True
isFirstDoubleZeroArray xs             = False
 
isThirdEven :: [Int] -> Boolean
isThirdEven (x:y:z:rest) | even z = True
isThirdEven xs                    = False
```

---

## Pattern magic - strings

```hs
isKey :: String -> Boolean
isKey "key"    = True
isKey ('K':xs) = True
isKey _        = False

reverseKeys :: String -> Boolean
reverseKeys (x:y:z:rest)
    | isKey (x:y:z:[]) = z:y:x:(reverseKeys rest)

reverseKeys (x:rest)   = x : reverseKeys rest
reverseKeys []         = []

reverseKeys "This is a Killer key"
> "This is a liKer yek"
```

---

## GHCi

Това е интерактивен Haskell REPL. Повечето програми, които ще пишем, няма да компилираме до executables, а ще ги използваме през GHCi с цел по-лесна, интерактивна и бърза работа. Haskell е известен с относително сложния си модел на IO. Нека това не ви безпокои, нито отказва. Ще стигнем до там, но преди това трябва да се заредим с по-базовите средства и концепции :)

---

## GHCi - команди

`:l <source_path>` - служи за зареждане на сорс файла достъпен на съответния път

```text
:l Demo.hs
[1 of 1] Compiling Demo ( Demo.hs, interpreted )
Ok, modules loaded: Demo.
```

<br>`:r` - презареждане на вече заредени сорсове
```text
:r
[1 of 1] Compiling Demo ( Demo.hs, interpreted )
Ok, modules loaded: Demo.
```

---

## GHCi - команди

`:t <expression>` - показва типа на съответния _expression_

```text
> :t True
True :: Bool

> :t 10
10 :: Num a => a

> :t (2 + 2)
(2 + 2) :: Num a => a

> :t words
words :: String -> [String]
```

---

**Важно:** не се притеснявайте, че типът на числата не е `Int`, а `Num a => a`. Това е така понеже Haskell има много гъвкава числова система и позволява дефинирането на собсвтени числа, които работят натурално в целия език. С напредване на курса ще видим как това става в детайли. За сега важното е, че `Num a => a` е някакво число. Малките букви ще ги видите и на други места, те могат да бъдат тълкувани, като "работя със всеки тип, който за удобство съм нарекал `a`" или друго яче казано - не се интересувам от конкретния тип. Отново, за момента просто ги пропускайте.

---

## GHCi - команди

`:i <identifier>` - показа информация за дадената променлива с име _identifier_, какъв й е типът и къде е дефинирана

<br>

```text
> :i words

words :: String -> [String]
  	-- Defined in ‘base-4.8.2.0:Data.OldList’
```

---

## GHCi - други полезни команди

Тези команди са общовалидни за терминала и са част от така нареченият emacs mode. Повечето приложения, които има някаква форма на интеракция, използват именно тези shortcuts.

---

| Command  | Usage |
|----------|-------|
| tab      | допълване / completion |
| Ctrl + r | reverse search - търси в историята на вече въвежданите команди |
| Ctrl + a | изместване на курсора в началото на реда |
| Ctrl + e | изместване на курсора в края на реда     |
| Ctrl + w | изтриване на думата, при/преди курсора   |
| Ctrl + d | спира текущия shell / command line програма |
| Ctrl + l | почиства екрана |

---

## GHCi - последни думи

Не използвайте GHCi за големи дефиниции (примерно на функции). GHCi не работи добре за въвеждане на многоредов код, затова е препоръчително кодът да е във файл, а в GHCi само да бъде презареждан и отделните функции тествани. Разглеждане на типове, малки експерименти и тестове са силните страни на всеки REPL. 