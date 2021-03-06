# markov [![Build Status](https://travis-ci.org/kmein/markov.svg?branch=master)](https://travis-ci.org/kmein/markov)
Markov-chain based text generator inspired by a Python text generator from
chapter 7 – _Ich verstehe nur Bahnhof – der Goethe-Generator_ – of _Coding for
Fun mit Python_ by Lars Heppert.

## Building
Before attempting to build, make sure you have
[`stack`](https://haskellstack.org) installed, then run
the following command in the cloned repository.
```
$ stack build
```

## Usage

```
Usage: markov PATH [-l|--length LENGTH]
  Generate a random text based on a corpus.

Available options:
  -h,--help                Show this help text
  PATH                     File containing the corpus
  -l,--length LENGTH       Length of the text (default 1000)
```

## Example

```
$ stack exec markov -- twain_tom-sawyer.txt --length=200
joined the weeping mourners in a paper. He unrolled it. Huckleberry viewed
it wistfully. The temptation was too late. Ah, if he came into the Elysian
world, of which she has had such bright dreams. How fairy-like does everything
appear to her ear. And then it was that stranger's voice--the stranger of the
stile leading into Widow Douglas' splendid hospitality was a fine round of
applause and said it was beating. Suddenly there was nothing for him by the
slaughter-house. Don't you remember what I was so brimful of exultation that
was wanted. He still said it was by far the most frivolous and the tears came
to a bewitching spring, whose basin was incrusted with a dull woody accent, and
within five minutes he was dressed and down-stairs, feeling sore and drowsy. The
family were still in her hot resentment, that she had to keep mum. But let's
swear again, anyway. It's more surer." "I'm agreed." So they swore again with
a startled look in them-- "My! I never heard the talk went on, till at last it
considered a ship and therefore belongs the

$ stack exec markov -- goethe_die-leiden-des-jungen-werther-2.txt --length=100
Schmerzens, küßten ihm die Pistolen." --"Ich lasse ihm glückliche Reise
wünschen." sagte er zum Jungen. --Das fiel auf sie wie ein verlechter
Eimer. Ich habe manchen sterben sehen; aber so eingeschränkt ist die
Zeit, da er sich bald wieder und trug dem Amtmann vorstellen wollte, schon
halblaut auszusprechen. Als er sich in ewigem Unfrieden lebte, schien ihm
auch der Zustand andrer nur bedenklicher und verworrner, er glaubte, das
schöne Herz. War es Unwille über seine Verwegenheit? War es eine herzliche,
freundschaftliche Schonung, weil sie wußte, wie viel fehlgeschlagenen
Hoffnungen, und dann--ich ging in der Stube auf und ab, und weil Geburtsort
```
