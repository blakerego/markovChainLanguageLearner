
This started out as a homework assignment for an artificial class I took at Harvard's extension school in 2009. 

The idea is to build a frequency table of what letters proceed other letters in different languages. Based on these tendencies, we can test a single line and the program will be able to distinguish the language using a markov chain analysis.

Dialect: 
This program is written in ANSI Common Lisp.

Usage: 
0) load the file: 
(load "languageLearner.l")

1) once the file is loaded, you can use it as follows from a clisp interpretter: 

(learn "[language name]")

Note: this assumes you have a file named [language name] that has a significant sample of the language. In this repository, there are several sample language files (english, french, german, italian, etc.) with a news article in the designated language. 

2) Repeat step (1) with several languages.

3) Create a test file with several lines of languages you would like to test. The program will tell you which language the line is, and let you know the degree of certainty - 

I love you.: 
This line is most likely english 
The probability of this is 0.8328989 


Je t'aime.: 
This line is most likely french 
The probability of this is 0.82981193 


Te amo.: 
This line is most likely portuguese 
The probability of this is 0.6070289 


Ich liebe dich.: 
This line is most likely german 
The probability of this is 0.99999833 


Eu te amo.: 
This line is most likely portuguese 
The probability of this is 0.96111965 


Ti amo.: 
This line is most likely italian 
The probability of this is 0.6207875 


: 
This line is most likely english 
