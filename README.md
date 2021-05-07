# A2
## Lexer for these tokens...  
- punctuation
- number literals
- string literals
- keywords  
- names

# A3
## Parser following this grammar...  
program     := exprList  
exprList    := expr optExprList   
optExprList := ɛ | exprList  
expr        := atom | invocation  
atom        := NAME | STRING | number  
number      := INT | FLOAT  
invocation  := OPAREN exprList CPAREN  

# A5  
## simple interpreter  

# A6   
## 	environment-passing interpreter
 
# A7  
## OOP interpreter (not finished)
