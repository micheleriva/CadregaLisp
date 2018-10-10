parser grammar JavaScriptParser;

options {
    tokenVocab=JavaScriptLexer;
    superClass=JavaScriptBaseParser;
}

program
    : sourceElements? EOF
    ;

sourceElement
    : Export? statement
    ;

statement
    : block
    | variableStatement
    | emptyStatement
    | expressionStatement
    | ifStatement
    | iterationStatement
    | continueStatement
    | breakStatement
    | returnStatement
    | withStatement
    | labelledStatement
    | switchStatement
    | throwStatement
    | tryStatement
    | debuggerStatement
    | functionDeclaration
    | classDeclaration
    ;

block
    : '{' statementList? '}'
    ;

statementList
    : statement+
    ;

variableStatement
    : varModifier variableDeclarationList eos
    ;

variableDeclarationList
    : variableDeclaration (',' variableDeclaration)*
    ;

variableDeclaration
    : (Identifier | arrayLiteral | objectLiteral) ('=' singleExpression)? // ECMAScript 6: Array & Object Matching
    ;

emptyStatement
    : SemiColon
    ;

expressionStatement
    : {notOpenBraceAndNotFunction()}? expressionSequence eos
    ;

ifStatement
    : If '(' expressionSequence ')' statement (Else statement)?
    ;


iterationStatement
    : Do statement While '(' expressionSequence ')' eos
    | While '(' expressionSequence ')' statement
    | For '(' expressionSequence? ';' expressionSequence? ';' expressionSequence? ')' statement
    | For '(' varModifier variableDeclarationList ';' expressionSequence? ';' expressionSequence? ')'
          statement
    | For '(' singleExpression (In | Identifier{p("of")}?) expressionSequence ')' statement
    | For '(' varModifier variableDeclaration (In | Identifier{p("of")}?) expressionSequence ')' statement
    ;

varModifier
    : Var
    | Let
    | Const
    ;

continueStatement
    : Continue ({notLineTerminator()}? Identifier)? eos
    ;

breakStatement
    : Break ({notLineTerminator()}? Identifier)? eos
    ;

returnStatement
    : Return ({notLineTerminator()}? expressionSequence)? eos
    ;

withStatement
    : With '(' expressionSequence ')' statement
    ;

switchStatement
    : Switch '(' expressionSequence ')' caseBlock
    ;

caseBlock
    : '{' caseClauses? (defaultClause caseClauses?)? '}'
    ;

caseClauses
    : caseClause+
    ;

caseClause
    : Case expressionSequence ':' statementList?
    ;

defaultClause
    : Default ':' statementList?
    ;

labelledStatement
    : Identifier ':' statement
    ;

throwStatement
    : Throw {notLineTerminator()}? expressionSequence eos
    ;

tryStatement
    : Try block (catchProduction finallyProduction? | finallyProduction)
    ;

catchProduction
    : Catch '(' Identifier ')' block
    ;

finallyProduction
    : Finally block
    ;

debuggerStatement
    : Debugger eos
    ;

functionDeclaration
    : Function Identifier '(' formalParameterList? ')' '{' functionBody '}'
    ;

classDeclaration
    : Class Identifier classTail
    ;

classTail
    : (Extends singleExpression)? '{' classElement* '}'
    ;

classElement
    : (Static | {n("static")}? Identifier)? methodDefinition
    | emptyStatement
    ;

methodDefinition
    : propertyName '(' formalParameterList? ')' '{' functionBody '}'
    | getter '(' ')' '{' functionBody '}'
    | setter '(' formalParameterList? ')' '{' functionBody '}'
    | generatorMethod
    ;

generatorMethod
    : '*'? Identifier '(' formalParameterList? ')' '{' functionBody '}'
    ;

formalParameterList
    : formalParameterArg (',' formalParameterArg)* (',' lastFormalParameterArg)?
    | lastFormalParameterArg
    | arrayLiteral
    | objectLiteral
    ;

formalParameterArg
    : Identifier ('=' singleExpression)?
    ;

lastFormalParameterArg 
    : Ellipsis Identifier
    ;

functionBody
    : sourceElements?
    ;

sourceElements
    : sourceElement+
    ;

arrayLiteral
    : '[' ','* elementList? ','* ']'
    ;

elementList
    : singleExpression (','+ singleExpression)* (','+ lastElement)?
    | lastElement
    ;

lastElement
    : Ellipsis Identifier
    ;

objectLiteral
    : '{' (propertyAssignment (',' propertyAssignment)*)? ','? '}'
    ;

propertyAssignment
    : propertyName (':' |'=') singleExpression
    | '[' singleExpression ']' ':' singleExpression
    | getter '(' ')' '{' functionBody '}'
    | setter '(' Identifier ')' '{' functionBody '}'
    | generatorMethod
    | Identifier
    ;

propertyName
    : identifierName
    | StringLiteral
    | numericLiteral
    ;

arguments
    : '('(
          singleExpression (',' singleExpression)* (',' lastArgument)? |
          lastArgument
       )?')'
    ;

lastArgument 
    : Ellipsis Identifier
    ;

expressionSequence
    : singleExpression (',' singleExpression)*
    ;

singleExpression
    : Function Identifier? '(' formalParameterList? ')' '{' functionBody '}'
    | Class Identifier? classTail
    | singleExpression '[' expressionSequence ']'
    | singleExpression '.' identifierName 
    | singleExpression arguments 
    | New singleExpression arguments?    
    | singleExpression {notLineTerminator()}? '++'  
    | singleExpression {notLineTerminator()}? '--'  
    | Delete singleExpression                       
    | Void singleExpression                         
    | Typeof singleExpression                       
    | '++' singleExpression                         
    | '--' singleExpression                         
    | '+' singleExpression                          
    | '-' singleExpression                          
    | '~' singleExpression                          
    | '!' singleExpression                                         
    | singleExpression ('*' | '/' | '%') singleExpression           
    | singleExpression ('+' | '-') singleExpression                  
    | singleExpression ('<<' | '>>' | '>>>') singleExpression        
    | singleExpression ('<' | '>' | '<=' | '>=') singleExpression    
    | singleExpression Instanceof singleExpression                   
    | singleExpression In singleExpression                            
    | singleExpression ('==' | '!=' | '===' | '!==') singleExpression   
    | singleExpression '&' singleExpression                             
    | singleExpression '^' singleExpression                             
    | singleExpression '|' singleExpression                             
    | singleExpression '&&' singleExpression                            
    | singleExpression '||' singleExpression                            
    | singleExpression '?' singleExpression ':' singleExpression        
    | singleExpression '=' singleExpression                             
    | singleExpression assignmentOperator singleExpression              
    | singleExpression TemplateStringLiteral                            
    | This                                                              
    | Identifier                                                        
    | Super                                                             
    | literal                                                           
    | arrayLiteral                                                      
    | objectLiteral                                                     
    | '(' expressionSequence ')'                                        
    | arrowFunctionParameters '=>' arrowFunctionBody                    
    ;

arrowFunctionParameters
    : Identifier
    | '(' formalParameterList? ')'
    ;

arrowFunctionBody
    : singleExpression
    | '{' functionBody '}'
    ;

assignmentOperator
    : '*='
    | '/='
    | '%='
    | '+='
    | '-='
    | '<<='
    | '>>='
    | '>>>='
    | '&='
    | '^='
    | '|='
    ;

literal
    : NullLiteral
    | BooleanLiteral
    | StringLiteral
    | TemplateStringLiteral
    | RegularExpressionLiteral
    | numericLiteral
    ;

numericLiteral
    : DecimalLiteral
    | HexIntegerLiteral
    | OctalIntegerLiteral
    | OctalIntegerLiteral2
    | BinaryIntegerLiteral
    ;

identifierName
    : Identifier
    | reservedWord
    ;

reservedWord
    : keyword
    | NullLiteral
    | BooleanLiteral
    ;

keyword
    : Break
    | Do
    | Instanceof
    | Typeof
    | Case
    | Else
    | New
    | Var
    | Catch
    | Finally
    | Return
    | Void
    | Continue
    | For
    | Switch
    | While
    | Debugger
    | Function
    | This
    | With
    | Default
    | If
    | Throw
    | Delete
    | In
    | Try

    | Class
    | Enum
    | Extends
    | Super
    | Const
    | Export
    | Import
    | Implements
    | Let
    | Private
    | Public
    | Interface
    | Package
    | Protected
    | Static
    | Yield
    ;

getter
    : Identifier{p("get")}? propertyName
    ;

setter
    : Identifier{p("set")}? propertyName
    ;

eos
    : SemiColon
    | EOF
    | {lineTerminatorAhead()}?
    | {closeBrace()}?
    ;