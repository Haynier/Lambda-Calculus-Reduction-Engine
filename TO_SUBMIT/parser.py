import sys
import os
import time

test1 = 'two := fn f => fn x => f (f x);succ := fn n => (fn f => fn x => f (n f x));plus := fn n => (n succ);main := plus two two;'
test2 = 'zero := fn f => fn x => x;succ := fn n => (fn f => fn x => f (n f x));plus := fn n => fn m => (n succ m);times := fn n => fn m => (fn f => fn x => n (m f) x);two := succ (succ zero);main := plus (succ two) two;'
test3 = 'true := fn n => fn m => n; false := fn n => fn m => m; isZero := fn n => (fn x => false) true; not := fn n => n false true; pred := fn n => fn f => fn x => n ( fn g => fn h => h (g f))(fn u => x)(fn u => u); two := fn f => fn x => f (f x); minus := fn n => fn m => m pred n; less := fn n => fn m => not isZero minus m n; and := fn n => fn m => n m n; equal := fn n => fn m => and (isZero minus m n)(isZero minus n m); main := equal two two;'
test4 = 'power := fn n => fn m => (n m); main := power a b;'
test5 = 'pred := fn n => fn f => fn x => n (fn g => fn h => h (g f))(fn u => x)(fn u => u); two := fn f => fn x => f (f x); main := pred two;'
test6 = 'true := fn n => fn m => n;false := fn n => fn m => m;isZero := fn n => (fn x => false) true;not := fn n => n false true;pred := fn n => fn f => fn x => n ( fn g => fn h => h (g f))(fn u => x)(fn u => u);two := fn f => fn x => f (f x);one := pred two;minus := fn n => fn m => m pred n;less := fn n => fn m => not isZero minus m n;succ := fn n => (fn f => fn x => f (n f x));plus := fn n => (n succ);four := plus two two;Y := fn n => (fn m => n(m m))(fn m => n(m m));fibrec := fn n => Y(fn m => (less n two)(n)(plus(f(fibrec pred n))(f(fibrec minus n two))));main := fibrec four;'

def interpret(tks):
    functions = []
    fs = parseTerm(tks,functions)                # Parse the entry.
    tks.checkEOF()                      # Check if everything was consumed by the parse
    # newfs = functions
    # replaceAll(functions, newfs)
    f = open("reducable.txt", 'a')
    f.truncate(0)
    f.write(buildSmlStr(functions))
    f.close()
    print (buildSmlStr(functions))


def lookUpVar(x,env,err):
    for (y,v) in env:
        if y == x:
            return v
    raise RunTimeError("Use of variable '"+x+"'. "+err)

def buildSmlStr(functions):
    s = 'let\n'
    for i in range(0,len(functions)-1):
        s += 'val x' + str(i+1) + ' = ' + toString(functions[i][0]) + '\n'
        s += 'val t' + str(i+1) + ' = ' + toString(functions[i][1]) + '\n'
    s += 'val t = ' + toString(functions[len(functions)-1][1]) + '\n'
    s += 'val main = ' + buildMain(functions,0) +'\n'
    s += 'val value = norReduce main\nin\n   print (pretty value)\nend;'
    return s

def buildMain(functions, i):
    if functions[i][0] == 'main':
        if len(functions) != 1:
            return 't)'
        else:
            return "t"
    s = 'AP(LM(x' + str(i+1) + ','
    e = buildMain(functions, i+1) 
    s += e + ',t' + str(i+1)
    if i == 0:
        s += ')'
    else:
        s += '))' 
    return s


#
# ------------------------------------------------------------
#

#
# Exceptions
#
# These define the exception raised by the interpreter.
#
class TypeError(Exception):
    pass

class RunTimeError(Exception):
    pass

class ParseError(Exception):
    pass

class SyntaxError(Exception):
    pass

class LexError(Exception):
    pass
def replace(ast,target,x):
    if type(ast) == type(''):
        pass
    elif ast[0] != 'VA':
        ast[1] = replace(ast[1],target,x)
        ast[2] = replace(ast[2],target,x)
    else:
        if ast[1] == target:
            ast = x
            #can be easily changed to include this as part of VA instead of full replace
    return ast

def replaceAll(functions, changes):
    for change in changes:
        for f in functions:
            replace(change[1], f[0], f[1])

def toString(ast):
    if type(ast) != type([]):
        return '"' + str(ast) + '"'
    label = ast[0]
    if label in ['LM','AP']:
        e = toString(ast[1])
        ep = toString(ast[2])
        return label + "(" + e + ',' + ep + ')'
    else:
        e = toString(ast[1])
        return label + e
    





# <term> ::= fn <name> => <term>
# <term> ::= <term> <term>
# <term> ::= <name>

def parseTerm(tokens, functions):
    if tokens.nextIsName():
        x = tokens.eatName()
        if tokens.next() == ':=':
            tokens.eat(':=')
            e = parseTerm(tokens, functions)
            while tokens.next() != ';':
                f = parseTerm(tokens, functions)
                e = ['AP', e , f]
            tokens.eat(';')
            functions.append((x,e))
            while tokens.next() != 'eof':
                parseTerm(tokens, functions)
            

        elif tokens.next() == '(':
            tokens.eat('(')
            e = parseTerm(tokens, functions)
            while tokens.next() == '(':
                f = parseTerm(tokens,functions)
                e = ['AP', e , f]
            tokens.eat(')')
            x = ['AP', ['VA',x], e]

        elif tokens.nextIsName():
            x = ['VA', x]
            if tokens.nextIsName():
                e = parseTerm(tokens, functions)
                x = ['AP', x, e]
        else: # ) or ; dont eat them
            x = ['VA', x]
        return x


    elif tokens.next() == '(':
        tokens.eat('(')
        e = parseTerm(tokens,functions)
        while tokens.next() != ')':
            f = parseTerm(tokens,functions)
            e = ['AP', e , f]
        tokens.eat(')')
        return e
        
    elif tokens.next() == "fn":
        tokens.eat('fn')
        name = tokens.eatName()
        tokens.eat('=>')
        e = parseTerm(tokens, functions)
        return ['LM', name, e]





#
# Keywords, primitives, unary operations, and binary operations.
#
# The code below defines several strings or string lists used by
# the lexical analyzer (housed as class TokenStream, below).
#

RESERVED = ['fn', ':=',')']

# Characters that separate expressions.
DELIMITERS = '();'

# Characters that make up unary and binary operations.
OPERATORS = ':=>'


#
# LEXICAL ANALYSIS / TOKENIZER
#
# The code below converts ML source code text into a sequence
# of tokens (a list of strings).  It does so by defining the
#
#    class TokenStream
#
# which describes the methods of an object that supports this
# lexical conversion.  The key method is "analyze" which provides
# the conversion.  It is the lexical analyzer for ML source code.
#
# The lexical analyzer works by CHOMP methods that processes the
# individual characters of the source code's string, packaging
# them into a list of token strings.
#
# The class also provides a series of methods that can be used
# to consume (or EAT) the tokens of the token stream.  These are
# used by the parser.
#


class TokenStream:

    def __init__(self,src,filename="STDIN"):
        """
        Builds a new TokenStream object from a source code string.
        """
        self.sourcename = filename
        self.source = src # The char sequence that gets 'chomped' by the lexical analyzer.
        self.tokens = []  # The list of tokens constructed by the lexical analyzer.
        self.extents = []
        self.starts = []

        # Sets up and then runs the lexical analyzer.
        self.initIssue()
        self.analyze()
        self.tokens.append("eof")

    #
    # PARSING helper functions
    #

    def lexassert(self,c):
        if not c:
            self.raiseLex("Unrecognized character.")

    def raiseLex(self,msg):
        s = self.sourcename + " line "+str(self.line)+" column "+str(self.column)
        s += ": " + msg
        raise LexError(s)

    def next(self):
        """
        Returns the unchomped token at the front of the stream of tokens.
        """
        return self.tokens[0]

    def numTokens(self):
        return len(self.tokens)

    def advance(self):
        """
        Advances the token stream to the next token, giving back the
        one at the front.
        """
        tk = self.next()
        del self.tokens[0]
        # del self.starts[0]
        return tk

    def report(self):
        """
        Helper function used to report the location of errors in the
        source code.
        """
        lnum = self.starts[0][0]
        cnum = self.starts[0][1]
        return self.sourcename + " line "+str(lnum)+" column "+str(cnum)

    def eat(self,tk):
        """
        Eats a specified token, making sure that it is the next token
        in the stream.
        """
        if tk == self.next():
            return self.advance()
        else:
            where = self.report()
            err1 = "Unexpected token. "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected: '"+tk+"'. "
            raise SyntaxError(err1 + err2 + err3)


    def eatName(self):
        """
        Eats a name token, making sure that such a token is next in the stream.
        """
        if self.nextIsName():
            return self.advance()
        else:
            where = self.report()
            err1 = "Unexpected token. "
            err2 = "Saw: '"+self.next()+"'. "
            err3 = "Expected a name. "
            raise SyntaxError(err1 + err2 + err3)


    def checkEOF(self):
        """
        Checks if next token is an integer literal token.
        """
        if self.next() != 'eof':
            raise ParseError("Parsing failed to consume tokens "+str(self.tokens[:-1])+".")


    def nextIsName(self):
        """
        Checks if next token is a name.
        """
        tk = self.next()
        isname = tk[0].isalpha() or tk[0] =='_'
        for c in tk[1:]:
            isname = isname and (c.isalnum() or c == '_')
        return isname and (tk not in RESERVED)


    #
    # TOKENIZER helper functions
    #
    # These are used by the 'analysis' method defined below them.
    #
    # The parsing functions EAT the token stream, whereas
    # the lexcial analysis functions CHOMP the source text
    # and ISSUE the individual tokens that form the stream.
    #

    def initIssue(self):
        self.line = 1
        self.column = 1
        self.markIssue()

    def markIssue(self):
        self.mark = (self.line,self.column)

    def issue(self,token):
        self.tokens.append(token)
        self.starts.append(self.mark)
        self.markIssue()

    def nxt(self,lookahead=1):
        if len(self.source) == 0:
            return ''
        else:
            return self.source[lookahead-1]

    def chompSelector(self):
        self.lexassert(self.nxt() == '#' and self.nxt(2).isdigit())
        token = self.chompChar()
        token = '#'
        while self.nxt().isdigit():
            token += self.chompChar()
        self.issue(token)

    def chompWord(self):
        self.lexassert(self.nxt().isalpha() or self.nxt() == '_')
        token = self.chompChar()
        while self.nxt().isalnum() or self.nxt() == '_':
            token += self.chompChar()
        self.issue(token)
        
    def chompInt(self):
        ck = self.nxt().isdigit()
        self.lexassert(ck)
        token = ""
        token += self.chompChar()     # first digit
        while self.nxt().isdigit():
            token += self.chompChar() # remaining digits=
        self.issue(token)
        
    def chompString(self):
        self.lexassert(self.nxt() == '"')
        self.chompChar() # eat quote
        token = ""
        while self.nxt() != '' and self.nxt() != '"':
            if self.nxt() == '\\':
                self.chompChar()
                if self.nxt() == '\n':
                    self.chompWhitespace(True)
                elif self.nxt() == '\\':
                    token += self.chompChar()
                elif self.nxt() == 'n':
                    self.chompChar()
                    token += '\n'
                elif self.nxt() == 't':
                    self.chompChar()
                    token += '\t'
                elif self.nxt() == '"':
                    self.chompChar()
                    token += '"'
                else:
                    self.raiseLex("Bad string escape character")
            elif self.nxt() == '\n':
                self.raiseLex("End of line encountered within string")
            elif self.nxt() == '\t':
                self.raiseLex("Tab encountered within string")
            else:
                token += self.chompChar()

        if self.nxt() == '':
            self.raiseLex("EOF encountered within string")
        else:
            self.chompChar() # eat endquote
            self.issue('"'+token+'"')

    def chompComment(self):
        self.lexassert(len(self.source)>1 and self.source[0:1] == '(*')
        self.chompChar() # eat (*
        self.chompChar() #
        while len(self.source) >= 2 and self.source[0:1] != '*)':
            self.chomp()
        if len(self.source) < 2:
            self.raiseLex("EOF encountered within comment")
        else:
            self.chompChar() # eat *)
            self.chompChar() #

    def chomp(self):
        if self.nxt() in "\n\t\r ":
            self.chompWhitespace()
        else:
            self.chompChar()

    def chompChar(self):
        self.lexassert(len(self.source) > 0)
        c = self.source[0]
        self.source = self.source[1:]
        self.column += 1
        return c

    def chompWhitespace(self,withinToken=False):
        self.lexassert(len(self.source) > 0)
        c = self.source[0]
        self.source = self.source[1:]
        if c == ' ':
            self.column += 1
        elif c == '\t':
            self.column += 4
        elif c == '\n':
            self.line += 1
            self.column = 1
        if not withinToken:
            self.markIssue()
        
    def chompOperator(self):
        token = ''
        while self.nxt() in OPERATORS:
            token += self.chompChar()
        self.issue(token)

    #
    # TOKENIZER
    #
    # This method defines the main loop of the
    # lexical analysis algorithm, one that converts
    # the source text into a list of token strings.

    def analyze(self):
        while self.source != '':
            # CHOMP a string literal
            if self.source[0] == '"':
                self.chompString()
            # CHOMP a comment
            elif self.source[0:1] == '(*':
                self.chompComment()
            # CHOMP whitespace
            elif self.source[0] in ' \t\n\r':
                self.chompWhitespace()
            # CHOMP an integer literal
            elif self.source[0].isdigit():
                self.chompInt()
            # CHOMP a single "delimiter" character
            elif self.source[0] in DELIMITERS:
                self.issue(self.chompChar())
            # CHOMP an operator
            elif self.source[0] in OPERATORS:
                self.chompOperator()
            # CHOMP a reserved word or a name.
            else:
                self.chompWord()

def evalAll(files):
    try:
        # Load definitions from the specified source files.
        for fname in files:
            print("[opening "+fname+"]")
            f = open(fname,"r")
            src = f.read()
            tks = TokenStream(src,filename=fname)
            interpret(tks)
    except RunTimeError as e:
        print("Error during evaluation.")
        print(e.args[0])
        print("Bailing command-line loading.")
    except RunTimeError as e:
        print("Type error during evaluation.")
        print(e.args[0])
        print("Bailing command-line loading.")
    except SyntaxError as e:
        print("Syntax error during parse.")
        print(e.args[0])
        print("Bailing command-line loading.")
    except ParseError as e:
        print("Failed to consume all the input in the parse.")
        print(e.args[0])
        print("Bailing command-line loading.")
    except LexError as e:
        print("Bad token reached.")
        print(e.args[0])
        print("Bailing command-line loading.")

#
#  usage #1:
#    python3 miniml.py
#
#      - Waits for a MiniML expression after the prompt
#        evaluates it, and prints the resulting value
#
#
#  usage #2:
#    python3 miniml.py <file 1> ... <file n>
#
#      - this runs the interpreter on each of the listed
#        source .mml files
#
mtime = str(time.ctime(os.path.getmtime("./parser.py")))
if len(sys.argv) > 1:
    evalAll(sys.argv[1:])
else:
    test = test6
    print("Enter an expression:")
    print (test)
    interpret(TokenStream(test))