package cup.example;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import java.lang.*;
import java.io.InputStreamReader;

%%

%class Lexer
%implements sym
%public
%unicode
%line
%column
%cup
%char
%{
	

    public Lexer(ComplexSymbolFactory sf, java.io.InputStream is){
		this(is);
        symbolFactory = sf;
    }
	public Lexer(ComplexSymbolFactory sf, java.io.Reader reader){
		this(reader);
        symbolFactory = sf;
    }
    
    private StringBuffer sb;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;

    public Symbol symbol(String name, int code){
		return symbolFactory.newSymbol(name, code,
						new Location(yyline+1,yycolumn+1, yychar), // -yylength()
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength())
				);
    }
    public Symbol symbol(String name, int code, String lexem){
	return symbolFactory.newSymbol(name, code, 
						new Location(yyline+1, yycolumn +1, yychar), 
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength()), lexem);
    }
    
    protected void emit_warning(String message){
    	System.out.println("scanner warning: " + message + " at : 2 "+ 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
    
    protected void emit_error(String message){
    	System.out.println("scanner error: " + message + " at : 2" + 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
%}

Newline    = \r | \n | \r\n
Whitespace = [ \t\f] | {Newline}
Number     = [0-9]+

/* comments */
Comment = {TraditionalComment}
TraditionalComment = "(*" {CommentContent} \*+ ")"
CommentContent = ( [^*] | \*+[^*/] )*

ident = ([:jletter:] | "_" ) ([:jletterdigit:] | [:jletter:] | "_" )*
Boolean = (true) | (false)

%eofval{
    return symbolFactory.newSymbol("EOF",sym.EOF);
%eofval}

%state CODESEG

%%  

<YYINITIAL> {
  
  /* whitespaces */
   {Whitespace} {                              }
  
  /* comments */
  {Comment}   	{ }
  
  /* keywords */
 
  "and"          { return symbolFactory.newSymbol("AND", AND); }
  "array"        { return symbolFactory.newSymbol("ARRAY", ARRAY); }
  "begin"        { return symbolFactory.newSymbol("BEGIN", BEGIN); }
  "by"           { return symbolFactory.newSymbol("BY", BY); }
  "div"          { return symbolFactory.newSymbol("DIV", DIV); }
  "do"           { return symbolFactory.newSymbol("DO", DO); }
  "else"         { return symbolFactory.newSymbol("ELSE", ELSE); }
  "elsif"        { return symbolFactory.newSymbol("ELSIF", ELSIF); }
  "end"          { return symbolFactory.newSymbol("END", END); }
  "exit"         { return symbolFactory.newSymbol("EXIT", EXIT); }
  "for"          { return symbolFactory.newSymbol("FOR", FOR); }
  "if"           { return symbolFactory.newSymbol("IF", IF); }
  "is"           { return symbolFactory.newSymbol("IS", IS); }
  "loop"         { return symbolFactory.newSymbol("LOOP", LOOP); }
  "mod"          { return symbolFactory.newSymbol("MOD", MOD); }
  "not"          { return symbolFactory.newSymbol("NOT", NOT); }
  "of"           { return symbolFactory.newSymbol("OF", OF); }
  "or"           { return symbolFactory.newSymbol("OR", OR); }
  "procedure"    { return symbolFactory.newSymbol("PROCEDURE", PROCEDURE); }
  "program"      { return symbolFactory.newSymbol("PROGRAM", PROGRAM); }
  "read"         { return symbolFactory.newSymbol("READ", READ); }
  "record"       { return symbolFactory.newSymbol("RECORD", RECORD); }
  "return"       { return symbolFactory.newSymbol("RETURN", RETURN); }
  "then"         { return symbolFactory.newSymbol("THEN", THEN); }
  "to"           { return symbolFactory.newSymbol("TO", TO); }
  "type"         { return symbolFactory.newSymbol("TYPE", TYPE); }
  "var"          { return symbolFactory.newSymbol("VAR", VAR); }
  "while"        { return symbolFactory.newSymbol("WHILE", WHILE); }
  "write"        { return symbolFactory.newSymbol("WRITE", WRITE); }

   /* operators */ 

  ":="           { return symbolFactory.newSymbol("ASSIGN", ASSIGN); }
  "+"            { return symbolFactory.newSymbol("PLUS", PLUS); }
  "-"            { return symbolFactory.newSymbol("MINUS", MINUS); }
  "*"            { return symbolFactory.newSymbol("TIMES", TIMES); }
  "/"            { return symbolFactory.newSymbol("DIVIDE", DIVIDE); }
  "<"            { return symbolFactory.newSymbol("LT", LT); }
  "<="           { return symbolFactory.newSymbol("LE", LE); }
  ">"            { return symbolFactory.newSymbol("GT", GT); }
  ">="           { return symbolFactory.newSymbol("GE", GE); }
  "="            { return symbolFactory.newSymbol("EQ", EQ); }
  "<>"           { return symbolFactory.newSymbol("NEQ", NEQ); }

  /* delimiters */ 

  ":"            { return symbolFactory.newSymbol("COLON", COLON); }
  ";"            { return symbolFactory.newSymbol("SEMI", SEMI); }
  ","            { return symbolFactory.newSymbol("COMMA", COMMA); }
  "."            { return symbolFactory.newSymbol("DOT", DOT); }
  "("            { return symbolFactory.newSymbol("LPAREN", LPAREN); }
  ")"            { return symbolFactory.newSymbol("RPAREN", RPAREN); }
  "["            { return symbolFactory.newSymbol("LBRACKET", LBRACKET); }
  "]"            { return symbolFactory.newSymbol("RBRACKET", RBRACKET); }
  "{"            { return symbolFactory.newSymbol("LBRACE", LBRACE); }
  "}"            { return symbolFactory.newSymbol("RBRACE", RBRACE); }
  "[<"           { return symbolFactory.newSymbol("LARROW", LARROW); }
  ">]"           { return symbolFactory.newSymbol("RARROW", RARROW); }

  /* literal constants */ 
  
  "integer"      { return symbolFactory.newSymbol("INTEGER_TYPE", INTEGER_TYPE); }
  "real"         { return symbolFactory.newSymbol("REAL_TYPE", REAL_TYPE); }
  "string"       { return symbolFactory.newSymbol("STRING_TYPE", STRING_TYPE); }
  "boolean"      { return symbolFactory.newSymbol("BOOLEAN_TYPE", BOOLEAN_TYPE); }
  
  /* numeric and boolean expressions */ 

  {Number}       { return symbolFactory.newSymbol("NUMBER", NUMBER, Integer.parseInt(yytext())); }
  {Boolean}      { return symbolFactory.newSymbol("BOOLEAN", BOOLEAN); }
  
  
  "\""			 { return symbolFactory.newSymbol("DQUOTE", DQUOTE); }
  "!"			 { return symbolFactory.newSymbol("EXCLAMATION", EXCLAMATION); }
  
  /* identifier */
  
  {ident}        { return symbolFactory.newSymbol("IDENTIFIER", IDENTIFIER, yytext()); }
  
  
}



// error fallback
.|\n          { emit_warning("Unrecognized character '" +yytext()+"' -- ignored"); }
