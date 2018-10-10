import org.antlr.v4.runtime.*;

import java.util.Stack;


public abstract class CadregaLexer extends Lexer{

    private Stack<Boolean> scopeStrictModes = new Stack<Boolean>();

    private Token lastToken = null;
    private boolean useStrictDefault = false;
    private boolean useStrictCurrent = false;

    public CadregaLexer(CharStream input) {
        super(input);
    }

    public boolean getStrictDefault() {
        return useStrictDefault;
    }

    public void setUseStrictDefault(boolean value) {
        useStrictDefault = value;
        useStrictCurrent = value;
    }

    public boolean IsSrictMode() {
        return useStrictCurrent;
    }

    @Override
    public Token nextToken() {
        Token next = super.nextToken();

        if (next.getChannel() == Token.DEFAULT_CHANNEL) {
            this.lastToken = next;
        }

        return next;
    }

    protected void ProcessOpenBrace() {
        useStrictCurrent = scopeStrictModes.size() > 0 && scopeStrictModes.peek() ? true : useStrictDefault;
        scopeStrictModes.push(useStrictCurrent);
    }

    protected void ProcessCloseBrace() {
        useStrictCurrent = scopeStrictModes.size() > 0 ? scopeStrictModes.pop() : useStrictDefault;
    }

    protected void ProcessStringLiteral() {
        if (lastToken == null || lastToken.getType() == CadregaLexer.OpenBrace)
        {
            String text = getText();
            if (text.equals("\"use strict\"") || text.equals("'use strict'"))
            {
                if (scopeStrictModes.size() > 0)
                    scopeStrictModes.pop();
                useStrictCurrent = true;
                scopeStrictModes.push(useStrictCurrent);
            }
        }
    }

    protected boolean IsRegexPossible() {
                                       
        if (this.lastToken == null) {
          return true;
        }
        
        switch (this.lastToken.getType()) {
            case CadregaLexer.Identifier:
            case CadregaLexer.NullLiteral:
            case CadregaLexer.BooleanLiteral:
            case CadregaLexer.This:
            case CadregaLexer.CloseBracket:
            case CadregaLexer.CloseParen:
            case CadregaLexer.OctalIntegerLiteral:
            case CadregaLexer.DecimalLiteral:
            case CadregaLexer.HexIntegerLiteral:
            case CadregaLexer.StringLiteral:
            case CadregaLexer.PlusPlus:
            case CadregaLexer.MinusMinus:
                return false;
            default:
                return true;
        }
    }
}