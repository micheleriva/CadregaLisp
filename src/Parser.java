import org.antlr.v4.runtime.*;

public abstract class CadregaParser extends Parser {
    public JavaScriptBaseParser(TokenStream input) {
        super(input);
    }

    protected boolean p(String str) {
        return prev(str);
    }

    protected boolean prev(String str) {
        return _input.LT(-1).getText().equals(str);
    }

    protected boolean n(String str) {
        return next(str);
    }

    protected boolean next(String str) {
        return _input.LT(1).getText().equals(str);
    }

    protected boolean notLineTerminator() {
        return !here(CadregaParser.LineTerminator);
    }

    protected boolean notOpenBraceAndNotFunction() {
        int nextTokenType = _input.LT(1).getType();
        return nextTokenType != CadregaParser.OpenBrace && nextTokenType != CadregaParser.Function;
    }

    protected boolean closeBrace() {
        return _input.LT(1).getType() == CadregaParser.CloseBrace;
    }
    
    private boolean here(final int type) {
        int possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 1;
        Token ahead = _input.get(possibleIndexEosToken);

        return (ahead.getChannel() == Lexer.HIDDEN) && (ahead.getType() == type);
    }
    
    protected boolean lineTerminatorAhead() {

        int possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 1;
        Token ahead = _input.get(possibleIndexEosToken);

        if (ahead.getChannel() != Lexer.HIDDEN) {
            return false;
        }

        if (ahead.getType() == CadregaParser.LineTerminator) {
            return true;
        }

        if (ahead.getType() == CadregaParser.WhiteSpaces) {
            possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 2;
            ahead = _input.get(possibleIndexEosToken);
        }

        String text = ahead.getText();
        int type = ahead.getType();

        return (type == CadregaParser.MultiLineComment && (text.contains("\r") || text.contains("\n"))) ||
                (type == CadregaParser.LineTerminator);
    }
}