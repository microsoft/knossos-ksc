
# Pretty printer for Knossos s-expressions.
# Uses the Wadler constructors from PrettyPrinter
# http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

from prettyprinter import register_pretty

# Wadler constructors
from prettyprinter.doc import (
    #flat_choice,
    annotate,   # annotations affect syntax coloring
    concat,
    group,      # make what's in here a single line if enough space
    nest,       # newlines within here are followed by indent + arg
    align,
    hang,
    #NIL,
    LINE,       # Space or Newline
    #SOFTLINE,  # nothing or newline
    HARDLINE    # newline
)

from prettyprinter.prettyprinter import (
    Token, 
    LPAREN, RPAREN,
    pretty_dispatch
)

from prettyprinter.utils import intersperse

# Local imports
from ksc.type import Type
from ksc.expr import Expr, Def, EDef, Rule, Const, Var, Lam, Call, Let, If, Assert

# These are primarily to enable syntax highlighting -- 
# it would otherwise be fine to just use the string/doc "s" 
def pp_reserved(s):
    return annotate(Token.NAME_BUILTIN, s)

def pp_function_name(s):
    return annotate(Token.NAME_FUNCTION, s)

def pp_variable(s):
    return annotate(Token.NAME_VARIABLE, s)

def pp_string(v):
    r = repr(v) # single quotes
    r = '"' + r[1:-1] + '"'
    return r

def parens(hangindent, *docs):
    return group(hang(hangindent, concat([
        LPAREN,
        *docs,
        RPAREN
    ])))

def parens_interline(hangindent, *docs):
    return parens(hangindent, *intersperse(LINE, docs))

def interline(*docs):
    return concat(intersperse(LINE, docs))

# @singledispatch
# def pp_as_s_exp(expr, ctx):
#     """
#     Expression to string, pretty-printed in round-trippable s_exp
#     """
#     # Default implementation, for types not specialized below
#     return str(expr)

# @pp_as_s_exp.register(Def)
# def _(ex, indent):
#     indent += 1
#     return "def " + ex.name + "(" + pystr_intercomma(indent, ex.args) + ") -> " \
#            + pystr(ex.return_type, indent) + ":" \
#            + nl(indent+1) + pystr(ex.body, indent+1)

# Declare pretty printer for our Expressions
@register_pretty(Expr)
def pretty_Expr(ex, ctx):
    pp = lambda v: pretty_dispatch(v, ctx)

    # Def
    if isinstance(ex, Def):
        return parens(2,
                    pp_reserved("def"), ' ',
                    hang(0, concat([pp_function_name(ex.name), LINE,
                                    pp(ex.return_type), LINE,
                                    parens_interline(1, *map(pp, ex.args))])), HARDLINE,
                    pp(ex.body))

    if isinstance(ex, EDef):
        return parens(2,
                    pp_reserved("edef"), LINE,
                    pp_function_name(ex.name), LINE,
                    pp(ex.return_type), LINE,
                    parens_interline(1, *map(pp, ex.arg_types)))

    if isinstance(ex, Rule):
        return parens(2,
                    pp_reserved("rule"), LINE,
                    pp_string(ex.name), LINE,
                    parens_interline(1, *map(pp, ex.args)), LINE,
                    pp(ex.e1), LINE,
                    pp(ex.e2))

    # Variable gets syntax-highlighted name
    if isinstance(ex, Var):
        v = pp_variable(ex.name)
        if ex.decl:
            return parens(1, v, ' : ', pp(ex.type_))
        else:
            return v

    # Constants just use str()
    if isinstance(ex, Const):
        if isinstance(ex.value, str):
            return f"\"{ex.value}\""
        else:
            return repr(ex.value)

    # Let bindings are printed with no indent, so e.g. print as
    # (let (a  1)
    # (let (b  2)
    # (use a b)))
    # This saves horizontal space, and may be easier to read
    # TODO: gather lets so above is let ((a 1) (b 2))?
    if isinstance(ex, Let):
        if isinstance(ex.vars, list):
            vars = parens_interline(2, *map(pp, ex.vars))
        else:
            vars = pp(ex.vars)
        return parens(0,
                    pp_reserved("let"),
                    ' ',
                    hang(0, parens(2, vars, LINE, pp(ex.rhs))),
                    HARDLINE,
                    pp(ex.body))

    # If cond t_body f_body
    if isinstance(ex, If):
        return nest(ctx.indent, parens(2,
            pp_reserved("if"), ' ', pp(ex.cond), HARDLINE, 
            pp(ex.t_body), HARDLINE, 
            pp(ex.f_body)))

    # Assert cond body
    if isinstance(ex, Assert):
        return nest(ctx.indent, parens(2,
            pp_reserved("assert"), ' ', pp(ex.cond), HARDLINE, 
            pp(ex.body)))

    # Call name args
    if isinstance(ex, Call):
        return nest(ctx.indent, 
                    parens_interline(2,
                                pp_function_name(ex.name),
                                *map(pp, ex.args)))

    # Lambda arg body
    if isinstance(ex, Lam):
        return nest(ctx.indent, parens(2,
            pp_reserved("lam"),
            ' ',
            pp(ex.arg),
            nest(ctx.indent, concat([HARDLINE, pp(ex.body)]))
        ))

    raise NotImplementedError("unimp: ", ex)


# Declare printer for our Type
@register_pretty(Type)
def pretty_Type(type, ctx):
    # Tuples can be hung
    if type.kind == "Tuple":
        docs = [pretty_dispatch(c, ctx) for c in type.children]
        return group(align(nest(ctx.indent, concat([
                        LPAREN,
                        type.kind,
                        LINE, 
                        *intersperse(LINE, docs), 
                        RPAREN
                        ]))))

    # For all others, just use "str"
    return str(type)

if __name__ == "__main__":
    ###############################
    # Quick sanity check

    from ksc.parse_ks import parse_tld, s_exps_from_string

    # Importing prettyprint to get the decorated printers for Expression and Type
    import prettyprint # pylint: disable=unused-import

    # Import the prettyprinter routines we use explicitly in this file
    from prettyprinter import cpprint

    def tld(s):
        return parse_tld(s_exps_from_string(s)[0])

    e = tld("""
        (def myfun (Tuple Float Float)
            ((x : Float) 
             (y : Vec Integer)
             (t : (Tuple Float Float))) 
            (let (l (lam (i : Integer) (add i 1)))
            (let (b 2)
              (assert (gt b 0) 
                 (tuple 
                    (if (lt 1 b) x (add x (get$1$2 t))) 
                    (mul b (get$1$2 t)))))))
    """)
    cpprint(e, width=40)
    cpprint(e, width=60)
    cpprint(e, width=100)
