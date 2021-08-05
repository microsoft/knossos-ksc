# fmt: off
from typing import NamedTuple, Mapping

# Pretty printer for Knossos s-expressions.
# Uses the Wadler constructors from PrettyPrinter
# http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

# import cpprint and pformat so clients don't need to import both prettyprint and prettyprinter
from prettyprinter import register_pretty, cpprint, pformat # pylint: disable=unused-import
# Also import routines for ksc.type.Type
from ksc import prettyprint # pylint: disable=unused-import
# Wadler constructors
from prettyprinter.doc import (
    #flat_choice,
    annotate,   # annotations affect syntax coloring
    concat,
    group,      # make what's in here a single line if enough space
    nest,       # newlines within here are followed by indent + arg
    hang,
    #NIL,
    LINE,       # Space or Newline
    #SOFTLINE,   # nothing or newline
    HARDLINE    # newline
)

from prettyprinter.prettyprinter import (
    Token,
    LPAREN, RPAREN,
    pretty_dispatch
)

from prettyprinter.utils import intersperse

# Set default style to light, as it's readable on both light/dark backgrounds
from prettyprinter import set_default_style
set_default_style('light')

from rlo.expression import Expression, Type

# Configuration variables

# COLLAPSE_LETS: Collapse chains of lets
# (let (a x)                 (let ((a x)
#   (let (b y)         ->          (b y))
#     c)))                       c)
COLLAPSE_LETS = False  

# EXTRACT_DEFS: Convert let-bound lambdas to defs
# (let (v (lam a b)) c) -> (def v RET a b)
EXTRACT_DEFS = True

# These are primarily to enable syntax highlighting --
# it would otherwise be fine to just use the string/doc "s"
def pp_reserved(s):
    return annotate(Token.NAME_BUILTIN, s)

def ppfnname(s):
    return annotate(Token.NAME_FUNCTION, s)

def ppvar(s):
    return annotate(Token.NAME_VARIABLE, s)

def interline(*docs):
    return concat(intersperse(LINE, docs))

class ExprWithComments(NamedTuple):
    """ A facade or proxy object that masquerades as an Expression for pretty-printing and other purposes,
        but maintains the current position/subtree index within the root Expression,
        and a mapping from subtree index to a comment to display in front of that node.
    """
    expr: Expression
    comments: Mapping[int, str]
    idx: int  = 0

    @property
    def children(self):
        return [ExprWithComments(ch, self.comments, idx)
                for ch,idx in self.expr.children_with_indices(self.idx)]

    def __getattr__(self, attr_name):
        assert attr_name not in ["expr", "idx", "comments", "children"]
        ch_and_idx = self.expr.child_and_index_accessor(attr_name, self.idx)
        if ch_and_idx is not None:
            ch, idx = ch_and_idx
            return ExprWithComments(ch, self.comments, idx)
        return getattr(self.expr, attr_name)

    def __eq__(self, other):
        return self.expr == other or (
            isinstance(other, ExprWithComments)
            and other.comments is self.comments
            and other.expr == self.expr)

# Declare pretty printer for our Expressions
@register_pretty(ExprWithComments)
def pretty_ExprWithComments(ex_c, ctx):
    val = pretty_Expression(ex_c, ctx)
    cmt = ex_c.comments.get(ex_c.idx)
    return val if cmt is None else concat([
        annotate(Token.COMMENT_SINGLE, concat(["#|", cmt, "|#"])),
        val])

# Note this also gets called directly, with an ExprWithComments (masquerading as an Expression)
@register_pretty(Expression)
def pretty_Expression(ex, ctx):
    def parens(*docs):
        return nest(ctx.indent, group(concat([
            LPAREN,
            *docs,
            RPAREN
        ])))

    # Variable gets syntax-highlighted name
    if ex.op == "variable":
        return ppvar(ex.name)

    # Constants just use str()
    if ex.op == "constant":
        return str(ex.value)

    # select is carbuncular for now
    if ex.op == "select":
        idx = ex.right.value + 1
        get = "get${}${}".format(idx, ex.size)
        tup = pretty_dispatch(ex.left, ctx)
        return parens(get, LINE, tup)

    def pretty_arg(arg, ctx):
        vardoc = pretty_dispatch(arg, ctx)
        typedoc = pretty_dispatch(arg.type, ctx)
        return parens(vardoc, ' : ', typedoc)

    # At top level, find defs
    if EXTRACT_DEFS and ex.op == "let" and ex.children[1].op == "lam":
        # TODO Check this is top level only ctx.ident == 0, but it's always 4...?
        from rlo.sparser import remove_multi_arg_wrappings
        # (let v (lam arg lambody) letbody) 
        # ->
        # (def v RETTYPE arg body) 
        # letbody
        try:
            assert len(ex.children) == 3
            v, lam, letbody = ex.children

            # Logic from Expression.ksc_str to change tupled one-arg to multi-arg
            arg, lambody = lam.children
            args = [arg]  # Assume unary unless we discover otherwise (next)
            if arg.type.is_tuple:
                multi_args, inner_body = remove_multi_arg_wrappings(lambody, arg)
                if multi_args and arg.name not in inner_body.free_var_names:
                    lambody = inner_body # What's left after removing the lets
                    args = list(multi_args)
            
            return_type = lam.type.lam_return_type

            # Now pretty-print the various components.
            # Don't use parens here, otherwise first arg is indented less
            # argdoc = parens(interline(*[pretty_arg(arg, ctx) for arg in args]))
            argdoc = group(concat([
                LPAREN,
                hang(0,
                    interline(*[pretty_arg(arg, ctx) for arg in args])),
                RPAREN,
            ]))
            
            vdoc = pretty_dispatch(v, ctx)
            lambodydoc = pretty_dispatch(lambody, ctx)
            letbodydoc = pretty_dispatch(letbody, ctx)
            
            return_type_doc = pretty_dispatch(return_type, ctx)

            defdoc = parens(
                pp_reserved("def"),
                ' ',
                vdoc,
                nest(ctx.indent, concat([
                    LINE,
                    return_type_doc,
                    LINE,
                    argdoc,
                ])),
                HARDLINE,
                lambodydoc,
            )
            elems = [defdoc, HARDLINE]
            if letbody.op == "variable":
                # The innermost let+lam has its name as the body: (let (f (lam.....)) f).
                # Put the trailing f in a comment.
                assert letbody.name == v.name
                elems.append("; letbody ")
            return concat(elems + [letbodydoc])
        except Exception as exc:
            print("Error in trying to print def:", exc)
            # Fall through to print as normal let

    # Let bindings are printed with a hang of 1, so e.g. print as
    # (let (a 1)
    #  (let (b 2)
    #   (use a b)))
    # Optionally, can collapse lets, to transform to:
    # (let ((a 1)
    #       (b 2))
    #   (use a b))

    def pretty_let(ex, ctx):
        def get_bindings(ex, bindings):
            assert len(ex.children) == 3
            v = ex.children[0]
            rhs = ex.children[1]
            binding = (v,rhs)
            bindings.append(binding)

            body = ex.children[2]
            if COLLAPSE_LETS and body.op == "let":
                bindings, body = get_bindings(body, bindings)

            return bindings, body
            
        def pretty_binding(binding,ctx):
            v,rhs = binding
            vdoc = pretty_dispatch(v,ctx)
            rhsdoc = pretty_dispatch(rhs,ctx)
            return parens(vdoc, LINE, rhsdoc)

        bindings, body = get_bindings(ex, [])

        if len(bindings) > 1 and COLLAPSE_LETS:
            bindingsdoc = group(concat([
                LPAREN,
                hang(0,
                    interline(*[pretty_binding(b, ctx) for b in bindings])),
                RPAREN,
            ]))
            bodydoc = pretty_dispatch(body, ctx)
            return parens(pp_reserved("let"),
                ' ',
                bindingsdoc,
                HARDLINE,
                bodydoc
            )
        else:
            v = pretty_dispatch(ex.children[0], ctx)
            rhs = pretty_dispatch(ex.children[1], ctx)
            body = pretty_dispatch(ex.children[2], ctx)

            # Using two nested groups here to obtain the following behaviour for
            # expression (let (x y) e)
            # 1. If possible, put the entire expression into a single line.
            # 2. If not, print as
            # (let (x y)
            # e)
            return group(concat([
                group(concat([
                    LPAREN,
                    pp_reserved("let"),
                    ' ',
                    LPAREN,
                    v,
                    nest(ctx.indent, group(concat([LINE, rhs, RPAREN]))),
                ])),
                LINE,
                body,
                RPAREN,
            ]))

    if ex.op == "let":
        return pretty_let(ex, ctx)
               
    # Other constructs have children -- gather pp docs for them.
    # Elide any "tuple" on the RHS of an apply, so we get (... arg1 arg2) not (... (tuple arg1 arg2)).
    children = ([ex.left] + ex.right.children) if ex.op == "apply" and ex.right.op == "tuple" else ex.children

    docs = [pretty_dispatch(v, ctx) for v in children]
        
    # Lambda needs its type annotation
    if ex.op == "lam":
        return parens(
            pp_reserved("lam"),
            ' ',
            pretty_arg(ex.children[0], ctx),
            LINE,
            docs[1],
        )

    # (build n var body) needs a lambda added
    if ex.op in ["build", "sumbuild"]:
        n = docs[0]
        body = docs[2]
        return parens(
            ppfnname(ex.op),
            ' ',
            n,
            ' ',
            LPAREN,
            pp_reserved("lam"),
            ' ',
            pretty_arg(Expression.Variable(children[1].name, Type.Integer), ctx),
            LINE,
            body,
            RPAREN
        )

    # Others, including "if", treated as function calls:
    # op on first line (except for 'apply' which is skipped for brevity and not needed for roundtrip),
    # then args indented if placed on their own lines.
    op_prefix = ([] if ex.op == "apply" else [ppfnname(ex.op)])
    return parens(*intersperse(LINE, op_prefix + docs))
