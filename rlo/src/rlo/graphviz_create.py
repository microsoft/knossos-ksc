## Create GraphViz tree from an annotated s-expression
## input file contains an sexp like this
## (/ (/ 1 x) (+ 1 (let t (/ 1 x) t)))
## But with optional "histogram" annotations on the symbol name like this:
##   (/ (* a b) c)
## ->
##   (/$H<1,8,4,1> (*$H<5,5,8,3> a$H<5,5,8,3> b$H<5,5,8,3>) c)
## Which render as little boxes in SVG (which can be dropped straight into powerpoint)

## It's hacky:
## 1. Parse sexps
## 2. Render to ".dot" file, pass to GraphViz dot.exe, generating SVG with histogram info in comments
## 3. Read the SVG, and emit histograms from the comments

import io
import sexpdata
import subprocess
import re

## Little xml utils

# tago: Emit e.g. <str attr1 attr2>
def tago(str, attrs):
    return "<" + str + " " + " ".join(attrs) + ">"


# tag: Emit e.g. <str attr1 attr2/>
def tag(str, attrs):
    return "<" + str + " " + " ".join(attrs) + "/>"


# tag: Emit tagstr="value"
def attr(tagstr, value):
    return tagstr + '="' + value + '"'


# Extract the label from an sexp -- the term if it's a leaf, and the lead term if it's a list
def to_label(se):
    if isinstance(se, sexpdata.Symbol):
        return se.value()
    elif isinstance(se, list):
        return to_label(se[0])
    else:
        return str(se)


# GraphViz header
def header():
    return """
digraph G {
  node [shape=record,fontname="courier-bold",fontsize=24,color=gray]
  graph [ordering="out"]
"""


# Send s-exps to GraphViz, emitting histogram annotations as comments
def emit_nodes(writeln, se, indent, node_id):
    node = "n" + str(node_id)
    label = to_label(se)
    hist_location = label.find("$H")
    if hist_location >= 0:
        hist = label[hist_location + 2 :]  # Grab the hist data
        label = label[0:hist_location]  # Strip histo info
        label = label + "| "  # Make a record node with space for the histogram
        comment = "," + attr("comment", "HIST" + hist)
    else:
        comment = ""
    writeln(" " * indent + node + "[" + attr("label", label) + comment + "]")
    node_id += 1
    if isinstance(se, list):
        indent += 1
        for child in se[1:]:
            childnode, node_id = emit_nodes(writeln, child, indent, node_id)
            writeln(" " * indent + node + " -> " + childnode)
    return node, node_id


# GraphViz trailer
def trailer():
    return """
}
"""


# Make GraphViz file from s-expression
def sexp_to_dot(string_or_stream, dotfile):
    strm = (
        io.StringIO(string_or_stream)
        if isinstance(string_or_stream, str)
        else string_or_stream
    )
    se = sexpdata.load(strm, nil=None, true=None, false=None)
    writeln = lambda s: print(s, file=dotfile)
    writeln(header())
    emit_nodes(writeln, se, indent=2, node_id=0)
    writeln(trailer())


## Cycle through fill colours
fill_colors = ["#fa0", "#aaa", "#e93", "#08d"]


def fill_color(index):
    return fill_colors[index % len(fill_colors)]


## Emit SVG histogram.
# TODO: Very hard-coded to length 4, but should be easy to generalize
def emit_hist(writeln, pos, hist):
    writeln(
        tago(
            "g",
            [
                attr("transform", "translate(%f %f)" % pos + " scale(2 -9)"),
                attr("stroke", "none"),
            ],
        )
    )
    for index, val in enumerate(hist):
        y = len(hist) - index - 1
        writeln(
            tag(
                "polygon",
                [
                    attr("fill", fill_color(index)),
                    attr("transform", "translate(0 %.2f) scale(%.2f 1)" % (y, val)),
                    attr("points", "0,0 1,0 1,1 0,1"),
                ],
            )
        )
    writeln("</g>")


def main(argv):
    if len(argv) != 3:
        print("Usage: {} <filename> <tmpfilebasename>".format(sys.argv[0]))
        sys.exit(-1)
    dotfilename = sys.argv[2] + ".dot"

    print("Writing to", dotfilename, file=sys.stderr)
    with open(dotfilename, "w") as dotfile, open(sys.argv[1]) as f:
        sexp_to_dot(f, dotfile)

    print("Now running GraphViz dot.exe", file=sys.stderr)
    dot = r"C:\Program Files (x86)\Graphviz2.38\bin\dot.exe"
    proc = subprocess.Popen(
        [dot, "-Tsvg", dotfilename], stdout=subprocess.PIPE, universal_newlines=True
    )

    svgfilename = sys.argv[2] + ".svg"
    print("Writing to", svgfilename, file=sys.stderr)
    with open(svgfilename, "w") as svg:
        hist = None
        svgwriteln = lambda s: print(s, file=svg)
        while True:
            line = proc.stdout.readline().rstrip()
            if not line:
                break
            s = re.search(r"HIST&lt;(.*)&gt;", line)
            if s:
                hist = [float(x) for x in s.group(1).split(",")]

            s = re.search(r'<polyline .* points="([0-9.-]+),([0-9.-]+)', line)
            if s and hist:
                x = float(s.group(1))
                y = float(s.group(2))
                print(x, y, hist)
                emit_hist(svgwriteln, (x, y), hist)
                hist = None

            print(line, file=svg)


## Call using e.g.
# python graphviz-create.py graphs.ks tmp\graphs
# which will create
#    tmp/graphs.dot
#    tmp/graphs.svg
# TODO: pipe to dot.exe so we don't make tmp dot file
if __name__ == "__main__":
    import sys

    main(sys.argv)
