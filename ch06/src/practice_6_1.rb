require 'graphviz'

# Create a new graph
gr = GraphViz.new(:G, type: :digraph)

# Create nodes
empty = gr.add_nodes("empty `[]`           (3)")
a     = gr.add_nodes("a     `[0]`          (3)")
b     = gr.add_nodes("b     `[0, 1]`       (3)")
c     = gr.add_nodes("c     `[1]`          (2)")
d     = gr.add_nodes("d     `[0, 1, 2]`    (2)")
e     = gr.add_nodes("e     `[1, 0, 1, 2]` (0)")
f     = gr.add_nodes("f     `[]`           (0)")
g     = gr.add_nodes("g     `[0, 1, 2, 3]` (0)")

# Create an edge between the two nodes
gr.add_edges(empty, a)
gr.add_edges(a, b)
gr.add_edges(b, c)
gr.add_edges(b, d)
gr.add_edges(c, e)
gr.add_edges(d, e)
gr.add_edges(d, g)
gr.add_edges(c, f)

# Generate output image
gr.output(png: "ch_6_1.png")
