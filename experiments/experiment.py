import forgi.graph.bulge_graph as fgb
bg = fgb.BulgeGraph()
bg.from_dotbracket('((..))..((..))')
print bg.to_bg_string()

