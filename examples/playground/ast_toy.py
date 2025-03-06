
import ast
import astpretty

code = "x = a + b * c"
parsed_ast = ast.parse(code)

# Pretty print using astpretty
astpretty.pprint(parsed_ast)