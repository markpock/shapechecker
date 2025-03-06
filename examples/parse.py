import ast
import astpretty

code = \
"""
def tensor_add(
    a : 'Tensor (l + [a, b])',
    b : 'Tensor (l + [a, b])'
) -> 'Tensor (l + [a, b])':
    x = torch.zeros(a, b)
    return a + b
"""

tree = ast.parse(code)
print(tree)

astpretty.pprint(tree)

for node in ast.walk(tree):
    match node:
        case ast.Module(body=a):
            print('successful match')
        case ast.FunctionDef(name=name, args=ast.arguments(args=l)):
            print(f'fcn with {name=}')
            for arg in l:
                match arg:
                    case ast.arg(annotation=ast.Attribute(attr=attr)):
                        print(attr)
                    case x: print(x)
        case _: pass
            # print('other')
