a = "global"

# reassignment of 'a' in if -> error
if 10 == 10:
    a = "inside if"
else:
    a = "inside else"

# if 'a' is redeclared only inside if branch or else branch wo errors are yielded:
#   - 'a' is possibly unbound (because is declared inside only one possible branch)
#   - 'a' is being redeclared (because there is one declaration of 'a' before the if/else)
