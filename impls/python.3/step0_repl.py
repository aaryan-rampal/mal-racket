def READ(str):
    return str


def EVAL(ast, env):
    return ast


def PRINT(exp):
    return exp


def rep(str):
    return PRINT(EVAL(READ(str), ""))


def loop():
    print(rep(input("users> ")))
    loop()


def main():
    loop()


if __name__ == "__main__":
    main()
