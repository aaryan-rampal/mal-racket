def READ(str):
    from reader import read_str
    return read_str(str)


def EVAL(ast, env):
    return ast


def PRINT(exp):
    from printer import pr_str
    return pr_str(exp)


def rep(str):
    return PRINT(EVAL(READ(str), ""))


def loop():
    try:
        print(rep(input("users> ")))
    except Exception as e:
        print(e)

    loop()


def main():
    loop()


if __name__ == "__main__":
    main()
