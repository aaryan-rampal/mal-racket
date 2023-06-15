import re


def pr_str(expr):
    # TODO if for symbol?
    if isinstance(expr, list):
        return pr_list(expr)
    else:
        return expr


def pr_list(mal_list):
    ans = "("
    first_item = 0

    for x in mal_list:
        if first_item == 0:
            ans += pr_str(x)
            first_item = 1
        else:
            ans += " " + pr_str(x)

    ans += ")"
    return ans
