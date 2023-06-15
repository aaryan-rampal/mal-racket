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
        to_append = str(pr_str(x))
        if first_item == 0:
            ans += to_append
            first_item = 1
        else:
            ans += " " + to_append

    ans += ")"
    return ans
