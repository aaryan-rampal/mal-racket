import re

from blinker._utilities import _symbol


class Reader:
    def __init__(self, tokens):
        self.tokens = tokens
        self.position = 0

    def next(self):
        if self.end():
            None
        else:
            self.position += 1
            return self.peek_prev()

    def peek(self):
        if self.end():
            None
        else:
            return self.tokens[self.position]

    def peek_prev(self):
        if (self.position - 1) >= len(self.tokens):
            None
        else:
            return self.tokens[self.position - 1]

    def end(self):
        return self.position >= len(self.tokens)


def read_str(str):
    rdr = Reader(tokenize(str))
    return read_form(rdr)


def tokenize(str):
    pattern = r'[\s,]*(~@|[\[\]{}()\'+`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}(\'"``,;)]+)'  # Updated pattern
    tokens = re.findall(pattern, str)
    return tokens


def read_list(rdr, end):
    first = rdr.next()
    list_tokens = []
    while first != end:
        if rdr.end():
            pass  # TODO pass error here
        # TODO a is symbol but list_tokens.append(a) appends a string
        a = read_form(rdr)
        list_tokens.append(a)
        first = rdr.peek()

    return list_tokens


def read_atom(rdr):
    first = rdr.next()
    if first.isnumeric():
        return int(first)
    elif not first.isalnum():
        return _symbol(first)
    else:
        return first


def read_form(rdr):
    first = rdr.peek()
    if rdr.end():
        return []  # TODO is this actually what i want
    elif first == '(':
        return read_list(rdr, ')')
    # TODO another if statement that raises error for ')'
    else:
        return read_atom(rdr)
