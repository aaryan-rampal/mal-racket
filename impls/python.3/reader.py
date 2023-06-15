import re
from mal_types import _list, _vector, _hash_map


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


def read_seq(rdr, end, typ=list):
    first = rdr.next()
    list_tokens = typ()
    while first != end:
        if rdr.end():
            raise Exception("EOF")
            return None
        # TODO a is symbol but list_tokens.append(a) appends a string
        next_form = read_form(rdr)
        if next_form == end:
            break
        list_tokens.append(next_form)
        first = rdr.peek()

    return list_tokens


def read_list(rdr):
    return read_seq(rdr, ')', _list)


def read_vector(rdr):
    return read_seq(rdr, ']', _vector)


def read_hash_map(rdr):
    return read_seq(rdr, '}', _hash_map)


def read_atom(rdr):
    first = rdr.next()
    if first.isnumeric():
        return int(first)
    # elif not first.isalnum():
    #     return _symbol(first)
    else:
        return first


def read_form(rdr):
    first = rdr.peek()
    if rdr.end():
        return []  # TODO is this actually what i want
    elif first == '(':
        return read_list(rdr)
    # elif first == ')':
    #     raise Exception("Encountered unexpected )")
    elif first == '{':
        return read_hash_map(rdr)
    # elif first == '}':
    #     raise Exception("Encountered unexpected }")
    elif first == '[':
        return read_vector(rdr)
    # elif first == ']':
    #     raise Exception("Encountered unexpected ]")
    else:
        return read_atom(rdr)
