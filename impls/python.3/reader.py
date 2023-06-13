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
    read_form(rdr)


def tokenize(str):
    return ""


def read_form(rdr):
    return ""
