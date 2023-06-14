import unittest

from blinker._utilities import _symbol

import reader
from reader import *


class TestReader(unittest.TestCase):
    def setUp(self):
        self.r1 = Reader(tokenize("(+ 2 3)"))
        self.r2 = Reader(tokenize("(/ (+ 2 3) (* 3 4))"))
        self.r3 = Reader(tokenize("(+ 2 3 (* 3 4))"))

    def test_read_atom_int(self):
        self.r1.next()
        self.r1.next()
        self.assertEqual(2, read_atom(self.r1))

    def test_read_form_multiple(self):
        self.assertEqual([_symbol("/"), [_symbol("+"), 2, 3]], read_form(self.r2))


if __name__ == '__main__':
    unittest.main()
