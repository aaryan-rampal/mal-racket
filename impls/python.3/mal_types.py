import sys, copy, types as pytypes


# lists
class List(list):
    def __add__(self, rhs): return List(list.__add__(self, rhs))
    def __getitem__(self, i):
        if type(i) == slice: return List(list.__getitem__(self, i))
        elif i >= len(self): return None
        else:                return list.__getitem__(self, i)
    def __getslice__(self, *a): return List(list.__getslice__(self, *a))
def _list(*vals): return List(vals)
def _list_Q(exp):   return type(exp) == List


# vectors
class Vector(list):
    def __add__(self, rhs): return Vector(list.__add__(self, rhs))
    def __getitem__(self, i):
        if type(i) == slice: return Vector(list.__getitem__(self, i))
        elif i >= len(self): return None
        else:                return list.__getitem__(self, i)
    def __getslice__(self, *a): return Vector(list.__getslice__(self, *a))
def _vector(*vals): return Vector(vals)
def _vector_Q(exp): return type(exp) == Vector

# Hash maps
class Hash_Map(dict): pass
def _hash_map(*key_vals):
    hm = Hash_Map()
    for i in range(0,len(key_vals),2): hm[key_vals[i]] = key_vals[i+1]
    return hm
def _hash_map_Q(exp): return type(exp) == Hash_Map
