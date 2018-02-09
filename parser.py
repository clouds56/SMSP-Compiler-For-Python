
# coding: utf-8

# In[1]:


import lark


# In[2]:


scalelist={"1": 0, "2": 2, "3": 4, "4": 5, "5": 7, "6": 9, "7": 11}
pitchlist={"@": -1,"#": 1,"!": -12,"^": 12}
beatlist={"$": 1/8,"=": 1/4,";": 1/3,"_": 1/2,":": 2/3,".": 3/2,"-": 1}
forcelist={"\"": 0.25,"'": 0.5,"+": 2,"*": 4}
fixedholdlist={"~": 2}
freeholdrule={"d": "default","n": "normal","p": "pedal"}


# In[3]:


def to_token(l):
    return " | ".join(['"%s"'%i.replace("\"", "\\\"") for i in l])


# In[4]:


# TODO: int vs number

parser = lark.Lark("""
BAR: "|"
FORCE: """ + to_token(forcelist) + """
PITCH: """ + to_token(pitchlist) + """
BEAT: """ + to_token(beatlist) + """
HOLD: """ + to_token(list(fixedholdlist) + ["`"]) + """
HOLD_END: "&"

CNAME: LETTER | DIGIT
INT: DIGIT+
SIGNED_INT: "-"? INT
NUMBER: /[+-]?/ (INT? "." INT | INT ("." INT)? /[eE][+-]?/ INT)
NAME: LETTER CNAME*
FUNC_CALL: NAME "["

name: NAME
number: NUMBER | INT | SIGNED_INT
int: INT | SIGNED_INT
item: name | number

force: FORCE+
pitch: PITCH+
beat: BEAT+
hold: HOLD+

bar: BAR
chord_param: pitch? item
chord_params: "<" chord_param ("," chord_param)* ">"
chord: name chord_params
section: "(" number ("," number)* ")"
note_main: int
         | name -> chord_instance
         | chord_params -> chord_const
note: force? pitch? note_main beat? hold? HOLD_END?
?funcparam: funccall | item
?funccall: FUNC_CALL funcparam? ("," funcparam)* "]" -> funccall
holdmode: "&[" item "]"
switch_track: "[" name "]"
switch_chord: pitch? name ">"
statement: funccall
         | holdmode
         | switch_track
         | switch_chord
         | chord
         | section
         | bar
         | note
smsp: statement*

%import common.WS
%import common.DIGIT
%import common.LETTER
%ignore WS
""", start="smsp", parser='lalr')


# In[5]:


# print(parser.parse(txt).pretty())


# In[6]:


from typing import List, Union
import sys


# In[7]:


def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

def get_item(x: (lark.Tree, lark.lexer.Token)) -> Union[int, str]:
    if isinstance(x, lark.Tree):
        assert(x.data == "item" and len(x.children) == 1)
        x = x.children[0]
    if isinstance(x, lark.lexer.Token):
        if x.type == "NUMBER":
            return num(x.value)
        elif x.type == "NAME":
            return x.value
    print(x)
    assert(False)


# In[8]:


class AdditiveMap():
    def __init__(self, l, default=1):
        self.name = "add_map"
        self.map = l
        self.dft = default
        self.s = ""
    
    def get_value(self, s=None):
        if s is None:
            s = self.s
        if len(s) == 0:
            return self.dft
        return sum([self.map[i] for i in s])
    
    def __repr__(self):
        return "<%s %s %s>" % (self.name, self.s, self.get_value())
    
    def __add__(self, i):
        assert(self.name == i.name)
        return self.__class__(self.s + i.s)

class Force(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(forcelist)
        self.name = "force"
        self.s = s

class Pitch(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(pitchlist, 0)
        self.name = "pitch"
        self.s = s

class Beat(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(beatlist)
        self.name = "beat"
        self.s = s
    
    def get_value(self, s=None):
        if s is None:
            s = self.s
        return super().get_value(s) + (1 if len(s) > 0 and s[0] == "-" else 0)

class FixedHold(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(fixedholdlist, 0)
        self.name = "hold"
        self.s = s

class Statement:
    def __init__(self, line_begin=-1, line_end=-1):
        self.line_begin = line_begin
        self.line_end = line_end

class Func(Statement):
    def __init__(self, s, line_begin, line_end):
        super().__init__(line_begin, line_end)
        self.name = s
        self.args = None
    
    def bind(self, args):
        self.args = args
        return self
    
    def __repr__(self):
        return "<func %s %s>" % (self.name, self.args or "")
    
class transformer(lark.Transformer):
    @staticmethod
    def statement(args):
        [stat] = args
        if stat is not None:
            print("statement", stat)
        
    @staticmethod
    def chord(args):
        [name, params] = args
        # print("chord", name, params)

    @staticmethod
    def funccall(args):
        [name, *params] = args
        name: str = name[:-1]
        if name == "i":
            pass
        elif name == "m":
            pass
        elif name == "v":
            pass
        else:
            # print("*funccall", name, params)
            return Func(name).bind(params)
        print("funccall", name, params)

    @staticmethod
    def funcparam(args):
        [i] = args
        # print("funcparam", i, file=sys.stderr)
        return i
    
    @staticmethod
    def switch_track(args):
        [name] = args
        print("switch_track", name)

    @staticmethod
    def switch_chord(ch):
        print("switch_chord", ch)
        
    @staticmethod
    def section(args):
        secs = args
        print("section", secs)
    
    @staticmethod
    def bar(args):
        print("bar")

    @staticmethod
    def note(args):
        force, pitch, note, beat, hold, hold_end = Force(), Pitch(), None, Beat(), FixedHold(), False
        for i in args:
            if isinstance(i, Force):
                force = i
            elif isinstance(i, Pitch):
                pitch = i
            elif isinstance(i, Beat):
                beat = i
            elif isinstance(i, FixedHold):
                hold = i
            elif isinstance(i, lark.Tree):
                note = i
            elif isinstance(i, lark.lexer.Token) and i.type == "HOLD_END":
                hold_end = True
        # print("note", note, force, pitch, beat, hold, hold_end)
    
    @staticmethod
    def chord_params(args):
        return args
    
    @staticmethod
    def chord_param(args):
        # TODO
        if len(args) == 1:
            return args[0]
        else:
            return args

    @staticmethod
    def item(args):
        return args[0]
    
    @staticmethod
    def name(args):
        [x] = args
        return x.value

    @staticmethod
    def number(args):
        [x] = args
        try:
            return int(x.value)
        except ValueError:
            return float(x.value)
        
    @staticmethod
    def int(args):
        [x] = args
        return int(x.value)

    @staticmethod
    def force(args) -> Force:
        return sum([Force(x.value) for x in args], Force())

    @staticmethod
    def pitch(args) -> Pitch:
        return sum([Pitch(x.value) for x in args], Pitch())
    
    @staticmethod
    def beat(args) -> Beat:
        return sum([Beat(x.value) for x in args], Beat())

    @staticmethod
    def hold(args) -> FixedHold:
        return sum([FixedHold(x.value) for x in args], FixedHold())

# In[9]:


def test():
    print(parser.parse("""
        C5<0,x,7>
        s<1,2,3,4,5>
        s [t] g[t[Piano, -10]]
        (0,2) 1&[p] ^1.-` 2 3& 4| !s- 6 7 8

        """).pretty())


# In[10]:


if __name__ == "__main__":
    with open("tmp1.txt") as f:
        txt = f.read()
    transformer().transform(parser.parse(txt));

