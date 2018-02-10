
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
majorlist={"C":0,"D":2,"E":4,"F":5,"G":7,"A":9,"B":11}
#ChordInterval:List[int]=[1,2,2,2,3,4,4,1,5,2,6,3,7,4]


# In[3]:


def to_token(l):
    return " | ".join(['"%s"'%i.replace("\"", "\\\"") for i in l])


# In[4]:


# TODO: int vs number

parser = lark.Lark("""
_BAR: "|"
FORCE: """ + to_token(forcelist) + """
PITCH: """ + to_token(pitchlist) + """
BEAT: """ + to_token(beatlist) + """
HOLD: """ + to_token(fixedholdlist) + """
_HOLD_END: "&"

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
hold: HOLD+ | "`"

bar: _BAR
chord_param: pitch? item
chord_params: "<" chord_param ("," chord_param)* ">"
chord: name? chord_params
section: "(" number ("," number)* ")"
note_main: int
         | name -> chord_instance
note: force? pitch? note_main beat? hold?
hold_end: _HOLD_END
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
         | hold_end
         | note
smsp: statement*

%import common.WS
%import common.DIGIT
%import common.LETTER
%ignore WS
""", start="smsp", parser='lalr', propagate_positions=True)


# In[5]:


# print(parser.parse(txt).pretty())


# In[6]:


from typing import List, Dict, Tuple, Union, Iterable, Optional
import collections
from typeguard import typechecked

Number = Union[int, float]
item_t = Union[Number, str]


# In[7]:


def num(s):
    try:
        return int(s)
    except ValueError:
        return float(s)

def yield_from(l):
    yield from l

def gather_from(l):
    for i in l:
        yield from i

# In[8]:


class AdditiveMap():
    def __init__(self, l, default):
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

    def __add__(self, o):
        assert(self.__class__ != AdditiveMap)
        return self.make(self.__class__, self.s + o.s)

    def make(self, cls, s):
        return cls(s)

class Force(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(forcelist, 1)
        self.name = "force"
        self.s = s

class Pitch(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(pitchlist, 0)
        self.name = "pitch"
        self.s = s
chord_elem_t = Tuple[Pitch, Union[int, str]]

class Beat(AdditiveMap):
    def __init__(self, s=""):
        super().__init__(beatlist, 1)
        self.name = "beat"
        self.s = s

    def get_value(self, s=None):
        if s is None:
            s = self.s
        return super().get_value(s) + (1 if s is not None and len(s) > 0 and s[0] == "-" else 0)

class Hold:
    def __init__(self, fixed=None):
        self.fixed = fixed

    def get_value(self):
        if self.fixed is False:
            return "?"
        return None

class FixedHold(AdditiveMap, Hold):
    def __init__(self, s=""):
        super().__init__(fixedholdlist, default=0)
        Hold.__init__(self, fixed=True)
        self.name = "hold"
        self.s = s

    def get_value(self, s=None) -> (Optional[str], float):
        return super().get_value(s)

class SrcLocation:
    def __init__(self, node: lark.Tree=None, **kwargs):
        try:
            self.line: int = node.line
            self.column: int = node.column
            self.end_line: int = node.end_line
            self.end_column: int = node.end_column
        except AttributeError:
            pass
        self.info = kwargs

    def __repr__(self):
        return "#%d:%d" % (self.line, self.column)

class IR:
    def __init__(self, loc: SrcLocation):
        self.loc = loc

class Control(IR):
    def __init__(self, name, value, *, loc):
        super().__init__(loc)
        self.name = name
        self.value = value

    def __repr__(self):
        return "<control%s %s=%s>" % (self.loc, self.name, self.value)

class Section(IR):
    def __init__(self, value=None, *, loc, relative=False):
        super().__init__(loc)
        if value is None:
            value = 1 if relative else 0
        self.value = value
        self.relative = relative

    def __repr__(self):
        return "<section%s %s%s>" % (self.loc, "+" if self.relative else "", self.value)

class Func:
    def __init__(self, s):
        self.name = s
        self.args = None

    def bind(self, args):
        self.args = args
        return self

    def __repr__(self):
        return "<func %s %s>" % (self.name, self.args or "")

class ChordDef(IR):
    def __init__(self, name: str, args: Iterable[chord_elem_t], *, loc: SrcLocation):
        super().__init__(loc)
        self.name = name
        self.args: Tuple[Union[int, str], ...] = tuple([p.get_value()+i if not isinstance(i, str) else i for p, i in args])

    def __repr__(self):
        return "<chord_def%s %s %s>" % (self.loc, self.name, self.args)

class ChordMap(IR):
    def __init__(self, name: str, args: Iterable[chord_elem_t], *, loc: SrcLocation):
        super().__init__(loc)
        self.name = name
        self.args: Tuple[Tuple[Pitch, int], ...] = tuple((Pitch(), i) if isinstance(i, (int, float)) else i for i in args)

    def __repr__(self):
        return "<chord_map%s %s %s>" % (self.loc, self.name, self.args)

class Note(IR):
    def __init__(self, note, force: Force, pitch: Pitch, beat: Beat, hold: Hold, *, loc: SrcLocation):
        super().__init__(loc)
        self.type, self.note = note
        self.force = force
        self.pitch = pitch
        self.beat = beat
        self.hold = hold

    def __repr__(self):
        h = self.hold.get_value()
        return "<note%s %s f=%s,p=%s,b=%s%s>" % (self.loc, self.note, self.force.get_value(), self.pitch.get_value(), self.beat.get_value(), ("h="+h) if h is not None else "")

class HoldEnd(IR):
    def __init__(self, value=None, *, loc: SrcLocation):
        super().__init__(loc)
        self.value = value

    def __repr__(self):
        return "<hold_end%s%s>" % (self.loc, "" if self.value is None else " " + self.value)

import lark.tree
class Transformer:
    def _get_func(self, name):
        return getattr(self, name)

    def transform(self, tree):
        items = []
        for c in tree.children:
            try:
                items.append(self.transform(c) if isinstance(c, lark.Tree) else c)
            except lark.tree.Discard:
                pass
        try:
            f = self._get_func(tree.data)
        except AttributeError:
            return self.__default__(tree.data, items)
        else:
            return f(tree, *items)

    def __default__(self, data, children):
        return lark.Tree(data, children)

    def __mul__(self, other):
        return lark.tree.TransformerChain(self, other)

class transformer(Transformer):
    @typechecked
    def statement(self, node: lark.Tree, *items):
        [stat] = items
        if stat is not None:
            if isinstance(stat, collections.Iterable):
                yield from(stat)
            elif isinstance(stat, IR):
                yield stat
            else:
                print("unknown statement ", stat)

    @typechecked
    def chord(self, node: lark.Tree, *items: Union[str, Tuple[chord_elem_t, ...]]):
        params: Tuple[chord_elem_t, ...] = items[-1]
        if len(items) > 1:
            name = items[0]
        else:
            name = "U#" + str(tuple([i if p.s == "" else p.s+str(i) for p, i in params]))
        if name == "" or name[0].isupper():
            yield ChordDef(name, params, loc=SrcLocation(node))
            if name.startswith("U#"):
                yield Control("chord", (Pitch(), name), loc=SrcLocation(node))
        else:
            yield ChordMap(name, params, loc=SrcLocation(node))
        # print("chord", name, params)

    @typechecked
    def funccall(self, node: lark.Tree, name: str, *params):
        name = name[:-1]
        if name == "i":
            return yield_from([Control("instrument", params[0], loc=SrcLocation(node))])
        elif name == "m":
            return yield_from([Control(v, params[i], loc=SrcLocation(node)) for i, v in enumerate(["barbeat", "bpm", "base"])])
        elif name == "v":
            return yield_from([Control("volume", params[0], loc=SrcLocation(node))])
        else:
            # print("*funccall", name, params)
            return Func(name).bind(params)

    @typechecked
    def funcparam(self, node: lark.Tree, i: Union[Func, item_t]):
        # print("funcparam", i, file=sys.stderr)
        return i

    @typechecked
    def switch_track(self, node: lark.Tree, name: str):
        yield Control("track", name, loc=SrcLocation(node))

    @typechecked
    def switch_chord(self, node: lark.Tree, *items: Union[Pitch, str]):
        if len(items) == 2:
            pitch, chord = items
        elif len(items) == 1:
            pitch, chord = Pitch(), *items
        else:
            pitch, chord = Pitch(), None
        yield Control("chord", (pitch, chord), loc=SrcLocation(node))

    @typechecked
    def section(self, node: lark.Tree, *items: Number):
        secs = tuple(items)
        yield Section(secs, loc=SrcLocation(node))

    @typechecked
    def bar(self, node: lark.Tree):
        yield Section(relative=True, loc=SrcLocation(node))

    @typechecked
    def hold_end(self, node: lark.Tree):
        yield HoldEnd(loc=SrcLocation(node))

    @typechecked
    def holdmode(self, node: lark.Tree, i: item_t):
        if i in freeholdrule:
            i = freeholdrule[i]
        if i in freeholdrule.values():
            yield Control("holdmode", i, loc=SrcLocation(node))
        elif isinstance(i, Number):
            yield HoldEnd(i, loc=SrcLocation(node))
        else:
            assert(False)

    @typechecked
    def note(self, node: lark.Tree, *items: Union[Force, Pitch, Beat, Hold, lark.Tree]):
        force, pitch, note, beat, hold = Force(), Pitch(), None, Beat(), Hold()
        for i in items:
            if isinstance(i, Force):
                force = i
            elif isinstance(i, Pitch):
                pitch = i
            elif isinstance(i, Beat):
                beat = i
            elif isinstance(i, Hold):
                hold = i
            elif isinstance(i, lark.Tree):
                note = (i.data, i.children[0])
        yield Note(note, force=force, pitch=pitch, beat=beat, hold=hold, loc=SrcLocation(node))
        # print("note", note, force, pitch, beat, hold)

    @typechecked
    def chord_params(self, node, *items: chord_elem_t):
        return items

    @typechecked
    def chord_param(self, node, *items: Union[Pitch, int, str]) -> chord_elem_t:
        # TODO
        if len(items) == 1:
            return Pitch(), items[0]
        else:
            return sum(items[:-1], Pitch()), items[-1]

    @typechecked
    def item(self, node, x: item_t):
        return x

    @typechecked
    def name(self, node, x: lark.lexer.Token) -> str:
        return x.value

    @typechecked
    def number(self, node, x: lark.lexer.Token) -> Number:
        try:
            return int(x.value)
        except ValueError:
            return float(x.value)

    @typechecked
    def int(self, node, x: lark.lexer.Token) -> int:
        return int(x.value)

    @typechecked
    def force(self, node, *items: lark.lexer.Token) -> Force:
        return sum([Force(x.value) for x in items], Force())

    @typechecked
    def pitch(self, node, *items: lark.lexer.Token) -> Pitch:
        return sum([Pitch(x.value) for x in items], Pitch())

    @typechecked
    def beat(self, node, *items: lark.lexer.Token) -> Beat:
        return sum([Beat(x.value) for x in items], Beat())

    @typechecked
    def hold(self, node, *items: lark.lexer.Token) -> Hold:
        if len(items) == 0:
            return Hold(fixed=False)
        return sum([FixedHold(x.value) for x in items], FixedHold())

# In[9]:


def test():
    print(parser.parse("""
        C5<0,x,7>
        s<1,2,3,4,5>
        s [t] g[t[Piano, -10]]
        (0,2) 1&[p] ^1.-` 2 3& 4| !s- 6 7 8

        """).pretty())


# In[10]:

class Timeline:
    def __init__(self, i):
        if isinstance(i, (list, tuple)):
            self.i = tuple(i)
        elif isinstance(i, (int, float)):
            self.i = (i,)
        else:
            raise Exception("not supported type")

    def __add__(self, o):
        if isinstance(o, (int, float)):
            # print("__add__", self.i, o)
            return Timeline([x+o for x in self.i])
        else:
            raise Exception("not supported type")

    def __iadd__(self, o):
        if isinstance(o, (int, float)):
            # print("__iadd__", self.i, o)
            self.i = tuple(x+o for x in self.i)
            return self
        else:
            raise Exception("not supported type")

    def __copy__(self):
        # print("__copy__", self.i)
        return Timeline(self.i)

class Track:
    def __init__(self):
        self.instrument = "Midi[1]"
        self.volume = 1.
        self.chord = Chord((0, 4, 7))
        self.holdcount = 0
        self.holdmode = "default"
        self.pointertime = Timeline(0.)
        self.starttime = Timeline(0.)

class VM:
    def __init__(self, chordextend=True, intervalunit=False):
        self.chordlist: Dict[str, ChordDef] = {}
        self.chordmap: Dict[str, ChordMap] = {}
        self.trackname: str = ""
        self.tracklist = collections.defaultdict(Track)
        self.barbeat: int = 4
        self.bps: float = 120. / 60
        self.base: int = 0
        self.pending: List[Sound] = []
        self.chordextend: bool = chordextend
        self.intervalunit: bool = intervalunit

    def cur_track(self) -> Track:
        return self.tracklist[self.trackname]

class Chord:
    def __init__(self, chord: (ChordDef, Iterable[Union[int, str]])):
        if isinstance(chord, ChordDef):
            chord = chord.args
        self.chord: Tuple[Union[int, str], ...] = tuple(chord)

    def __repr__(self):
        return "<chord %s>" % (self.chord,)

    def __add__(self, o: (Pitch, int)):
        if isinstance(o, Pitch):
            o: int = o.get_value()
        return Chord([i+o if not isinstance(i, str) else i for i in self.chord])

    def map(self, o: ChordMap):
        return Chord([p.get_value()+self.chord[i-1] for p, i in o.args if i <= len(self.chord) and not isinstance(self.chord[i-1], str)])

class Sound:
    def __init__(self, track: str, starttime: (float, Tuple[float, ...]), note: (List[int], str), type: str, force: float, beat: float, hold: (int, str), loc: SrcLocation):
        self.track = track
        self.start = starttime
        self.type = type
        self.note = note
        self.force = force
        self.beat = beat
        self.hold = hold
        self.loc = loc

    def __repr__(self):
        return "<sound%s %s %s (%s) %s f=%s,b=%s,h=%s>" % (self.loc, self.track, self.start, self.type, self.note, self.force, self.beat, self.hold)

    @staticmethod
    def from_note(note: Note, state: VM):
        h = note.hold.get_value()
        b = note.beat.get_value() / state.bps
        if h is None and state.cur_track().holdmode == "pedal":
            h = "?"
        if isinstance(h, (int, float)):
            h = h / state.bps
        n, t = note.note, note.type
        if t == "chord_instance":
            if n in state.chordmap:
                t = "chord"
                # print("map", state.cur_track().chord, n)
                n = state.cur_track().chord.map(state.chordmap[n]).chord
            else:
                t = "tag"
        elif t == "chord_const":
            t = "chord"
            n = tuple(n)
        elif t == "note_main":
            t = "note"
            n = (n,)
        return Sound(state.trackname, state.cur_track().starttime.i, n,
                     type=t, force=note.force.get_value(), beat=b, hold=h or 0, loc=note.loc)

def gen_ast(txt: str) -> lark.Tree:
    return parser.parse(txt)

def gen_ir(ast: lark.Tree) -> Iterable[IR]:
    yield from gather_from(transformer().transform(ast).children)

def exec_ir(ir: Iterable[IR], *, state: VM=None) -> Iterable[Sound]:
    if state is None:
        state = VM()
    for i in ir:
        if isinstance(i, Control):
            if i.name == "volume":
                state.cur_track().volume = float(i.value)
            elif i.name == "instrument":
                state.cur_track().instrument = i.value
            elif i.name == "chord":
                pitch, name = i.value
                chord = None
                if name in state.chordlist:
                    chord = Chord(state.chordlist[name])
                elif state.chordextend:
                    if name[0] in majorlist:
                        name1 = "C" + name[1:]
                        chord = Chord(state.chordlist[name1]) + majorlist[name[0]]
                state.cur_track().chord = chord + pitch
            elif i.name == "holdmode":
                state.cur_track().holdmode = i.value
            elif i.name == "barbeat":
                state.barbeat = int(i.value)
            elif i.name == "bpm":
                state.bps = float(i.value) / 60
            elif i.name == "base":
                state.base = int(i.value)
            elif i.name == "track":
                state.trackname = str(i.value)
        elif isinstance(i, ChordDef):
            state.chordlist[i.name] = i
        elif isinstance(i, ChordMap):
            state.chordmap[i.name] = i
        elif isinstance(i, Section):
            if i.relative:
                state.cur_track().pointertime += i.value * state.barbeat / state.bps
            else:
                state.cur_track().pointertime = Timeline([x * state.barbeat / state.bps for x in i.value])
            state.cur_track().starttime = state.cur_track().pointertime.__copy__()
        elif isinstance(i, Note):
            sound = Sound.from_note(i, state)
            state.cur_track().starttime += sound.beat
            if sound.hold == "?":
                state.pending.append(sound)
            else:
                yield sound
        if isinstance(i, HoldEnd) or (isinstance(i, Section) and state.cur_track().holdmode in ("pedal", "normal")):
            if i.value is None:
                cur_time = state.cur_track().starttime.i[0]
            else:
                cur_time = i.value * state.barbeat / state.bps
            _pending = []
            for s in state.pending:
                if s.track == state.trackname:
                    s.hold = cur_time - s.start[0]
                    if s.hold < s.beat:
                        s.hold = s.beat
                    yield s
                else:
                    _pending.append(s)
            state.pending = _pending



if __name__ == "__main__":
    with open("tmp1.txt") as f:
        txt = f.read()
    for ii in exec_ir(gen_ir(gen_ast(txt))):
        print(ii)
