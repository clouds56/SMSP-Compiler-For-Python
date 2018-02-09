
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
BAR: "|"
FORCE: """ + to_token(forcelist) + """
PITCH: """ + to_token(pitchlist) + """
BEAT: """ + to_token(beatlist) + """
HOLD: """ + to_token(fixedholdlist) + """
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
hold: HOLD+ | "`"

bar: BAR
chord_param: pitch? item
chord_params: "<" chord_param ("," chord_param)* ">"
chord: name? chord_params
section: "(" number ("," number)* ")"
note_main: int
         | name -> chord_instance
note: force? pitch? note_main beat? hold?
hold_end: HOLD_END
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
""", start="smsp", parser='lalr')


# In[5]:


# print(parser.parse(txt).pretty())


# In[6]:


from typing import List, Dict, Tuple, Union, Iterable, Callable, Optional
import sys
import collections


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
        assert(self.name == o.name)
        return self.__class__(self.s + o.s)

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

class IR:
    def __init__(self, line_begin=-1, line_end=-1):
        self.line_begin = line_begin
        self.line_end = line_end

class Control(IR):
    def __init__(self, name, value):
        super().__init__()
        self.name = name
        self.value = value

    def __repr__(self):
        return "<control %s=%s>" % (self.name, self.value)

class Section(IR):
    def __init__(self, value=None, *, relative=False):
        super().__init__()
        if value is None:
            value = 1 if relative else 0
        self.value = value
        self.relative = relative

    def __repr__(self):
        return "<section %s%s>" % ("+" if self.relative else "", self.value)

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
    def __init__(self, name: str, args: Iterable[Union[int, str]]):
        super().__init__()
        self.name = name
        self.args = tuple(args)

    def __repr__(self):
        return "<chord_def %s %s>" % (self.name, self.args)

class ChordMap(IR):
    def __init__(self, name: str, args: Iterable[Union[int, Tuple[Pitch, int]]]):
        super().__init__()
        self.name = name
        self.args: Tuple[Tuple[Pitch, int], ...] = tuple((Pitch(), i) if isinstance(i, (int, float)) else i for i in args)

    def __repr__(self):
        return "<chord_map %s %s>" % (self.name, self.args)

class Note(IR):
    def __init__(self, note, force: Force, pitch: Pitch, beat: Beat, hold: Hold):
        super().__init__()
        self.type, self.note = note
        self.force = force
        self.pitch = pitch
        self.beat = beat
        self.hold = hold

    def __repr__(self):
        h = self.hold.get_value()
        return "<note %s f=%s,p=%s,b=%s%s>" % (self.note, self.force.get_value(), self.pitch.get_value(), self.beat.get_value(), ("h="+h) if h is not None else "")

class HoldEnd(IR):
    def __init__(self, value=None):
        super().__init__()
        self.value = value

    def __repr__(self):
        return "<hold_end>" if self.value is None else "<hold_end %s>" % self.value

class transformer(lark.Transformer):
    @staticmethod
    def statement(items):
        [stat] = items
        if stat is not None:
            if isinstance(stat, collections.Iterable):
                yield from(stat)
            elif isinstance(stat, IR):
                yield stat
            else:
                print("unknown statement ", stat)

    @staticmethod
    def chord(items):
        params = items[-1]
        if len(items) > 1:
            name = items[0]
        else:
            name = "U#" + str(tuple(params))
        if name == "" or name[0].isupper():
            yield ChordDef(name, params)
            if name.startswith("U#"):
                yield Control("chord", (Pitch(), name))
        else:
            yield ChordMap(name, params)
        # print("chord", name, params)

    @staticmethod
    def funccall(items):
        [name, *params] = items
        name: str = name[:-1]
        if name == "i":
            return yield_from([Control("instrument", params[0])])
        elif name == "m":
            return yield_from([Control(v, params[i]) for i, v in enumerate(["barbeat", "bpm", "base"])])
        elif name == "v":
            return yield_from([Control("volume", params[0])])
        else:
            # print("*funccall", name, params)
            return Func(name).bind(params)

    @staticmethod
    def funcparam(items):
        [i] = items
        # print("funcparam", i, file=sys.stderr)
        return i

    @staticmethod
    def switch_track(items):
        [name] = items
        yield Control("track", name)

    @staticmethod
    def switch_chord(items):
        pitch, chord = Pitch(), items[-1]
        if len(items) > 1:
            pitch = items[0]
        yield Control("chord", (pitch, chord))

    @staticmethod
    def section(items):
        secs = tuple(items)
        yield Section(secs)

    @staticmethod
    def bar(items):
        yield Section(relative=True)

    @staticmethod
    def hold_end(items):
        yield HoldEnd()

    @staticmethod
    def holdmode(items):
        [i] = items
        if i in freeholdrule:
            i = freeholdrule[i]
        if i in freeholdrule.values():
            yield Control("holdmode", i)
        elif isinstance(i, (float, int)):
            yield HoldEnd(i)
        else:
            assert(False)

    @staticmethod
    def note(items):
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
        yield Note(note, force=force, pitch=pitch, beat=beat, hold=hold)
        # print("note", note, force, pitch, beat, hold)

    @staticmethod
    def chord_params(items):
        return items

    @staticmethod
    def chord_param(items):
        # TODO
        if len(items) == 1:
            return items[0]
        else:
            return sum(items[:-1], Pitch()), items[-1]

    @staticmethod
    def item(items):
        return items[0]

    @staticmethod
    def name(items):
        [x] = items
        return x.value

    @staticmethod
    def number(items):
        [x] = items
        try:
            return int(x.value)
        except ValueError:
            return float(x.value)

    @staticmethod
    def int(items):
        [x] = items
        return int(x.value)

    @staticmethod
    def force(items) -> Force:
        return sum([Force(x.value) for x in items], Force())

    @staticmethod
    def pitch(items) -> Pitch:
        return sum([Pitch(x.value) for x in items], Pitch())

    @staticmethod
    def beat(items) -> Beat:
        return sum([Beat(x.value) for x in items], Beat())

    @staticmethod
    def hold(items) -> Hold:
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
    def __init__(self, track: str, starttime: (float, Tuple[float, ...]), note: (List[int], str), type: str, force: float, beat: float, hold: (int, str)):
        self.track = track
        self.start = starttime
        self.type = type
        self.note = note
        self.force = force
        self.beat = beat
        self.hold = hold

    def __repr__(self):
        return "<sound %s %s (%s) %s f=%s,b=%s,h=%s>" % (self.track, self.start, self.type, self.note, self.force, self.beat, self.hold)

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
                     type=t, force=note.force.get_value(), beat=b, hold=h or 0)

def gen_ast(txt: str) -> lark.Tree:
    return parser.parse(txt)

def gen_ir(ast: lark.Tree) -> Iterable[IR]:
    yield from gather_from(transformer().transform(ast).children)

def exec_ir(ir: Iterable[IR], *, state=None) -> Iterable[Sound]:
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
