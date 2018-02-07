import os, re, sys
from typing import Dict, Tuple, List, Union, Callable

"""CMajor={"C"->0,"D"->2,"E"->4,"F"->5,"G"->7,"A"->9,"B"->11};"""
CMajor={"C":0,"D":2,"E":4,"F":5,"G":7,"A":9,"B":11}
ChordInterval:List[int]=[1,2,2,2,3,4,4,1,5,2,6,3,7,4]

def name_to_number(name:str) -> int:
    """
    NameToNumber[namestring_]:=
      If[ StringLength[namestring]>3,
        namestring,
        (#1/.CMajor)+Boole[#2=="#"]+12*(ToExpression[#3]-4)& @@ StringPart[namestring,{1,2,-1}]
      ]
    """
    if len(name) > 3:
        return name
    return CMajor[name[0]]+(1 if name[1]=="#" else 0)+(int(name[2:])-4)*12

def music_element_midi(address):
    """
    MusicElementMidi[address_]:={
      {#[[2,1]]},
      {NameToNumber@#[[1]]},
      #[[2,2]]-#[[2,1]],
      (SoundVolume/.#[[-1]])^2,
      If[Length[#]==4,ToString@Midi[#[[3]]],"Midi[]"],1
    }& /@ Import[address][[1]]
    """
    raise

def midi(instrument:str="Piano", list_:(int,list)=0):
    """
    Midi[instrument_:"Piano",list_:0][notelist_,time_,force_]:=
      SoundNote[
        Flatten@Outer[Plus,notelist,Flatten[{list}]],
        time,
        If[Head[instrument]===Symbol,ToString[instrument],instrument],
        SoundVolume->Sqrt[force]
      ]
    """
    def midi_(notelist:list, time, force:float):
        return fake_sound()

class compiler:
    """
    barbeat=4,
    bpm=120,
    base=0,
    chordlist={},
    chordmap={},
    freeholdlist={},
    trackname="",

    trclist,NowTrack,TrackSet,TrackAddTo,

    DeleteComment,Directory,Include,
    InputQ,parlist,pattern,flist={},dlist={},FReplace,

    forcepat=forcelist[[;;,1]],
    pitchpat=pitchlist[[;;,1]],
    beatpat=beatlist[[;;,1]]|rightbar,
    holdpat=fixedholdlist[[;;,1]]|holdstart,
    NotePitch,NoteBeat,NoteForce,NoteHold,NoteSpan,Note,
    BarMain,Bar,Track,Method,Volume,Instrument,FreeHold,FreeHoldMain,CMajor,Interval,ChordDef,ChordMap,ChordMain,

    trclist={trackname: deftrack};
    """
    def __init__(self):
        self.scale={"1": 0, "2": 2, "3": 4, "4": 5, "5": 7, "6": 9, "7": 11}
        self.pitchlist={"@": -1,"#": 1,"!": -12,"^": 12}
        self.beatlist={"$": 1/8,"=": 1/4,";": 1/3,"_": 1/2,":": 2/3,".": 3/2,"-": 2}
        self.forcelist={"\"": 0.25,"'": 0.5,"+": 2,"*": 4}
        self.fixedholdlist={"~": 2}
        self.freeholdrule={"d": "default","n": "normal","p": "pedal"}

        self.symbols = {
            "rest": "0",
            "mainbar": "|",
            "mtdset": "m",
            "volset": "v",
            "insset": "i",
            "holdstart": "`",
            "holdend": "&",
            "leftfun": "[",
            "rightfun": "]",
            "leftdef": "{",
            "rightdef": "}",
            "leftbar": "(",
            "rightbar": ")",
            "leftchord": "<",
            "rightchord": ">",
            "include": "<<",
            "comment": "%",
            "split": ",",
        }

        self.deftrack={ # TODO: class
            "instrument": "Midi[1]",
            "volume": 1,
            "chord": [0, 4, 7],
            "holdcount": 0,
            "holdmode": "default",
            "pointertime": 0,
            "starttime": 0,}

        self.option = {
            "chordextend": True,
            "intervalunit": False,
        }
        self.flist = {}
        self.chordlist:Dict[str, List[Union[int, str]]] = {}
        self.chordmap:Dict[str, Callable[[List[int]],List[int]]] = {}
        self.trclist = {}
        self.trackname = ""
        self.barbeat = 4
        self.bpm = 120
        self.base = 0

    def now_track(self):
        """NowTrack[]:=trclist[trackname];"""
        return self.trclist[self.trackname]

    def track_set(self, name:str, value):
        """TrackSet[name_,value_]:=(trclist[trackname][name]=value);"""
        self.trclist[self.trackname][name]=value

    def track_add_to(self, name:str, value):
        """TrackAddTo[name_,value_]:=(trclist[trackname][name]+=value);"""
        self.trclist[self.trackname][name]+=value # TODO: list add has different behaviour

    def delete_comment(self, input_:str) -> str:
        """DeleteComment[input_]:=StringDelete[input,comment~~Except["\n"]..|""];"""
        return re.sub(self.symbols["comment"]+"[^\n]*", "", input_)

    def import_(self, filename:str, *, directories=None) -> str:
        try:
            with open(filename) as f:
                return self.delete_comment(f.read())
        except:
            if directories == None or len(directories) == 0:
                raise
            for i in directories:
                try:
                    return self.import_(os.path.join(i, filename))
                except:
                    pass

    def include(self, input_:str, *, directories=None) -> str:
        """
        Include[input_]:=StringReplace[
          DeleteComment[input],
          include~~address:Except[WhitespaceCharacter]..:>
            Include@Import@If[
              StringContainsQ[address,":"],
              address,
              Directory<>"/"<>If[StringPart[address,1]==="/",StringDrop[address,1],address]]];
        """
        return re.sub(self.symbols["include"]+"(\S+)", lambda s: self.include(self.import_(s.group(1), directories=directories)), input_)

    def is_valid_input(self, input_:str) -> bool:
        count = 0
        for i in input_:
            if i == '[':
                count += 1
            elif ']':
                count -= 1
                if count < 0:
                    return False
        return count == 0

    def func_replace(self, input_:str) -> str:
        """
        FReplace[string_]:=StringReplace[
          string,
          {
            Shortest[
              name:WordCharacter..~~leftfun~~input:(WordCharacter|",")...~~rightfun~~leftdef~~output___~~rightdef]:>
                (
                  parlist=StringCases[input,par:WordCharacter..:>{par,ToExpression[par]}];
                  pattern=Riffle[Pattern[#,__]/;InputQ[#]&/@parlist[[;;,2]],","]/.List->StringExpression;
                  AssociateTo[flist,name->pattern:>#]&@StringReplace[FReplace[output],#1->#2&@@@parlist];
                  ""
                ),
              Shortest[name:WordCharacter..~~leftdef~~output___~~rightdef]:>
                (AssociateTo[dlist,name->FReplace[output]];""),
              name:WordCharacter..~~leftfun~~input:Except[WhitespaceCharacter]...~~rightfun:>
                If[
                  KeyExistsQ[flist,name],
                  FReplace@StringReplace[input,flist[name]],
                  name~~leftfun~~input~~rightfun
                ],
              name:WordCharacter..:>(name/.dlist)
            }];
        """
        # TODO: the order of function define/override matters?
        input_ = re.sub("(?P<name>[0-9a-zA-Z]+)\[(?P<input>[0-9a-zA-Z,]+)\]\s*\{(?P<output>[^}]*)\}",
            lambda s: self.set_flist(s.group('name'), self.compile_f(s.group("input"),s.group("output"))), input_)
        input_ = re.sub("(?P<name>[0-9a-zA-Z]+)\s*\{(?P<output>[^}]*)\}",
            lambda s: self.set_flist(s.group('name'), self.compile_f("", s.group('output'))), input_)
        input_ = re.sub("(?P<name>[0-9a-zA-Z]+)\[(?P<input>[0-9a-zA-Z,]+)\]",
            lambda s: s.group(0) if s.group('name') not in self.flist else self.apply_f(s.group('input'), self.flist[s.group('name')]), input_)
        input_ = re.sub("(?P<name>[0-9a-zA-Z]+)(\[\])?",
            lambda s: s.group(0) if s.group('name') not in self.flist else self.apply_f("", self.flist[s.group('name')]), input_)
        return input_

    def compile_f(self, input_:str, output:str):
        params = re.findall("[0-9a-zA-Z]+", input_)
        def _fun(*args):
            rd = dict(zip(params, args))
            return re.sub("|".join(params), lambda s: rd[s.group(0)], output)
        return _fun if len(params) > 0 else lambda a: output

    def apply_f(self, input_:str, func) -> str:
        args = input_.split(",")
        return func(*args)

    def set_flist(self, name:str, func) -> str:
        self.flist[name] = func
        return ""

    def note_pitch(self, pitch:str, element:str):
        """
        NotePitch[pitch_,element_]:=
          Total@StringCases[pitch,pitchlist]+
          Flatten@{
            If[
              KeyExistsQ[chordmap,element],
              Cases[chordmap[element][NowTrack[]["chord"]],_Integer],
              element/.scale]}+
          base/.Plus[n_,s_String]:>s;
        """
        pitch_add = sum([self.pitchlist[i] for i in pitch if i in self.pitchlist])
        if element in self.chordmap:
            r = [i for i in self.chordmap[element](self.now_track()["chord"]) if isinstance(i, int)]
        else:
            r = [self.scale[i] for i in element]
        return [self.base + i + pitch_add for i in r]

    def note_span(self, beat:str):
        """
        NoteSpan[beat_]:=
          TrackAddTo["pointertime", 60barbeat/bpm StringCount[beat,rightbar]];
        """
        self.track_add_to("pointertime", 60*self.barbeat/self.bpm*len([i for i in beat if i in ")"]))

    def note_beat(self, beat:str):
        """
        NoteBeat[beat_]:=If[#==={},1,Total[#-UnitStep[#-2]]+UnitStep[#[[1]]-2]]&@StringCases[beat,beatlist];
        """
        t = [self.beatlist[i] for i in beat if i in self.beatlist]
        return sum([i if i<2 else i-1 for i in t]) + (1 if t[0]==2 else 0)

    def note_hold(self, hold:str, starttime:float):
        """
        NoteHold[hold_,beatvalue_,starttime_]:=
          If[
            NowTrack[]["holdmode"]=="pedal"||StringContainsQ[hold,holdstart],
            (* fixed fermata *)
            trackname<>" "<>ToString[NowTrack[]["holdcount"]+1]-First[starttime],
            (* non-fixed fermata *)
            1/bpm 60Max[Total@StringCases[hold,fixedholdlist],beatvalue]];
        """
        if self.now_track()["holdmode"] == "pedal" or self.symbols["holdstart"] in hold:
            "%s_%d"%(self.trackname, self.now_track()["holdcount"]+1) - starttime

    def note(self, force, pitch, element, beat, hold):
        """
        Note[force_,pitch_,element_,beat_,hold_]:=
          Module[{starttime,beatvalue},
            NoteSpan[beat];
            If[
              element==rest,
              TrackAddTo["starttime",60 NoteBeat[beat]/bpm];Nothing,
              beatvalue=NoteBeat[beat];
              starttime=NowTrack[]["starttime"];
              TrackAddTo["starttime",60beatvalue/bpm];
              {
                starttime,
                NotePitch[pitch,element],
                NoteHold[hold,beatvalue,starttime],
                NoteForce[force],
                NowTrack[]["instrument"],
                NowTrack[]["volume"]
              }]];
        """
        self.note_span(beat) # TODO: why?

        beatvalue = self.note_beat(beat)
        if element == self.symbols["rest"]:
            self.track_add_to("starttime", 60*beatvalue/self.bpm)
        else:
            starttime = self.now_track()["starttime"]
            self.track_add_to("starttime", 60*beatvalue/self.bpm)
            return starttime, self.note_pitch(pitch, element), self.note_hold(hold, beatvalue, starttime), self.note_force(force), self.now_track()["instrument"], self.now_track()["volume"]

    def freeholdmain(self):
        """
        FreeHoldMain[]:=
          (
            TrackAddTo["holdcount",1];
            AssociateTo[
              freeholdlist,
              trackname<>" "<>ToString[NowTrack[]["holdcount"]]->
                First[NowTrack[]["starttime"]]];
            Nothing
          )
        """
        self.track_add_to("holdcount", 1)
        self.freeholdlist["%s_%d" % (self.trackname, self.now_track()["holdcount"])] = self.now_track()["starttime"][0]

    def bar_main(self):
        """
        BarMain[]:=
          (
            If[
              MemberQ[{"normal","pedal"},NowTrack[]["holdmode"]],
              FreeHoldMain[]
            ];
            TrackSet[
              "starttime",
              TrackAddTo["pointertime",60barbeat/bpm]
            ];
            Nothing);
        """
        if self.now_track()["holdmode"] in ["normal", "pedal"]:
            self.freeholdmain()
        self.track_add_to("pointertime", 60*self.barbeat/self.bpm)
        self.track_set("starttime", self.now_track()["pointertime"])

    def chord_def(self, name:str, input_:str):
        """
        ChordDef[name_,input_]:=
          (
            AssociateTo[
              chordlist,
              If[
                name!=""&&OptionValue["CChordExtend"]&&StringPart[name,1]=="C",
                StringReplacePart[name,#,{1,1}]->
                  ToExpression["{"<>input<>"}"]+(#/.CMajor)&/@CMajor[[;;,1]]/.Plus[_,x_]:>x,
                name->ToExpression["{"<>input<>"}"]]];
            TrackSet["chord",chordlist[name]];
            Nothing
          );
        """
        l = [int(x) if not re.match("[a-zA-Z][a-zA-Z0-9]+",x) else x for x in [x.strip() for x in input_.split(",")]]
        if name != "" and self.option["chordextend"] and name[0] == "C":
            for i, j in CMajor.items():
                self.chordlist[i+name[1:]] = [x+j if isinstance(x, int) else x for x in l]
        else:
            self.chordlist[name] = l

    def chord_map(self, name:str, input_:str):
        """
        ChordMap[name_,input_]:=
          With[
            {
              method={
                Total[StringCases[#,pitchlist]],
                If[
                  OptionValue["IntervalUnit"],
                  Interval[[ToExpression@StringCases[#,DigitCharacter,1]]],
                  ToExpression@First@StringCases[#,DigitCharacter,1]]
              }&/@StringSplit[input,","]
            },
            AssociateTo[
              chordmap,
              name->
                Function[
                  chord,
                  #1+chord[[#2]]&@@@
                    Cases[
                      method,
                      element_/;element[[2]]<=Length[chord]]]];
            Nothing];
        """
        l_ = [x.strip() for x in input_.split(",")]
        l = [(sum([self.pitchlist[j] for j in i if j in self.chordlist]), int(re.findall("[0-9]+",i)[0])) for i in l_]
        if self.option["intervalunit"]:
            l = [(i[0], ChordInterval[i[1]]) for i in l]
        def _fun(chord:List[int]):
            return [i[0]+chord[i[1]] for i in l if i[1] <= len(chord)]
        self.chordmap[name] = _fun

    def chord_main(self, pitch:str, name:str):
        """
        ChordMain[pitch_,name_]:=(
          TrackSet[
            "chord",
            Total@StringCases[pitch,pitchlist]+name/.chordlist];
          Nothing);
        """
        pitch_add = sum([self.pitchlist[i] for i in pitch if i in self.pitchlist])
        self.trclist[self.trackname]["chord"] = [j+pitch_add for j in self.chordlist[name]]

    def parse(self, input_:str, *, directories=None):
        """
        StringCases[
          FReplace@Fold[
            StringReplace[#1,#2]&,
            Include[test],
            {
              s:Except[WordCharacter|WhitespaceCharacter|holdend]~~Whitespace:>s,
              Whitespace~~s:Except[WordCharacter|WhitespaceCharacter|leftfun]:>s,
              {
                rightfun~~c:Except[rightfun|split|leftdef]:>rightfun~~" "~~c,
                split->","
              }
            }
          ],
          {
            name:WordCharacter...~~leftfun~~input:Except[WhitespaceCharacter]...~~rightfun:>
              Switch[
                name,
                (* track *)
                "",Track[input],
                (* method *)
                mtdset,Method[input],
                (* volume *)
                volset,Volume[input],
                (* instrument *)
                insset,Instrument[input]
              ],

            (* main bar *)
            mainbar:>BarMain[],

            (* section *)
            Shortest[leftbar~~input___~~rightbar]:>Bar[input],

            (* fermata *)
            Shortest[holdend~~leftfun~~input___~~rightfun]:>FreeHold[input],

            (* main fermata *)
            holdend:>FreeHoldMain[],

            (* chord *)
            Shortest[name:WordCharacter...~~leftchord~~input___~~rightchord]:>
              If[
                name==""||UpperCaseQ@StringPart[name,1],
                (* chord define *)
                ChordDef[name,input],
                (* chord map *)
                ChordMap[name,input]],

            (* main chord *)
            Shortest[pitch:pitchpat...~~name:WordCharacter...~~rightchord]:>ChordMain[pitch,name],

            (* note *)
            force:forcepat...~~pitch:pitchpat...~~element:WordCharacter..~~beat:beatpat...~~hold:holdpat...:>
              Note[force,pitch,element,beat,hold]
          }
        ]/.freeholdlist
        """
        s = self.delete_comment(input_)
        s = self.include(s, directories=directories)
        s = self.func_replace(s)

        tokens = [
            ("func", r"(?P<name>[a-zA-Z0-9]+)\[(?P<input>[^\]]+)\]"),
            ("bar", r"\|"),
            ("section", r"\((?P<input>[0-9]+)\)"),
            ("fermata", r"&\[(?P<input>[^\]]+)\]"),
            ("fermatemain", r"&"),
            ("chord", r"(?P<name>[0-9a-zA-Z]+)?<(?P<input>[^>]+)>"),
            ("chordmain", r"(?P<pitch>[%s]+)?(?P<name>[a-zA-Z0-9]+)?>" % re.escape("".join(self.pitchlist.keys()))),
            ("note", r"(?P<force>[%s]+)?(?P<pitch>[%s]+)?(?P<element>[0-9a-zA-Z]+)(?<beat>[%s]+)?(?P<hold>[%s]+)?" %
             (re.escape("".join(self.forcelist.keys())), re.escape("".join(self.pitchlist.keys())),
              re.escape("".join(self.beatlist.keys())+")"), re.escape("".join(self.fixedholdlist.keys())+"`")))
        ]

        tokens = [(i, v.replace("?P<", "?P<"+i+"_")) for i, v in tokens]

        for m in re.finditer("|".join(["(?P<%s_>%s)"%i for i in tokens]), s):
            kind = m.lastgroup
            if kind == "func_":
                name, input_ = m.group(kind+"name"), m.group(kind+"input")
                if name == "":
                    yield self.track(input_)
                elif name == self.symbols["mtdset"]:
                    yield self.method(input_)
                elif name == self.symbols["volset"]:
                    yield self.volume(input_)
                elif name == self.symbols["insset"]:
                    yield self.instrument(input_)
                else:
                    print("WARN unknown function: %s" % name)
            elif kind == "bar_":
                yield self.bar_main()
            elif kind == "section_":
                yield self.section(m.group(kind+"_input"))
            elif kind == "fermata_":
                yield self.freehold(m.group(kind+"_input"))
            elif kind == "fermatamain_":
                yield self.freeholdmain()
            elif kind == "chord_":
                name, input_ = m.group(kind+"name"), m.group(kind+"input")
                if name == "" or name[0].isupper():
                    yield self.chord_def(name, input_)
                else:
                    yield self.chord_map(name, input_)
            elif kind == "chordmain_":
                yield self.chord_main(m.group(kind+"pitch"), m.group(kind+"name"))
            elif kind == "note_":
                force, pitch, element, beat, hold = m.group(kind+"force"),m.group(kind+"pitch"), m.group(kind+"element"), m.group(kind+"beat"), m.group(kind+"hold")
                yield self.note(force, pitch, element, beat, hold)

    def track(self, trackname:str):
        self.trackname = trackname

    def method(self, input_:str):
        self.barbeat, self.bpm, self.base = [int(i) for i in input_.split(",")] # TODO: bpm could be float?

    def volume(self, volume:str):
        self.track_set("volume", float(volume))

    def instrument(self, instrument:str):
        self.track_set("instrument", instrument)







def test(filename, *, directories=None):
    c = compiler()
    s = c.import_(filename, directories=directories)

    print(list(c.parse(s, directories=directories)))

if __name__ == "__main__":
    test("AlphaChord-Demo.txt", directories=["SMSP-Compiler-For-Mathematica"])
