datasave=<||>;
SetSharedVariable[datasave];
CMajor={"C"->0,"D"->2,"E"->4,"F"->5,"G"->7,"A"->9,"B"->11};
NameToNumber[namestring_]:=
  If[
    StringLength[namestring]>3,
    amestring,
    (#1/.CMajor)+Boole[#2=="#"]+12*(ToExpression[#3]-4)&@@
      StringPart[namestring,{1,2,-1}]]
MusicElementMidi[address_]:=
  {
    {#[[2,1]]},
    {NameToNumber@#[[1]]},
    #[[2,2]]-#[[2,1]],
    (SoundVolume/.#[[-1]])^2,
    If[Length[#]==4,ToString@Midi[#[[3]]],"Midi[]"],
    1
  }&/@Import[address][[1]]
Midi[instrument_:"Piano",list_:0][notelist_,time_,force_]:=
  SoundNote[
    Flatten@Outer[Plus,notelist,Flatten[{list}]],
    time,
    If[
      Head[instrument]===Symbol,
      ToString[instrument],
      instrument],
    SoundVolume->Sqrt[force]]
KS[base_:0,\[Alpha]_:0.875,start_:0.015,cut_:100,damp_:0.2,mix_:0.5][notelist_,time_,force_]:=
  SampledSoundList[
    AudioData[
      force AudioNormalize@Total[
        HighpassFilter[
          AudioDelay[
            HighpassFilter[
              AudioGenerator[{"Color",\[Alpha]},start],
              Quantity[cut,"Hz"]],
              1/#,
              damp^(1/#),
              mix,
              PaddingSize->4],
            Quantity[cut,"Hz"]
        ]&/@(440*2^((notelist-9+base)/12))]
    ],
    {44100,16}];
Options[MusicCreate]={"Repeat"->True};
MusicCreate[elementmatrix_,scant_:{0,\[Infinity]},OptionsPattern[]]:=
  Module[
    {eventsave,soundvolume},
    Sound@Flatten@Map[
      (
        If[
          OptionValue["Repeat"]&&KeyExistsQ[datasave,#[[2;;5]]],
          eventsave=datasave[#[[2;;5]]],
          eventsave=ToExpression[#[[5]]][#[[2]],#[[3]],#[[4]]];
            If[
              Head[eventsave]=!=SoundNote,
              AssociateTo[datasave, #[[2;;5]]->eventsave]];
        ];
        soundvolume=If[Head[eventsave]===SoundNote,Sqrt[#[[6]]],#[[6]]];
        Table[
          Sound[eventsave,{t-scant[[1]]},SoundVolume->soundvolume],
          {t,Cases[#[[1]],t_/;scant[[1]]<=t<=scant[[2]]]}]
      )&,
      elementmatrix]]

(*编译开关*)
Options[SMSPCompile]=
  {
    (*是否扩展C和弦定义至全音阶，默认是*)
    "CChordExtend"->True,
    (*定义和弦映射时是否采用音程单位，默认否*)
    "IntervalUnit"->False,
    (*编译目录，默认当前工作目录*)
    "Directory"->NotebookDirectory[]
  };

(*编译器*)
SMSPCompile[test_,OptionsPattern[]]:=With[{
  (*全局不变量*)
  (*音阶*)
  scale={"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11},
  (*变调*)
  pitchlist={"@"->-1,"#"->1,"!"->-12,"^"->12},
  (*节拍*)
  beatlist={"$"->1/8,"="->1/4,";"->1/3,"_"->1/2,":"->2/3,"."->3/2,"-"->2},
  (*增益*)
  forcelist={"\""->0.25,"'"->0.5,"+"->2,"*"->4},
  (*定长延音*)
  fixedholdlist={"~"->2},
  (*非定长延音规则*)
  freeholdrule={"d"->"default","n"->"normal","p"->"pedal"},

  (*控制符号*)
  (*休止符*)rest="0",
  (*主小节符*)mainbar="|",
  (*方法符*)mtdset="m",
  (*音量符*)volset="v",
  (*音色符*)insset="i",
  (*延音启动符*)holdstart="`",
  (*延音结束符*)holdend="&",
  (*左函数符*)leftfun="[",
  (*右函数符*)rightfun="]",
  (*左定义符*)leftdef="{",
  (*右定义符*)rightdef="}",
  (*左小节符*)leftbar="(",
  (*右小节符*)rightbar=")",
  (*左和弦符*)leftchord="<",
  (*右和弦符*)rightchord=">",
  (*导入符*)include="<<",
  (*注释符*)comment="%",
  (*分隔符*)split=",",

  (*音轨对象*)
  deftrack=<|
      (*乐器*)"instrument"->"Midi[1]",
      (*音量*)"volume"->1,
      (*和弦*)"chord"->{0,4,7},
      (*延音计数*)"holdcount"->0,
      (*延音模式*)"holdmode"->"default",
      (*指针时间*)"pointertime"->{0},
      (*启动时间*)"starttime"->{0}
    |>
},Module[{
(*全局变量*)
(*拍性*)barbeat=4,
(*速度*)bpm=120,
(*基准音*)base=0,
(*可用和弦*)chordlist=<||>,
(*和弦映射*)chordmap=<||>,
(*非定长延音标记*)freeholdlist=<||>,
(*音轨名称*)trackname="",

trclist,NowTrack,TrackSet,TrackAddTo,

DeleteComment,Directory,Include,
InputQ,parlist,pattern,flist=<||>,dlist=<||>,FReplace,

forcepat=forcelist[[;;,1]],
pitchpat=pitchlist[[;;,1]],
beatpat=beatlist[[;;,1]]|rightbar,
holdpat=fixedholdlist[[;;,1]]|holdstart,
NotePitch,NoteBeat,NoteForce,NoteHold,NoteSpan,Note,
BarMain,Bar,Track,Method,Volume,Instrument,FreeHold,FreeHoldMain,CMajor,Interval,ChordDef,ChordMap,ChordMain},

(*默认音轨对象集合*)
trclist=<|trackname->deftrack|>;(*初始化全局变量*)

(*当前音轨,不可用于赋值*)
NowTrack[]:=trclist[trackname];

(*设置音轨对象内参数的值*)
TrackSet[name_,value_]:=(trclist[trackname][name]=value);

(*加等于音轨对象内参数的值*)
TrackAddTo[name_,value_]:=(trclist[trackname][name]+=value);

(*删除注释*)
DeleteComment[input_]:=StringDelete[input,comment~~Except["\n"]..|""];

(*编译目录*)
Directory=If[MemberQ[{"/","\\"},StringPart[OptionValue["Directory"],-1]],StringDrop[OptionValue["Directory"],-1],OptionValue["Directory"]];

(*导入文件*)
Include[input_]:=StringReplace[
  DeleteComment[input],
  include~~address:Except[WhitespaceCharacter]..:>
    Include@Import@If[
      StringContainsQ[address,":"],
      address,
      Directory<>"/"<>If[StringPart[address,1]==="/",StringDrop[address,1],address]]];

InputQ[input_]:=
  Module[
    {count=0},
    Catch[
      StringCases[
        input,
        {
          leftfun:>(count+=1),
          rightfun:>If[(count-=1)<0,Throw[False]]
        }
      ];
      count==0]];

FReplace[string_]:=StringReplace[
  string,
  {
    (*函数定义*)
    Shortest[name:WordCharacter..~~leftfun~~input:(WordCharacter|",")...~~rightfun~~leftdef~~output___~~rightdef]:>
      (
        parlist=StringCases[input,par:WordCharacter..:>{par,ToExpression[par]}];
        pattern=Riffle[Pattern[#,__]/;InputQ[#]&/@parlist[[;;,2]],","]/.List->StringExpression;
        AssociateTo[flist,name->pattern:>#]&@StringReplace[FReplace[output],#1->#2&@@@parlist];
        ""
      ),
    (*直接定义*)
    Shortest[name:WordCharacter..~~leftdef~~output___~~rightdef]:>
      (AssociateTo[dlist,name->FReplace[output]];""),
    (*函数替换*)
    name:WordCharacter..~~leftfun~~input:Except[WhitespaceCharacter]...~~rightfun:>
      If[
        KeyExistsQ[flist,name],
        FReplace@StringReplace[input,flist[name]],
        name~~leftfun~~input~~rightfun
      ],
    (*直接替换*)
    name:WordCharacter..:>(name/.dlist)
  }];

(*获取音符的音高*)
NotePitch[pitch_,element_]:=
  Total@StringCases[pitch,pitchlist]+
  Flatten@{
    If[
      KeyExistsQ[chordmap,element],
      Cases[chordmap[element][NowTrack[]["chord"]],_Integer],
      element/.scale]}+
  base/.Plus[n_,s_String]:>s;

(*获取音符的节拍*)
NoteBeat[beat_]:=
  If[
    #==={},
    1,
    Total[#-UnitStep[#-2]]+UnitStep[#[[1]]-2]
  ]&@StringCases[beat,beatlist];

(*获取音符的力度*)
NoteForce[force_]:=
  If[
    #==={},
    1,
    Times@@#
  ]&@StringCases[force,forcelist];

(*获取音符的延续时间*)
NoteHold[hold_,beatvalue_,starttime_]:=
  If[
    NowTrack[]["holdmode"]=="pedal"||StringContainsQ[hold,holdstart],
    (*非定长延音*)
    trackname<>" "<>ToString[NowTrack[]["holdcount"]+1]-First[starttime],
    (*定长延音*)
    1/bpm 60Max[Total@StringCases[hold,fixedholdlist],beatvalue]];

(*检测音符的跨小节情况*)
NoteSpan[beat_]:=
  TrackAddTo["pointertime", 60barbeat/bpm StringCount[beat,rightbar]];

(*翻译音符成事件*)
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

(*翻译主小节符*)
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

(*翻译小节符*)
Bar[input_]:=
  (
    TrackSet[
      "starttime",
      TrackSet[
        "pointertime",
        60barbeat/bpm ToExpression["{"<>input<>"}"]]];
    Nothing
  );

(*翻译音轨符*)
Track[input_]:=(If[!KeyExistsQ[trclist,input],AssociateTo[trclist,input->deftrack]];trackname=input;
  Nothing);

(*翻译方法符*)
Method[input_]:=({barbeat,bpm,base}=ToExpression["{"<>input<>"}"];
  Nothing);

(*翻译音量符*)
Volume[input_]:=(TrackSet["volume",ToExpression[input]];
  Nothing);

(*翻译音色符*)
Instrument[input_]:=(TrackSet["instrument",input];
  Nothing);

(*翻译延音符*)
FreeHold[input_]:=
  (
    If[
      MemberQ[{"default","normal","pedal"},input/.freeholdrule],
      (*非定长延音模式变更*)
      TrackSet["holdmode",input/.freeholdrule],
      (*非定长延音结束符*)
      TrackAddTo["holdcount",1];
        AssociateTo[
          freeholdlist,
          trackname<>" "<>ToString[NowTrack[]["holdcount"]]->
            60barbeat ToExpression[input]/bpm]
        ];
  Nothing);

(*翻译主延音符*)
FreeHoldMain[]:=
  (
    TrackAddTo["holdcount",1];
    AssociateTo[
      freeholdlist,
      trackname<>" "<>ToString[NowTrack[]["holdcount"]]->
        First[NowTrack[]["starttime"]]];
    Nothing
  )

CMajor={"C"->0,"D"->2,"E"->4,"F"->5,"G"->7,"A"->9,"B"->11};

Interval={1,2,2,2,3,4,4,1,5,2,6,3,7,4};

(*翻译和弦定义符*)
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

(*翻译和弦映射符*)
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

(*翻译主和弦符*)
ChordMain[pitch_,name_]:=(
  TrackSet[
    "chord",
    Total@StringCases[pitch,pitchlist]+name/.chordlist];
  Nothing);

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
        (*音轨符*)
        "",Track[input],
        (*方法符*)
        mtdset,Method[input],
        (*音量符*)
        volset,Volume[input],
        (*音色符*)
        insset,Instrument[input]
      ],
    (*主小节符*)
    mainbar:>BarMain[],
    (*小节符*)
    Shortest[leftbar~~input___~~rightbar]:>Bar[input],
    (*延音符*)
    Shortest[holdend~~leftfun~~input___~~rightfun]:>FreeHold[input],
    (*主延音符*)
    holdend:>FreeHoldMain[],
    (*和弦符*)
    Shortest[name:WordCharacter...~~leftchord~~input___~~rightchord]:>
      If[
        name==""||UpperCaseQ@StringPart[name,1],
        (*翻译和弦定义符*)
        ChordDef[name,input],
        (*翻译和弦映射符*)
        ChordMap[name,input]],
    (*主和弦符*)
    Shortest[pitch:pitchpat...~~name:WordCharacter...~~rightchord]:>ChordMain[pitch,name],
    (*音符*)
    force:forcepat...~~pitch:pitchpat...~~element:WordCharacter..~~beat:beatpat...~~hold:holdpat...:>
      Note[force,pitch,element,beat,hold]
  }
]/.freeholdlist
]]
