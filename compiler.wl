datasave=<||>;\[IndentingNewLine]SetSharedVariable[datasave];\[IndentingNewLine]CMajor={"C"->0,"D"->2,"E"->4,"F"->5,"G"->7,"A"->9,"B"->11};\[IndentingNewLine]NameToNumber[namestring_]:=\[IndentingNewLine]If[StringLength[namestring]>3,namestring,(#1/.CMajor)+Boole[#2=="#"]+12*(ToExpression[#3]-4)&@@StringPart[namestring,{1,2,-1}]]\[IndentingNewLine]MusicElementMidi[address_]:={{#[[2,1]]},{NameToNumber@#[[1]]},#[[2,2]]-#[[2,1]],SuperscriptBox[RowBox[{"(", RowBox[{"SoundVolume", "/.", RowBox[{"#", "[", RowBox[{"[", RowBox[{"-", "1"}], "]"}], "]"}]}], ")"}], "2"],If[Length[#]\[Equal]4,ToString@Midi[#[[3]]],"Midi[]"],1}&/@Import[address][[1]]\[IndentingNewLine]Midi[instrument_:"Piano",list_:0][notelist_,time_,force_]:=SoundNote[Flatten@Outer[Plus,notelist,Flatten[{list}]],time,If[Head[instrument]===Symbol,ToString[instrument],instrument],SoundVolume->SqrtBox["force"]]\[IndentingNewLine]KS[base_:0,\[Alpha]_:0.875,start_:0.015,cut_:100,damp_:0.2,mix_:0.5][notelist_,time_,force_]:=SampledSoundList[AudioData[ force AudioNormalize @Total[ HighpassFilter[AudioDelay[HighpassFilter[AudioGenerator[{"Color",\[Alpha]},start],TemplateBox[{"cut", "\"Hz\"", "hertz", "\"Hertz\""}, "Quantity"]],1/#,SuperscriptBox["damp", FractionBox["1", "#"]],mix,PaddingSize->4],TemplateBox[{"cut", "\"Hz\"", "hertz", "\"Hertz\""}, "Quantity"]]&/@(440*SuperscriptBox["2", FractionBox[RowBox[{"notelist", "-", "9", "+", "base"}], "12"]])]],{44100,16}];\[IndentingNewLine]Options[MusicCreate]={"Repeat"->True};\[IndentingNewLine]MusicCreate[elementmatrix_,scant_:{0,\[Infinity]},OptionsPattern[]]:=\[IndentingNewLine]Module[{eventsave,soundvolume},\[IndentingNewLine]Sound@Flatten@Map[(\[IndentingNewLine]If[OptionValue["Repeat"]&&KeyExistsQ[datasave,#[[2;;5]]],\[IndentingNewLine]eventsave=datasave[#[[2;;5]]],\[IndentingNewLine]eventsave=ToExpression[#[[5]]][#[[2]],#[[3]],#[[4]]];\[IndentingNewLine]If[Head[eventsave]=!=SoundNote,AssociateTo[datasave, #[[2;;5]]->eventsave]];\[IndentingNewLine]];\[IndentingNewLine]soundvolume=If[Head[eventsave]===SoundNote,SqrtBox[RowBox[{"#", "[", RowBox[{"[", "6", "]"}], "]"}]],#[[6]]];\[IndentingNewLine]Table[Sound[eventsave,{t-scant[[1]]},SoundVolume->soundvolume],{t,Cases[#[[1]],t_/;scant[[1]]<=t<=scant[[2]]]}])&,elementmatrix]]\[IndentingNewLine]\[IndentingNewLine](*编译开关*)\[IndentingNewLine]Options[SMSPCompile]=\[IndentingNewLine]{\[IndentingNewLine](*是否扩展C和弦定义至全音阶，默认是*)\[IndentingNewLine]"CChordExtend"->True,\[IndentingNewLine](*定义和弦映射时是否采用音程单位，默认否*)\[IndentingNewLine]"IntervalUnit"->False,\[IndentingNewLine](*编译目录，默认当前工作目录*)\[IndentingNewLine]"Directory"->NotebookDirectory[]\[IndentingNewLine]};\[IndentingNewLine]\[IndentingNewLine](*编译器*)\[IndentingNewLine]SMSPCompile[test_,OptionsPattern[]]:=\[IndentingNewLine]With[{\[IndentingNewLine](*全局不变量*)\[IndentingNewLine]	(*音阶*)\[IndentingNewLine]	scale={"1"->0,"2"->2,"3"->4,"4"->5,"5"->7,"6"->9,"7"->11},\[IndentingNewLine]\[IndentingNewLine]	(*变调*)\[IndentingNewLine]	pitchlist={"@"->-1,"#"->1,"!"->-12,"^"->12},\[IndentingNewLine]\[IndentingNewLine]	(*节拍*)\[IndentingNewLine]	beatlist={"$"->1/8,"="->1/4,";"->1/3,"_"->1/2,":"->2/3,"."->3/2,"-"->2},\[IndentingNewLine]\[IndentingNewLine]	(*增益*)\[IndentingNewLine]	forcelist={"\""->0.25,"'"->0.5,"+"->2,"*"->4},\[IndentingNewLine]\[IndentingNewLine]	(*定长延音*)\[IndentingNewLine]	fixedholdlist={"~"->2},\[IndentingNewLine]\[IndentingNewLine]	(*非定长延音规则*)\[IndentingNewLine]	freeholdrule={"d"->"default","n"->"normal","p"->"pedal"},\[IndentingNewLine]\[IndentingNewLine](*控制符号*)\[IndentingNewLine]	(*休止符*)rest="0",\[IndentingNewLine]	(*主小节符*)mainbar="|",\[IndentingNewLine]	(*方法符*)mtdset="m",	\[IndentingNewLine]	(*音量符*)volset="v",\[IndentingNewLine]	(*音色符*)insset="i",\[IndentingNewLine]	(*延音启动符*)holdstart="`",\[IndentingNewLine]	(*延音结束符*)holdend="&",\[IndentingNewLine]	(*左函数符*)leftfun="[",\[IndentingNewLine]	(*右函数符*)rightfun="]",\[IndentingNewLine]	(*左定义符*)leftdef="{",\[IndentingNewLine]	(*右定义符*)rightdef="}",\[IndentingNewLine]	(*左小节符*)leftbar="(",\[IndentingNewLine]	(*右小节符*)rightbar=")",\[IndentingNewLine]	(*左和弦符*)leftchord="<",\[IndentingNewLine]	(*右和弦符*)rightchord=">",\[IndentingNewLine]	(*导入符*)include="<<",\[IndentingNewLine]	(*注释符*)comment="%",\[IndentingNewLine]	(*分隔符*)split=",",\[IndentingNewLine]\[IndentingNewLine](*音轨对象*)\[IndentingNewLine]deftrack=<|\[IndentingNewLine](*乐器*)"instrument"->"Midi[1]",\[IndentingNewLine](*音量*)"volume"->1,\[IndentingNewLine](*和弦*)"chord"->{0,4,7},\[IndentingNewLine](*延音计数*)"holdcount"->0,\[IndentingNewLine](*延音模式*)"holdmode"->"default",\[IndentingNewLine](*指针时间*)"pointertime"->{0},\[IndentingNewLine](*启动时间*)"starttime"->{0}|>\[IndentingNewLine]},Module[{\[IndentingNewLine](*全局变量*)\[IndentingNewLine]	(*拍性*)barbeat=4,\[IndentingNewLine]	(*速度*)bpm=120,\[IndentingNewLine]	(*基准音*)base=0,\[IndentingNewLine]	(*可用和弦*)chordlist=<||>,\[IndentingNewLine]	(*和弦映射*)chordmap=<||>,\[IndentingNewLine]	(*非定长延音标记*)freeholdlist=<||>,\[IndentingNewLine]	(*音轨名称*)trackname="",\[IndentingNewLine]\[IndentingNewLine]trclist,NowTrack,TrackSet,TrackAddTo,\[IndentingNewLine]\[IndentingNewLine]DeleteComment,Directory,Include,\[IndentingNewLine]InputQ,parlist,pattern,flist=<||>,dlist=<||>,FReplace,\[IndentingNewLine]\[IndentingNewLine]forcepat=forcelist[[;;,1]],\[IndentingNewLine]pitchpat=pitchlist[[;;,1]],\[IndentingNewLine]beatpat=beatlist[[;;,1]]|rightbar,\[IndentingNewLine]holdpat=fixedholdlist[[;;,1]]|holdstart,\[IndentingNewLine]NotePitch,NoteBeat,NoteForce,NoteHold,NoteSpan,Note,\[IndentingNewLine]BarMain,Bar,Track,Method,Volume,Instrument,FreeHold,FreeHoldMain,CMajor,Interval,ChordDef,ChordMap,ChordMain},\[IndentingNewLine](*默认音轨对象集合*)\[IndentingNewLine]trclist=<|trackname->deftrack|>;(*初始化全局变量*)\[IndentingNewLine]\[IndentingNewLine](*当前音轨,不可用于赋值*)\[IndentingNewLine]NowTrack[]:=trclist[trackname];\[IndentingNewLine]\[IndentingNewLine](*设置音轨对象内参数的值*)\[IndentingNewLine]TrackSet[name_,value_]:=(trclist[trackname][name]=value);\[IndentingNewLine]\[IndentingNewLine](*加等于音轨对象内参数的值*)\[IndentingNewLine]TrackAddTo[name_,value_]:=(trclist[trackname][name]+=value);\[IndentingNewLine]\[IndentingNewLine](*删除注释*)\[IndentingNewLine]DeleteComment[input_]:=StringDelete[input,comment~~Except["\n"]..|""];\[IndentingNewLine](*编译目录*)\[IndentingNewLine]Directory=If[MemberQ[{"/","\\"},StringPart[OptionValue["Directory"],-1]],StringDrop[OptionValue["Directory"],-1],OptionValue["Directory"]];\[IndentingNewLine]\[IndentingNewLine](*导入文件*)\[IndentingNewLine]Include[input_]:=StringReplace[DeleteComment[input],include~~address:Except[WhitespaceCharacter]..:>\[IndentingNewLine]Include@Import@If[StringContainsQ[address,":"],\[IndentingNewLine]address,\[IndentingNewLine]Directory<>"/"<>If[StringPart[address,1]==="/",StringDrop[address,1],address]]];\[IndentingNewLine]\[IndentingNewLine]InputQ[input_]:=Module[{count=0},\[IndentingNewLine]Catch[\[IndentingNewLine]StringCases[input,{leftfun:>(count+=1),rightfun:>If[(count-=1)<0,Throw[False]]}];\[IndentingNewLine]count\[Equal]0\[IndentingNewLine]]\[IndentingNewLine]];\[IndentingNewLine]FReplace[string_]:=\[IndentingNewLine]StringReplace[string,\[IndentingNewLine]{(*函数定义*)\[IndentingNewLine]Shortest[name:WordCharacter..~~leftfun~~input:(WordCharacter|",")...~~rightfun~~leftdef~~output___~~rightdef]:>\[IndentingNewLine](parlist=StringCases[input,par:WordCharacter..:>{par,ToExpression[par]}];\[IndentingNewLine]pattern=Riffle[Pattern[#,__]/;InputQ[#]&/@parlist[[;;,2]],","]/.List->StringExpression;\[IndentingNewLine]AssociateTo[flist,name->pattern:>#]&@StringReplace[FReplace[output],#1->#2&@@@parlist];\[IndentingNewLine]""),\[IndentingNewLine]\[IndentingNewLine](*直接定义*)\[IndentingNewLine]Shortest[name:WordCharacter..~~leftdef~~output___~~rightdef]:>\[IndentingNewLine](\[IndentingNewLine]AssociateTo[dlist,name->FReplace[output]];\[IndentingNewLine]"")\[IndentingNewLine],\[IndentingNewLine]\[IndentingNewLine](*函数替换*)\[IndentingNewLine]name:WordCharacter..~~leftfun~~input:Except[WhitespaceCharacter]...~~rightfun:>If[KeyExistsQ[flist,name],FReplace@StringReplace[input,flist[name]],name~~leftfun~~input~~rightfun],\[IndentingNewLine]\[IndentingNewLine](*直接替换*)\[IndentingNewLine]name:WordCharacter..:>(name/.dlist)}];\[IndentingNewLine]\[IndentingNewLine]\[IndentingNewLine](*获取音符的音高*)\[IndentingNewLine]NotePitch[pitch_,element_]:=\[IndentingNewLine]Total@StringCases[pitch,pitchlist]+Flatten@{If[KeyExistsQ[chordmap,element],Cases[chordmap[element][NowTrack[]["chord"]],_Integer],element/.scale]}+base/.Plus[n_,s_String]:>s;\[IndentingNewLine]\[IndentingNewLine](*获取音符的节拍*)\[IndentingNewLine]NoteBeat[beat_]:=If[#==={},1,Total[#-UnitStep[#-2]]+UnitStep[#[[1]]-2]]&@StringCases[beat,beatlist];\[IndentingNewLine]\[IndentingNewLine](*获取音符的力度*)\[IndentingNewLine]NoteForce[force_]:=If[#==={},1,Times@@#]&@StringCases[force,forcelist];\[IndentingNewLine]\[IndentingNewLine](*获取音符的延续时间*)\[IndentingNewLine]NoteHold[hold_,beatvalue_,starttime_]:=\[IndentingNewLine]If[NowTrack[]["holdmode"]\[Equal]"pedal"||StringContainsQ[hold,holdstart],\[IndentingNewLine](*非定长延音*)\[IndentingNewLine]trackname<>" "<>ToString[NowTrack[]["holdcount"]+1]-First[starttime],\[IndentingNewLine](*定长延音*)\[IndentingNewLine]FractionBox["1", "bpm"]60Max[Total@StringCases[hold,fixedholdlist],beatvalue]];\[IndentingNewLine]\[IndentingNewLine](*检测音符的跨小节情况*)\[IndentingNewLine]NoteSpan[beat_]:=\[IndentingNewLine]TrackAddTo["pointertime",FractionBox[RowBox[{"60", "barbeat"}], "bpm"]StringCount[beat,rightbar]];\[IndentingNewLine](*翻译音符成事件*)\[IndentingNewLine]Note[force_,pitch_,element_,beat_,hold_]:=\[IndentingNewLine]Module[{starttime,beatvalue},\[IndentingNewLine]NoteSpan[beat];\[IndentingNewLine]If[element\[Equal]rest,\[IndentingNewLine]TrackAddTo["starttime",FractionBox[RowBox[{"60", " ", RowBox[{"NoteBeat", "[", "beat", "]"}]}], "bpm"]];\[IndentingNewLine]\[IndentingNewLine]Nothing,\[IndentingNewLine]beatvalue=NoteBeat[beat];\[IndentingNewLine]starttime=NowTrack[]["starttime"];\[IndentingNewLine]TrackAddTo["starttime",FractionBox[RowBox[{"60", " ", "beatvalue"}], "bpm"]];\[IndentingNewLine]{starttime,NotePitch[pitch,element], NoteHold[hold,beatvalue,starttime],NoteForce[force],NowTrack[]["instrument"],NowTrack[]["volume"]}\[IndentingNewLine]]];\[IndentingNewLine]\[IndentingNewLine](*翻译主小节符*)\[IndentingNewLine]BarMain[]:=\[IndentingNewLine](If[MemberQ[{"normal","pedal"},NowTrack[]["holdmode"]],FreeHoldMain[]];\[IndentingNewLine]TrackSet["starttime",TrackAddTo["pointertime",FractionBox[RowBox[{"60", "barbeat"}], "bpm"]]];\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译小节符*)\[IndentingNewLine]Bar[input_]:=(TrackSet["starttime",TrackSet["pointertime",FractionBox[RowBox[{"60", "barbeat"}], "bpm"]ToExpression["{"<>input<>"}"]]];\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译音轨符*)\[IndentingNewLine]Track[input_]:=\[IndentingNewLine](If[ !KeyExistsQ[trclist,input],AssociateTo[trclist,input->deftrack]];\[IndentingNewLine]trackname=input;\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译方法符*)\[IndentingNewLine]Method[input_]:=\[IndentingNewLine]({barbeat,bpm,base}=ToExpression["{"<>input<>"}"];Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译音量符*)\[IndentingNewLine]Volume[input_]:=(TrackSet["volume",ToExpression[input]];Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译音色符*)\[IndentingNewLine]Instrument[input_]:=(TrackSet["instrument",input];Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译延音符*)\[IndentingNewLine]FreeHold[input_]:=\[IndentingNewLine](If[MemberQ[{"default","normal","pedal"},input/.freeholdrule],\[IndentingNewLine](*非定长延音模式变更*)\[IndentingNewLine]TrackSet["holdmode",input/.freeholdrule],\[IndentingNewLine]\[IndentingNewLine](*非定长延音结束符*)\[IndentingNewLine]TrackAddTo["holdcount",1];\[IndentingNewLine]AssociateTo[freeholdlist,trackname<>" "<>ToString[NowTrack[]["holdcount"]]->\[IndentingNewLine]FractionBox[RowBox[{"60", "barbeat", " ", RowBox[{"ToExpression", "[", "input", "]"}]}], "bpm"]]];\[IndentingNewLine]Nothing);\[IndentingNewLine](*翻译主延音符*)\[IndentingNewLine]FreeHoldMain[]:=\[IndentingNewLine](TrackAddTo["holdcount",1];\[IndentingNewLine]AssociateTo[freeholdlist,\[IndentingNewLine]trackname<>" "<>ToString[NowTrack[]["holdcount"]]->First[NowTrack[]["starttime"]]];\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine]CMajor={"C"->0,"D"->2,"E"->4,"F"->5,"G"->7,"A"->9,"B"->11};\[IndentingNewLine]\[IndentingNewLine]Interval={1,2,2,2,3,4,4,1,5,2,6,3,7,4};\[IndentingNewLine]\[IndentingNewLine](*翻译和弦定义符*)\[IndentingNewLine]ChordDef[name_,input_]:=\[IndentingNewLine](AssociateTo[chordlist,\[IndentingNewLine]If[name!=""&&OptionValue["CChordExtend"]&&StringPart[name,1]\[Equal]"C",StringReplacePart[name,#,{1,1}]->ToExpression["{"<>input<>"}"]+(#/.CMajor)&/@CMajor[[;;,1]]/.Plus[_,x_]:>x,\[IndentingNewLine]name->ToExpression["{"<>input<>"}"]\[IndentingNewLine]]];\[IndentingNewLine]TrackSet["chord",chordlist[name]];\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine](*翻译和弦映射符*)\[IndentingNewLine]ChordMap[name_,input_]:=With[{method={Total[StringCases[#,pitchlist]],If[OptionValue["IntervalUnit"],Interval[[ToExpression@StringCases[#,DigitCharacter,1]]],\[IndentingNewLine]ToExpression@First@StringCases[#,DigitCharacter,1]\[IndentingNewLine]]}&/@StringSplit[input,","]},\[IndentingNewLine]AssociateTo[chordmap,\[IndentingNewLine]name->Function[chord,#1+chord[[#2]]&@@@Cases[method,element_/;element[[2]]<=Length[chord]]]];\[IndentingNewLine]Nothing];\[IndentingNewLine]\[IndentingNewLine]\[IndentingNewLine](*翻译主和弦符*)\[IndentingNewLine]ChordMain[pitch_,name_]:=\[IndentingNewLine](TrackSet["chord",Total@StringCases[pitch,pitchlist]+name/.chordlist];\[IndentingNewLine]Nothing);\[IndentingNewLine]\[IndentingNewLine]StringCases[\[IndentingNewLine]FReplace@Fold[StringReplace[#1,#2]&,Include[test],{s:Except[WordCharacter|WhitespaceCharacter|holdend]~~Whitespace:>s,Whitespace~~s:Except[WordCharacter|WhitespaceCharacter|leftfun]:>s,\[IndentingNewLine]{rightfun~~c:Except[rightfun|split|leftdef]:>rightfun~~" "~~c,split->","}}],\[IndentingNewLine]{\[IndentingNewLine]name:WordCharacter...~~leftfun~~input:Except[WhitespaceCharacter]...~~rightfun:>\[IndentingNewLine]Switch[name,\[IndentingNewLine](*音轨符*)\[IndentingNewLine]"",Track[input],\[IndentingNewLine]\[IndentingNewLine](*方法符*)\[IndentingNewLine]mtdset,Method[input],\[IndentingNewLine](*音量符*)\[IndentingNewLine]volset,Volume[input],\[IndentingNewLine](*音色符*)\[IndentingNewLine]insset,Instrument[input]\[IndentingNewLine]],\[IndentingNewLine]\[IndentingNewLine](*主小节符*)\[IndentingNewLine]mainbar:>BarMain[],\[IndentingNewLine]\[IndentingNewLine](*小节符*)\[IndentingNewLine]Shortest[leftbar~~input___~~rightbar]:>Bar[input],\[IndentingNewLine]\[IndentingNewLine](*延音符*)\[IndentingNewLine]Shortest[holdend~~leftfun~~input___~~rightfun]:>FreeHold[input],\[IndentingNewLine]\[IndentingNewLine](*主延音符*)\[IndentingNewLine]holdend:>FreeHoldMain[],\[IndentingNewLine]\[IndentingNewLine](*和弦符*)\[IndentingNewLine]Shortest[name:WordCharacter...~~leftchord~~input___~~rightchord]:>If[name\[Equal]""||UpperCaseQ@StringPart[name,1],\[IndentingNewLine](*翻译和弦定义符*)\[IndentingNewLine]ChordDef[name,input],\[IndentingNewLine]\[IndentingNewLine](*翻译和弦映射符*)\[IndentingNewLine]ChordMap[name,input]],\[IndentingNewLine]\[IndentingNewLine](*主和弦符*)\[IndentingNewLine]Shortest[pitch:pitchpat...~~name:WordCharacter...~~rightchord]:>ChordMain[pitch,name],\[IndentingNewLine]\[IndentingNewLine](*音符*)\[IndentingNewLine]force:forcepat...~~pitch:pitchpat...~~element:WordCharacter..~~beat:beatpat...~~hold:holdpat...:>Note[force,pitch,element,beat,hold]\[IndentingNewLine]}\[IndentingNewLine]]/.freeholdlist\[IndentingNewLine]]]
