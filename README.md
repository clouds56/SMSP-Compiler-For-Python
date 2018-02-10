SMSP
====

Standard Musical String Protocol

forked from [MMA implementation](https://github.com/yxlllc/SMSP-Compiler-For-Mathematica)

FAQ
----
1. What is SMSP
  * SMSP is a protocol that transform string to music, in order to record or create music. It is intended to be a representation of the sheet. Different from traditional representation, SMSP could be input using standard keyboard. SMSP also combines some features of functional programming and modern DAW, so that SMSP could be readable and efficiently expressible at the same time
2. What is SMSP compiler
  * A compiler receives SMSP strings and outputs playable music file.
  The playable file could be in forms of MIDI or wave, or even some executable file.
3. What is basic of SMSP?
  * There're 2 kinds of symbols in SMSP, "Note" and "Control". It is recommended to have whitespace (space, tab or newline) between each symbol, while it is not necessary without ambiguity.
  * Note could be interpreted as music events or draw a Note in DAW. Control is some string that not represent some music events, or any other operation in DAW, such as import file, set position of timeline, switch track, change orchestration.
  * The SMSP is interpreted in order, including all note and control.
  * For example, in order to create a melody in helloword.txt

  ```
  1 1 5 5|6 6 5-|4 4 3 3|2 2 1-|
  5 5 4 4|3 3 2-|5 5 4 4|3 3 2-|
  1 1 5 5|6 6 5-|4 4 3 3|2 2 1-
  ```

  in SMSP, use "(section)" to specify which section to start playing, called section control (default is 0). So this source code produce the same result

  ```
  (0) 1 1 5 5|6 6 5-|4 4 3 3|2 2 1-
  (4) 5 5 4 4|3 3 2-|5 5 4 4|3 3 2-
  (8) 1 1 5 5|6 6 5-|4 4 3 3|2 2 1-
  ```

  and also

  ```
  (8) 1 1 5 5|6 6 5-|4 4 3 3|2 2 1-
  (0) 1 1 5 5|6 6 5-|4 4 3 3|2 2 1-
  (4) 5 5 4 4|3 3 2-|5 5 4 4|3 3 2-
  ```

Control symbols
----
* import symbol `<<filename`  
filename often end with ".txt", and it must be encoding in ansi or utf-8, the default search space is location of compiler (could change by compile flags). The symbol is to import all the characters of the file in place (just like "#inlcude" in c/c++). The import could be recursive.  
e.g. `<<chords.txt`

* method symbol `m[barbeat, bpm, base]`  
barbeat is the count of beats in a bar (integer), bpm is the count of beats per minute (float), both are measured by crotchet (quarter note), base is offset of reference tone (integer), measured in temperament, 0 means when interpret pitch, 0 means C4. If not specified, the default value is barbeat=4, bpm=120, base=0.  
e.g. `m[4, 130, 0]`

* track control symbol `[track]`  
must appear in beginning of file or after whitespace (to differ from function call `f[...]`), it would switch to the track or create one of the name. if not specified, it would use the default track with no name (just like have `[]` in head)  
every track could has its own attributes, like timeline, volume and instruments.  
every track has its own timeline, locate at the origin by default  
e.g. `[P]`

* instrument control symbol `i[instrument]`  
Set instrument of current track via name. Here name could be MIDI instrument, or general instrument directly output wave. If not specified, it would use MIDI Piano.  
e.g. `i[Midi[Piano]]`

* volume control symbol `v[volume]`  
Set the volume of current track, measured via extent of amplitude. 0 mean no volumes (-infinity dB), 1 means no adjustment (0dB). if not specified, default volume is 1.  
e.g. `v[0.6]`

* main section symbol `|`  
increase a section to current timeline, it would force align the timeline.  
the force alignment means pad with quiet sign if the section is not full (`1|2|` means `1 0--|2 0--|`), and it has no effect when the section is full. the section should not be overfull, if so, it would overlap in the latter section (`1 1 5 5 6 6 5-|4 4 3 3|2 2 1-` means `(0) 1 1 5 5 (1) 6 6 5- (1) 4 4 3 3 (2) 2 2 1-`  
e.g. `1 1 5 5|6 6 5-`

* cross section control symbol `)`  
like `|`, no force alignment, used for note that cross section  
e.g. `1- 2-)- 3--|` the note `2` last for 3 beats, 2 in the former section and 1 in the latter  

* section control symbol `(section)`  
Set and force align current section in current timeline, 0 means the origin in timeline. It is often non-negative, but could be negative for convenient. (time that <0 would be ignored)  
e.g. `(0) 1 1 5 5|6 6 5-`  

* beat symbol list  
`{"$": 1/8, "=": 1/4, ";": 1/3, "_":1/2, ":":2/3, ".": 3/2, "-": 2}` (could be change in compile flag).  
beat symbol appears after note, must be special characters, calculating total length using sum, may use arbitrary number of `)` to separate cross section note, could be empty.  
if not present, it means 1 beat by default. "-" is special, it means "+1" beat, to the default 1 beat. so notice that `x-.` does not equal to `x.-`  
e.g. `x` 1 beat, `x=_` 1/4+1/2=3/4 beat, `x--` 2+1=3 beats, `x.-` 3/2+1=5/2 beats, `x-.` 2+3/2=7/2 beats  

* fixed fermata symbol list  
`{"~": 2}`  
fermata symbol appears after beat symbol. Like beat symbol, it must be special characters, calculating total length using sum, may be empty.  
if not present, it means no fermata by default.  
it the fermata length is less then the length represent via beat, it would use beat length instead.  
e.g. `1~~ 3 5~ 7`, 1 last for 4 beats, 5 last for 2 beats and the start time acts like `1 3 5 7`


Issue
----
1. change bpm or barbeat in the middle, the translate of section would be wrong
2. freeholdmode is strange could not efficiently express `1~~ 2~ 3 4`

dictionary
----
* orchestration 配器
* melody 旋律
* metre 节拍
* bar 小节
* beats 拍
* crotchet 四分音符
* minim 二分音符
* temperament (十二)平均律
* pitch 音高
* instruments 乐器
* timbre/tone color 音色
* fermata 延音
