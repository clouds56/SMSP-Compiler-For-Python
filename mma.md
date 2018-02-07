`StringDelete["pattern"]["string"]` or `StringDelete["string", "pattern"]`
  e.g. `StringDelete["abcde12345abcde", DigitCharacter ..]`
`Except[c, p?]` matches p but not c
  e.g. `StringReplace["the cat in the hat", Except[Characters["aeiou"]] -> ""] (*eaiea*)`
`StringReplace[]`

`Shortest[pattern]`
  e.g. `StringCases["a[b,c[d]]e[f]", Shortest["["~~x__~~"]"]-> x] (*{b,c[d,f}*)`
`Riffle[a, b]`
  e.g. `Riffle[{1, 2, 3}, x] (*{1,x,2,x,3}*)`
`WordCharacter` `LetterQ` or `DigitQ`

`StringPart`
  e.g. `StringPart["abcdefghijlkm", 6] (*f*)`
`Cases`
  e.g. `Cases[{1, 1, f[a], 2, 3, y, f[8], 9, f[10]}, f[x_] -> x] (*{4,8,10}*)`
    `Cases[{1, 1, f[a], 2, 3, y, f[8], 9, f[10]}, Except[_Integer]] (*{f[a],y,f[8],f[10]}*)`
`/;` `Condition`
`UnitStep` UnitStep[x_/;x<0]:=0, UnitStep[x_/;x>=0]:=1
