DEFINT A-Z

TYPE Registers
 ax    AS INTEGER
 bx    AS INTEGER
 cx    AS INTEGER
 dx    AS INTEGER
 ps    AS INTEGER
 si    AS INTEGER
 di    AS INTEGER
 flags AS INTEGER
END TYPE

DECLARE SUB Ask (x, Question$, Answer)
DECLARE SUB Center (x, Text$)
DECLARE SUB DispASCIIValues ()
DECLARE SUB DisplayHelp ()
DECLARE SUB DisplayOptions ()
DECLARE SUB DisplayMemoryPosition ()
DECLARE SUB DisplaySelectedOption ()
DECLARE SUB DrawBox (x, y, w, h, Shadow)
DECLARE SUB DrawInterface ()
DECLARE SUB GetMouseInput (Key$, x, y, w)
DECLARE SUB GetMouseStatus (Mx, My, Mb, Position)
DECLARE SUB GetRowCount (Text$, w, h)
DECLARE SUB Interrupt (InterruptN, RegInp AS Registers, RegOut AS Registers)
DECLARE SUB InputBox (x, Prompt$, Text$, MaxLength, InputBoxWidth)
DECLARE SUB KeyPress (Key$, Position)
DECLARE SUB Main ()
DECLARE SUB MakeButton (x, y, Text$, Selected)
DECLARE SUB MouseClick (Mx, My, Mb, Position)
DECLARE SUB MsgBox (x, Text$)
DECLARE SUB PressButton (x, y, Text$)
DECLARE SUB Quit ()
DECLARE SUB ReadMemory ()
DECLARE SUB SearchMemory ()
DECLARE SUB SetMouseBorders (x1, y1, x2, y2)
DECLARE SUB SetVariables ()
OPTION BASE 1
ON ERROR GOTO ErrorTrap
DIM SHARED CurrentFile$, CursorX, CursorY
DIM SHARED DisplayASCValues, DisplayHexValues, DisplayOffsets, DisplayPeriods, DisplayReadableChars
DIM SHARED HexSegment$, OptionsMenuClosed, Offset AS LONG, Segment$
DIM SHARED ASCIIValues$(42), Option$(10)
DIM SHARED RegInp AS Registers, RegOut AS Registers
SCREEN 0: CLS : WIDTH 80, 50: PALETTE 5, 63: PALETTE 6, 56
SetMouseBorders 0, 0, 632, 372
SetVariables
CALL Main

ErrorTrap:
PCOPY 0, 1
 IF LEN(CurrentFile$) > 12 THEN CurrentFile$ = LEFT$(CurrentFile$, 8) + "..."
Choice = 1: ErrorCode = ERR
COLOR 12, 4
DrawBox 10, 25, 26, 3, 1
COLOR 14, 4: LOCATE 11
 IF ErrorCode = 7 OR ErrorCode = 14 THEN
  Center 0, "Not enough memory."
 ELSEIF ErrorCode = 53 THEN
  Center 0, "Cannot find " + CurrentFile$ + "."
 ELSEIF ErrorCode = 55 THEN
  Center 0, CurrentFile$ + " already"
  Center 0, "opened."
 ELSEIF ErrorCode = 61 THEN
  Center 0, "The disk is full."
 ELSEIF ErrorCode = 62 THEN
  Center 0, "Error while reading"
  Center 0, CurrentFile$ + "."
 ELSEIF ErrorCode = 70 THEN
  Center 0, "The disk or file is"
  Center 0, "write protected."
 ELSEIF ErrorCode = 71 THEN
  Center 0, "No disk in diskdrive."
 ELSEIF ErrorCode = 72 THEN
  Center 0, "The disk is damaged."
 ELSEIF ErrorCode = 75 THEN
  Center 0, "Cannot open file."
 ELSEIF ErrorCode = 76 THEN
  Center 0, "Cannot find path."
 ELSE
  Center 0, "Unexpected error."
 END IF
COLOR , 7
 DO
   IF Choice = 1 THEN COLOR 15 ELSE COLOR 0
  LOCATE 13, 28: PRINT " Retry ";
   IF Choice = 2 THEN COLOR 15 ELSE COLOR 0
  LOCATE , 36: PRINT " Ignore ";
   IF Choice = 3 THEN COLOR 15 ELSE COLOR 0
  LOCATE , 45: PRINT " Quit "
   DO
    Key$ = INKEY$
   LOOP WHILE Key$ = ""
   IF Key$ = CHR$(0) + "K" THEN
    IF Choice > 1 THEN Choice = Choice - 1 ELSE Choice = 3
   ELSEIF Key$ = CHR$(0) + "M" THEN
    IF Choice < 3 THEN Choice = Choice + 1 ELSE Choice = 1
   ELSEIF Key$ = CHR$(13) THEN
    IF Choice = 1 THEN PCOPY 1, 0: RESUME
    IF Choice = 2 THEN PCOPY 1, 0: RESUME NEXT
    IF Choice = 3 THEN WIDTH 80, 25: COLOR 7, 0: CLS : SYSTEM
   END IF
 LOOP

SUB Ask (x, Question$, Answer)
PCOPY 0, 2
w = LEN(Question$) + 4
 IF w < 16 THEN w = 16
y = 40 - (w \ 2)
DrawBox x, y, w, 5, 1
MakeButton x + 2, y + 2, " Yes ", 1
MakeButton x + 2, (y + w) - 6, " No  ", 0
Center x + 1, Question$
 DO
  RegInp.ax = 1: Interrupt 51, RegInp, RegOut
  RegInp.ax = 3: Interrupt 51, RegInp, RegOut
  Mb = RegOut.bx
  Mx = (RegOut.cx \ 8) + 1
  My = (RegOut.dx \ 8) + 1
   IF Mb = 1 THEN
    RegInp.ax = 2: Interrupt 51, RegInp, RegOut
     IF My > x + 1 AND My < x + 5 THEN
      IF Mx > y AND Mx < y + 7 THEN
       PressButton x + 2, y + 2, " Yes "
       Answer = 1
       EXIT DO
      ELSEIF Mx > (y + w) - 7 AND Mx < y + w THEN
       MakeButton x + 2, y + 2, " Yes ", 0
       PressButton x + 2, (y + w) - 6, " No  "
       Answer = 0
       EXIT DO
      END IF
     END IF
   END IF
  Key$ = INKEY$
   IF Key$ = CHR$(13) THEN Answer = 1: EXIT DO
   IF Key$ = CHR$(27) THEN Answer = 0: EXIT DO
 LOOP
PCOPY 2, 0
END SUB

SUB Center (x, Text$)
 IF x > 0 THEN LOCATE x
LOCATE , INT(40 - (LEN(Text$) / 2))
PRINT Text$
END SUB

SUB DispASCIIValues
 FOR l = 1 TO 42
  LOCATE l, 1: PRINT MID$(ASCIIValues$(l), CursorY, 37); SPC(37 - LEN(MID$(ASCIIValues$(l), CursorY, 37))); "³"
 NEXT l
COLOR 15: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "Û"
END SUB

SUB DisplayHelp
MakeButton 44, 43, " Help    ", 0
COLOR 0, 5
CurrentFile$ = "Viewmem2.hlp"
OPEN "Viewmem2.hlp" FOR INPUT AS 1
 HelpText$ = INPUT$(LOF(1), 1)
CLOSE 1
MsgBox 10, HelpText$
MakeButton 44, 43, " Help    ", 1
END SUB

SUB DisplayMemoryPosition
 IF DisplayHexValues THEN
  DisplayedOffset$ = HEX$(Offset) + "H"
  DisplayedSegment$ = HEX$(VAL(Segment$)) + "H"
 ELSE
  DisplayedOffset$ = LTRIM$(RTRIM$(STR$(Offset)))
  DisplayedSegment$ = Segment$
 END IF
 
COLOR 7, 1
LOCATE 45, 5
PRINT USING "Offset: \   \   Segment: \     \"; DisplayedOffset$; DisplayedSegment$
END SUB

SUB DisplayOptions
CursorX = 1
Option$(1) = "Display offsets "
Option$(2) = "Display hexadecimal numbers "
Option$(3) = "Display readable characters only "
Option$(4) = "Replace non readable characters by periods "
Option$(5) = "Display characters' ASCII values "
Option$(6) = "Set memory segment"
Option$(7) = "Find text in memory block"
Option$(8) = "Display amount of free memory"
Option$(9) = "Save memory block"
Option$(10) = "Load memory block"

 IF DisplayOffsets THEN MID$(Option$(1), LEN(Option$(1)), 1) = "û" ELSE MID$(Option$(1), LEN(Option$(1)), 1) = " "
 IF DisplayHexValues THEN MID$(Option$(2), LEN(Option$(2)), 1) = "û" ELSE MID$(Option$(2), LEN(Option$(2)), 1) = " "
 IF DisplayReadableChars THEN MID$(Option$(3), LEN(Option$(3)), 1) = "û" ELSE MID$(Option$(3), LEN(Option$(3)), 1) = " "
 IF DisplayPeriods THEN MID$(Option$(4), LEN(Option$(4)), 1) = "û" ELSE MID$(Option$(4), LEN(Option$(4)), 1) = " "
 IF DisplayASCValues THEN MID$(Option$(5), LEN(Option$(5)), 1) = "û" ELSE MID$(Option$(5), LEN(Option$(5)), 1) = " "

MakeButton 44, 54, " Options ", 0

COLOR 0, 5
DrawBox 10, 17, 44, 14, 1
MakeButton 21, 36, " Close ", 1
COLOR 0, 5: Center 10, "´Options Ã"
 FOR mn = 1 TO 10
  LOCATE 10 + mn, 18: PRINT " "; Option$(mn)
 NEXT mn
COLOR 0, 2: LOCATE 11, 18: PRINT " "; Option$(1); SPACE$(43 - LEN(Option$(1)))
 DO
  GetMouseStatus Mx, My, Mb, 2
 LOOP UNTIL OptionsMenuClosed
OptionsMenuClosed = 0
ReadMemory
MakeButton 44, 54, " Options ", 1
LOCATE 43, 1
 IF DisplayASCValues THEN
  COLOR 7, 0
  PRINT CHR$(27);
   FOR Position = 2 TO 37
    LOCATE , Position
    PRINT "°";
   NEXT Position
  PRINT CHR$(26)
  COLOR 15: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "Û"
 ELSE
  COLOR 15, 1
  PRINT STRING$(80, "Ä")
 END IF
END SUB

SUB DisplaySelectedOption
 IF CursorX > 5 THEN MakeButton 21, 36, " Close ", 0
COLOR 0, 5
 IF CursorX = 1 THEN
  IF DisplayASCValues THEN
   MsgBox 10, " Cannot display both offsets and° character's ASCII values at once. "
  ELSE
   DisplayOffsets = NOT DisplayOffsets
  END IF
 ELSEIF CursorX = 2 THEN
  DisplayHexValues = NOT DisplayHexValues
 ELSEIF CursorX = 3 THEN
  DisplayReadableChars = NOT DisplayReadableChars
   IF NOT DisplayReadableChars THEN DisplayPeriods = 0
 ELSEIF CursorX = 4 THEN
  DisplayPeriods = NOT DisplayPeriods
   IF DisplayPeriods THEN DisplayReadableChars = -1
 ELSEIF CursorX = 5 THEN
  IF DisplayOffsets THEN
   MsgBox 10, " Cannot display both offsets and° character's ASCII values at once. "
  ELSE
    IF NOT DisplayASCValues THEN CursorY = 1
   DisplayASCValues = NOT DisplayASCValues
  END IF
 ELSEIF CursorX = 6 THEN
  DIM MinValue AS LONG, MaxValue AS LONG, Segment
  PrevSegment$ = Segment$
  PrevSegment2$ = HexSegment$
  InputBox 10, "Enter memory segment address:", Text$, 5, 10
   IF NOT Text$ = "" THEN
    Segment$ = LTRIM$(RTRIM$(UCASE$(Text$)))
    COLOR 7, 0: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "°"
    Offset = 0
    COLOR 15: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "Û"
    SegmentHex = 0
     IF RIGHT$(Segment$, 1) = "H" THEN
      SegmentHex = -1
     ELSE
      FOR CharASCII = 64 TO 70
       IF INSTR(Segment$, CHR$(CharASCII)) THEN
        SegmentHex = -1
        EXIT FOR
       END IF
      NEXT CharASCII
     END IF
     IF SegmentHex THEN
      HexSegment$ = "&H" + Segment$
      MinValue = -32768
      MaxValue = 32767
     ELSE
      HexSegment$ = Segment$
      MinValue = 0
      MaxValue = 65535
     END IF
    Segment = VAL(HexSegment$)
     IF Segment < MinValue OR Segment > MaxValue THEN
      COLOR 15, 4
      Msg$ = " Segment address ° cannot be° "
       IF Segment < MinValue THEN
        Msg$ = Msg$ + "lower than zero. "
       ELSEIF Segment > MaxValue THEN
        Msg$ = Msg$ + "higher than 65535. "
       END IF
      MsgBox 10, Msg$
      Segment$ = PrevSegment$
      HexSegment$ = PrevSegment2$
     END IF
    DisplayMemoryPosition
   END IF
 ELSEIF CursorX = 7 THEN
  SearchMemory
 ELSEIF CursorX = 8 THEN
  Msg$ = "  String Space: " + LTRIM$(RTRIM$(STR$(FRE(0)))) + " °"
  Msg$ = Msg$ + " Numeric Array: " + LTRIM$(RTRIM$(STR$(FRE(-1)))) + " °"
  Msg$ = Msg$ + "    Stackspace: " + LTRIM$(RTRIM$(STR$(FRE(-2)))) + " °"
  Msg$ = Msg$ + "      Far Heap: " + LTRIM$(RTRIM$(STR$(SETMEM(0))))
  MsgBox 10, Msg$
 ELSEIF CursorX = 9 THEN
  InputBox 10, "Enter file name to save:", File$, 100, 10
   IF NOT File$ = "" THEN
    DEF SEG = VAL(HexSegment$)
    CurrentFile$ = File$
    BSAVE File$, 0, 65535
   END IF
 ELSEIF CursorX = 10 THEN
  COLOR 15, 4
  MsgBox 10, " Warning!° Loading a saved° memory block° can cause the° system to lock up. "
  COLOR 0, 5
  InputBox 10, "Enter file name to load:", File$, 100, 10
   IF NOT File$ = "" THEN
    DEF SEG = VAL(HexSegment$)
    CurrentFile$ = File$
    BLOAD File$, 0
   END IF
 END IF
 IF DisplayOffsets THEN MID$(Option$(1), LEN(Option$(1)), 1) = "û" ELSE MID$(Option$(1), LEN(Option$(1)), 1) = " "
 IF DisplayHexValues THEN MID$(Option$(2), LEN(Option$(2)), 1) = "û" ELSE MID$(Option$(2), LEN(Option$(2)), 1) = " "
 IF DisplayReadableChars THEN MID$(Option$(3), LEN(Option$(3)), 1) = "û" ELSE MID$(Option$(3), LEN(Option$(3)), 1) = " "
 IF DisplayPeriods THEN MID$(Option$(4), LEN(Option$(4)), 1) = "û" ELSE MID$(Option$(4), LEN(Option$(4)), 1) = " "
 IF DisplayASCValues THEN MID$(Option$(5), LEN(Option$(5)), 1) = "û" ELSE MID$(Option$(5), LEN(Option$(5)), 1) = " "
COLOR 0, 5
 FOR OptionN = 1 TO 10
  LOCATE 10 + OptionN, 18: PRINT " "; Option$(OptionN)
 NEXT OptionN
 IF CursorX > 5 THEN MakeButton 21, 36, " Close ", 1
END SUB

SUB DrawBox (x, y, w, h, Shadow)
LOCATE x, y: PRINT "Ú"; STRING$(w, "Ä"); "¿"
 FOR BoxX = 1 TO h
  LOCATE , y: PRINT "³"; SPC(w); "³";
   IF Shadow THEN PRINT "°" ELSE PRINT ""
 NEXT BoxX
LOCATE , y: PRINT "À"; STRING$(w, "Ä"); "Ù";
 IF Shadow THEN PRINT "°": LOCATE , y + 1: PRINT STRING$(w + 2, "°")
END SUB

SUB DrawInterface
COLOR , 1: LOCATE 1: PRINT SPACE$(3760)
MakeButton 44, 43, " Help    ", 1
MakeButton 44, 54, " Options ", 0
MakeButton 44, 65, " Quit    ", 0
COLOR 7, 0
LOCATE 1, 80: PRINT CHR$(24)
 FOR y = 2 TO 41
  LOCATE , 80: PRINT "°"
 NEXT y
LOCATE , 80: PRINT CHR$(25)
COLOR 15, 1: PRINT STRING$(80, "Ä")
LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "Û"
DisplayMemoryPosition
END SUB

SUB GetMouseInput (Key$, x, y, w)
RegInp.ax = 1: Interrupt 51, RegInp, RegOut
RegInp.ax = 3: Interrupt 51, RegInp, RegOut
Mb = RegOut.bx
Mx = (RegOut.cx) \ 8 + 1
My = (RegOut.dx) \ 8 + 1
 IF Mb = 1 THEN
  RegInp.ax = 2: Interrupt 51, RegInp, RegOut
   IF My > x + 1 AND My < x + 5 THEN
    IF Mx > y + 1 AND Mx < y + 10 THEN
     PressButton x + 2, y + 2, " Close  "
     Key$ = CHR$(13)
    ELSEIF Mx > (y + w) - 12 AND Mx < (y + w) - 3 THEN
     MakeButton x + 2, y + 2, " Close  ", 0
     PressButton x + 2, (y + w) - 11, " Cancel "
     Key$ = CHR$(27)
    END IF
   END IF
 END IF
END SUB

SUB GetMouseStatus (Mx, My, Mb, Position)
RegInp.ax = 1: Interrupt 51, RegInp, RegOut
RegInp.ax = 3: Interrupt 51, RegInp, RegOut
Mb = RegOut.bx
Mx = ((RegOut.cx) \ 8) + 1
My = ((RegOut.dx) \ 8) + 1
 IF Mb > 0 THEN MouseClick Mx, My, Mb, Position
Key$ = INKEY$
 IF NOT Key$ = "" THEN KeyPress Key$, Position
END SUB

SUB GetRowCount (Text$, w, h)
w = 0
h = 0
 DO
  BreakPos = INSTR(Text$, "°")
   IF BreakPos = 0 THEN
    l = LEN(Text$)
     IF l > w THEN w = l
    EXIT DO
   ELSE
    l = LEN(LEFT$(Text$, BreakPos - 1))
    Text$ = MID$(Text$, BreakPos + 1)
   END IF
   IF l > w THEN w = l
  h = h + 1
 LOOP
 IF w = 0 THEN w = LEN(Text$)
 IF w < 10 THEN w = 10
END SUB

SUB InputBox (x, Prompt$, Text$, MaxLength, InputBoxWidth)
PCOPY 0, 2
w = LEN(Prompt$) + InputBoxWidth + 6
 IF w < 22 THEN w = 22
y = 40 - (w \ 2)

DrawBox x, y, w - 2, 5, 1
MakeButton x + 2, y + 2, " Close  ", 1
MakeButton x + 2, (y + w) - 11, " Cancel ", 0

 DO
  COLOR 0
  LOCATE x + 1, y + 2: PRINT Prompt$; " "; RIGHT$(Text$, InputBoxWidth); : COLOR 16: PRINT "_ "
   DO
    Key$ = INKEY$
    GetMouseInput Key$, x, y, w
   LOOP WHILE Key$ = ""
  l = LEN(Text$)
   IF Key$ = CHR$(8) AND l > 0 THEN
    Text$ = LEFT$(Text$, l - 1)
   ELSEIF Key$ = CHR$(13) THEN
    EXIT DO
   ELSEIF Key$ = CHR$(27) THEN
    Text$ = "": EXIT DO
   ELSEIF ASC(Key$) > 31 THEN
    IF l < MaxLength THEN Text$ = Text$ + Key$
   END IF
 LOOP
PCOPY 2, 0
COLOR 7
END SUB

SUB KeyPress (Key$, Position)
 IF Position = 1 THEN
   IF DisplayOffsets THEN Stp = 68 ELSE Stp = 79
   IF DisplayASCValues THEN Stp = 41
  COLOR 7, 0: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "°"
   IF Key$ = CHR$(0) + "H" THEN
    IF Offset - Stp >= 0 THEN Offset = Offset - Stp
   ELSEIF Key$ = CHR$(0) + "P" THEN
    IF Offset + Stp < 65536 THEN Offset = Offset + Stp
   ELSEIF Key$ = CHR$(0) + "I" THEN
    IF Offset - (Stp * 43) > 0 THEN Offset = Offset - (Stp * 43)
   ELSEIF Key$ = CHR$(0) + "Q" THEN
    IF Offset + (Stp * 43) < 65536 THEN Offset = Offset + (Stp * 43)
   ELSEIF Key$ = CHR$(8) THEN
    MakeButton 44, 65, " Quit    ", 0
    MakeButton 44, 54, " Options ", 0
    MakeButton 44, 43, " Help    ", 1
    DisplayHelp
   ELSEIF Key$ = CHR$(15) THEN
    MakeButton 44, 65, " Quit    ", 0
    MakeButton 44, 43, " Help    ", 0
    MakeButton 44, 54, " Options ", 1
    DisplayOptions
   ELSEIF Key$ = CHR$(27) THEN
    MakeButton 44, 43, " Help    ", 0
    MakeButton 44, 54, " Options ", 0
    MakeButton 44, 65, " Quit    ", 1
    Quit
   ELSEIF DisplayASCValues THEN
    COLOR 7, 0: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "°"
     IF Key$ = CHR$(0) + "K" THEN
      IF CursorY > 1 THEN CursorY = CursorY - 1
     ELSEIF Key$ = CHR$(0) + "M" THEN
      IF CursorY < 164 THEN CursorY = CursorY + 1
     END IF
    COLOR 15: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "Û"
   END IF
  DisplayMemoryPosition
  COLOR 15, 1: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "Û"
  ReadMemory
 ELSEIF Position = 2 THEN
  COLOR 0, 5: LOCATE 10 + CursorX, 18: PRINT " "; Option$(CursorX); SPACE$(43 - LEN(Option$(CursorX)))
   IF Key$ = CHR$(0) + "H" THEN
    IF CursorX > 1 THEN CursorX = CursorX - 1 ELSE CursorX = 10
   ELSEIF Key$ = CHR$(0) + "P" THEN
    IF CursorX < 10 THEN CursorX = CursorX + 1 ELSE CursorX = 1
   ELSEIF Key$ = CHR$(13) THEN
    DisplaySelectedOption
   ELSEIF Key$ = CHR$(27) THEN
    OptionsMenuClosed = -1
   END IF
  COLOR 0, 2: LOCATE 10 + CursorX, 18: PRINT " "; Option$(CursorX); SPACE$(43 - LEN(Option$(CursorX)))
 END IF
END SUB

SUB Main
DrawInterface
ReadMemory
COLOR 0, 5
MsgBox 10, " Memory Viewer °   By: Peter°   Swinkels°  ***1999***° Version 2.14"
 DO
  GetMouseStatus Mx, My, Mb, 1
 LOOP
END SUB

SUB MakeButton (x, y, Text$, Selected)
LOCATE x, y
ForeColor = SCREEN(CSRLIN, POS(0), 1) - ((SCREEN(CSRLIN, POS(0), 1) \ 16) * 16)
BackColor = SCREEN(CSRLIN, POS(0), 1) \ 16

l = LEN(Text$)
COLOR 7, BackColor: LOCATE x, y: PRINT STRING$(l, "Ü")
 IF Selected THEN COLOR 15 ELSE COLOR 0
COLOR , 7: LOCATE , y: PRINT Text$; : COLOR 8: PRINT "Û"
COLOR 7, BackColor: LOCATE , y: PRINT "ß"; : COLOR , 6: PRINT STRING$(l - 1, "ß"); : COLOR 8: PRINT "Û"
COLOR ForeColor, BackColor
END SUB

SUB MouseClick (Mx, My, Mb, Position)
RegInp.ax = 2: Interrupt 51, RegInp, RegOut
 IF Position = 1 THEN
  IF My > 43 AND My < 47 THEN
   IF Mx > 42 AND Mx < 53 THEN
    MakeButton 44, 65, " Quit    ", 0
    MakeButton 44, 54, " Options ", 0
    PressButton 44, 43, " Help    "
    DisplayHelp
   ELSEIF Mx > 53 AND Mx < 64 THEN
    MakeButton 44, 43, " Help    ", 0
    MakeButton 44, 65, " Quit    ", 0
    PressButton 44, 54, " Options "
    DisplayOptions
   ELSEIF Mx > 64 AND Mx < 75 THEN
    MakeButton 44, 43, " Help    ", 0
    MakeButton 44, 54, " Options ", 0
    PressButton 44, 65, " Quit    "
    Quit
   END IF
  END IF
  IF My > 0 AND My < 44 THEN
   COLOR 7, 0: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "°"
    IF DisplayOffsets THEN Stp = 68 ELSE Stp = 79
    IF Mx = 80 THEN
     IF My = 1 THEN
      IF Offset - Stp > 0 THEN Offset = Offset - Stp
     ELSEIF My = 42 THEN
      IF Offset + Stp < 65536 THEN Offset = Offset + Stp
     ELSEIF My > 1 AND My < 42 THEN
      Offset = CDBL((My - 2) * 1680.41)
     END IF
    END IF
    IF DisplayASCValues THEN
     Stp = 41
     COLOR 7, 0: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "°"
      IF My = 43 THEN
       IF Mx = 1 THEN
        IF CursorY > 1 THEN CursorY = CursorY - 1
       ELSEIF Mx = 38 THEN
        IF CursorY < 164 THEN CursorY = CursorY + 1
       ELSEIF Mx > 1 AND Mx < 38 THEN
        CursorY = (Mx - 2) * 4.685714
         IF CursorY = 0 THEN CursorY = 1
       END IF
      END IF
     COLOR 15: LOCATE 43, (CursorY / 4.685714) + 2: PRINT "Û"
    END IF
   DisplayMemoryPosition
   COLOR 15, 1: LOCATE INT((Offset / 1680.41)) + 2, 80: PRINT "Û"
   ReadMemory
  END IF
 ELSEIF Position = 2 THEN
   IF Mx > 17 AND Mx < 62 AND My > 10 AND My < 21 THEN
    COLOR 0, 5: LOCATE 10 + CursorX, 18: PRINT " "; Option$(CursorX); SPACE$(43 - LEN(Option$(CursorX)))
    CursorX = My - 10
    DisplaySelectedOption
    COLOR 0, 2: LOCATE 10 + CursorX, 18: PRINT " "; Option$(CursorX); SPACE$(43 - LEN(Option$(CursorX)))
   ELSEIF Mx > 35 AND Mx < 43 AND My > 20 AND My < 24 THEN
    PressButton 21, 36, " Close "
    OptionsMenuClosed = -1
   END IF
  DO: RegInp.ax = 3: Interrupt 51, RegInp, RegOut: LOOP UNTIL RegOut.bx = 0
 END IF
END SUB

SUB MsgBox (x, Text$)
PCOPY 0, 2
Message$ = Text$
GetRowCount Text$, w, h
y = 39 - (w \ 2)
DrawBox x, y, w, 5 + h, 1
MakeButton x + h + 2, 36, " Close  ", 1
x2 = x
 DO
  x2 = x2 + 1
  LOCATE x2, y + 1
  BreakPos = INSTR(Message$, "°")
   IF INSTR(Message$, "°") = 0 THEN
    PRINT Message$
    EXIT DO
   ELSE
    PRINT LEFT$(Message$, BreakPos - 1)
    Message$ = MID$(Message$, BreakPos + 1)
   END IF
 LOOP
 DO: RegInp.ax = 3: Interrupt 51, RegInp, RegOut: LOOP UNTIL RegOut.bx = 0
 DO
  RegInp.ax = 1: Interrupt 51, RegInp, RegOut
  RegInp.ax = 3: Interrupt 51, RegInp, RegOut
  Mx = (RegOut.cx \ 8) + 1
  My = (RegOut.dx \ 8) + 1
   IF RegOut.bx = 1 THEN
    RegInp.ax = 2: Interrupt 51, RegInp, RegOut
     IF My > x + h + 1 AND My < x + h + 5 THEN
      IF Mx > 37 AND Mx < 42 THEN
       PressButton x + h + 2, 36, " Close  "
       EXIT DO
      END IF
     END IF
    END IF
 LOOP UNTIL INKEY$ = CHR$(13)
PCOPY 2, 0
END SUB

SUB PressButton (x, y, Text$)
l = LEN(Text$)
LOCATE x, y - 1
ForeColor = SCREEN(CSRLIN, POS(0), 1) - ((SCREEN(CSRLIN, POS(0), 1) \ 16) * 16)
BackColor = SCREEN(CSRLIN, POS(0), 1) \ 16

COLOR , BackColor

LOCATE x, y: PRINT SPACE$(l + 1)
LOCATE , y: PRINT SPACE$(l + 1)
LOCATE , y: PRINT SPACE$(l + 1)

COLOR 7: LOCATE x + 1, y + 1: PRINT STRING$(l, "Ü")
COLOR 0, 7: LOCATE , y + 1: PRINT Text$
COLOR 7, BackColor: LOCATE , y + 1: PRINT STRING$(l, "ß")
 DO: RegInp.ax = 3: Interrupt 51, RegInp, RegOut: LOOP UNTIL RegOut.bx = 0
LOCATE x + 1, y + 1: PRINT SPACE$(l)
LOCATE , y + 1: PRINT SPACE$(l)
LOCATE , y + 1: PRINT SPACE$(l)
MakeButton x, y, Text$, 1
COLOR ForeColor, BackColor
END SUB

SUB Quit
MakeButton 44, 65, " Quit    ", 0
COLOR 0, 5
Ask 10, " Do you want to Quit? ", Answer
 IF Answer = 1 THEN COLOR 7, 0: CLS : WIDTH 80, 25: SYSTEM
MakeButton 44, 65, " Quit    ", 1
END SUB

SUB ReadMemory
DIM MemoryPosition AS LONG

ERASE ASCIIValues$
COLOR 15, 1
LOCATE 1, 1
MemoryPosition = Offset
Row = 1
 IF DisplayOffsets THEN
  FirstColumn = 12
 ELSEIF DisplayASCValues THEN
  FirstColumn = 39
 ELSE
  FirstColumn = 1
 END IF
Column = FirstColumn
 DO
  COLOR 7: LOCATE Row, 1
   IF DisplayOffsets THEN
    IF DisplayHexValues THEN PRINT SPACE$(10 - LEN(HEX$(MemoryPosition))); HEX$(MemoryPosition); "³" ELSE PRINT ; USING "##########³"; MemoryPosition
   END IF
   DO UNTIL Column = 80
    DEF SEG = VAL(HexSegment$)
     IF MemoryPosition < 65535 THEN
      AsciiValue = PEEK(MemoryPosition)
       IF DisplayASCValues THEN
        IF DisplayHexValues THEN
         ASCIIValues$(Row) = ASCIIValues$(Row) + SPACE$(3 - LEN(HEX$(AsciiValue))) + HEX$(AsciiValue)
        ELSE
         ASCIIValues$(Row) = ASCIIValues$(Row) + SPACE$(4 - LEN(LTRIM$(STR$(AsciiValue)))) + LTRIM$(STR$(AsciiValue))
        END IF
       END IF
     ELSE
      AsciiValue = 32
     END IF
    DEF SEG = 47104
    POKE (((Row - 1) * 160) + (Column * 2)) - 1, 31
     IF DisplayReadableChars THEN
       IF AsciiValue > 31 AND AsciiValue < 127 THEN
        POKE (((Row - 1) * 160) + (Column * 2)) - 2, AsciiValue
       ELSE
        IF DisplayPeriods THEN
         POKE (((Row - 1) * 160) + (Column * 2)) - 2, 46
        ELSE
         POKE (((Row - 1) * 160) + (Column * 2)) - 2, 32
        END IF
       END IF
      Column = Column + 1
     ELSE
      POKE (((Row - 1) * 160) + (Column * 2)) - 2, AsciiValue
      Column = Column + 1
     END IF
    MemoryPosition = MemoryPosition + 1
   LOOP
  Column = FirstColumn
   IF Row = 42 THEN EXIT DO ELSE Row = Row + 1
 LOOP
 IF DisplayASCValues THEN DispASCIIValues
END SUB

SUB SearchMemory
PCOPY 0, 1
COLOR , 5
InputBox 10, "Enter text:", Text$, 100, 10
 IF Text$ = "" THEN EXIT SUB
COLOR 0, 5
 IF LTRIM$(RTRIM$(STR$(VAL(Text$)))) = LTRIM$(RTRIM$(Text$)) THEN
  Ask 10, " A number has been entered, look for a string any way?", Answer
   IF Answer = 0 THEN
    IF VAL(Text$) > -32769 AND VAL(Text$) < 32768 THEN
     Text$ = MKI$(VAL(Text$))
    ELSE
     MsgBox 10, " The number is too high. "
     PCOPY 1, 0
     EXIT SUB
    END IF
   END IF
 ELSE
  Ask 10, " Match upper and lower case?", Answer
   IF Answer = 0 THEN Text$ = UCASE$(Text$)
 END IF
DEF SEG = VAL(HexSegment$)
COLOR , 7
DrawBox 12, 26, 26, 3, 1
LOCATE 13, 28: PRINT "Searching memory block."
LOCATE , 28: PRINT "Press Escape to cancel."
COLOR 9, 1: LOCATE 15, 27: PRINT STRING$(25, "°")

DIM Byte AS LONG, FrgByte AS LONG

l = LEN(Text$) - 1
 FOR Byte = 0 TO 65535 - l
  Fragment$ = ""
  COLOR 9: LOCATE 15, 27: PRINT STRING$((Byte / 655.35) \ 4, "Û")
   FOR FrgByte = Byte TO Byte + l
    Fragment$ = Fragment$ + CHR$(PEEK(FrgByte))
    Key$ = INKEY$
     IF Key$ = CHR$(27) THEN EXIT FOR
   NEXT FrgByte
   IF Key$ = CHR$(27) THEN EXIT FOR
   IF Answer = 0 THEN Fragment$ = UCASE$(Fragment$)
   IF Fragment$ = Text$ THEN
    COLOR 0, 5
    MsgBox 10, "Text found at: " + LTRIM$(STR$(Byte)) + "."
   END IF
 NEXT Byte
PCOPY 1, 0
END SUB

SUB SetMouseBorders (x1, y1, x2, y2)
RegInp.ax = 7
RegInp.cx = x1
RegInp.cx = x2
Interrupt 51, RegInp, RegOut
RegInp.ax = 8
RegInp.cx = y1
RegInp.cx = y2
Interrupt 51, RegInp, RegOut
END SUB

SUB SetVariables
CurrentFile$ = ""
CursorX = 0: CursorY = 0
DisplayOffsets = 0
DisplayASCValues = 0
DisplayHexValues = 0
DisplayPeriods = 0
DisplayReadableChars = 0
HexSegment$ = "0"
Offset = 0
OptionsMenuClosed = 0
Segment$ = "0"

ERASE ASCIIValues$, Option$
END SUB

