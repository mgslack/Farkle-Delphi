unit Fark_Win;

(*                                                                      *)
(* Unit defines the main window used by the game of Farkle.             *)
(*                                                                      *)

interface

 uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
      StdCtrls, ExtCtrls, Buttons, Dice, OvcBase, OvcEF, OvcPB, OvcNF, FarkCnst;

 type TFarkleWin = class(TForm)
    Panel1       : TPanel;
    Label1       : TLabel;
    KeepBtn      : TBitBtn;
    Label5       : TLabel;
    Dice1        : TDice;
    Label6       : TLabel;
    Dice2        : TDice;
    Label7       : TLabel;
    Dice3        : TDice;
    Label8       : TLabel;
    Dice4        : TDice;
    Label9       : TLabel;
    Dice5        : TDice;
    Label10      : TLabel;
    Dice6        : TDice;
    Panel2       : TPanel;
    Label2       : TLabel;
    Dice7        : TDice;
    Dice8        : TDice;
    Dice9        : TDice;
    Dice10       : TDice;
    Dice11       : TDice;
    Dice12       : TDice;
    KeptScoreL   : TLabel;
    Panel3       : TPanel;
    Label3       : TLabel;
    Player1      : TLabel;
    Player2      : TLabel;
    Player3      : TLabel;
    Player4      : TLabel;
    Player5      : TLabel;
    Player6      : TLabel;
    GroupBox1    : TGroupBox;
    CB_VerboseOn : TCheckBox;
    CB_Sounds    : TCheckBox;
    CB_Strict    : TCheckBox;
    CB_Safe      : TCheckBox;
    CB_FourTwo   : TCheckBox;
    CB_Sort      : TCheckBox;    
    Label4       : TLabel;
    OvcN_ToS     : TOvcNumericField;
    NewBtn       : TBitBtn;
    ExitBtn      : TBitBtn;
    HelpBtn      : TBitBtn;
    RollBtn      : TBitBtn;
    EndBtn       : TBitBtn;
    OvcCntrl     : TOvcController;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure FormDestroy(Sender : TObject);
    procedure FormKeyUp(Sender : TObject; var Key : Word;
                        Shift : TShiftState);
    procedure NewBtnClick(Sender : TObject);
    procedure HelpBtnClick(Sender : TObject);
    procedure RollBtnClick(Sender : TObject);
    procedure EndBtnClick(Sender : TObject);
    procedure RolledDiceClick(Sender : TObject);
    procedure KeptDiceClick(Sender : TObject);
    procedure KeepBtnClick(Sender: TObject);
    procedure OvcN_ToSAfterExit(Sender : TObject);
   private
    RolledDice    : ARRAY[1..NumDice] OF TDice;
    KeptDice      : ARRAY[1..NumDice] OF TDice;
    Players       : PLAYER_ARRAY;
    PlayerScores  : ARRAY[1..MaxPlayers] OF INTEGER;
    PlayerDisplay : ARRAY[1..MaxPlayers] OF TLabel;
    KeptCount,
    OldKeptScore,
    KeptScore,
    WinScore,
    NumPlayers,
    FirstPlayerWin,
    WinningPlayer,
    CurrentPlayer : INTEGER;
    Exe_Path      : STRING[255];
    SoundFiles    : ARRAY[1..NumFiles] OF STRING[255];
    SoundAvail    : ARRAY[1..NumFiles] OF BOOLEAN;
    CompPlayerTwo : BOOLEAN;
    PROCEDURE InitGameAndGlobals;
    PROCEDURE DisplayPlayerPanel;
    PROCEDURE ShowKeptScore;
    PROCEDURE ShowScoreDialog;
    PROCEDURE ClearKept(IncScore : BOOLEAN);
    PROCEDURE ClearRolled;
    PROCEDURE DisplayDice(Dice : TDICE_ARRAY);
    PROCEDURE DisplayCompMove(KDice : TDICE_ARRAY; VAR N : INTEGER);
    PROCEDURE Pause_Me(Millisecs : INTEGER);
    PROCEDURE PauseMe(Seconds : INTEGER);
    PROCEDURE PlayExtraSound(Which : INTEGER);
    PROCEDURE Read_Reg;
    PROCEDURE Write_Reg;
    PROCEDURE LoadNameList;
    FUNCTION  CheckScore(KDice : TDICE_ARRAY) : BOOLEAN;
   PROTECTED
    PROCEDURE WMStartRound(VAR Msg : TMESSAGE);
     MESSAGE wm_StartRound;
    PROCEDURE WMCompMove(VAR Msg : TMESSAGE);
     MESSAGE wm_CompMove;
    PROCEDURE WMNextPlayer(VAR Msg : TMESSAGE);
     MESSAGE wm_NextPlayer;
    PROCEDURE WMSysCommand(VAR Msg : TMESSAGE);
     MESSAGE wm_SysCommand;
   public
    { Public declarations }
   end;

 var FarkleWin : TFarkleWin;

(************************************************************************)

implementation

{$R *.DFM}

 USES ShellApi, MMSystem, Registry, Center, FarkPDlg, FarkADlg, FarkGame;

 CONST Reg_Key : STRING[45] = 'Software\Slack and Associates\Games\Farkle';
       Ini_1K1 : STRING[7]  = 'Verbose';
       Ini_1K2 : STRING[6]  = 'Sounds';
       Ini_1K3 : STRING[6]  = 'Strict';
       Ini_1K4 : STRING[4]  = 'Safe';
       Ini_1K5 : STRING[7]  = 'FourTwo';
       Ini_1K6 : STRING[4]  = 'Sort';
       Ini_1K7 : STRING[7]  = 'ScoreTo';
       Ini_1K8 : STRING[6]  = 'Player';

(************************************************************************)

 procedure TFarkleWin.FormCreate(Sender : TObject);
    VAR I : INTEGER;
  begin
   Top := (Screen.Height - Height) DIV 2; Left := (Screen.Width - Width) DIV 2;
   // initialize vars
   KeptScore := 0; WinScore := DefWinScore; OldKeptScore := 0; KeptCount := 0;
   CompPlayerTwo := FALSE;
   FirstPlayerWin := 0; CurrentPlayer := 0; NumPlayers := 0; WinningPlayer := 0;
   CompPlyrNames := TSTRINGLIST.Create;
   FOR I := 1 TO MaxPlayers DO
    BEGIN
     PlayerScores[I] := 0;
     Players[I] := '';
    END;
   Randomize;
   // check for sound files, set exe_path
   Exe_Path := ParamStr(0); I := Length(Exe_Path);
   WHILE I > 0 DO
    IF Exe_Path[I] <> '\' THEN Dec(I) ELSE Break;
   SetLength(Exe_Path, I);
   FOR I := 1 TO NumFiles DO
    BEGIN
     SoundFiles[I] := Exe_Path + SoundFns[I];
     SoundAvail[I] := FileExists(SoundFns[I]);
    END;
   // set dice components in arrays
   RolledDice[1] := Dice1;
   RolledDice[2] := Dice2;
   RolledDice[3] := Dice3;
   RolledDice[4] := Dice4;
   RolledDice[5] := Dice5;
   RolledDice[6] := Dice6;
   KeptDice[1] := Dice7;
   KeptDice[2] := Dice8;
   KeptDice[3] := Dice9;
   KeptDice[4] := Dice10;
   KeptDice[5] := Dice11;
   KeptDice[6] := Dice12;
   // set player display labels in array
   PlayerDisplay[1] := Player1;
   PlayerDisplay[2] := Player2;
   PlayerDisplay[3] := Player3;
   PlayerDisplay[4] := Player4;
   PlayerDisplay[5] := Player5;
   PlayerDisplay[6] := Player6;
  end;

(************************************************************************)

 procedure TFarkleWin.FormShow(Sender : TObject);
    VAR S_Menu : HMENU;
  begin
   Caption := Application.Title;
   // get config settings from reg
   Read_Reg;
   // set other things
   OvcN_ToS.AsInteger := WinScore;
   NewBtn.SetFocus;
   // load comp player name list
   LoadNameList;
   {set up system menu}
   S_Menu := GetSystemMenu(Handle, FALSE);
   DeleteMenu(S_Menu, sc_Size, mf_ByCommand);
   DeleteMenu(S_Menu, sc_Maximize, mf_ByCommand);
   DeleteMenu(S_Menu, sc_TaskList, mf_ByCommand);
   AppendMenu(S_Menu, mf_Enabled, sc_About, '&About');
   {need to change minimized sys menu also}
   S_Menu := GetSystemMenu(Application.Handle, FALSE);
   DeleteMenu(S_Menu, sc_Size, mf_ByCommand);
   DeleteMenu(S_Menu, sc_Maximize, mf_ByCommand);
  end;

(************************************************************************)

 procedure TFarkleWin.FormDestroy(Sender : TObject);
  begin
   CompPlyrNames.Free;
   Write_Reg;
  end;

(************************************************************************)

 procedure TFarkleWin.FormKeyUp(Sender : TObject; var Key : Word;
                                Shift : TShiftState);
    VAR K   : CHAR;
        Idx : INTEGER;
  begin
   // if in edit field, do not process keystrokes
   IF NOT(OvcN_ToS.Focused)
    THEN BEGIN
          K := CHAR(Key);
          IF K IN ['1'..'6']
           THEN BEGIN
                 Idx := Ord(K) - Ord('0');
                 IF KeptDice[Idx].FaceNumber = dfn0
                  THEN RolledDiceClick(RolledDice[Idx])
                 ELSE KeptDiceClick(KeptDice[Idx]);
                END;
         END;
  end;

(************************************************************************)

 procedure TFarkleWin.NewBtnClick(Sender : TObject);
    VAR I, Ret : INTEGER;
  begin
   IF (CB_VerboseOn.Checked) AND (CurrentPlayer <> 0) AND (WinningPlayer = 0)
    THEN BEGIN
          Ret := MessageDlgPos('Are you sure you wish to start a new game?',
                               mtConfirmation, [mbYes, mbNo], 0,
                               Left+25, Top+25);
          IF Ret <> mrYes THEN Exit;
         END;
   // get players playing (number and names)
   PlayerDlg := TPlayerDlg.Create(Self);
   CenterPopUp(PlayerDlg, Handle);
   PlayerDlg.SoundsOn := CB_Sounds.Checked;
   FOR I := 1 TO MaxPlayers DO
    PlayerDlg.Players[I] := Players[I];
   IF CompPlayerTwo THEN PlayerDlg.Players[2] := '';
   Ret := PlayerDlg.ShowModal;
   IF Ret = mrOK
    THEN BEGIN {get names, start game}
          FOR I := 1 TO MaxPlayers DO
           Players[I] := PlayerDlg.Players[I];
          NumPlayers := PlayerDlg.NumPlayers;
          CompPlayerTwo := PlayerDlg.CompPlayer;
          InitGameAndGlobals;
          IF (CompPlayerTwo) AND (CurrentPlayer = 2)
           THEN PostMessage(Handle, wm_CompMove, 0, 0)
          ELSE PostMessage(Handle, wm_StartRound, 0, 0);
         END; {then}
   PlayerDlg.Free;
  end;

(************************************************************************)

 procedure TFarkleWin.HelpBtnClick(Sender : TObject);
    VAR EE : ARRAY[0..255] OF CHAR;
        RR : INTEGER;
  begin
   StrPCopy(EE, Exe_Path);
   // use users web-browser to display help page...
   RR := ShellExecute(Handle, 'open', 'Farkle_Help.html', NIL, EE, sw_ShowNormal);
   IF (RR <= 32)
    THEN MessageDlgPos('Failed to open help "Farkle_Help.html" (' +
                       IntToStr(RR) + ').', mtInformation, [mbOk], 0,
                       Left+25, Top+25);
  end;

(************************************************************************)

 procedure TFarkleWin.RollBtnClick(Sender : TObject);
    VAR Dice : TDICE_ARRAY;
        I, N : INTEGER;
  begin
   N := 0;
   FOR I := 1 TO NumDice DO
    IF RolledDice[I].FaceNumber = dfn0 THEN Inc(N);
   IF N <> 6
    THEN BEGIN {rolling again - have dice left on table}
          IF KeptCount = 0
           THEN BEGIN
                 IF CB_Sounds.Checked THEN MessageBeep(mb_OK);
                 MessageDlgPos('Need to keep some dice before rolling again.',
                               mtInformation, [mbOK], 0, Left+25, Top+25);
                 Exit;
                END;
          N := 0;
          FOR I := 1 TO NumDice DO
           IF RolledDice[I].FaceNumber <> dfn0 THEN Inc(N);
         END;
   FOR I := 1 TO NumDice DO
    Dice[I] := INTEGER(KeptDice[I].FaceNumber);
   IF NOT(ValidateKeptDice(Dice, CB_FourTwo.Checked))
    THEN BEGIN
          IF CB_Sounds.Checked THEN MessageBeep(mb_OK);
          MessageDlgPos(BadDiceMsg, mtInformation, [mbOK], 0, Left+25, Top+25);
          Exit;
         END;
   IF NOT(CB_Strict.Checked) AND (N = 6) AND (KeptScore <> 0)
    THEN BEGIN {rerolling also, add to players total, zero kept}
          IF CB_VerboseOn.Checked THEN ShowScoreDialog;
          PlayerScores[CurrentPlayer] := PlayerScores[CurrentPlayer] + KeptScore;
          KeptScore := 0;
          ShowKeptScore;
          DisplayPlayerPanel;
         END;
   IF (N <> 6) AND NOT(CheckScore(Dice)) THEN Exit;
   IF CB_Sounds.Checked
    THEN BEGIN
          PlayExtraSound(F_Roll_Snd);
          Pause_Me(500);
         END;
   ClearKept(FALSE);
   OldKeptScore := KeptScore; KeptCount := 0;
   IF CB_Sort.Checked THEN RollDiceSorted(Dice, N) ELSE RollDice(Dice, N);
   DisplayDice(Dice);
   KeepBtn.Enabled := TRUE; RollBtn.Enabled := FALSE; EndBtn.SetFocus;
  end;

(************************************************************************)

 procedure TFarkleWin.EndBtnClick(Sender : TObject);
    VAR I, Ret : INTEGER;
        Dice   : TDICE_ARRAY;
  begin
   IF (CB_VerboseOn.Checked)
    THEN Ret := MessageDlgPos('Are you sure you wish to end your turn?',
                              mtConfirmation, [mbYes, mbNo], 0,
                              Left+25, Top+25)
   ELSE BEGIN
         Ret := mrYes;
         IF (KeptCount = 0) AND (CB_Safe.Checked)
          THEN BEGIN // check dice for points or not and if rolled
                IF RolledDice[1].FaceNumber = dfn0
                 THEN BEGIN
                       Ret := MessageDlgPos('It appears as if you haven''t rolled.'#10+
                                            'Are you sure you want to pass?',
                                            mtConfirmation, [mbYes, mbNo], 0,
                                            Left+25, Top+25);
                      END
                ELSE BEGIN
                      FOR I := 1 TO NumDice DO
                       Dice[I] := INTEGER(RolledDice[I].FaceNumber);
                      I := GetKeptScore(Dice, CB_FourTwo.Checked);
                      IF I > 0
                       THEN Ret := MessageDlgPos('You have points to score with.'#10 +
                                                 'Are you sure you want to end your turn?',
                                                 mtConfirmation, [mbYes, mbNo],
                                                 0, Left+25, Top+25);
                     END;
               END;
        END;
   IF (KeptCount <> 0) AND (Ret = mrYes)
    THEN BEGIN {kept some dice during the round}
          FOR I := 1 TO NumDice DO
           Dice[I] := INTEGER(KeptDice[I].FaceNumber);
          IF NOT(ValidateKeptDice(Dice, CB_FourTwo.Checked))
           THEN BEGIN
                 IF CB_Sounds.Checked THEN MessageBeep(mb_OK);
                 MessageDlgPos(BadDiceMsg, mtInformation, [mbOK], 0,
                               Left+25, Top+25);
                 Ret := mrCancel;
                END
          ELSE BEGIN {potentially add to score}
                IF (CB_Strict.Checked) AND (KeptScore < DefMinScore)
                 THEN Ret := MessageDlgPos('You haven''t made the minimum ' +
                                           'score (' + IntToStr(DefMinScore) +
                                           ').'#10'Are you sure you want to ' +
                                           'end your turn?',
                                           mtConfirmation, [mbYes, mbNo], 0,
                                           Left+25, Top+25);
                IF Ret = mrYes
                 THEN BEGIN {score if can}
                       IF NOT(CB_Strict.Checked) OR (KeptScore >= DefMinScore)
                        THEN PlayerScores[CurrentPlayer] :=
                                      PlayerScores[CurrentPlayer] + KeptScore;
                      END;
               END;
         END;
   IF Ret = mrYes
    THEN BEGIN
          IF (CB_Sounds.Checked) AND (KeptCount = 0)
           THEN BEGIN
                 PlayExtraSound(F_Farkle_Snd);
                 Pause_Me(750);
                END;
          IF CB_VerboseOn.Checked
           THEN BEGIN
                 IF KeptCount > 0
                  THEN ShowScoreDialog
                 ELSE MessageDlgPos(F_Msg, mtInformation, [mbOK], 0,
                                    Left+25, Top+25);
                END;
          PostMessage(Handle, wm_NextPlayer, 0, 0);
         END;
  end;

(************************************************************************)

 procedure TFarkleWin.RolledDiceClick(Sender : TObject);
    VAR Which, I : INTEGER;
        Dice     : TDICE_ARRAY;
  begin
   Which := TDice(Sender).Tag;
   IF RolledDice[Which].FaceNumber <> dfn0
    THEN BEGIN {move down and potentially score?}
          Inc(KeptCount);
          KeptDice[Which].FaceNumber := RolledDice[Which].FaceNumber;
          RolledDice[Which].FaceNumber := dfn0;
          KeptDice[Which].Brush.Color := DiceColor;
          RolledDice[Which].Brush.Color := NoDiceColor;
          FOR I := 1 TO NumDice DO
           Dice[I] := INTEGER(KeptDice[I].FaceNumber);
          KeptScore := OldKeptScore + GetKeptScore(Dice, CB_FourTwo.Checked);
          ShowKeptScore;
          IF (KeptScore <> 0) AND (KeptScore <> OldKeptScore)
           THEN RollBtn.Enabled := TRUE
          ELSE RollBtn.Enabled := FALSE;
         END;
  end;

(************************************************************************)

 procedure TFarkleWin.KeptDiceClick(Sender : TObject);
    VAR Which, I : INTEGER;
        Dice     : TDICE_ARRAY;
  begin
   Which := TDice(Sender).Tag;
   IF KeptDice[Which].FaceNumber <> dfn0
    THEN BEGIN {move back up, reset kept score?}
          Dec(KeptCount);
          RolledDice[Which].FaceNumber := KeptDice[Which].FaceNumber;
          KeptDice[Which].FaceNumber := dfn0;
          RolledDice[Which].Brush.Color := DiceColor;
          KeptDice[Which].Brush.Color := NoDiceColor;
          FOR I := 1 TO NumDice DO
           Dice[I] := INTEGER(KeptDice[I].FaceNumber);
          KeptScore := OldKeptScore + GetKeptScore(Dice, CB_FourTwo.Checked);
          ShowKeptScore;
          IF (KeptScore <> 0) AND (KeptScore <> OldKeptScore)
           THEN RollBtn.Enabled := TRUE
          ELSE RollBtn.Enabled := FALSE;
         END;
  end;

(************************************************************************)

 procedure TFarkleWin.KeepBtnClick(Sender: TObject);

    VAR I : INTEGER;
    
  begin
   // move die down into kept dice
   FOR I := 1 TO NumDice DO
    IF RolledDice[I].FaceNumber <> dfn0
     THEN RolledDiceClick(TDice(RolledDice[I]));
  end;

(************************************************************************)

 procedure TFarkleWin.OvcN_ToSAfterExit(Sender : TObject);
  begin
   WinScore := OvcN_ToS.AsInteger;
  end;

(************************************************************************)

 PROCEDURE TFarkleWin.InitGameAndGlobals;

    VAR I : INTEGER;

  BEGIN
   KeepBtn.Enabled := FALSE; RollBtn.Enabled := FALSE; EndBtn.Enabled := FALSE;
   ClearRolled;
   ClearKept(TRUE);
   // initialize globals
   FirstPlayerWin := 0; CurrentPlayer := 0; WinningPlayer := 0;
   FOR I := 1 TO MaxPlayers DO
    PlayerScores[I] := 0;
   // get start player
   CurrentPlayer := Random(NumPlayers) + 1;
   MessageDlgPos(Players[CurrentPlayer] + ' will start.', mtInformation,
                 [mbOK], 0, Left+20, Top+20);
   // display player board
   DisplayPlayerPanel;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.DisplayPlayerPanel;

    VAR I : INTEGER;

  BEGIN
   FOR I := 1 TO MaxPlayers DO
    BEGIN
     IF CurrentPlayer = I
      THEN PlayerDisplay[I].Color := CurPlayerColor
     ELSE IF (FirstPlayerWin = I) AND (WinningPlayer = 0)
           THEN PlayerDisplay[I].Color := FirstPlayerClr
          ELSE IF WinningPlayer = I
                THEN PlayerDisplay[I].Color := WonPlayerColor
               ELSE PlayerDisplay[I].Color := DefPlayerColor;
     IF Players[I] <> ''
      THEN PlayerDisplay[I].Caption := Format(PlayerDispFmt,
                                              [Players[I], PlayerScores[I]])
     ELSE PlayerDisplay[I].Caption := '';
    END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.ShowKeptScore;

  BEGIN
   KeptScoreL.Caption := '[' + IntToStr(KeptScore) + ']';
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.ShowScoreDialog;

  BEGIN
   MessageDlgPos(Players[CurrentPlayer] + ' got ' + IntToStr(KeptScore) +
                 ' points.', mtInformation, [mbOK], 0, Left+25, Top+25);
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.ClearKept(IncScore : BOOLEAN);

    VAR I : INTEGER;

  BEGIN
   FOR I := 1 TO NumDice DO
    BEGIN
     KeptDice[I].FaceNumber := dfn0;
     KeptDice[I].Brush.Color := NoDiceColor;
    END;
   IF IncScore
    THEN BEGIN
          KeptScore := 0; OldKeptScore := 0;
          ShowKeptScore;
         END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.ClearRolled;

    VAR I : INTEGER;

  BEGIN
   FOR I := 1 TO NumDice DO
    BEGIN
     RolledDice[I].FaceNumber := dfn0;
     RolledDice[I].Brush.Color := NoDiceColor;
    END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.DisplayDice(Dice : TDICE_ARRAY);

    VAR I : INTEGER;

  BEGIN
   FOR I := 1 TO NumDice DO
    BEGIN
     RolledDice[I].FaceNumber := TDiceFace(Dice[I]);
     IF Dice[I] <> 0
      THEN RolledDice[I].Brush.Color := DiceColor
     ELSE RolledDice[I].Brush.Color := NoDiceColor;
    END;
   Application.ProcessMessages;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.DisplayCompMove(KDice : TDICE_ARRAY; VAR N : INTEGER);

    VAR I : INTEGER;

  BEGIN
   FOR I := 1 TO NumDice DO
    IF KDice[I] <> 0
     THEN BEGIN {switch to kept dice area}
           KeptDice[I].FaceNumber := TDiceFace(KDice[I]);
           RolledDice[I].FaceNumber := dfn0;
           KeptDice[I].Brush.Color := DiceColor;
           RolledDice[I].Brush.Color := NoDiceColor;
          END;
   ShowKeptScore;
   // count how many we'll roll next time, if rolling
   N := 0;
   FOR I := 1 TO NumDice DO
    IF RolledDice[I].FaceNumber <> dfn0 THEN Inc(N);
   IF N = 0 THEN N := 6;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.Pause_Me(Millisecs : INTEGER);

    VAR I, J : INTEGER;

  BEGIN
   I := 50; J := Millisecs;
   WHILE (J > 0) DO
    BEGIN
     Sleep(I);
     Application.ProcessMessages;
     J := J - I;
     IF I > J THEN I := J;
    END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.PauseMe(Seconds : INTEGER);

  BEGIN
   Pause_Me(Seconds * 1000);
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.PlayExtraSound(Which : INTEGER);

    VAR Fn : ARRAY[0..255] OF CHAR;

  BEGIN
   IF (CB_Sounds.Checked) AND (Which <= NumFiles)
    THEN BEGIN
          StrPCopy(Fn, SoundFiles[Which]);
          IF SoundAvail[Which]
           THEN PlaySound(Fn, 0, SND_FILENAME + SND_ASYNC);
         END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.Read_Reg;

    VAR Reg : TREGISTRY;
        T1  : BOOLEAN;
        T2  : LONGINT;
        I   : INTEGER;
        SS  : CPLAYSTR;

  BEGIN
   Reg := NIL;
   TRY {to open registry and read it}
    Reg := TREGISTRY.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    IF Reg.OpenKey(Reg_Key, FALSE)
     THEN BEGIN {read values}
           TRY T1 := Reg.ReadBool(Ini_1K1); EXCEPT T1 := CB_VerboseOn.Checked; END;
           CB_VerboseOn.Checked := T1;
           TRY T1 := Reg.ReadBool(Ini_1K2); EXCEPT T1 := CB_Sounds.Checked; END;
           CB_Sounds.Checked := T1;
           TRY T1 := Reg.ReadBool(Ini_1K3); EXCEPT T1 := CB_Strict.Checked; END;
           CB_Strict.Checked := T1;
           TRY T1 := Reg.ReadBool(Ini_1K4); EXCEPT T1 := CB_Safe.Checked; END;
           CB_Safe.Checked := T1;
           TRY T1 := Reg.ReadBool(Ini_1K5); EXCEPT T1 := CB_FourTwo.Checked; END;
           CB_FourTwo.Checked := T1;
           TRY T1 := Reg.ReadBool(Ini_1K6); EXCEPT T1 := CB_Sort.Checked; END;
           CB_Sort.Checked := T1;
           TRY T2 := Reg.ReadInteger(Ini_1K7); EXCEPT T2 := -1; END;
           IF T2 <> -1 THEN WinScore := T2;
           FOR I := 1 TO MaxPlayers DO
            BEGIN
             TRY SS := Reg.ReadString(Ini_1K8 + IntToStr(I)); EXCEPT SS := ''; END;
             Players[I] := SS;
            END;
           IF Players[1] <> '' THEN DisplayPlayerPanel;
          END; {then}
   FINALLY
    IF Reg <> NIL THEN Reg.Free;
   END; {try}
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.Write_Reg;

    VAR Reg : TREGISTRY;
        I   : INTEGER;
        SS  : CPLAYSTR;

  BEGIN
   Reg := NIL;
   TRY {to open up registry}
    Reg := TREGISTRY.Create;
    Reg.RootKey := HKEY_CURRENT_USER;
    IF Reg.OpenKey(Reg_Key, TRUE)
     THEN BEGIN {write out all this stuff}
           TRY Reg.WriteBool(Ini_1K1, CB_VerboseOn.Checked); EXCEPT END;
           TRY Reg.WriteBool(Ini_1K2, CB_Sounds.Checked); EXCEPT END;
           TRY Reg.WriteBool(Ini_1K3, CB_Strict.Checked); EXCEPT END;
           TRY Reg.WriteBool(Ini_1K4, CB_Safe.Checked); EXCEPT END;
           TRY Reg.WriteBool(Ini_1K5, CB_FourTwo.Checked); EXCEPT END;
           TRY Reg.WriteBool(Ini_1K6, CB_Sort.Checked); EXCEPT END;
           TRY Reg.WriteInteger(Ini_1K7, WinScore); EXCEPT END;
           FOR I := 1 TO MaxPlayers DO
            BEGIN
             SS := Players[I];
             IF (I = 2) AND (CompPlayerTwo) THEN SS := '';
             IF SS <> ''
              THEN TRY Reg.WriteString(Ini_1K8 + IntToStr(I), Players[I]);
                    EXCEPT END
             ELSE TRY Reg.DeleteValue(Ini_1K8 + IntToStr(I)); EXCEPT END;
            END;
          END; {then}
   FINALLY
    IF Reg <> NIL THEN Reg.Free;
   END; {try}
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.LoadNameList;

    VAR PF   : TEXTFILE;
        PFn  : STRING[255];
        PlyN : CPLAYSTR;
        I    : INTEGER;

  BEGIN
   PFn := Exe_Path + NamesFn;
   IF FileExists(PFn)
    THEN BEGIN {potentially load from file}
          AssignFile(PF, PFn);
          {$I-} Reset(PF); {$I+}
          IF IOResult = 0
           THEN BEGIN {looking good}
                 WHILE NOT(EOF(PF)) DO
                  BEGIN
                   Readln(PF, PlyN);
                   PlyN := Trim(PlyN);
                   IF PlyN <> '' THEN CompPlyrNames.Add(PlyN);
                  END;
                 CloseFile(PF);
                END; {then}
         END; {then}
   IF CompPlyrNames.Count = 0
    THEN BEGIN {load defaults}
          FOR I := 1 TO MaxCompNames DO
           CompPlyrNames.Add(CompPlayerNms[I]);
         END; {then}
  END;

(************************************************************************)

 FUNCTION TFarkleWin.CheckScore(KDice : TDICE_ARRAY) : BOOLEAN;
    VAR RDice                       : TDICE_ARRAY;
        KScore, AScore, Dif, Ret, I : INTEGER;
  BEGIN
   Result := TRUE;
   IF CB_Safe.Checked
    THEN BEGIN
          FOR I := 1 TO NumDice DO
           RDice[I] := INTEGER(RolledDice[I].FaceNumber);
          KScore := GetKeptScore(KDice, CB_FourTwo.Checked);
          AScore := GetTotalScore(RDice, KDice, CB_FourTwo.Checked);
          Dif := AScore - KScore;
          IF Dif > SafetyScoreDif
           THEN BEGIN
                 Ret := MessageDlgPos('You''ve left ' + IntToStr(Dif) +
                                      ' points behind.'#10'Are you sure you ' +
                                      'want to roll?',
                                      mtConfirmation, [mbYes, mbNo], 0,
                                      Left+25, Top+25);
                 Result := Ret = mrYes;
                END;
         END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.WMStartRound(VAR Msg : TMESSAGE);

  BEGIN
   KeepBtn.Enabled := FALSE; RollBtn.Enabled := TRUE;
   EndBtn.Enabled := TRUE; EndBtn.SetFocus;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.WMCompMove(VAR Msg : TMESSAGE);

    VAR Dice, KDice  : TDICE_ARRAY;
        RollAgn      : BOOLEAN;
        N, Tally, WP : INTEGER;

  BEGIN
   IF FirstPlayerWin > 0
    THEN BEGIN
          WP := PlayerScores[FirstPlayerWin] - PlayerScores[CurrentPlayer] + 50;
          IF (CB_Strict.Checked) AND (WP < DefMinScore) THEN WP := DefMinScore;
         END
   ELSE WP := 0;
   KeepBtn.Enabled := FALSE; RollBtn.Enabled := FALSE; EndBtn.Enabled := FALSE;
   RollAgn := TRUE; N := 6; Tally := 0;
   WHILE RollAgn DO
    BEGIN
     ClearKept(NOT(CB_Strict.Checked) AND (N = 6));
     IF CB_Sounds.Checked THEN PlayExtraSound(F_Roll_Snd);
     Pause_Me(500);
     IF CB_Sort.Checked THEN RollDiceSorted(Dice, N) ELSE RollDice(Dice, N);
     DisplayDice(Dice);
     PauseMe(1);
     PlayCompMove(Dice, KDice, RollAgn, KeptScore, N, FirstPlayerWin, WP,
                  CB_Strict.Checked, CB_FourTwo.Checked);
     DisplayCompMove(KDice, N);
     IF KeptScore = 0
      THEN BEGIN
            IF CB_Sounds.Checked THEN PlayExtraSound(F_Farkle_Snd);
            IF CB_VerboseOn.Checked
             THEN MessageDlgPos(F_Msg, mtInformation, [mbOK], 0, Left+25, Top+25)
            ELSE Pause_Me(2500);
           END
     ELSE Pause_Me(2500);
     IF (RollAgn) AND (N = 6) AND NOT(CB_Strict.Checked)
      THEN BEGIN
            PlayerScores[CurrentPlayer] := PlayerScores[CurrentPlayer] +
                                           KeptScore;
            Tally := Tally + KeptScore;
            DisplayPlayerPanel;
           END;
    END;
   IF KeptScore > 0
    THEN PlayerScores[CurrentPlayer] := PlayerScores[CurrentPlayer] + KeptScore;
   IF Tally > 0 THEN KeptScore := Tally + KeptScore;
   IF (CB_VerboseOn.Checked) THEN ShowScoreDialog;
   PostMessage(Handle, wm_NextPlayer, 0, 0);
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.WMNextPlayer(VAR Msg : TMESSAGE);

    VAR I : INTEGER;

  BEGIN
   ClearRolled;
   ClearKept(TRUE); KeptCount := 0;
   // check if prev player was first to go over winning score
   IF (FirstPlayerWin = 0) AND (PlayerScores[CurrentPlayer] >= WinScore)
    THEN FirstPlayerWin := CurrentPlayer;
   // continue with processing for next player
   Inc(CurrentPlayer);
   IF CurrentPlayer > NumPlayers THEN CurrentPlayer := 1;
   IF CurrentPlayer = FirstPlayerWin
    THEN BEGIN {have a winner - someone!}
          KeepBtn.Enabled := FALSE; RollBtn.Enabled := FALSE;
          EndBtn.Enabled := FALSE;
          WinningPlayer := 1; FirstPlayerWin := 0; CurrentPlayer := 0;
          // who's the winner...
          FOR I := 2 TO NumPlayers DO
           IF PlayerScores[I] > PlayerScores[WinningPlayer]
            THEN WinningPlayer := I;
          IF CB_Sounds.Checked THEN MessageBeep(mb_OK);
          MessageDlgPos(Players[WinningPlayer] + ' has won!', mtInformation,
                        [mbOK], 0, Left+25, Top+25);
          NewBtn.SetFocus;
         END;
   DisplayPlayerPanel;
   IF (WinningPlayer = 0)
    THEN BEGIN
          IF (CB_VerboseOn.Checked)
           THEN MessageDlgPos('It''s ' + Players[CurrentPlayer] + ' turn.',
                              mtInformation, [mbOK], 0, Left+25, Top+25);
          IF (CompPlayerTwo) AND (CurrentPlayer = 2)
           THEN PostMessage(Handle, wm_CompMove, 0, 0)
          ELSE BEGIN
                KeepBtn.Enabled := FALSE; RollBtn.Enabled := TRUE;
                EndBtn.Enabled := TRUE; RollBtn.SetFocus;
               END;
         END;
  END;

(************************************************************************)

 PROCEDURE TFarkleWin.WMSysCommand(VAR Msg : TMESSAGE);

  BEGIN
   IF Msg.wParam = sc_About
    THEN BEGIN {about selection tapped on - do it}
          AboutDlg := TAboutDlg.Create(Self);
          CenterPopUp(AboutDlg, Handle);
          AboutDlg.ShowModal;
          AboutDlg.Free;
         END; {then}
   INHERITED;
  END;

(************************************************************************)

end.

