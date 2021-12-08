unit FarkPDlg;

(*                                                                      *)
(* Unit defines the player name input dialog for the game of Farkle.    *)
(*                                                                      *)

interface

 uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
      Buttons, ExtCtrls, OvcBase, OvcEF, OvcSF, FarkCnst;

 type TPlayerDlg = class(TForm)
    Label2    : TLabel;
    Bevel1    : TBevel;
    Label1    : TLabel;
    OvcSF_P1  : TOvcSimpleField;
    Label3    : TLabel;
    OvcSF_P2  : TOvcSimpleField;
    Label4    : TLabel;
    OvcSF_P3  : TOvcSimpleField;
    Label5    : TLabel;
    OvcSF_P4  : TOvcSimpleField;
    Label6    : TLabel;
    OvcSF_P5  : TOvcSimpleField;
    Label7    : TLabel;
    OvcSF_P6  : TOvcSimpleField;
    OkBtn     : TBitBtn;
    CancelBtn : TBitBtn;
    OvcCntrl  : TOvcController;
    procedure FormCreate(Sender : TObject);
    procedure FormShow(Sender : TObject);
    procedure OkBtnClick(Sender : TObject);
   private
    PlayerArr  : ARRAY[1..MaxPlayers] OF TOvcSimpleField;
   public
    Players    : PLAYER_ARRAY;
    NumPlayers : INTEGER;
    SoundsOn,
    CompPlayer : BOOLEAN;
   end;

 var PlayerDlg : TPlayerDlg;

(************************************************************************)

implementation

{$R *.DFM}

 USES Dialogs;

(************************************************************************)

 procedure TPlayerDlg.FormCreate(Sender : TObject);
    VAR I : INTEGER;
  begin
   PlayerArr[1] := OvcSF_P1;
   PlayerArr[2] := OvcSF_P2;
   PlayerArr[3] := OvcSF_P3;
   PlayerArr[4] := OvcSF_P4;
   PlayerArr[5] := OvcSF_P5;
   PlayerArr[6] := OvcSF_P6;
   NumPlayers := 0; CompPlayer := FALSE; SoundsOn := TRUE;
   FOR I := 1 TO MaxPlayers DO
    Players[I] := '';
  end;

(************************************************************************)

 procedure TPlayerDlg.FormShow(Sender : TObject);
    VAR I : INTEGER;
  begin
   FOR I := 1 TO MaxPlayers DO
    PlayerArr[I].AsString := Players[I];
  end;

(************************************************************************)

 procedure TPlayerDlg.OkBtnClick(Sender : TObject);
    VAR I : INTEGER;
        S : STRING[100];
  begin
   S := '';
   FOR I := 1 TO MaxPlayers DO
    Players[I] := PlayerArr[I].AsString;
   // validate players
   IF Players[1] = ''
    THEN BEGIN
          S := 'Must enter name in first name field.';
          PlayerArr[1].SetFocus;
         END;
   IF S = ''
    THEN FOR I := MaxPlayers DOWNTO 2 DO
          IF (Players[I] <> '') AND (Players[I-1] = '')
           THEN BEGIN
                 S := 'Cannot skip name fields for player names.';
                 PlayerArr[I].SetFocus;
                 Break;
                END;
   // check if computer is playing, will be if only one name entered
   IF (S = '') AND (Players[2] = '')
    THEN BEGIN {computer player}
          CompPlayer := TRUE;
          I := Random(CompPlyrNames.Count);
          Players[2] := CompPlyrNames[I];
         END;
   // get number of players (will be at least 2)
   NumPlayers := 2;
   FOR I := MaxPlayers DOWNTO 3 DO
    IF Players[I] <> ''
     THEN BEGIN
           NumPlayers := I; Break;
          END;
   // validated, get out of here, else error!
   IF S = ''
    THEN ModalResult := mrOK
   ELSE BEGIN
         IF SoundsOn THEN MessageBeep(mb_OK);
         MessageDlgPos(S, mtInformation, [mbOK], 0, Left+25, Top+30);
        END;
  end;

(************************************************************************)

end.

