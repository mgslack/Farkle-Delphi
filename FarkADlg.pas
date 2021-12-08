unit FarkADlg;

(*                                                                      *)
(* Unit defines the 'about' dialog used in the game of Farkle.          *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* REVISED: 2021-03-08 - Changed GetFileCreateDate with call to         *)
(*                       GetFileLastModify in version date label.       *)
(*                       This gives a better date on when exe was last  *)
(*                       compiled than create date, which may be date   *)
(*                       copied to disk.                                *)
(*                                                                      *)

interface

 uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
      Buttons, ExtCtrls, StBase, StVInfo;

 type TAboutDlg = class(TForm)
    Panel1      : TPanel;
    AboutImg    : TImage;
    TitleLbl    : TLabel;
    AuthorLbl   : TLabel;
    VersionLblT : TLabel;
    VersionLbl  : TLabel;
    DateLbl     : TLabel;
    OkBtn       : TBitBtn;
    StVerInfo   : TStVersionInfo;
    procedure FormShow(Sender : TObject);
   private
    { Private declarations }
   public
    { Public declarations }
   end;

 var AboutDlg : TAboutDlg;

(************************************************************************)

implementation

{$R *.DFM}

 USES StSystem;

(************************************************************************)

 procedure TAboutDlg.FormShow(Sender : TObject);
    VAR AppFn : STRING[255];
  begin
   AppFn := ParamStr(0);
   StVerInfo.FileName := AppFn;
   VersionLbl.Caption := StVerInfo.FileVersion;
   //DateLbl.Caption := FormatDateTime('mmmm yyyy', GetFileCreateDate(AppFn));
   DateLbl.Caption := FormatDateTime('mmmm yyyy', GetFileLastModify(AppFn));
  end;

(************************************************************************)

end.

