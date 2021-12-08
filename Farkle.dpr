program Farkle;

(*                                                                      *)
(* AUTHOR: Michael G. Slack                    DATE WRITTEN: 2009-01-03 *)
(* ENVIRONMENT: Delphi v3                                               *)
(*                                                                      *)
(* Program defining a game of Farkle.  Allows for up to 6 players or    *)
(* against a computer player (single player mode).  Uses a simplified   *)
(* or a strict scoring method, user selected.                           *)
(*                                                                      *)
(* -------------------------------------------------------------------- *)
(*                                                                      *)
(* MODIFIED: 2009-01-17 - Completed first release.  (1.0.0.0)           *)
(*           2009-01-30 - Updated with several more options (safe mode, *)
(*                        sort roll) and registry load/save of options. *)
(*                        (1.1.0.0)                                     *)
(*           2009-02-01 - Added dynamic date and version labels to the  *)
(*                        about dialog (date from exe create date,      *)
(*                        version from ver_resource).  (1.1.1.0)        *)
(*           2009-04-20 - Added code to read 'computer names' from file *)
(*                        (if available).  Other updates.  (1.2.0.0)    *)
(*           2010-06-29 - Fixed 'help' to open.  Needed to push button  *)
(*                        twice before.  Added 'sw_ShowNormal' to call  *)
(*                        which documentation said was not needed.      *)
(*                        (1.3.0.0)                                     *)
(*           2014-02-06 - Rolled in the check score routine to warn     *)
(*                        players if they are throwing away a lot of    *)
(*                        points, if save mode is on. (1.4.0.0)         *)
(*           2020-06-20 - Fixed bug in comp score method.  (1.5.0.0)    *)
(*           2021-01-28 - Added 'Keep All' button to keep all rolled    *)
(*                        dice.  (1.6.0.0)                              *)
(*           2021-03-08 - Updated about box slightly.  (1.7.0.0)        *)
(*                                                                      *)

 uses Forms,
      Fark_Win in 'Fark_Win.pas' {FarkleWin},
      FarkPDlg in 'FarkPDlg.pas' {PlayerDlg},
      FarkADlg in 'FarkADlg.pas' {AboutDlg},
      FarkGame in 'FarkGame.pas',
      FarkCnst in 'FarkCnst.pas';

{$R *.RES}

begin
 Application.Initialize;
 Application.Title := 'Farkle';
 Application.CreateForm(TFarkleWin, FarkleWin);
 Application.Run;
end.

